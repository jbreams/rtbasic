#include <algorithm>
#include <iostream>

#include "llvm/ADT/APFloat.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"

#include "ast.h"

// CreateEntryBlockAlloca - creates an alloca instruction in the entry block
// of the function. Used for mutable vars etc
llvm::AllocaInst* CreateEntryBlockAlloca(BasicContext* ctx,
                                         const std::string& name,
                                         llvm::Type* type) {
    auto curFunction = ctx->getCurrentFunction();
    auto& entryBlock = curFunction->getEntryBlock();
    llvm::IRBuilder<> tmp(&entryBlock, entryBlock.begin());
    return tmp.CreateAlloca(type, 0, name.c_str());
}

llvm::Type* astTypeToNativeType(BasicContext* ctx, VariableType type) {
    switch (type) {
        case Double:
            return llvm::Type::getDoubleTy(ctx->context);
        case String:
            return llvm::Type::getInt8PtrTy(ctx->context);
        case Integer:
            return llvm::Type::getInt64Ty(ctx->context);
        case Void:
            return llvm::Type::getVoidTy(ctx->context);
    }
}

llvm::Function* BasicContext::getCurrentFunction() {
    if (currentFunction)
        return currentFunction->function();
    else
        return mainFunction->function();
}

llvm::Function* BasicContext::getFunction(const std::string& name) {
    return module->getFunction(name);
}

bool BasicContext::codegenAllProtos() {
    if (!mainFunction->proto()->codegen()) {
        std::cerr << "Error generating prototype for main";
        return false;
    }

    for (auto proto : externFunctions) {
        if (!module->getFunction(proto.second->name()) && !proto.second->codegen()) {
            std::cerr << "Error generating prototype for " << proto.first;
            return false;
        }
    }

    return true;
}

llvm::Constant* makeLiteralDouble(BasicContext* ctx, double val) {
    return llvm::ConstantFP::get(ctx->context, llvm::APFloat(val));
}

llvm::Constant* makeLiteralInteger(BasicContext* ctx, int64_t val) {
    return llvm::ConstantInt::getSigned(ctx->builder.getInt64Ty(), val);
}

llvm::Value* StatementAST::codegen() {
    return nullptr;
}

const std::string& LabelAST::name() const {
    return token().strValue;
}

llvm::Value* LabelAST::value() {
    if (!_value) {
        _value = llvm::BasicBlock::Create(ctx()->context, token().strValue);
    }
    return _value;
}

llvm::Value* LabelAST::codegen() {
    auto& builder = ctx()->builder;
    auto& labelInfo = ctx()->labels.at(name());
    if (labelInfo.gotos == 0) {
        return nullptr;
    }

    auto curBB = builder.GetInsertBlock();
    auto newBB = static_cast<llvm::BasicBlock*>(value());
    ctx()->getCurrentFunction()->getBasicBlockList().insertAfter(curBB->getIterator(), newBB);

    if (curBB && !curBB->getTerminator()) {
        builder.CreateBr(newBB);
    }

    builder.SetInsertPoint(newBB);
    return _value;
}

llvm::Value* LineAST::codegen() {
    LabelAST* label = static_cast<LabelAST*>(_label.get());
    if (label && !label->codegen() && ctx()->labels.at(label->name()).gotos > 0)
        return nullptr;

    if (_statement) {
        return _statement->codegen();
    }
    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(ctx()->context));
}

llvm::Value* BlockAST::codegen() {
    llvm::Value* ret;

    for (auto& line : _lines) {
        if (!(ret = line->codegen())) {
            return nullptr;
        }
    }
    return ret;
}

llvm::Value* NumberExprAST::codegen() {
    if (token().tag == Token::Double) {
        return makeLiteralDouble(ctx(), boost::get<double>(token().value));
    } else if (token().tag == Token::Integer) {
        return makeLiteralInteger(ctx(), boost::get<int64_t>(token().value));
    } else {
        std::cerr << "Invalid token for number expr" << std::endl;
        return nullptr;
    }
}

llvm::Value* StringAST::codegen() {
    return ctx()->builder.CreateGlobalStringPtr(token().strValue);
}

llvm::Value* VariableExprAST::codegenLookup() {
    auto it = ctx()->namedVariables.find(_name);
    if (it == ctx()->namedVariables.end()) {
        std::cerr << "Could not find named value for " << _name << std::endl;
        return nullptr;
    }

    return it->second->lookup(_dimensions);
}

llvm::Value* VariableExprAST::codegen() {
    auto lookup = codegenLookup();
    if (!lookup)
        return nullptr;
    return ctx()->builder.CreateLoad(lookup, _name);
}

llvm::Value* VariableDeclartionAST::codegen() {
    if (_alloca)
        return _alloca;

    if (ctx()->namedVariables.find(name()) != ctx()->namedVariables.end()) {
        std::cerr << "Duplicate variable definition for " << name();
        return nullptr;
    }

    llvm::Type* type = nativeType();

    if (isGlobal()) {
        auto globalPtr = ctx()->module->getOrInsertGlobal(name(), type);
        auto global = ctx()->module->getGlobalVariable(name());
        if (!global->hasInitializer()) {
            llvm::Constant* initV;
            if (type->isDoubleTy()) {
                initV = makeLiteralDouble(ctx(), 0.0);
            } else if (type->isIntegerTy()) {
                initV = makeLiteralInteger(ctx(), 0);
            } else if (type == ctx()->builder.getInt8PtrTy()) {
                initV = llvm::ConstantPointerNull::get(llvm::Type::getInt8PtrTy(ctx()->context));
            } else if (!type->isArrayTy()) {
                std::cerr << "Cannot find initial value for variable " << name() << std::endl;
                return nullptr;
            }

            if (!dimensions().empty()) {
                initV = llvm::ConstantAggregateZero::get(type);
            }
            global->setInitializer(initV);
            global->setLinkage(llvm::GlobalValue::CommonLinkage);
        }

        ctx()->namedVariables[name()] = this;
        _alloca = globalPtr;
        return globalPtr;
    } else {
        ctx()->namedVariables[name()] = this;
        _alloca = CreateEntryBlockAlloca(ctx(), name(), type);
        return _alloca;
    }
}

llvm::Value* VariableDeclartionAST::lookup() const {
    return _alloca;
}

llvm::Value* VariableDeclartionAST::lookup(
    const std::vector<std::unique_ptr<ExprAST>>& dimensions) const {
    auto& builder = ctx()->builder;
    if (dimensions.empty()) {
        return lookup();
    } else {
        std::vector<llvm::Value*> generatedDimensions = {makeLiteralInteger(ctx(), 0)};
        for (const auto& dimension : dimensions) {
            generatedDimensions.push_back(dimension->codegen());
        }

        return builder.CreateInBoundsGEP(nativeType(), lookup(), generatedDimensions);
    }
}

llvm::Type* DimAST::nativeType() const {
    llvm::Type* type = astTypeToNativeType(ctx(), _type);

    for (auto dimension : _dimensions) {
        type = llvm::ArrayType::get(type, dimension);
    }
    return type;
}

llvm::Value* DimAST::codegen() {
    if (!VariableDeclartionAST::codegen()) {
        return nullptr;
    }

    return lookup();
}

const std::string& DimAST::name() const {
    return _name;
}

llvm::Type* LetAST::nativeType() const {
    return astTypeToNativeType(ctx(), _type);
}

llvm::Value* LetAST::codegen() {
    auto& builder = ctx()->builder;

    if (_name == ctx()->getCurrentFunction()->getName()) {
        if (!_value) {
            std::cerr << "Missing value in assignment used as return statement" << std::endl;
            return nullptr;
        }
        return builder.CreateRet(_value->codegen());
    }

    llvm::Value* alloca;
    if (ctx()->isNamedVariable(_name)) {
        alloca = _variableExpr->codegenLookup();
    } else {
        alloca = VariableDeclartionAST::codegen();
    }

    auto type = alloca->getType();
    if (type->isPointerTy()) {
        auto ptr = static_cast<llvm::PointerType*>(type);
        type = ptr->getPointerElementType();
    }

    auto value = _value->codegen();
    if (!value) {
        std::cerr << "Error generating value for assignment" << std::endl;
        return nullptr;
    }

    if (type->isDoubleTy() && value->getType() != ctx()->builder.getDoubleTy()) {
        value = ctx()->builder.CreateIntCast(value, ctx()->builder.getDoubleTy(), true);
    } else if (type->isIntegerTy() && value->getType() != ctx()->builder.getInt64Ty()) {
        value = ctx()->builder.CreateFPCast(value, ctx()->builder.getInt64Ty());
    }
    if (!value) {
        std::cerr << "Error casting value for assignment" << std::endl;
        return nullptr;
    }

    return builder.CreateStore(value, alloca);
}

llvm::Value* BinaryExprAST::codegen() {
    auto l = _lhs->codegen();
    auto r = _rhs->codegen();
    if (!l || !r) {
        std::cerr << "Error evaluating operands for binary operator" << std::endl;
        return nullptr;
    }

    if (l->getType() != r->getType()) {
        std::cerr << "Type for LHS " << l->getType() << " is not the same as the RHS "
                  << r->getType() << std::endl;
        return nullptr;
    }
    bool intVal = l->getType()->isIntegerTy();

    auto& builder = ctx()->builder;
    switch (token().tag) {
        case Token::Plus:
            if (intVal)
                return builder.CreateAdd(l, r, "addtmp");
            else
                return builder.CreateFAdd(l, r, "addtmp");
        case Token::Minus:
            if (intVal)
                return builder.CreateSub(l, r, "subtmp");
            else
                return builder.CreateFSub(l, r, "subtmp");
        case Token::Multiply:
            if (intVal)
                return builder.CreateMul(l, r, "multmp");
            else
                return builder.CreateFMul(l, r, "multmp");
        case Token::Divide:
            if (intVal)
                return builder.CreateSDiv(l, r, "divtmp");
            else
                return builder.CreateFDiv(l, r, "divtmp");
        case Token::Exp: {
            auto powFunction = ctx()->getFunction("pow");
            if (!powFunction) {
                std::cerr << "No POW function found" << std::endl;
                return nullptr;
            }
            llvm::Value* ops[] = {l, r};
            return builder.CreateCall(powFunction, ops, "powop");
        }
        default:
            std::cerr << "Invalid token for binary expression" << std::endl;
            return nullptr;
    }
}

llvm::Value* RelOpExprAST::codegen() {
    auto l = _lhs->codegen();
    auto r = _rhs->codegen();
    if (!l || !r) {
        std::cerr << "Error exaluating operands for comparison operator" << std::endl;
        return nullptr;
    }

    if (l->getType() != r->getType()) {
        std::cerr << "Type for LHS " << l->getType() << " is not the same as the RHS "
                  << r->getType() << std::endl;
        return nullptr;
    }
    bool intVal = l->getType()->isIntegerTy();

    auto& builder = ctx()->builder;

    llvm::Value* res;
    switch (token().tag) {
        case Token::Gt:
            if (intVal)
                res = builder.CreateICmpSGT(l, r, "cmptmp");
            else
                res = builder.CreateFCmpUGT(l, r, "cmptmp");
            break;
        case Token::Gte:
            if (intVal)
                res = builder.CreateICmpSGE(l, r, "cmptmp");
            else
                res = builder.CreateFCmpUGE(l, r, "cmptmp");
            break;
        case Token::Lt:
            if (intVal)
                res = builder.CreateICmpSLT(l, r, "cmptmp");
            else
                res = builder.CreateFCmpULT(l, r, "cmptmp");
            break;
        case Token::Lte:
            if (intVal)
                res = builder.CreateICmpSLE(l, r, "cmptmp");
            else
                res = builder.CreateFCmpULE(l, r, "cmptmp");
            break;
        case Token::Neq:
            if (intVal)
                res = builder.CreateICmpNE(l, r, "cmptmp");
            else
                res = builder.CreateFCmpUNE(l, r, "cmptmp");
            break;
        case Token::Eq:
            if (intVal)
                res = builder.CreateICmpEQ(l, r, "cmptmp");
            else
                res = builder.CreateFCmpUEQ(l, r, "cmptmp");
            break;
        default:
            std::cerr << "Invalid token for comparison operator" << std::endl;
            return nullptr;
    }

    return res;
}

llvm::Value* GotoAST::codegen() {
    auto& builder = ctx()->builder;
    auto labelIt = ctx()->labels.find(_label);
    if (labelIt == ctx()->labels.end() || !labelIt->second.label) {
        std::cerr << "Could not find label for goto statement: " << _label;
        return nullptr;
    }
    builder.CreateBr(static_cast<llvm::BasicBlock*>(labelIt->second.label->value()));
    auto newBB = llvm::BasicBlock::Create(ctx()->context, "afterGoto", ctx()->getCurrentFunction());
    builder.SetInsertPoint(newBB);
    return newBB;
}

llvm::Value* GosubAST::codegen() {
    std::cerr << "Gosubs aren't a real codegen type" << std::endl;
    return nullptr;
}

llvm::Value* ReturnAST::codegen() {
    return ctx()->builder.CreateRetVoid();
}

llvm::Value* WhileAST::codegen() {
    auto& builder = ctx()->builder;

    auto loopBB = llvm::BasicBlock::Create(ctx()->context, "loop");
    auto condBB = llvm::BasicBlock::Create(ctx()->context, "while");
    auto afterBB = llvm::BasicBlock::Create(ctx()->context, "afterWhile");
    builder.CreateBr(condBB);

    ctx()->getCurrentFunction()->getBasicBlockList().push_back(condBB);
    builder.SetInsertPoint(condBB);
    auto condResult = _condExpr->codegen();
    if (!condResult)
        return nullptr;

    if (_isNot) {
        builder.CreateCondBr(condResult, afterBB, loopBB);
    } else {
        builder.CreateCondBr(condResult, loopBB, afterBB);
    }

    ctx()->getCurrentFunction()->getBasicBlockList().push_back(loopBB);
    builder.SetInsertPoint(loopBB);
    if (!_body->codegen())
        return nullptr;
    builder.CreateBr(condBB);

    ctx()->getCurrentFunction()->getBasicBlockList().push_back(afterBB);
    builder.SetInsertPoint(afterBB);
    return condResult;
}

llvm::Value* DoAST::codegen() {
    auto& builder = ctx()->builder;

    auto loopBB = llvm::BasicBlock::Create(ctx()->context, "loop");
    auto condBB = llvm::BasicBlock::Create(ctx()->context, "until");
    auto afterBB = llvm::BasicBlock::Create(ctx()->context, "afterDo");

    builder.CreateBr(loopBB);
    ctx()->getCurrentFunction()->getBasicBlockList().push_back(loopBB);
    builder.SetInsertPoint(loopBB);
    if (!_body->codegen())
        return nullptr;

    builder.CreateBr(condBB);

    ctx()->getCurrentFunction()->getBasicBlockList().push_back(condBB);
    builder.SetInsertPoint(condBB);
    auto condResult = _condExpr->codegen();
    if (!condResult)
        return nullptr;

    builder.CreateCondBr(condResult, afterBB, loopBB);

    ctx()->getCurrentFunction()->getBasicBlockList().push_back(afterBB);
    builder.SetInsertPoint(afterBB);
    return condResult;
}

llvm::Value* ForAST::codegen() {
    auto& builder = ctx()->builder;
    auto controlVar = static_cast<LetAST*>(_controlVar.get());

    if (!controlVar->codegen()) {
        std::cerr << "Error generating control variable" << std::endl;
        return nullptr;
    }
    auto controlVarIsInt = controlVar->nativeType()->isIntegerTy();

    auto loopBB = llvm::BasicBlock::Create(ctx()->context, "loop");

    builder.CreateBr(loopBB);
    ctx()->getCurrentFunction()->getBasicBlockList().push_back(loopBB);
    builder.SetInsertPoint(loopBB);

    auto endV = _toExpr->codegen();
    if (!endV) {
        std::cerr << "Error generating end condition for for loop" << std::endl;
        return nullptr;
    }

    auto curVar = builder.CreateLoad(controlVar->lookup());
    if (controlVarIsInt) {
        endV = builder.CreateICmpSLT(curVar, endV, "loopcond");
    } else {
        endV = builder.CreateFCmpOLT(curVar, endV, "loopcond");
    }

    auto bodyBB = llvm::BasicBlock::Create(ctx()->context, "loopbody", ctx()->getCurrentFunction());
    auto afterBB = llvm::BasicBlock::Create(ctx()->context, "afterloop");
    builder.CreateCondBr(endV, bodyBB, afterBB);
    builder.SetInsertPoint(bodyBB);

    if (!_bodyExpr->codegen()) {
        std::cerr << "Error generating for loop body" << std::endl;
        return nullptr;
    }

    curVar = builder.CreateLoad(controlVar->lookup());
    llvm::Value* stepV = nullptr;
    if (_stepExpr) {
        stepV = _stepExpr->codegen();
        if (!stepV) {
            std::cerr << "Error generating step expression" << std::endl;
            return nullptr;
        }
    } else {
        if (controlVarIsInt) {
            stepV = makeLiteralInteger(ctx(), 1.0);
        } else {
            stepV = makeLiteralDouble(ctx(), 1);
        }
    }

    llvm::Value* nextVar;
    if (controlVarIsInt) {
        nextVar = builder.CreateAdd(curVar, stepV, "nextvar");
    } else {
        nextVar = builder.CreateFAdd(curVar, stepV, "nextvar");
    }
    builder.CreateStore(nextVar, controlVar->lookup());
    builder.CreateBr(loopBB);

    ctx()->getCurrentFunction()->getBasicBlockList().push_back(afterBB);
    builder.SetInsertPoint(afterBB);

    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(ctx()->context));
}


llvm::Value* IfAST::codegen() {
    auto cond = _condition->codegen();
    if (!cond) {
        std::cerr << "Error generating conditional for if" << std::endl;
        return nullptr;
    }

    auto& builder = ctx()->builder;
    auto curFunction = ctx()->getCurrentFunction();
    auto thenBB = llvm::BasicBlock::Create(ctx()->context, "then", curFunction);
    auto elseBB = llvm::BasicBlock::Create(ctx()->context, "else");
    auto mergeBB = llvm::BasicBlock::Create(ctx()->context, "ifcont");
    llvm::Value *thenV = nullptr, *elseV = nullptr;

    if (_else) {
        builder.CreateCondBr(cond, thenBB, elseBB);
    } else {
        builder.CreateCondBr(cond, thenBB, mergeBB);
    }
    builder.SetInsertPoint(thenBB);
    if (!(thenV = _then->codegen())) {
        std::cerr << "Error generating then for if statement" << std::endl;
        return nullptr;
    }

    thenBB = builder.GetInsertBlock();
    if (!thenBB->getTerminator()) {
        builder.CreateBr(mergeBB);
    }

    if (_else) {
        curFunction->getBasicBlockList().push_back(elseBB);
        builder.SetInsertPoint(elseBB);
        if (!(elseV = _else->codegen())) {
            std::cerr << "Error generating else block for if statement" << std::endl;
            return nullptr;
        }

        if (!elseBB->getTerminator()) {
            builder.CreateBr(mergeBB);
        }
        elseBB = builder.GetInsertBlock();
    }

    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(mergeBB);
    }
    curFunction->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);

    return cond;
}

llvm::Value* RemarkAST::codegen() {
    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(ctx()->context));
}

llvm::Value* FunctionCallAST::codegen() {
    auto callee = ctx()->getFunction(_function);
    if (!callee) {
        std::cerr << "Unknown function call" << std::endl;
        return nullptr;
    }

    if (_args.size() < callee->arg_size() ||
        (_args.size() > callee->arg_size() && !callee->isVarArg())) {
        std::cerr << "Incorrect number of arguments for function call" << std::endl;
        return nullptr;
    }

    std::vector<llvm::Value*> args;
    for (auto& argExpr : _args) {
        args.push_back(argExpr->codegen());
        if (args.back() == nullptr) {
            std::cerr << "Error generating argument for function call" << std::endl;
            return nullptr;
        }
    }

    if (callee->getReturnType()->isVoidTy()) {
        return ctx()->builder.CreateCall(callee, args);
    } else {
        return ctx()->builder.CreateCall(callee, args, "calltmp");
    }
}

llvm::Type* ProtoDefAST::Argument::nativeType() const {
    return astTypeToNativeType(ctx(), type);
}

llvm::Value* ProtoDefAST::codegen() {
    if (auto preGenerated = ctx()->getFunction(_name)) {
        return preGenerated;
    }

    std::vector<llvm::Type*> args;
    for (const auto& arg : _args) {
        args.push_back(arg.nativeType());
    }

    llvm::Type* returnType = astTypeToNativeType(ctx(), _returnType);

    auto functionType = llvm::FunctionType::get(returnType, args, _isVarArg);

    _function = llvm::Function::Create(
        functionType, llvm::Function::ExternalLinkage, _name, ctx()->module.get());
    int idx = 0;
    for (auto& arg : _function->args()) {
        arg.setName(_args[idx].name());
    }

    return _function;
}

llvm::Function* FunctionAST::function() const {
    return _function;
}

llvm::Value* FunctionAST::codegen() {
    _function = ctx()->getFunction(_proto->name());
    if (!_function) {
        std::cerr << "Error finding prototype for function" << std::endl;
        return nullptr;
    }

    ctx()->currentFunction = this;
    auto bb = llvm::BasicBlock::Create(ctx()->context, "entry", _function);
    ctx()->builder.SetInsertPoint(bb);

    ctx()->clearNonGlobalVariables();
    for (auto& arg : _proto->args()) {
        if (!arg.codegen()) {
            return nullptr;
        }
    }
    auto codegenResult = _body->codegen();

    auto lastBlock = ctx()->builder.GetInsertBlock();
    if (!lastBlock->getTerminator()) {
        if (_proto->returnType() == Void)
            ctx()->builder.CreateRetVoid();
        else
            ctx()->builder.CreateRet(codegenResult);
    }
    ctx()->currentFunction = nullptr;

    if (codegenResult) {
        return _function;
    } else {
        std::cerr << "Error parsing function body" << std::endl;
        _function->eraseFromParent();
        return nullptr;
    }
}
