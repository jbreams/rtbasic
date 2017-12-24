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

llvm::Value* makeLiteralDouble(BasicContext* ctx, double val) {
    return llvm::ConstantFP::get(ctx->context, llvm::APFloat(val));
}

llvm::Value* StatementAST::codegen() {
    return nullptr;
}

const std::string& LabelAST::name() const {
    return boost::get<std::string>(token().value);
}

void LabelAST::incrementUsed() {
    _uses++;
}

llvm::Value* LabelAST::codegen() {
    auto& builder = ctx()->builder;
    if (_uses == 0) {
        return nullptr;
    }

    _value = llvm::BasicBlock::Create(
        ctx()->context, boost::get<std::string>(token().value), ctx()->getCurrentFunction());

    auto curBB = builder.GetInsertBlock();
    auto newBB = llvm::BasicBlock::Create(ctx()->context, name(), ctx()->getCurrentFunction());

    if (curBB && !curBB->getTerminator()) {
        builder.CreateBr(newBB);
    }

    ctx()->getCurrentFunction()->getBasicBlockList().insertAfter(curBB->getIterator(), newBB);
    builder.SetInsertPoint(newBB);
    _value = newBB;
    return _value;
}

llvm::Value* LineAST::codegen() {
    LabelAST* label = static_cast<LabelAST*>(_label.get());
    if (label && !label->codegen() && label->used() > 0)
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
    return makeLiteralDouble(ctx(), boost::get<double>(token().value));
}

llvm::Value* StringAST::codegen() {
    const std::string& value = boost::get<std::string>(token().value);
    return ctx()->builder.CreateGlobalStringPtr(value);
}

llvm::Value* VariableExprAST::codegen() {
    auto name = boost::get<std::string>(token().value);
    auto it = ctx()->namedVariables.find(name);
    if (it == ctx()->namedVariables.end()) {
        auto global = ctx()->module->getGlobalVariable(name);
        if (!global) {
            std::cerr << "Could not find named value for " << name << std::endl;
            return nullptr;
        }

        return ctx()->builder.CreateLoad(global, name);
    }

    auto& builder = ctx()->builder;
    return builder.CreateLoad(it->second, it->first);
}

llvm::Value* LetAST::codegen() {
    auto& builder = ctx()->builder;

    if (_global) {
        llvm::Type* type = nullptr;
        llvm::Constant* initV;
        if (_type == Double) {
            type = llvm::Type::getDoubleTy(ctx()->context);
            initV = llvm::ConstantFP::get(type, 0.0);
        } else if (_type == String) {
            type = llvm::Type::getInt8PtrTy(ctx()->context);
            initV = llvm::ConstantPointerNull::get(llvm::Type::getInt8PtrTy(ctx()->context));
        }
        auto globalPtr = ctx()->module->getOrInsertGlobal(_name, type);
        auto global = ctx()->module->getGlobalVariable(_name);
        if (!global->hasInitializer()) {
            global->setInitializer(initV);
            global->setLinkage(llvm::GlobalValue::CommonLinkage);
        }

        auto ret = _value->codegen();
        if (!ret) {
            std::cerr << "Error generating value for global variable" << std::endl;
            return nullptr;
        }
        builder.CreateStore(ret, globalPtr);
        return ret;
    }

    llvm::Value* initV;
    if (_value) {
        initV = _value->codegen();
        if (!initV) {
            std::cerr << "Error generating initial value for let variable" << std::endl;
            return nullptr;
        }
    } else if (_type == Double) {
        initV = makeLiteralDouble(ctx(), 0.0);
    } else if (_type == String) {
        initV = llvm::ConstantPointerNull::get(llvm::Type::getInt8PtrTy(ctx()->context));
    } else {
        std::cerr << "Cannot find initial value for variable " << _name << std::endl;
        return nullptr;
    }

    if (ctx()->namedVariables.find(_name) != ctx()->namedVariables.end()) {
        builder.CreateStore(initV, ctx()->namedVariables[_name]);
    } else if (_name == ctx()->getCurrentFunction()->getName()) {
        builder.CreateRet(initV);
    } else {
        auto alloc = CreateEntryBlockAlloca(ctx(), _name, initV->getType());
        builder.CreateStore(initV, alloc);

        _alloca = alloc;
        ctx()->namedVariables[_name] = alloc;
    }

    return initV;
}

llvm::Value* BinaryExprAST::codegen() {
    auto l = _lhs->codegen();
    auto r = _rhs->codegen();
    if (!l || !r) {
        std::cerr << "Error evaluating operands for binary operator" << std::endl;
        return nullptr;
    }

    auto& builder = ctx()->builder;
    switch (token().tag) {
        case Token::Plus:
            return builder.CreateFAdd(l, r, "addtmp");
        case Token::Minus:
            return builder.CreateFSub(l, r, "subtmp");
        case Token::Multiply:
            return builder.CreateFMul(l, r, "multmp");
        case Token::Divide:
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

    auto& builder = ctx()->builder;
    auto& llvmcontext = ctx()->context;

    llvm::Value* res;
    switch (token().tag) {
        case Token::Gt:
            res = builder.CreateFCmpUGT(l, r, "cmptmp");
            break;
        case Token::Gte:
            res = builder.CreateFCmpUGE(l, r, "cmptmp");
            break;
        case Token::Lt:
            res = builder.CreateFCmpULT(l, r, "cmptmp");
            break;
        case Token::Lte:
            res = builder.CreateFCmpULE(l, r, "cmptmp");
            break;
        case Token::Neq:
            res = builder.CreateFCmpUNE(l, r, "cmptmp");
            break;
        case Token::Eq:
            res = builder.CreateFCmpUEQ(l, r, "cmptmp");
            break;
        default:
            std::cerr << "Invalid token for comparison operator" << std::endl;
            return nullptr;
    }

    return builder.CreateUIToFP(res, llvm::Type::getDoubleTy(llvmcontext), "booltmp");
}

llvm::Value* GotoAST::codegen() {
    auto& builder = ctx()->builder;
    auto labelIt = ctx()->labels.find(_label);
    if (labelIt == ctx()->labels.end()) {
        std::cerr << "Could not find label for goto statement: " << _label;
        return nullptr;
    }
    builder.CreateBr(static_cast<llvm::BasicBlock*>(labelIt->second->value()));
    return llvm::ConstantFP::get(builder.getDoubleTy(), 0);
}

llvm::Value* GosubAST::codegen() {
    std::cerr << "Gosubs aren't a real codegen type" << std::endl;
    return nullptr;
}

llvm::Value* ReturnAST::codegen() {
    return ctx()->builder.CreateRetVoid();
}

llvm::Value* ForAST::codegen() {
    auto& builder = ctx()->builder;
    auto curFunction = ctx()->getCurrentFunction();
    auto controlVar = static_cast<LetAST*>(_controlVar.get());

    if (!controlVar->codegen()) {
        std::cerr << "Error generating control variable" << std::endl;
        return nullptr;
    }

    auto loopBB = llvm::BasicBlock::Create(ctx()->context, "loop", curFunction);
    builder.CreateBr(loopBB);
    builder.SetInsertPoint(loopBB);

    if (!_bodyExpr->codegen()) {
        std::cerr << "Error generating for loop body" << std::endl;
        return nullptr;
    }

    llvm::Value* stepV = nullptr;
    if (_stepExpr) {
        stepV = _stepExpr->codegen();
        if (!stepV) {
            std::cerr << "Error generating step expression" << std::endl;
            return nullptr;
        }
    } else {
        stepV = makeLiteralDouble(ctx(), 1.0);
    }

    auto endV = _toExpr->codegen();
    if (!endV) {
        std::cerr << "Error generating end condition for for loop" << std::endl;
        return nullptr;
    }

    auto curVar = builder.CreateLoad(controlVar->allocaInst());
    auto nextVar = builder.CreateFAdd(curVar, stepV, "nextvar");
    builder.CreateStore(nextVar, controlVar->allocaInst());

    endV = builder.CreateFCmpONE(endV, nextVar, "loopcond");

    auto afterBB = llvm::BasicBlock::Create(ctx()->context, "afterloop", curFunction);
    builder.CreateCondBr(endV, loopBB, afterBB);
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
    cond = builder.CreateFCmpONE(cond, makeLiteralDouble(ctx(), 0.0), "ifcond");

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

    if (!thenBB->getTerminator()) {
        builder.CreateBr(mergeBB);
    }
    thenBB = builder.GetInsertBlock();

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

llvm::Value* ProtoDefAST::codegen() {
    if (auto preGenerated = ctx()->getFunction(_name)) {
        return preGenerated;
    }

    std::vector<llvm::Type*> args;
    for (const auto& arg : _args) {
        switch (arg.type) {
            case Double:
                args.push_back(llvm::Type::getDoubleTy(ctx()->context));
                break;
            case String:
                args.push_back(llvm::Type::getInt8PtrTy(ctx()->context));
                break;
            default:
                std::cerr << "Unsupported type for function argument";
                break;
        }
    }

    llvm::Type* returnType;
    switch (_returnType) {
        case Double:
            returnType = llvm::Type::getDoubleTy(ctx()->context);
            break;
        case String:
            returnType = llvm::PointerType::getInt8PtrTy(ctx()->context);
            break;
        case Void:
            returnType = llvm::Type::getVoidTy(ctx()->context);
            break;
    }

    auto functionType = llvm::FunctionType::get(returnType, args, _isVarArg);

    _function = llvm::Function::Create(
        functionType, llvm::Function::ExternalLinkage, _name, ctx()->module.get());
    int idx = 0;
    for (auto& arg : _function->args()) {
        arg.setName(_args[idx].name);
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
    ctx()->namedVariables.clear();
    for (auto& arg : _function->args()) {
        auto argAlloc = CreateEntryBlockAlloca(ctx(), arg.getName(), arg.getType());
        ctx()->builder.CreateStore(&arg, argAlloc);
        ctx()->namedVariables[arg.getName()] = argAlloc;
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
