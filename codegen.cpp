#include <algorithm>
#include <iostream>

#include "llvm/ADT/APFloat.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"

#include "ast.h"
#include "controlflow.h"
#include "functions.h"
#include "variables.h"

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

llvm::Constant* BasicContext::makeLiteralDouble(double val) {
    return llvm::ConstantFP::get(context, llvm::APFloat(val));
}

llvm::Constant* BasicContext::makeLiteralInteger(int64_t val) {
    return llvm::ConstantInt::getSigned(builder.getInt64Ty(), val);
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
        return ctx()->makeLiteralDouble(boost::get<double>(token().value));
    } else if (token().tag == Token::Integer) {
        return ctx()->makeLiteralInteger(boost::get<int64_t>(token().value));
    } else {
        std::cerr << "Invalid token for number expr" << std::endl;
        return nullptr;
    }
}

llvm::Value* StringAST::codegen() {
    return ctx()->builder.CreateGlobalStringPtr(token().strValue);
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

llvm::Value* RemarkAST::codegen() {
    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(ctx()->context));
}
