#include <algorithm>
#include <iostream>

#include "functions.h"

#include "variables.h"

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

struct AssignException : public std::exception {
public:
    enum Reason {
        NotString,
        NoEquals,
    };
    AssignException(Reason reason) : _reason(reason) {}

    const char* what() const noexcept override {
        switch (_reason) {
            case NotString:
                return "First token wasn't a string";
            case NoEquals:
                return "Expected equals sign between variable and value in assignment";
        }
    }

    Reason reason() const {
        return _reason;
    }

private:
    Reason _reason;
};

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

std::unique_ptr<ExprAST> VariableExprAST::parse(const Token& tok, BasicContext* ctx) {
    std::string variableName(ctx->makeVariableName(tok));
    std::vector<std::unique_ptr<ExprAST>> dimensions;

    auto nextTok = ctx->lexer.lex();
    if (nextTok.tag == Token::LParens) {
        dimensions = parseParensList<std::unique_ptr<ExprAST>>(
            ctx, [ctx](Token tok) { return ExprAST::parse(tok, ctx); });
    } else {
        ctx->lexer.putBack(nextTok);
    }
    return std::make_unique<VariableExprAST>(tok, ctx, variableName, std::move(dimensions));
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
                initV = ctx()->makeLiteralDouble(0.0);
            } else if (type->isIntegerTy()) {
                initV = ctx()->makeLiteralInteger(0);
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
        std::vector<llvm::Value*> generatedDimensions = {ctx()->makeLiteralInteger(0)};
        for (const auto& dimension : dimensions) {
            generatedDimensions.push_back(dimension->codegen());
        }

        return builder.CreateInBoundsGEP(nativeType(), lookup(), generatedDimensions);
    }
}

std::unique_ptr<ExprAST> DimAST::parse(const Token& tok, BasicContext* ctx) {
    Token nameTok = ctx->lexer.lex();
    if (nameTok.tag != Token::String && nameTok.tag != Token::Variable) {
        throw AssignException(AssignException::NotString);
    }

    auto nextTok = ctx->lexer.lex();
    std::vector<int> dimensions;
    VariableType type = Integer;
    if (nextTok.tag == Token::LParens) {
        dimensions = parseParensList<int>(ctx, [](Token tok) {
            if (tok.tag != Token::Integer) {
                throw ParseException(tok, "Expected integer list for array dimensions");
            }
            return mpark::get<int64_t>(*tok.value);
        });

        // Consume the RParens
        nextTok = ctx->lexer.lex();
    }

    if (nextTok.tag == Token::As) {
        nextTok = ctx->lexer.lex();
        switch (nextTok.tag) {
            case Token::IntegerType:
                type = Integer;
                break;
            case Token::StringType:
                type = String;
                break;
            case Token::DoubleType:
                type = Double;
                break;
            default:
                throw ParseException(nextTok, "Invalid token while parsing variable type");
        }
    } else {
        ctx->lexer.putBack(nextTok);
    }
    bool global = (ctx->currentFunction == nullptr);
    auto ret =
        std::make_unique<DimAST>(tok, ctx, nameTok.strValue, type, std::move(dimensions), global);

    ctx->namedVariables[nameTok.strValue] = ret.get();

    return ret;
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

std::unique_ptr<ExprAST> LetAST::parse(const Token& tok, BasicContext* ctx, bool maybeGlobal) {
    Token nameTok = tok;
    if (tok.tag == Token::Let) {
        nameTok = ctx->lexer.lex();
    }
    if (nameTok.tag != Token::String && nameTok.tag != Token::Variable) {
        throw AssignException(AssignException::NotString);
    }

    std::string name;
    VariableType type = Void;
    std::unique_ptr<VariableExprAST> variableExpr;
    if (ctx->externFunctions.find(nameTok.strValue) == ctx->externFunctions.end()) {
        variableExpr.reset(
            static_cast<VariableExprAST*>(VariableExprAST::parse(nameTok, ctx).release()));
        name = ctx->makeVariableName(variableExpr->token(), &type);
        if (type == Void) {
            type = Integer;
        }
    } else {
        name = nameTok.strValue;
    }

    if (ctx->lexer.lex().tag != Token::Eq) {
        throw AssignException(AssignException::NoEquals);
    }

    auto value = ExprAST::parse(ctx->lexer.lex(), ctx);
    bool global = (ctx->currentFunction == nullptr && maybeGlobal);
    bool isVariable = static_cast<bool>(variableExpr);
    auto ret = std::make_unique<LetAST>(
        tok, ctx, name, type, std::move(variableExpr), std::move(value), global);

    if (isVariable)
        ctx->namedVariables[name] = ret.get();
    return ret;
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

llvm::Type* ArgumentAST::nativeType() const {
    return astTypeToNativeType(ctx(), type);
}
