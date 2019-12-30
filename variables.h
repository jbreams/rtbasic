#pragma once
#include "ast.h"

llvm::Type* astTypeToNativeType(BasicContext* ctx, VariableType type);
llvm::Constant* makeLiteralDouble(BasicContext* ctx, double val);
llvm::Constant* makeLiteralInteger(BasicContext* ctx, int64_t val);

class VariableExprAST : public ExprAST {
public:
    VariableExprAST(Token tok,
                    BasicContext* ctx,
                    std::string name,
                    std::vector<std::unique_ptr<ExprAST>> dimensions)
        : ExprAST(std::move(tok), ctx),
          _name(std::move(name)),
          _dimensions(std::move(dimensions)) {}

    llvm::Value* codegen() override;
    llvm::Value* codegenLookup();
    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);

private:
    std::string _name;
    std::vector<std::unique_ptr<ExprAST>> _dimensions;
};

class VariableDeclartionAST : public ExprAST {
public:
    VariableDeclartionAST(Token tok, BasicContext* ctx) : ExprAST(std::move(tok), ctx) {}

    virtual llvm::Type* nativeType() const = 0;
    virtual const std::string& name() const = 0;
    virtual bool isGlobal() const = 0;
    virtual const std::vector<int>& dimensions() const {
        static std::vector<int> ret;
        return ret;
    }

    llvm::Value* codegen() override;
    llvm::Value* lookup() const;
    llvm::Value* lookup(const std::vector<std::unique_ptr<ExprAST>>& dimensions) const;

private:
    llvm::Value* _alloca;
};

class LetAST : public VariableDeclartionAST {
public:
    LetAST(Token tok,
           BasicContext* ctx,
           std::string name,
           VariableType type,
           std::unique_ptr<VariableExprAST> variableExpr,
           std::unique_ptr<ExprAST> value,
           bool global)
        : VariableDeclartionAST(std::move(tok), ctx),
          _name(std::move(name)),
          _variableExpr(std::move(variableExpr)),
          _value(std::move(value)),
          _type(type),
          _global(global) {}

    llvm::Value* codegen() override;
    static std::unique_ptr<ExprAST> parse(const Token& tok,
                                          BasicContext* ctx,
                                          bool maybeGlobal = true);
    static std::unique_ptr<ExprAST> parse(std::string name,
                                          BasicContext* ctx,
                                          bool maybeGlobal = true);

    llvm::Type* nativeType() const override;

    const std::string& name() const override {
        return _name;
    }


    bool isGlobal() const override {
        return _global;
    }

private:
    std::string _name;
    std::unique_ptr<VariableExprAST> _variableExpr;
    std::unique_ptr<ExprAST> _value;
    VariableType _type;
    bool _global;
};

class DimAST : public VariableDeclartionAST {
public:
    DimAST(Token tok,
           BasicContext* ctx,
           std::string name,
           VariableType type,
           std::vector<int> dimensions,
           bool global)
        : VariableDeclartionAST(std::move(tok), ctx),
          _name(std::move(name)),
          _type(type),
          _dimensions(std::move(dimensions)),
          _global(global) {}

    DimAST(Token tok, BasicContext* ctx, std::string name, VariableType type)
        : VariableDeclartionAST(std::move(tok), ctx),
          _name(std::move(name)),
          _type(type),
          _dimensions(),
          _global(ctx->currentFunction == nullptr) {}

    llvm::Value* codegen() override;
    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);

    llvm::Type* nativeType() const override;
    const std::string& name() const override;
    const std::vector<int>& dimensions() const override {
        return _dimensions;
    }
    bool isGlobal() const override {
        return _global;
    }

private:
    std::string _name;
    VariableType _type;
    std::vector<int> _dimensions;
    bool _global;
};

class ArgumentAST : public VariableDeclartionAST {
public:
    ArgumentAST(Token tok, BasicContext* ctx, VariableType t, std::string n)
        : VariableDeclartionAST(std::move(tok), ctx), type(t), _name(std::move(n)) {}

    ArgumentAST(Token tok, BasicContext* ctx)
        : VariableDeclartionAST(std::move(tok), ctx),
          type(_parseType(token())),
          _name(ctx->makeVariableName(token(), nullptr)) {}

    const VariableType type;

    const std::string& name() const override {
        return _name;
    }

    bool isGlobal() const override {
        return _isGlobal;
    }
    llvm::Type* nativeType() const override;

private:
    VariableType _parseType(const Token& name) {
        VariableType ret;
        std::ignore = ctx()->makeVariableName(name, &ret);
        return ret;
    }

    std::string _name;
    bool _isGlobal = false;
};
