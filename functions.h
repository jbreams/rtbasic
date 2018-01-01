#pragma once

#include "ast.h"
#include "variables.h"

class FunctionCallAST : public ExprAST {
public:
    FunctionCallAST(Token tok,
                    BasicContext* ctx,
                    std::string function,
                    std::vector<std::unique_ptr<ExprAST>> args)
        : ExprAST(std::move(tok), ctx), _function(std::move(function)), _args(std::move(args)) {}

    llvm::Value* codegen() override;
    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);

private:
    std::string _function;
    std::vector<std::unique_ptr<ExprAST>> _args;
};

class ArgumentAST;
class ProtoDefAST : public ExprAST {
public:
    ProtoDefAST(Token tok,
                BasicContext* ctx,
                std::string name,
                std::vector<ArgumentAST> args,
                bool isVarArg,
                VariableType returnType)
        : ExprAST(std::move(tok), ctx),
          _name(std::move(name)),
          _args(std::move(args)),
          _isVarArg(isVarArg),
          _returnType(returnType) {}
    llvm::Value* codegen() override;
    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);
    const std::string& name() const {
        return _name;
    }

    std::vector<ArgumentAST>& args() {
        return _args;
    }

    VariableType returnType() const {
        return _returnType;
    }

private:
    std::string _name;
    std::vector<ArgumentAST> _args;
    bool _isVarArg;
    VariableType _returnType;
    llvm::Function* _function = nullptr;
};

class FunctionAST : public ExprAST {
public:
    FunctionAST(Token tok,
                BasicContext* ctx,
                std::unique_ptr<ProtoDefAST> proto,
                std::unique_ptr<ExprAST> body)
        : ExprAST(std::move(tok), ctx), _proto(std::move(proto)), _body(std::move(body)) {}

    FunctionAST(Token tok, BasicContext* ctx, std::unique_ptr<ProtoDefAST> proto)
        : ExprAST(tok, ctx),
          _proto(std::move(proto)),
          _body(std::make_unique<BlockAST>(tok, ctx)) {}

    llvm::Value* codegen() override;
    llvm::Function* function() const;
    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);
    void addStatement(std::unique_ptr<ExprAST> statement);
    ProtoDefAST* proto() const {
        return _proto.get();
    }

private:
    void _setBody(std::unique_ptr<ExprAST> body);

    llvm::Function* _function = nullptr;
    std::unique_ptr<ProtoDefAST> _proto;
    std::unique_ptr<ExprAST> _body;
};

class ReturnAST : public ExprAST {
public:
    ReturnAST(Token tok, BasicContext* ctx) : ExprAST(std::move(tok), ctx) {}

    llvm::Value* codegen() override;
    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);
};
