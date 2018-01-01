#pragma once

#include "ast.h"
#include "functions.h"

class GotoAST : public ExprAST {
public:
    GotoAST(Token tok, BasicContext* ctx, std::string label)
        : ExprAST(std::move(tok), ctx), _label(label) {}

    llvm::Value* codegen() override;

    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);

private:
    std::string _label;
};

class GosubAST : public ExprAST {
public:
    GosubAST(Token tok, BasicContext* ctx, std::string label)
        : ExprAST(std::move(tok), ctx), _label(label) {}

    llvm::Value* codegen() override;

    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);

private:
    std::string _label;
};

std::unique_ptr<FunctionCallAST> makeGosubCall(const Token& tok,
                                               BasicContext* ctx,
                                               std::string name);
std::unique_ptr<ProtoDefAST> makeGosubProto(const LabelAST* label);

class ForAST : public ExprAST {
public:
    llvm::Value* codegen() override;
    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);

    ForAST(Token tok,
           BasicContext* ctx,
           std::unique_ptr<ExprAST> controlVar,
           std::unique_ptr<ExprAST> toExpr,
           std::unique_ptr<ExprAST> stepExpr,
           std::unique_ptr<ExprAST> bodyExpr)
        : ExprAST(std::move(tok), ctx),
          _controlVar(std::move(controlVar)),
          _toExpr(std::move(toExpr)),
          _stepExpr(std::move(stepExpr)),
          _bodyExpr(std::move(bodyExpr)) {}

private:
    LetAST* _getControlVar() const;

    std::unique_ptr<ExprAST> _controlVar, _toExpr, _stepExpr, _bodyExpr;
};

class WhileAST : public ExprAST {
public:
    WhileAST(Token tok,
             BasicContext* ctx,
             std::unique_ptr<ExprAST> condExpr,
             bool isNot,
             std::unique_ptr<ExprAST> body)
        : ExprAST(std::move(tok), ctx),
          _condExpr(std::move(condExpr)),
          _isNot(isNot),
          _body(std::move(body)) {}

    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);
    llvm::Value* codegen() override;

private:
    std::unique_ptr<ExprAST> _condExpr;
    bool _isNot;
    std::unique_ptr<ExprAST> _body;
};

class DoAST : public ExprAST {
public:
    DoAST(Token tok,
          BasicContext* ctx,
          std::unique_ptr<ExprAST> condExpr,
          std::unique_ptr<ExprAST> body)
        : ExprAST(std::move(tok), ctx), _condExpr(std::move(condExpr)), _body(std::move(body)) {}

    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);
    llvm::Value* codegen() override;

private:
    std::unique_ptr<ExprAST> _condExpr;
    std::unique_ptr<ExprAST> _body;
};

class IfAST : public ExprAST {
public:
    IfAST(Token tok,
          BasicContext* ctx,
          std::unique_ptr<ExprAST> condition,
          std::unique_ptr<ExprAST> then,
          std::unique_ptr<ExprAST> elseExpr)
        : ExprAST(std::move(tok), ctx),
          _condition(std::move(condition)),
          _then(std::move(then)),
          _else(std::move(elseExpr)) {}

    llvm::Value* codegen() override;

    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);

private:
    std::unique_ptr<ExprAST> _condition;
    std::unique_ptr<ExprAST> _then;
    std::unique_ptr<ExprAST> _else;
};
