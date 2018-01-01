#pragma once

#include <memory>
#include <ostream>
#include <stack>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Value.h>

#include "lexer.h"

class ExprAST;
class LabelAST;
struct BasicContext;
class ProtoDefAST;
class FunctionAST;
class VariableDeclartionAST;
class LetAST;

struct LabelInfo {
    explicit LabelInfo(int initialGotos) : gotos(initialGotos) {}
    explicit LabelInfo(bool initialGosub) : isGosub(initialGosub), gotos(1) {}
    explicit LabelInfo(LabelAST* ptr) : label(ptr) {}

    int gotos = 0;
    bool isGosub = false;
    LabelAST* label = nullptr;
};

enum VariableType {
    Double,
    Integer,
    String,
    Void  // There are no void variables, but SUB's return void
};

struct BasicContext {
    BasicContext(Lexer lexer, std::string fileName, std::string mainName = "main")
        : lexer(std::move(lexer)),
          context(),
          builder(context),
          module(std::make_unique<llvm::Module>(fileName, context)),
          mainFunction(makeMainFunction(mainName)) {}

    Lexer lexer;

    llvm::Function* getFunction(const std::string& name);
    llvm::Function* getCurrentFunction();

    std::unique_ptr<FunctionAST> makeMainFunction(std::string name);

    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;

    std::unique_ptr<FunctionAST> mainFunction;
    FunctionAST* currentFunction = nullptr;

    bool codegenAllProtos();
    bool isNamedVariable(const Token& tok);
    bool isNamedVariable(const std::string& name);
    void clearNonGlobalVariables();
    std::string makeVariableName(const Token& tok, VariableType* typeOut = nullptr);

    llvm::Constant* makeLiteralInteger(int64_t val);
    llvm::Constant* makeLiteralDouble(double val);

    std::unordered_map<std::string, ProtoDefAST*> externFunctions;
    std::unordered_map<std::string, LabelInfo> labels;
    std::unordered_map<std::string, VariableDeclartionAST*> namedVariables;
};

template <typename T, typename Func>
std::vector<T> parseParensList(BasicContext* ctx, Func converter) {
    auto nextTok = ctx->lexer.lex();
    std::vector<T> ret;
    while (nextTok.tag != Token::RParens) {
        if (nextTok.tag == Token::Comma) {
            nextTok = ctx->lexer.lex();
        }
        ret.push_back(converter(nextTok));
        nextTok = ctx->lexer.lex();
    }

    return ret;
}

class ParseException : public std::exception {
public:
    ProtoDefAST* currentFunction = nullptr;
    ParseException(const Token& tok, std::string str);
    const char* what() const noexcept override {
        return _str.c_str();
    }

private:
    std::string _str;
};

class StopCheck {
public:
    using List = std::initializer_list<Token::Tag>;

    StopCheck(List stopAt) : _stopAt(stopAt) {}

    bool operator()(const Token& tok) {
        if (tok.isEnding()) {
            return true;
        }
        return isStopTerm(tok);
    }

    bool isStopTerm(const Token& tok) {
        auto it = std::find(_stopAt.begin(), _stopAt.end(), tok.tag);
        return (it != _stopAt.end());
    }

private:
    List _stopAt;
};


class ExprAST {
public:
    virtual ~ExprAST() = default;
    virtual llvm::Value* codegen() = 0;

    explicit ExprAST(Token tok, BasicContext* ctx) : _tok(std::move(tok)), _ctx(ctx) {}

    virtual const Token& token() const {
        return _tok;
    }

    BasicContext* ctx() const {
        return _ctx;
    }

    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);

private:
    Token _tok;
    BasicContext* _ctx;
};

class StatementAST : public ExprAST {
public:
    StatementAST(Token tok, BasicContext* ctx) : ExprAST(std::move(tok), ctx) {}

    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);
    llvm::Value* codegen() override;
};

class LabelAST : public ExprAST {
public:
    LabelAST(Token tok, BasicContext* ctx) : ExprAST(std::move(tok), ctx) {}

    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);
    llvm::Value* codegen() override;
    llvm::Value* value();

    const std::string& name() const;

private:
    llvm::BasicBlock* _value = nullptr;
};

class LineAST : public ExprAST {
public:
    LineAST(Token tok,
            BasicContext* ctx,
            std::unique_ptr<ExprAST> label,
            std::unique_ptr<ExprAST> statement)
        : ExprAST(std::move(tok), ctx),
          _label(std::move(label)),
          _statement(std::move(statement)) {}

    static std::unique_ptr<ExprAST> parse(const Token& tok,
                                          BasicContext* ctx,
                                          StopCheck::List stopAt);
    llvm::Value* codegen() override;
    const Token& token() const override;
    LabelAST* label() const {
        return static_cast<LabelAST*>(_label.get());
    }

    std::unique_ptr<ExprAST> releaseLabel() {
        return std::move(_label);
    }

private:
    std::unique_ptr<ExprAST> _label;
    std::unique_ptr<ExprAST> _statement;
};

class BlockAST : public ExprAST {
public:
    BlockAST(Token tok, BasicContext* ctx, std::vector<std::unique_ptr<ExprAST>> lines)
        : ExprAST(std::move(tok), ctx), _lines(std::move(lines)) {}

    BlockAST(Token tok, BasicContext* ctx) : ExprAST(std::move(tok), ctx) {}

    static std::unique_ptr<ExprAST> parse(const Token& tok,
                                          BasicContext* ctx,
                                          StopCheck::List stopAt,
                                          bool topLevel = false);
    llvm::Value* codegen() override;

    void addStatement(std::unique_ptr<ExprAST> statement);


private:
    std::vector<std::unique_ptr<ExprAST>> _lines;
};

class NumberExprAST : public ExprAST {
public:
    NumberExprAST(Token tok, BasicContext* ctx) : ExprAST(std::move(tok), ctx) {}

    llvm::Value* codegen() override;
    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);
};

class StringAST : public ExprAST {
public:
    StringAST(Token tok, BasicContext* ctx) : ExprAST(std::move(tok), ctx) {}

    llvm::Value* codegen() override;
    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);
};

class BinaryExprAST : public ExprAST {
public:
    BinaryExprAST(Token tok,
                  BasicContext* ctx,
                  std::unique_ptr<ExprAST> lhs,
                  std::unique_ptr<ExprAST> rhs)
        : ExprAST(std::move(tok), ctx), _lhs(std::move(lhs)), _rhs(std::move(rhs)) {}

    llvm::Value* codegen() override;

    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);

private:
    static std::unique_ptr<ExprAST> _parseRHS(BasicContext* ctx,
                                              int prec,
                                              std::unique_ptr<ExprAST> lhs);
    std::unique_ptr<ExprAST> _lhs;
    std::unique_ptr<ExprAST> _rhs;
};

class RelOpExprAST : public ExprAST {
public:
    RelOpExprAST(Token tok,
                 BasicContext* ctx,
                 std::unique_ptr<ExprAST> lhs,
                 std::unique_ptr<ExprAST> rhs)
        : ExprAST(std::move(tok), ctx), _lhs(std::move(lhs)), _rhs(std::move(rhs)) {}

    llvm::Value* codegen() override;

    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);

private:
    std::unique_ptr<ExprAST> _lhs;
    std::unique_ptr<ExprAST> _rhs;
};

class RemarkAST : public ExprAST {
public:
    RemarkAST(Token tok, BasicContext* ctx) : ExprAST(std::move(tok), ctx) {}

    llvm::Value* codegen() override;
};

class EndAST : public ExprAST {
public:
    EndAST(Token tok, BasicContext* ctx) : ExprAST(std::move(tok), ctx) {}

    llvm::Value* codegen() override;
};
