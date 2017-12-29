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
    std::string makeVariableName(const Token& tok, VariableType* typeOut = nullptr);

    std::unordered_map<std::string, ProtoDefAST*> externFunctions;
    std::unordered_map<std::string, LabelInfo> labels;
    std::unordered_map<std::string, VariableDeclartionAST*> namedVariables;
};

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
                                          StopCheck::List stopAt = {Token::End});
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
                                          StopCheck::List stopAt = {Token::End},
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

    virtual llvm::Value* allocaInst() const = 0;
    virtual llvm::Type* nativeType() const = 0;
    virtual const std::string& name() const = 0;
    virtual bool isGlobal() const = 0;
    virtual const std::vector<int>& dimensions() const {
        static std::vector<int> ret;
        return ret;
    }
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

    llvm::Value* allocaInst() const override;
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
    llvm::AllocaInst* _alloca;
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

    llvm::Value* codegen() override;
    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);

    llvm::Value* allocaInst() const override;
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
    llvm::AllocaInst* _alloca;
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

class ReturnAST : public ExprAST {
public:
    ReturnAST(Token tok, BasicContext* ctx) : ExprAST(std::move(tok), ctx) {}

    llvm::Value* codegen() override;
    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);
};

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
    LetAST* _getControlVar() const {
        return static_cast<LetAST*>(_controlVar.get());
    }

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

class RemarkAST : public ExprAST {
public:
    RemarkAST(Token tok, BasicContext* ctx) : ExprAST(std::move(tok), ctx) {}

    llvm::Value* codegen() override;
};

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

class ProtoDefAST : public ExprAST {
public:
    class Argument : public VariableDeclartionAST {
    public:
        Argument(Token tok, BasicContext* ctx, VariableType t, std::string n)
            : VariableDeclartionAST(std::move(tok), ctx), type(t), _name(std::move(n)) {}

        const VariableType type;

        llvm::Value* codegen() override;
        llvm::Value* allocaInst() const override {
            return _alloca;
        }

        const std::string& name() const override {
            return _name;
        }

        bool isGlobal() const override {
            return false;
        }
        llvm::Type* nativeType() const;

    private:
        std::string _name;
        llvm::AllocaInst* _alloca;
    };

    ProtoDefAST(Token tok,
                BasicContext* ctx,
                std::string name,
                std::vector<Argument> args,
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

    std::vector<Argument>& args() {
        return _args;
    }

    VariableType returnType() const {
        return _returnType;
    }

private:
    std::string _name;
    std::vector<Argument> _args;
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

class EndAST : public ExprAST {
public:
    EndAST(Token tok, BasicContext* ctx) : ExprAST(std::move(tok), ctx) {}

    llvm::Value* codegen() override;
};
