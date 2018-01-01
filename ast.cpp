#include <cstdlib>
#include <deque>
#include <iostream>
#include <list>
#include <sstream>

#include "boost/optional.hpp"

#include "ast.h"
#include "controlflow.h"
#include "functions.h"
#include "variables.h"

ParseException::ParseException(const Token& tok, std::string str) {
    std::stringstream ss;
    ss << "Error parsing: " << str << ". Token was: " << tok;
    _str = ss.str();
}

std::unique_ptr<FunctionAST> BasicContext::makeMainFunction(std::string name) {
    Token programToken(&lexer, Token::Sub, std::string("main"));
    std::vector<ArgumentAST> args;

    auto programProto =
        std::make_unique<ProtoDefAST>(programToken, this, "main", args, false, Void);
    return std::make_unique<FunctionAST>(programToken, this, std::move(programProto));
}

std::string BasicContext::makeVariableName(const Token& tok, VariableType* typeOut) {
    const std::string& name = tok.strValue;
    VariableType type = Void;
    char end = name.at(name.size() - 1);
    if (end == '#') {
        type = Double;
    } else if (end == '$') {
        type = String;
    }
    if (typeOut)
        *typeOut = type;
    return name;
}

bool BasicContext::isNamedVariable(const Token& tok) {
    return namedVariables.find(makeVariableName(tok)) != namedVariables.end();
}

bool BasicContext::isNamedVariable(const std::string& name) {
    return namedVariables.find(name) != namedVariables.end();
}

void BasicContext::clearNonGlobalVariables() {
    for (auto it = namedVariables.begin(); it != namedVariables.end();) {
        if (!it->second->isGlobal()) {
            it = namedVariables.erase(it);
        } else {
            it++;
        }
    }
}

std::unique_ptr<ExprAST> LineAST::parse(const Token& tok,
                                        BasicContext* ctx,
                                        StopCheck::List stopAt) {
    Token curTok = tok;
    std::unique_ptr<ExprAST> label;
    StopCheck stopCheck(stopAt);

    if (curTok.tag == Token::Label) {
        label = LabelAST::parse(tok, ctx);
        curTok = ctx->lexer.lex();
    }

    if (stopCheck.isStopTerm(curTok)) {
        ctx->lexer.putBack(curTok);
    }

    if (stopCheck(curTok) || curTok.isEnding()) {
        return std::make_unique<LineAST>(tok, ctx, std::move(label), nullptr);
    }

    auto statement = StatementAST::parse(curTok, ctx);

    // consume the new line.
    auto endTok = ctx->lexer.lex();
    if (!stopCheck(endTok)) {
        throw ParseException(endTok, "Expected new line or EOF at end of line");
    }

    return std::make_unique<LineAST>(tok, ctx, std::move(label), std::move(statement));
}

const Token& LineAST::token() const {
    if (_statement)
        return _statement->token();
    return ExprAST::token();
}

std::unique_ptr<ExprAST> StatementAST::parse(const Token& tok, BasicContext* ctx) {
    switch (tok.tag) {
        case Token::If:
            return IfAST::parse(tok, ctx);
        case Token::Goto:
            return GotoAST::parse(tok, ctx);
        case Token::Return:
            return ReturnAST::parse(tok, ctx);
        case Token::Gosub:
            return GosubAST::parse(tok, ctx);
        case Token::End:
            throw ParseException(tok, "Reached END token outside of block parsing");
        case Token::For:
            return ForAST::parse(tok, ctx);
        case Token::Do:
            return DoAST::parse(tok, ctx);
        case Token::While:
            return WhileAST::parse(tok, ctx);
        case Token::Input:
            return FunctionCallAST::parse(tok, ctx);
        case Token::Print:
            return FunctionCallAST::parse(tok, ctx);
        case Token::Rem:  // Remark tokens get a placeholder value in the AST
            return std::make_unique<RemarkAST>(tok, ctx);
        case Token::Extern:
            return ProtoDefAST::parse(tok, ctx);
        case Token::Sub:
            return FunctionAST::parse(tok, ctx);
        case Token::Function:
            return FunctionAST::parse(tok, ctx);
        case Token::Variable:
            if (ctx->lexer.peek().tag != Token::Eq) {
                throw ParseException(tok, "Expected = in variable assignment");
            }
        case Token::Let:
            return LetAST::parse(tok, ctx);
        case Token::Dim:
            return DimAST::parse(tok, ctx);
        case Token::String: {
            const auto& name = tok.strValue;
            if (ctx->externFunctions.find(name) != ctx->externFunctions.end()) {
                auto nextTag = ctx->lexer.peek().tag;
                if (nextTag == Token::LParens) {
                    return FunctionCallAST::parse(tok, ctx);
                } else if (nextTag == Token::Eq) {
                    return LetAST::parse(tok, ctx);
                }
            } else if (ctx->isNamedVariable(tok)) {
                return LetAST::parse(tok, ctx);
            }
        }
        default:
            throw ParseException(tok, "Unknown statement type");
    }
}

void BlockAST::addStatement(std::unique_ptr<ExprAST> statement) {
    _lines.push_back(std::move(statement));
}


std::unique_ptr<ExprAST> BlockAST::parse(const Token& tok,
                                         BasicContext* ctx,
                                         StopCheck::List stopAt,
                                         bool topLevel) {
    StopCheck stopCheck(std::move(stopAt));

    std::vector<std::unique_ptr<ExprAST>> blockLines;
    std::list<std::unique_ptr<ExprAST>> mainLines;
    Token curTok;
    for (curTok = tok; !stopCheck(curTok); curTok = ctx->lexer.lex()) {
        auto line = LineAST::parse(curTok, ctx);
        if (ctx->currentFunction || line->token().isFunctionDef() || !topLevel) {
            blockLines.push_back(std::move(line));
        } else {
            mainLines.push_back(std::move(line));
        }
    }

    if (!curTok.isEnding() || curTok.tag != Token::End) {
        ctx->lexer.putBack(curTok);
    }

    std::deque<FunctionAST*> curGosubs;
    for (auto& lineExpr : mainLines) {
        auto line = static_cast<LineAST*>(lineExpr.get());
        auto label = line->label();
        LabelInfo* labelInfo;
        if (label) {
            labelInfo = &ctx->labels.at(label->name());
        }

        if (!curGosubs.empty() && !(label && labelInfo->isGosub)) {
            auto& curGosub = curGosubs.front();
            curGosub->addStatement(std::move(lineExpr));
            if (line->token().tag == Token::Return) {
                curGosubs.pop_front();
                while (!curGosubs.empty()) {
                    curGosubs.front()->addStatement(
                        std::make_unique<ReturnAST>(line->token(), ctx));
                    curGosubs.pop_front();
                }
            }
            continue;
        }

        if (!label || !labelInfo->isGosub) {
            continue;
        }

        auto proto = makeGosubProto(label);
        bool inserted;
        std::tie(std::ignore, inserted) = ctx->externFunctions.emplace(proto->name(), proto.get());
        if (!inserted) {
            throw ParseException(label->token(), "Gosub function being created already exists!");
        }

        blockLines.push_back(std::make_unique<FunctionAST>(label->token(), ctx, std::move(proto)));
        auto curGosub = static_cast<FunctionAST*>(blockLines.back().get());
        curGosub->addStatement(std::move(lineExpr));
        lineExpr = std::make_unique<LineAST>(line->token(),
                                             ctx,
                                             line->releaseLabel(),
                                             makeGosubCall(line->token(), ctx, label->name()));
        if (!curGosubs.empty()) {
            curGosubs.front()->addStatement(makeGosubCall(line->token(), ctx, label->name()));
        }
        curGosubs.push_front(curGosub);
    }

    for (auto& lineExpr : mainLines) {
        if (lineExpr) {
            ctx->mainFunction->addStatement(std::move(lineExpr));
        }
    }

    return std::make_unique<BlockAST>(tok, ctx, std::move(blockLines));
}

struct OperatorInfo {
    int precedence;
    static std::unique_ptr<ExprAST> parse(const Token& tok, BasicContext* ctx);
    enum Associativity { Left, Right };
    Associativity associativity;

    explicit OperatorInfo(int p, Associativity a) : precedence(p), associativity(a) {}

    OperatorInfo(int p) : precedence(p), associativity(Left) {}

    using Handle = boost::optional<const OperatorInfo&>;
    static Handle get(const Token& tok);
};

OperatorInfo::Handle OperatorInfo::get(const Token& tok) {
    const static std::unordered_map<Token::Tag, OperatorInfo> operators = {
        {Token::Exp, OperatorInfo(4, OperatorInfo::Right)},
        {Token::Multiply, 3},
        {Token::Divide, 3},
        {Token::Plus, 2},
        {Token::Minus, 2},
    };
    auto it = operators.find(tok.tag);
    if (it == operators.end())
        return boost::none;
    return it->second;
}

std::unique_ptr<ExprAST> parsePrimary(const Token& tok, BasicContext* ctx) {
    switch (tok.tag) {
        case Token::Variable:
        case Token::String: {
            if (ctx->isNamedVariable(tok)) {
                return VariableExprAST::parse(tok, ctx);
            } else {
                return FunctionCallAST::parse(tok, ctx);
            }
        }
        case Token::Integer:
        case Token::Double:
            return NumberExprAST::parse(tok, ctx);
        case Token::EscapedString:
            return StringAST::parse(tok, ctx);
        case Token::LParens: {
            auto ret = ExprAST::parse(ctx->lexer.lex(), ctx);
            auto rparens = ctx->lexer.lex();
            if (rparens.tag != Token::RParens) {
                throw ParseException(rparens, "Expected RParens");
            }
            return ret;
        }
        default:
            ctx->lexer.putBack(tok);
            return nullptr;
    }
}

std::unique_ptr<ExprAST> BinaryExprAST::parse(const Token& tok, BasicContext* ctx) {
    auto lhs = parsePrimary(tok, ctx);
    return _parseRHS(ctx, 0, std::move(lhs));
}

std::unique_ptr<ExprAST> BinaryExprAST::_parseRHS(BasicContext* ctx,
                                                  int minPrec,
                                                  std::unique_ptr<ExprAST> lhs) {
    OperatorInfo::Handle opInfo;
    Token lookahead = ctx->lexer.peek();
    while ((opInfo = OperatorInfo::get(lookahead)) && (opInfo->precedence >= minPrec)) {
        auto op = ctx->lexer.lex();
        auto rhs = parsePrimary(ctx->lexer.lex(), ctx);
        if (!rhs) {
            return nullptr;
        }

        lookahead = ctx->lexer.peek();

        OperatorInfo::Handle lookaheadOp;
        while ((lookaheadOp = OperatorInfo::get(lookahead)) &&
               ((lookaheadOp->precedence > opInfo->precedence) ||
                (lookaheadOp->associativity == OperatorInfo::Right &&
                 lookaheadOp->precedence == opInfo->precedence))) {
            rhs = _parseRHS(ctx, lookaheadOp->precedence, std::move(rhs));
            lookahead = ctx->lexer.peek();
        }
        lhs = std::make_unique<BinaryExprAST>(op, ctx, std::move(lhs), std::move(rhs));
    }

    return lhs;
}

std::unique_ptr<ExprAST> ExprAST::parse(const Token& tok, BasicContext* ctx) {
    return BinaryExprAST::parse(tok, ctx);
}

std::unique_ptr<ExprAST> NumberExprAST::parse(const Token& tok, BasicContext* ctx) {
    return std::make_unique<NumberExprAST>(tok, ctx);
}

std::unique_ptr<ExprAST> StringAST::parse(const Token& tok, BasicContext* ctx) {
    return std::make_unique<StringAST>(tok, ctx);
}

std::unique_ptr<ExprAST> LabelAST::parse(const Token& tok, BasicContext* ctx) {
    bool inserted = false;
    const auto& labelName = tok.strValue;
    decltype(ctx->labels)::iterator it;

    std::tie(it, inserted) = ctx->labels.emplace(labelName, nullptr);
    if (!inserted && it->second.label) {
        throw ParseException(tok, "Duplicate label definition");
    }

    auto ret = std::make_unique<LabelAST>(tok, ctx);
    it->second.label = ret.get();
    return ret;
}

std::unique_ptr<ExprAST> RelOpExprAST::parse(const Token& tok, BasicContext* ctx) {
    auto lhs = ExprAST::parse(ctx->lexer.lex(), ctx);
    auto condToken = ctx->lexer.lex();
    switch (condToken.tag) {
        case Token::Lt:
        case Token::Gt:
        case Token::Lte:
        case Token::Gte:
        case Token::Eq:
        case Token::Neq:
            break;
        default:
            throw ParseException(condToken, "Unsupported relop");
    }

    auto rhs = ExprAST::parse(ctx->lexer.lex(), ctx);

    return std::make_unique<RelOpExprAST>(condToken, ctx, std::move(lhs), std::move(rhs));
}
