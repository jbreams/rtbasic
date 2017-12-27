#include <cstdlib>
#include <deque>
#include <iostream>
#include <list>
#include <sstream>

#include "boost/optional.hpp"

#include "ast.h"

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

std::unique_ptr<FunctionAST> BasicContext::makeMainFunction(std::string name) {
    Token programToken(Token::Sub, std::string("main"));
    std::vector<ProtoDefAST::Argument> args;

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

std::unique_ptr<ExprAST> LineAST::parse(const Token& tok,
                                        BasicContext* ctx,
                                        StopCheck::List stopAt) {
    Token curTok = tok;
    std::unique_ptr<ExprAST> label;
    StopCheck stopCheck(stopAt);

    if (curTok.tag == Token::Label) {
        if (tok.value.which() != 0) {
            throw ParseException("Expected line number to be a string");
        }
        label = LabelAST::parse(tok, ctx);
        curTok = ctx->lexer.lex();
    }

    if (stopCheck.isStopTerm(curTok)) {
        ctx->lexer.putBack(curTok);
    }

    if (stopCheck(curTok)) {
        return std::make_unique<LineAST>(tok, ctx, std::move(label), nullptr);
    }

    auto statement = StatementAST::parse(curTok, ctx);

    // consume the new line.
    auto endTok = ctx->lexer.lex();
    if (!endTok.isEnding()) {
        throw ParseException("Expected new line or EOF at end of line");
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
            throw ParseException("Reached END token outside of block parsing");
        case Token::For:
            return ForAST::parse(tok, ctx);
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
                throw ParseException("Expected = in variable assignment");
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
            throw ParseException("Unknown statement type");
    }
}

void BlockAST::addStatement(std::unique_ptr<ExprAST> statement) {
    _lines.push_back(std::move(statement));
}

std::unique_ptr<ProtoDefAST> makeGosubProto(const LabelAST* label) {
    std::vector<ProtoDefAST::Argument> args;
    return std::make_unique<ProtoDefAST>(
        label->token(), label->ctx(), label->token().strValue, std::move(args), false, Void);
}

std::unique_ptr<FunctionCallAST> makeGosubCall(const Token& tok,
                                               BasicContext* ctx,
                                               std::string name) {
    std::vector<std::unique_ptr<ExprAST>> args;
    return std::make_unique<FunctionCallAST>(tok, ctx, name, std::move(args));
}

std::unique_ptr<ExprAST> BlockAST::parse(const Token& tok,
                                         BasicContext* ctx,
                                         StopCheck::List stopAt,
                                         bool topLevel) {
    StopCheck stopCheck(stopAt);

    std::vector<std::unique_ptr<ExprAST>> blockLines;
    std::list<std::unique_ptr<ExprAST>> mainLines;
    for (Token curTok = tok; !stopCheck(curTok); curTok = ctx->lexer.lex()) {
        auto line = LineAST::parse(curTok, ctx);
        if (ctx->currentFunction || line->token().isFunctionDef() || !topLevel) {
            blockLines.push_back(std::move(line));
        } else {
            mainLines.push_back(std::move(line));
        }
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
            throw ParseException("Gosub function being created already exists!");
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
            auto nextTok = ctx->lexer.peek();
            auto nextTag = nextTok.tag;
            if (nextTag == Token::LParens) {
                return FunctionCallAST::parse(tok, ctx);
            } else {
                return VariableExprAST::parse(tok, ctx);
            }
        }
        case Token::Integer:
        case Token::Double:
            return NumberExprAST::parse(tok, ctx);
        case Token::EscapedString:
            return StringAST::parse(tok, ctx);
        case Token::LParens: {
            auto ret = ExprAST::parse(ctx->lexer.lex(), ctx);
            if (ctx->lexer.lex().tag != Token::RParens) {
                throw ParseException("Expected RParens");
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

std::unique_ptr<ExprAST> VariableExprAST::parse(const Token& tok, BasicContext* ctx) {
    std::string variableName(ctx->makeVariableName(tok));

    if (ctx->namedVariables.find(variableName) == ctx->namedVariables.end()) {
        throw ParseException("Could not find variable");
    }
    return std::make_unique<VariableExprAST>(tok, ctx);
}

std::unique_ptr<ExprAST> LabelAST::parse(const Token& tok, BasicContext* ctx) {
    bool inserted = false;
    const auto& labelName = tok.strValue;
    decltype(ctx->labels)::iterator it;

    std::tie(it, inserted) = ctx->labels.emplace(labelName, nullptr);
    if (!inserted && it->second.label) {
        throw ParseException("Duplicate label definition");
    }

    auto ret = std::make_unique<LabelAST>(tok, ctx);
    it->second.label = ret.get();
    return ret;
}

std::unique_ptr<ExprAST> IfAST::parse(const Token& tok, BasicContext* ctx) {
    auto lhs = ExprAST::parse(ctx->lexer.lex(), ctx);
    if (!lhs)
        return nullptr;
    auto condToken = ctx->lexer.lex();
    auto rhs = ExprAST::parse(ctx->lexer.lex(), ctx);
    if (!rhs)
        return nullptr;
    std::unique_ptr<ExprAST> condExpr;

    switch (condToken.tag) {
        case Token::Lt:
        case Token::Gt:
        case Token::Lte:
        case Token::Gte:
        case Token::Eq:
        case Token::Neq:
            condExpr =
                std::make_unique<RelOpExprAST>(condToken, ctx, std::move(lhs), std::move(rhs));
            break;
        default:
            throw ParseException("Unsupported relop");
    }

    auto nextTok = ctx->lexer.peek();
    std::unique_ptr<ExprAST> statementExpr;
    if (nextTok.tag == Token::Then) {
        ctx->lexer.lex();
        nextTok = ctx->lexer.peek();
    }

    if (nextTok.tag == Token::Newline) {
        statementExpr = BlockAST::parse(ctx->lexer.lex(), ctx);
    } else {
        statementExpr = StatementAST::parse(ctx->lexer.lex(), ctx);
    }

    nextTok = ctx->lexer.peek();
    std::unique_ptr<ExprAST> elseBody;

    if (nextTok.tag == Token::Else) {
        ctx->lexer.lex();
        nextTok = ctx->lexer.lex();
        if (nextTok.tag == Token::Newline) {
            elseBody = BlockAST::parse(nextTok, ctx);
        } else {
            elseBody = StatementAST::parse(nextTok, ctx);
        }
    } else if (!nextTok.isEnding()) {
        throw ParseException("Expected new statement after if/then without else");
    }
    return std::make_unique<IfAST>(
        tok, ctx, std::move(condExpr), std::move(statementExpr), std::move(elseBody));
}

std::unique_ptr<ExprAST> LetAST::parse(const Token& tok, BasicContext* ctx, bool maybeGlobal) {
    Token nameTok = tok;
    if (tok.tag == Token::Let) {
        nameTok = ctx->lexer.lex();
    }
    if (nameTok.tag != Token::String && nameTok.tag != Token::Variable) {
        throw AssignException(AssignException::NotString);
    }

    VariableType type;
    auto name = ctx->makeVariableName(nameTok, &type);
    if (type == Void) {
        type = Integer;
    }

    if (ctx->lexer.lex().tag != Token::Eq) {
        throw AssignException(AssignException::NoEquals);
    }

    auto value = ExprAST::parse(ctx->lexer.lex(), ctx);
    bool global = (ctx->currentFunction == nullptr && maybeGlobal);
    ctx->namedVariables[name] = nullptr;

    return std::make_unique<LetAST>(tok, ctx, name, type, std::move(value), global);
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
        nextTok = ctx->lexer.lex();
        while (nextTok.tag != Token::RParens) {
            if (nextTok.tag == Token::Integer) {
                dimensions.push_back(boost::get<int64_t>(nextTok.value));
            } else if (nextTok.tag == Token::Comma) {
                nextTok = ctx->lexer.lex();
            } else {
                throw ParseException("Invalid token while parsing dimensions list");
            }
            nextTok = ctx->lexer.lex();
        }

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
                throw ParseException("Invalid token while parsing variable type");
        }
    } else {
        ctx->lexer.putBack(nextTok);
    }
    bool global = (ctx->currentFunction == nullptr);

    return std::make_unique<DimAST>(
        tok, ctx, nameTok.strValue, type, std::move(dimensions), global);
}

std::string lexString(Lexer* lexer) {
    auto labelTok = lexer->lex();
    std::string gotoName;
    if (labelTok.tag == Token::String) {
        gotoName = labelTok.strValue;
    } else if (labelTok.tag == Token::Double) {
        std::stringstream ss;
        ss << boost::get<double>(labelTok.value);
        gotoName = ss.str();
    } else if (labelTok.tag == Token::Integer) {
        std::stringstream ss;
        ss << boost::get<int64_t>(labelTok.value);
        gotoName = ss.str();
    }
    return gotoName;
}

std::unique_ptr<ExprAST> GotoAST::parse(const Token& tok, BasicContext* ctx) {
    auto gotoName = lexString(&ctx->lexer);
    auto it = ctx->labels.find(gotoName);
    if (it == ctx->labels.end()) {
        std::tie(it, std::ignore) = ctx->labels.emplace(gotoName, 1);
    } else {
        it->second.gotos++;
    }
    return std::make_unique<GotoAST>(tok, ctx, it->first);
}

std::unique_ptr<ExprAST> GosubAST::parse(const Token& tok, BasicContext* ctx) {
    auto labelName = lexString(&ctx->lexer);
    auto it = ctx->labels.find(labelName);
    if (it == ctx->labels.end()) {
        std::tie(it, std::ignore) = ctx->labels.emplace(labelName, true);
    } else {
        it->second.isGosub = true;
    }

    return makeGosubCall(tok, ctx, labelName);
}

std::unique_ptr<ExprAST> ReturnAST::parse(const Token& tok, BasicContext* ctx) {
    return std::make_unique<ReturnAST>(tok, ctx);
}

std::unique_ptr<ExprAST> ForAST::parse(const Token& tok, BasicContext* ctx) {
    auto controlVar = LetAST::parse(ctx->lexer.lex(), ctx, false);

    if (ctx->lexer.lex().tag != Token::To) {
        throw ParseException("Expected TO after FOR let statement");
    }
    auto endExpr = ExprAST::parse(ctx->lexer.lex(), ctx);

    std::unique_ptr<ExprAST> stepExpr;
    auto maybeStep = ctx->lexer.lex();
    Token bodyTok;
    if (maybeStep.tag == Token::Step) {
        stepExpr = ExprAST::parse(ctx->lexer.lex(), ctx);
        bodyTok = ctx->lexer.lex();
    } else {
        bodyTok = maybeStep;
    }

    std::unique_ptr<ExprAST> bodyExpr;
    if (bodyTok.isEnding()) {
        bodyExpr = BlockAST::parse(ctx->lexer.lex(), ctx, {Token::Next});
    } else {
        bodyExpr = StatementAST::parse(bodyTok, ctx);
        if (ctx->lexer.lex().tag != Token::Next) {
            throw ParseException("Expected NEXT after for body");
        }
    }

    return std::make_unique<ForAST>(tok,
                                    ctx,
                                    std::move(controlVar),
                                    std::move(endExpr),
                                    std::move(stepExpr),
                                    std::move(bodyExpr));
}

std::unique_ptr<ExprAST> FunctionCallAST::parse(const Token& tok, BasicContext* ctx) {
    const auto& functionName = tok.strValue;
    static const std::unordered_set<std::string> noParensFunctions = {"PRINT", "INPUT"};
    bool needsParens = noParensFunctions.find(functionName) == noParensFunctions.end();

    Token curTok = ctx->lexer.peek();
    if (needsParens) {
        if (curTok.tag != Token::LParens) {
            throw ParseException("No LParens to start argument list of function call");
        }
        ctx->lexer.lex();
    }

    std::vector<std::unique_ptr<ExprAST>> args;
    do {
        auto arg = ExprAST::parse(ctx->lexer.lex(), ctx);
        if (arg) {
            args.push_back(std::move(arg));
        }
        curTok = ctx->lexer.lex();
    } while (curTok.tag == Token::Comma);

    if (needsParens) {
        if (curTok.tag != Token::RParens) {
            throw ParseException("No RParens at end of argument list of function call");
        }
    } else {
        ctx->lexer.putBack(curTok);
    }

    return std::make_unique<FunctionCallAST>(tok, ctx, std::move(functionName), std::move(args));
}

std::unique_ptr<ExprAST> ProtoDefAST::parse(const Token& tok, BasicContext* ctx) {
    auto nameTok = ctx->lexer.lex();
    if (nameTok.tag != Token::String && nameTok.tag != Token::Print &&
        nameTok.tag != Token::Input && nameTok.tag != Token::Variable) {
        throw ParseException("Expected function name");
    }

    VariableType returnType = Void;
    std::string name;
    if (tok.tag == Token::Function || tok.tag == Token::Extern) {
        name = ctx->makeVariableName(nameTok, &returnType);
    } else {
        name = nameTok.strValue;
    }

    if (ctx->lexer.lex().tag != Token::LParens) {
        throw ParseException("Expected LParens to start argument list of extern def");
    }

    std::vector<Argument> args;
    bool isVarArg = false;
    Token lastToken = ctx->lexer.peek();
    do {
        auto argNameTok = ctx->lexer.lex();
        if (argNameTok.tag == Token::Ellipsis) {
            isVarArg = true;
            lastToken = ctx->lexer.lex();
            break;
        }
        if (argNameTok.tag == Token::RParens) {
            break;
        } else if (argNameTok.tag != Token::String) {
            throw ParseException("Expected argument name");
        }

        if (ctx->lexer.lex().tag != Token::As) {
            throw ParseException("Expected AS between argument name and type");
        }

        auto argTypeTok = ctx->lexer.lex();
        VariableType type;
        if (argTypeTok.tag == Token::DoubleType) {
            type = Double;
        } else if (argTypeTok.tag == Token::StringType) {
            type = String;
        } else if (argTypeTok.tag == Token::IntegerType) {
            type = Integer;
        } else {
            throw ParseException("Unknown type for argument in extern def");
        }

        args.emplace_back(type, argNameTok.strValue);

        lastToken = ctx->lexer.lex();
    } while (lastToken.tag == Token::Comma);

    if (lastToken.tag != Token::RParens) {
        throw ParseException("Expected closing RParens in extern def");
    }

    auto ret = std::make_unique<ProtoDefAST>(tok, ctx, name, std::move(args), isVarArg, returnType);
    bool inserted;
    std::tie(std::ignore, inserted) = ctx->externFunctions.emplace(name, ret.get());
    if (!inserted) {
        throw ParseException("Duplicate definition of extern def");
    }

    return ret;
}

void FunctionAST::addStatement(std::unique_ptr<ExprAST> statement) {
    BlockAST* body = static_cast<BlockAST*>(_body.get());
    body->addStatement(std::move(statement));
}

void FunctionAST::_setBody(std::unique_ptr<ExprAST> body) {
    _body = std::move(body);
}

std::unique_ptr<ExprAST> FunctionAST::parse(const Token& tok, BasicContext* ctx) {
    std::unique_ptr<ProtoDefAST> proto(
        static_cast<ProtoDefAST*>(ProtoDefAST::parse(tok, ctx).release()));
    if (ctx->lexer.lex().tag != Token::Newline) {
        throw ParseException("Expected new line after function prototype");
    }
    ctx->namedVariables.clear();
    for (const auto& arg : proto->args()) {
        ctx->namedVariables[arg.name] = nullptr;
    }

    auto emptyBody = std::make_unique<BlockAST>(tok, ctx, std::vector<std::unique_ptr<ExprAST>>());
    auto ret = std::make_unique<FunctionAST>(tok, ctx, std::move(proto), std::move(emptyBody));
    ctx->currentFunction = ret.get();
    ret->_setBody(BlockAST::parse(ctx->lexer.lex(), ctx));
    ctx->currentFunction = nullptr;

    return ret;
}
