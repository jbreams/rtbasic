#include "controlflow.h"

#include "functions.h"
#include "variables.h"

#include <iostream>
#include <sstream>

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

std::unique_ptr<ProtoDefAST> makeGosubProto(const LabelAST* label) {
    std::vector<ArgumentAST> args;
    return std::make_unique<ProtoDefAST>(
        label->token(), label->ctx(), label->token().strValue, std::move(args), false, Void);
}

std::unique_ptr<FunctionCallAST> makeGosubCall(const Token& tok,
                                               BasicContext* ctx,
                                               std::string name) {
    std::vector<std::unique_ptr<ExprAST>> args;
    return std::make_unique<FunctionCallAST>(tok, ctx, name, std::move(args));
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

std::unique_ptr<ExprAST> IfAST::parse(const Token& tok, BasicContext* ctx) {
    auto condExpr = RelOpExprAST::parse(tok, ctx);
    auto nextTok = ctx->lexer.lex();
    std::unique_ptr<ExprAST> statementExpr;
    if (nextTok.tag == Token::Then) {
        nextTok = ctx->lexer.lex();
    }

    if (nextTok.tag == Token::Newline) {
        statementExpr = BlockAST::parse(ctx->lexer.lex(), ctx, {Token::Else});
    } else {
        statementExpr = StatementAST::parse(nextTok, ctx);
    }

    nextTok = ctx->lexer.lex();
    std::unique_ptr<ExprAST> elseBody;

    if (nextTok.tag == Token::Else) {
        nextTok = ctx->lexer.lex();
        if (nextTok.tag == Token::Newline) {
            elseBody = BlockAST::parse(ctx->lexer.lex(), ctx);
        } else {
            elseBody = StatementAST::parse(nextTok, ctx);
        }
    } else if (!nextTok.isEnding()) {
        throw ParseException(nextTok, "Expected new statement after if/then without else");
    } else {
        ctx->lexer.putBack(nextTok);
    }
    return std::make_unique<IfAST>(
        tok, ctx, std::move(condExpr), std::move(statementExpr), std::move(elseBody));
}


std::unique_ptr<ExprAST> WhileAST::parse(const Token& tok, BasicContext* ctx) {
    auto nextTok = ctx->lexer.peek();
    bool isNot = (nextTok.tag == Token::Not);
    if (isNot) {
        ctx->lexer.lex();
    }

    auto condExpr = RelOpExprAST::parse(tok, ctx);
    std::unique_ptr<ExprAST> bodyExpr;
    nextTok = ctx->lexer.lex();
    if (nextTok.tag == Token::Newline) {
        bodyExpr = BlockAST::parse(ctx->lexer.lex(), ctx, {Token::Wend});
    } else {
        bodyExpr = StatementAST::parse(nextTok, ctx);
    }

    nextTok = ctx->lexer.lex();
    if (nextTok.tag != Token::Wend) {
        throw ParseException(nextTok, "Expected WEND at end of WHILE");
    }

    return std::make_unique<WhileAST>(tok, ctx, std::move(condExpr), isNot, std::move(bodyExpr));
}

std::unique_ptr<ExprAST> DoAST::parse(const Token& tok, BasicContext* ctx) {
    std::unique_ptr<ExprAST> bodyExpr;
    auto nextTok = ctx->lexer.lex();
    if (nextTok.tag == Token::Newline) {
        bodyExpr = BlockAST::parse(ctx->lexer.lex(), ctx, {Token::Loop});
    } else {
        bodyExpr = StatementAST::parse(nextTok, ctx);
    }

    nextTok = ctx->lexer.lex();
    if (nextTok.tag != Token::Loop) {
        throw ParseException(nextTok, "Expected LOOP at end of DO");
    }

    nextTok = ctx->lexer.lex();
    if (nextTok.tag != Token::Until) {
        throw ParseException(nextTok, "Expected UNTIL after LOOP in DO statement");
    }

    auto condExpr = RelOpExprAST::parse(tok, ctx);

    return std::make_unique<DoAST>(tok, ctx, std::move(condExpr), std::move(bodyExpr));
}

std::unique_ptr<ExprAST> ForAST::parse(const Token& tok, BasicContext* ctx) {
    auto controlVar = LetAST::parse(ctx->lexer.lex(), ctx, false);

    auto toTok = ctx->lexer.lex();
    if (toTok.tag != Token::To) {
        throw ParseException(toTok, "Expected TO after FOR let statement");
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
    if (bodyTok.tag == Token::Newline) {
        bodyExpr = BlockAST::parse(ctx->lexer.lex(), ctx, {Token::Next});
    } else {
        bodyExpr = StatementAST::parse(bodyTok, ctx);
    }

    auto nextTok = ctx->lexer.lex();
    if (nextTok.tag != Token::Next) {
        throw ParseException(nextTok, "Expected NEXT after for body");
    }

    return std::make_unique<ForAST>(tok,
                                    ctx,
                                    std::move(controlVar),
                                    std::move(endExpr),
                                    std::move(stepExpr),
                                    std::move(bodyExpr));
}
llvm::Value* GotoAST::codegen() {
    auto& builder = ctx()->builder;
    auto labelIt = ctx()->labels.find(_label);
    if (labelIt == ctx()->labels.end() || !labelIt->second.label) {
        std::cerr << "Could not find label for goto statement: " << _label;
        return nullptr;
    }
    builder.CreateBr(static_cast<llvm::BasicBlock*>(labelIt->second.label->value()));
    auto newBB = llvm::BasicBlock::Create(ctx()->context, "afterGoto", ctx()->getCurrentFunction());
    builder.SetInsertPoint(newBB);
    return newBB;
}

llvm::Value* GosubAST::codegen() {
    std::cerr << "Gosubs aren't a real codegen type" << std::endl;
    return nullptr;
}

llvm::Value* ReturnAST::codegen() {
    return ctx()->builder.CreateRetVoid();
}

llvm::Value* WhileAST::codegen() {
    auto& builder = ctx()->builder;

    auto loopBB = llvm::BasicBlock::Create(ctx()->context, "loop");
    auto condBB = llvm::BasicBlock::Create(ctx()->context, "while");
    auto afterBB = llvm::BasicBlock::Create(ctx()->context, "afterWhile");
    builder.CreateBr(condBB);

    ctx()->getCurrentFunction()->getBasicBlockList().push_back(condBB);
    builder.SetInsertPoint(condBB);
    auto condResult = _condExpr->codegen();
    if (!condResult)
        return nullptr;

    if (_isNot) {
        builder.CreateCondBr(condResult, afterBB, loopBB);
    } else {
        builder.CreateCondBr(condResult, loopBB, afterBB);
    }

    ctx()->getCurrentFunction()->getBasicBlockList().push_back(loopBB);
    builder.SetInsertPoint(loopBB);
    if (!_body->codegen())
        return nullptr;
    builder.CreateBr(condBB);

    ctx()->getCurrentFunction()->getBasicBlockList().push_back(afterBB);
    builder.SetInsertPoint(afterBB);
    return condResult;
}

llvm::Value* DoAST::codegen() {
    auto& builder = ctx()->builder;

    auto loopBB = llvm::BasicBlock::Create(ctx()->context, "loop");
    auto condBB = llvm::BasicBlock::Create(ctx()->context, "until");
    auto afterBB = llvm::BasicBlock::Create(ctx()->context, "afterDo");

    builder.CreateBr(loopBB);
    ctx()->getCurrentFunction()->getBasicBlockList().push_back(loopBB);
    builder.SetInsertPoint(loopBB);
    if (!_body->codegen())
        return nullptr;

    builder.CreateBr(condBB);

    ctx()->getCurrentFunction()->getBasicBlockList().push_back(condBB);
    builder.SetInsertPoint(condBB);
    auto condResult = _condExpr->codegen();
    if (!condResult)
        return nullptr;

    builder.CreateCondBr(condResult, afterBB, loopBB);

    ctx()->getCurrentFunction()->getBasicBlockList().push_back(afterBB);
    builder.SetInsertPoint(afterBB);
    return condResult;
}

llvm::Value* ForAST::codegen() {
    auto& builder = ctx()->builder;
    auto controlVar = static_cast<LetAST*>(_controlVar.get());

    if (!controlVar->codegen()) {
        std::cerr << "Error generating control variable" << std::endl;
        return nullptr;
    }
    auto controlVarIsInt = controlVar->nativeType()->isIntegerTy();

    auto loopBB = llvm::BasicBlock::Create(ctx()->context, "loop");

    builder.CreateBr(loopBB);
    ctx()->getCurrentFunction()->getBasicBlockList().push_back(loopBB);
    builder.SetInsertPoint(loopBB);

    auto endV = _toExpr->codegen();
    if (!endV) {
        std::cerr << "Error generating end condition for for loop" << std::endl;
        return nullptr;
    }

    auto curVar = builder.CreateLoad(controlVar->lookup());
    if (controlVarIsInt) {
        endV = builder.CreateICmpSLT(curVar, endV, "loopcond");
    } else {
        endV = builder.CreateFCmpOLT(curVar, endV, "loopcond");
    }

    auto bodyBB = llvm::BasicBlock::Create(ctx()->context, "loopbody", ctx()->getCurrentFunction());
    auto afterBB = llvm::BasicBlock::Create(ctx()->context, "afterloop");
    builder.CreateCondBr(endV, bodyBB, afterBB);
    builder.SetInsertPoint(bodyBB);

    if (!_bodyExpr->codegen()) {
        std::cerr << "Error generating for loop body" << std::endl;
        return nullptr;
    }

    curVar = builder.CreateLoad(controlVar->lookup());
    llvm::Value* stepV = nullptr;
    if (_stepExpr) {
        stepV = _stepExpr->codegen();
        if (!stepV) {
            std::cerr << "Error generating step expression" << std::endl;
            return nullptr;
        }
    } else {
        if (controlVarIsInt) {
            stepV = ctx()->makeLiteralInteger(1.0);
        } else {
            stepV = ctx()->makeLiteralDouble(1);
        }
    }

    llvm::Value* nextVar;
    if (controlVarIsInt) {
        nextVar = builder.CreateAdd(curVar, stepV, "nextvar");
    } else {
        nextVar = builder.CreateFAdd(curVar, stepV, "nextvar");
    }
    builder.CreateStore(nextVar, controlVar->lookup());
    builder.CreateBr(loopBB);

    ctx()->getCurrentFunction()->getBasicBlockList().push_back(afterBB);
    builder.SetInsertPoint(afterBB);

    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(ctx()->context));
}

LetAST* ForAST::_getControlVar() const {
    return static_cast<LetAST*>(_controlVar.get());
}

llvm::Value* IfAST::codegen() {
    auto cond = _condition->codegen();
    if (!cond) {
        std::cerr << "Error generating conditional for if" << std::endl;
        return nullptr;
    }

    auto& builder = ctx()->builder;
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

    thenBB = builder.GetInsertBlock();
    if (!thenBB->getTerminator()) {
        builder.CreateBr(mergeBB);
    }

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

    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(mergeBB);
    }
    curFunction->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);

    return cond;
}
