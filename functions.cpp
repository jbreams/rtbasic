#include <iostream>

#include "functions.h"

std::unique_ptr<ExprAST> FunctionCallAST::parse(const Token& tok, BasicContext* ctx) {
    const auto& functionName = tok.strValue;

    Token curTok = ctx->lexer.lex();
    if (curTok.tag != Token::LParens) {
        throw ParseException(curTok, "No LParens to start argument list of function call");
    }

    std::vector<std::unique_ptr<ExprAST>> args;
    do {
        auto arg = ExprAST::parse(ctx->lexer.lex(), ctx);
        if (arg) {
            args.push_back(std::move(arg));
        }
        curTok = ctx->lexer.lex();
    } while (curTok.tag == Token::Comma);

    if (curTok.tag != Token::RParens) {
        throw ParseException(curTok, "No RParens at end of argument list of function call");
    }

    return std::make_unique<FunctionCallAST>(tok, ctx, std::move(functionName), std::move(args));
}

std::unique_ptr<ExprAST> ProtoDefAST::parse(const Token& tok, BasicContext* ctx) {
    auto nameTok = ctx->lexer.lex();
    if (nameTok.tag != Token::String && nameTok.tag != Token::Print &&
        nameTok.tag != Token::Input && nameTok.tag != Token::Variable) {
        throw ParseException(nameTok, "Expected function name");
    }

    VariableType returnType = Void;
    std::string name;
    if (tok.tag == Token::Function) {
        name = ctx->makeVariableName(nameTok, &returnType);
    } else {
        name = nameTok.strValue;
    }

    auto lParens = ctx->lexer.lex();
    if (lParens.tag != Token::LParens) {
        throw ParseException(lParens, "Expected LParens to start argument list of extern def");
    }

    bool isVarArg = false;
    auto args = parseParensList<ArgumentAST>(ctx, [&isVarArg, ctx](Token argNameTok) {
        VariableType type = Void;
        std::string name = ctx->makeVariableName(argNameTok, &type);
        if (argNameTok.tag == Token::Ellipsis) {
            isVarArg = true;
            return ArgumentAST(argNameTok, ctx, Void, "...");
        } else if (argNameTok.tag != Token::String && argNameTok.tag != Token::Variable) {
            throw ParseException(argNameTok, "Expected argument name to be a string");
        }

        auto maybeAs = ctx->lexer.lex();
        if (maybeAs.tag != Token::As) {
            ctx->lexer.putBack(maybeAs);
            if (type == Void) {
                type = Integer;
            }
            return ArgumentAST(argNameTok, ctx, type, name);
        }

        auto argTypeTok = ctx->lexer.lex();
        if (argTypeTok.tag == Token::DoubleType) {
            type = Double;
        } else if (argTypeTok.tag == Token::StringType) {
            type = String;
        } else if (argTypeTok.tag == Token::IntegerType) {
            type = Integer;
        } else {
            throw ParseException(argTypeTok, "Unknown type for argument in extern def");
        }

        return ArgumentAST(argNameTok, ctx, type, name);
    });

    if (isVarArg) {
        if (args.back().name() != "...") {
            throw ParseException(args.back().token(),
                                 "Function prototype has a va_arg, but doesn't end in ...");
        }
        args.pop_back();
    }

    if (ctx->lexer.peek().tag == Token::Return) {
        ctx->lexer.lex();
        auto retTok = ctx->lexer.lex();
        switch (retTok.tag) {
            case Token::IntegerType:
                returnType = Integer;
                break;
            case Token::DoubleType:
                returnType = Double;
                break;
            case Token::StringType:
                returnType = String;
                break;
            default:
                throw ParseException(retTok, "Expected type name for return type");
        }
    }

    auto ret = std::make_unique<ProtoDefAST>(tok, ctx, name, std::move(args), isVarArg, returnType);
    bool inserted;
    std::tie(std::ignore, inserted) = ctx->externFunctions.emplace(name, ret.get());
    if (!inserted) {
        throw ParseException(tok, "Duplicate definition of extern def");
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
    auto newLineTok = ctx->lexer.lex();
    if (newLineTok.tag != Token::Newline) {
        throw ParseException(newLineTok, "Expected new line after function prototype");
    }

    ctx->clearNonGlobalVariables();
    for (auto& arg : proto->args()) {
        ctx->namedVariables[arg.name()] = static_cast<VariableDeclartionAST*>(&arg);
    }

    auto emptyBody = std::make_unique<BlockAST>(tok, ctx, std::vector<std::unique_ptr<ExprAST>>());
    auto ret = std::make_unique<FunctionAST>(tok, ctx, std::move(proto), std::move(emptyBody));
    ctx->currentFunction = ret.get();
    ret->_setBody(BlockAST::parse(ctx->lexer.lex(), ctx, {Token::End}));
    ctx->currentFunction = nullptr;

    auto endTok = ctx->lexer.lex();
    if (endTok.tag != Token::End) {
        throw ParseException(endTok, "Expected END at end of function body");
    }

    return ret;
}

std::unique_ptr<ExprAST> ReturnAST::parse(const Token& tok, BasicContext* ctx) {
    return std::make_unique<ReturnAST>(tok, ctx);
}

llvm::Value* FunctionCallAST::codegen() {
    auto callee = ctx()->getFunction(_function);
    if (!callee) {
        std::cerr << "Unknown function call" << std::endl;
        return nullptr;
    }

    if (_args.size() < callee->arg_size() ||
        (_args.size() > callee->arg_size() && !callee->isVarArg())) {
        std::cerr << "Incorrect number of arguments for function call" << std::endl;
        return nullptr;
    }

    std::vector<llvm::Value*> args;
    for (auto& argExpr : _args) {
        args.push_back(argExpr->codegen());
        if (args.back() == nullptr) {
            std::cerr << "Error generating argument for function call" << std::endl;
            return nullptr;
        }
    }

    if (callee->getReturnType()->isVoidTy()) {
        return ctx()->builder.CreateCall(callee, args);
    } else {
        return ctx()->builder.CreateCall(callee, args, "calltmp");
    }
}

llvm::Value* ProtoDefAST::codegen() {
    if (auto preGenerated = ctx()->getFunction(_name)) {
        return preGenerated;
    }

    std::vector<llvm::Type*> args;
    for (const auto& arg : _args) {
        args.push_back(arg.nativeType());
    }

    llvm::Type* returnType = astTypeToNativeType(ctx(), _returnType);

    auto functionType = llvm::FunctionType::get(returnType, args, _isVarArg);

    _function = llvm::Function::Create(
        functionType, llvm::Function::ExternalLinkage, _name, ctx()->module.get());
    int idx = 0;
    for (auto& arg : _function->args()) {
        arg.setName(_args[idx].name());
    }

    return _function;
}

llvm::Function* FunctionAST::function() const {
    return _function;
}

llvm::Value* FunctionAST::codegen() {
    _function = ctx()->getFunction(_proto->name());
    if (!_function) {
        std::cerr << "Error finding prototype for function" << std::endl;
        return nullptr;
    }

    ctx()->currentFunction = this;
    auto bb = llvm::BasicBlock::Create(ctx()->context, "entry", _function);
    ctx()->builder.SetInsertPoint(bb);

    ctx()->clearNonGlobalVariables();
    for (auto& arg : _proto->args()) {
        if (!arg.codegen()) {
            return nullptr;
        }
    }
    auto codegenResult = _body->codegen();

    auto lastBlock = ctx()->builder.GetInsertBlock();
    if (!lastBlock->getTerminator()) {
        if (_proto->returnType() == Void)
            ctx()->builder.CreateRetVoid();
        else
            ctx()->builder.CreateRet(codegenResult);
    }
    ctx()->currentFunction = nullptr;

    if (codegenResult) {
        return _function;
    } else {
        std::cerr << "Error parsing function body" << std::endl;
        _function->eraseFromParent();
        return nullptr;
    }
}
