#include <cassert>
#include <fstream>
#include <iostream>

#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"

#include "ast.h"
#include "lexer.h"

int main(int argc, char** argv) {
    std::ifstream stream(argv[1]);
    Lexer lexer(&stream);

    BasicContext ctx(std::move(lexer), argv[1]);

    auto programBody = BlockAST::parse(ctx.lexer.lex(), &ctx);
    if (!ctx.codegenAllProtos())
        return 1;

    auto topLevel = programBody->codegen();
    if (!topLevel)
        return 1;

    if (!ctx.mainFunction->codegen())
        return 1;

    int errFd = fileno(stderr);
    llvm::raw_fd_ostream errStream(errFd, false);

    ctx.module->print(errStream, nullptr);

    if (llvm::verifyModule(*ctx.module, &errStream))
        return 1;

    return 0;
}
