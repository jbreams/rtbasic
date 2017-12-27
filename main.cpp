#include <cassert>
#include <fstream>
#include <iostream>

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include "ast.h"
#include "cli.h"
#include "lexer.h"

llvm::TargetMachine* buildTargetMachine() {
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);

    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!target) {
        std::cerr << "Error getting target: " << error << std::endl;
        return nullptr;
    }

    auto CPU = "generic";
    auto features = "";

    llvm::TargetOptions opt;
    auto RM = llvm::Optional<llvm::Reloc::Model>(llvm::Reloc::PIC_);
    return target->createTargetMachine(targetTriple, CPU, features, opt, RM);
}

int main(int argc, char** argv) {
    auto opts = Options::parseOptions(argc, argv);
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::ifstream inputFile;
    if (opts.inputFilename != "-") {
        inputFile = std::ifstream(opts.inputFilename);
    }
    Lexer lexer(opts.inputFilename == "-" ? &std::cin : &inputFile);

    BasicContext ctx(std::move(lexer), opts.inputFilename, opts.mainFunctionName);

    auto programBody = BlockAST::parse(ctx.lexer.lex(), &ctx, {Token::End}, true);

    auto target = buildTargetMachine();
    if (!target)
        return 1;

    ctx.module->setTargetTriple(target->getTargetTriple().str());
    ctx.module->setDataLayout(target->createDataLayout());
    if (!ctx.codegenAllProtos())
        return 1;

    auto topLevel = programBody->codegen();
    if (!topLevel)
        return 1;

    if (!ctx.mainFunction->codegen())
        return 1;

    llvm::raw_os_ostream errStream(std::cerr);
    if (opts.verifyModule && llvm::verifyModule(*ctx.module, &errStream))
        return 1;

    std::unique_ptr<llvm::raw_fd_ostream> output;
    llvm::SmallString<128> path(opts.outputFilename);
    if (opts.outputFilename.empty()) {
        path = opts.inputFilename;
        switch (opts.outputType) {
            case Options::LLVMIR:
                llvm::sys::path::replace_extension(path, "ll");
                break;
            case Options::Assembly:
                llvm::sys::path::replace_extension(path, "s");
                break;
            case Options::ObjectCode:
                llvm::sys::path::replace_extension(path, "o");
                break;
        }
    }

    std::error_code ec;
    output = std::make_unique<llvm::raw_fd_ostream>(path, ec, llvm::sys::fs::F_None);
    if (ec) {
        std::cerr << "Error opening output file: " << ec;
        return 1;
    }

    llvm::legacy::PassManager passManager;
    llvm::TargetMachine::CodeGenFileType fileType;
    switch (opts.outputType) {
        case Options::Assembly:
            fileType = llvm::TargetMachine::CGFT_AssemblyFile;
            break;
        case Options::ObjectCode:
            fileType = llvm::TargetMachine::CGFT_ObjectFile;
            break;
        case Options::LLVMIR:
            ctx.module->print(*output, nullptr);
            return 0;
    }

    if (target->addPassesToEmitFile(passManager, *output, fileType)) {
        std::cerr << "Cannot emit file of this type" << std::endl;
        return 1;
    }

    passManager.run(*ctx.module);
    output->flush();
    return 0;
}
