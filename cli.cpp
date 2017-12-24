#include "cli.h"

#include "llvm/Support/CommandLine.h"

Options Options::parseOptions(int argc, char** argv) {
    Options ret;
    llvm::cl::OptionCategory CompilerCategory("Compiler Options",
                                              "Options for controlling the compilation process.");

    static llvm::cl::opt<std::string> inputFilename(llvm::cl::Positional,
                                                    llvm::cl::desc("<input file>"),
                                                    llvm::cl::init("-"),
                                                    llvm::cl::cat(CompilerCategory));

    static llvm::cl::opt<std::string> outputFilename("o",
                                                     llvm::cl::desc("Output filename"),
                                                     llvm::cl::value_desc("filename"),
                                                     llvm::cl::cat(CompilerCategory));
    static llvm::cl::opt<std::string> mainFunctionName("mainFunctionName",
                                                       llvm::cl::desc("Main function name"),
                                                       llvm::cl::value_desc("name"),
                                                       llvm::cl::cat(CompilerCategory),
                                                       llvm::cl::init("main"));

    static llvm::cl::opt<OutputType> outputType(
        llvm::cl::desc("Output file type"),
        llvm::cl::values(clEnumValN(LLVMIR, "E", "Emit LLVM IR as text"),
                         clEnumValN(Assembly, "S", "Emit assembly as text"),
                         clEnumValN(ObjectCode, "c", "Emit binary object code")),
        llvm::cl::cat(CompilerCategory),
        llvm::cl::init(ObjectCode));

    llvm::cl::HideUnrelatedOptions(CompilerCategory);

    llvm::cl::ParseCommandLineOptions(argc, argv);


    return Options{inputFilename.getValue(),
                   outputFilename.getValue(),
                   mainFunctionName.getValue(),
                   outputType.getValue()};
}
