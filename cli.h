#pragma once

#include <string>

struct Options {
    std::string inputFilename;
    std::string outputFilename;
    std::string mainFunctionName;
    enum OutputType {
        LLVMIR,
        Assembly,
        ObjectCode,
    };
    OutputType outputType;
    static Options parseOptions(int argc, char** argv);
};
