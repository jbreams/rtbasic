cmake_minimum_required(VERSION 3.4.3)
project(RTBasic)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

find_package(LLVM REQUIRED CONFIG
             PATHS ${search_paths}
             NO_DEFAULT_PATH)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Now build our tools
add_executable(rtbasic
    ast.cpp
    lexer.cpp
    codegen.cpp
    main.cpp
    cli.cpp
    controlflow.cpp
    functions.cpp
    variables.cpp
)

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs core support codegen)

# Link against LLVM libraries
target_link_libraries(rtbasic LLVM)
