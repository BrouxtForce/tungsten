cmake_minimum_required(VERSION 3.30)

set(CMAKE_CXX_STANDARD 23)
add_compile_options(-Wall -Wextra -Wpedantic -Wimplicit-fallthrough)

project(Tungsten)

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    add_executable(tungsten
        # src/converter.cpp
        src/error.cpp
        src/lexer.cpp
        src/main.cpp
        src/parser.cpp
        src/utility.cpp
    )
else()
    add_library(tungsten
        src/converter.cpp
        src/error.cpp
        src/lexer.cpp
        src/parser.cpp
        src/utility.cpp
    )
endif()

target_include_directories(tungsten PRIVATE include)
