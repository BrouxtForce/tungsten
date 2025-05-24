cmake_minimum_required(VERSION 3.30)

set(CMAKE_CXX_STANDARD 20)
add_compile_options(-Wall -Wextra -Wpedantic -Wimplicit-fallthrough)

project(Tungsten)

add_executable(tungsten
    src/error.cpp
    src/lexer.cpp
    src/main.cpp
    src/utility.cpp
)

target_include_directories(tungsten PRIVATE src)
