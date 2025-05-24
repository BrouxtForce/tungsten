#include <cstdlib>
#include <iostream>
#include <string>
#include <cassert>

#include "lexer.hpp"
#include "utility.hpp"
#include "error.hpp"
#include "parser.hpp"

struct Arguments {
    int index = 1;

    int argc = 0;
    char** argv = nullptr;

    bool is_flag()
    {
        assert(index < argc);
        return argv[index][0] == '-';
    }

    const char* read_next()
    {
        if (index < argc)
        {
            return argv[index++];
        }
        return "";
    }

    char read_flag()
    {
        assert(is_flag());
        return argv[index++][1];
    }

    bool done()
    {
        return index >= argc;
    }
};

int main(int argc, char** argv)
{
    Arguments args = { .argc = argc, .argv = argv };

    std::string input_filepath;
    std::string output_filepath;

    while (!args.done())
    {
        if (args.is_flag())
        {
            char flag = args.read_flag();
            switch (flag)
            {
                case 'o':
                    if (output_filepath.empty())
                    {
                        output_filepath = args.read_next();
                        break;
                    }
                    std::cerr << "Extraneous output argument\n";
                    return EXIT_FAILURE;
                default:
                    std::cerr << "unknown flag: '" << flag << "'\n";
                    return EXIT_FAILURE;
            }
            continue;
        }

        if (input_filepath.empty())
        {
            input_filepath = args.read_next();
            continue;
        }
        std::cerr << "Extraneous input argument\n";
        return EXIT_FAILURE;
    }

    if (input_filepath.empty())
    {
        std::cerr << "No input file\n";
        return EXIT_FAILURE;
    }
    if (output_filepath.empty())
    {
        std::cerr << "No output file\n";
        return EXIT_FAILURE;
    }

    std::cout << "input filepath: '" << input_filepath << "'\n";
    std::cout << "output filepath: '" << output_filepath << "'\n";

    {
        using namespace tungsten;

        std::string code = utility::read_file(input_filepath);
        error::init_error_info(input_filepath, code);

        parser::Ast* ast = parser::generate_ast(code);
        parser::print_ast(ast);

        parser::free_ast(ast);
    }

    return EXIT_SUCCESS;
}
