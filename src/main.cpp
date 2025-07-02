#include <cstdlib>
#include <iostream>
#include <string>
#include <cassert>
#include <sstream>

#include "lexer.hpp"
#include "utility.hpp"
#include "error.hpp"
#include "parser.hpp"
#include "converter.hpp"

struct Arguments {
    int index = 1;

    int argc = 0;
    char** argv = nullptr;

    bool is_flag()
    {
        assert(index < argc);
        return argv[index][0] == '-' && std::strlen(argv[index]) == 2;
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

    bool should_print_ast = false;
    bool should_print_reflection = false;

    using tungsten::converter::LanguageTarget;
    LanguageTarget output_target = LanguageTarget::LanguageTargetMSL;

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

        std::string_view next_arg = args.read_next();
        if (next_arg.starts_with("--"))
        {
            if (next_arg == "--print-ast")
            {
                should_print_ast = true;
                continue;
            }
            if (next_arg == "--reflection")
            {
                should_print_reflection = true;
                continue;
            }
            if (next_arg == "--wgsl")
            {
                output_target = LanguageTarget::LanguageTargetWGSL;
                continue;
            }
            std::cerr << "Invalid argument '" << next_arg << "'\n";
            return EXIT_FAILURE;
        }

        if (input_filepath.empty())
        {
            input_filepath = next_arg;
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

    if (should_print_ast) {
        using namespace tungsten;

        std::string code = utility::read_file(input_filepath);
        error::init_error_info(input_filepath, code);

        parser::Ast* ast = parser::generate_ast(code);
        parser::print_ast(ast);

        parser::free_ast(ast);
        return EXIT_SUCCESS;
    }

    if (output_filepath.empty())
    {
        std::cerr << "No output file\n";
        return EXIT_FAILURE;
    }

    {
        using namespace tungsten;

        std::string code = utility::read_file(input_filepath);
        error::init_error_info(input_filepath, code);

        parser::Ast* ast = parser::generate_ast(code);
        std::stringstream reflection_stream;

        converter::convert(ast, std::cout, output_target, &reflection_stream);

        if (should_print_reflection)
        {
            std::cout << "/* Reflection info: \n" << reflection_stream.str() << "*/\n";
        }

        parser::free_ast(ast);
    }

    return EXIT_SUCCESS;
}
