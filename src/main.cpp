#include <cstdlib>
#include <iostream>
#include <string>
#include <cassert>
#include <sstream>
#include <vector>

#include "tungsten/utility.hpp"
#include "tungsten/error.hpp"
#include "tungsten/parser.hpp"
#include "tungsten/converter.hpp"

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
    std::vector<std::string> output_filepaths;

    bool should_print_ast = false;
    bool should_print_msl = false;
    bool should_print_wgsl = false;
    bool should_print_reflection = false;

    while (!args.done())
    {
        if (args.is_flag())
        {
            char flag = args.read_flag();
            switch (flag)
            {
                case 'o': {
                    std::string_view next = args.read_next();
                    if (next.empty())
                    {
                        std::cerr << "Missing output argument";
                        return EXIT_FAILURE;
                    }
                    output_filepaths.push_back((std::string)next);
                    break;
                }
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
            if (next_arg == "--msl")
            {
                should_print_msl = true;
                continue;
            }
            if (next_arg == "--wgsl")
            {
                should_print_wgsl = true;
                continue;
            }
            if (next_arg == "--reflection")
            {
                should_print_reflection = true;
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

    if (output_filepaths.empty())
    {
        using namespace tungsten;

        std::string code = utility::read_file(input_filepath);
        error::init_error_info(input_filepath, code);
        parser::Ast* ast = parser::generate_ast(code);

        std::stringstream reflection_stream;
        if (should_print_msl)
        {
            converter::convert(ast, std::cout, converter::LanguageTargetMSL, &reflection_stream);
        }
        else if (should_print_wgsl)
        {
            converter::convert(ast, std::cout, converter::LanguageTargetWGSL, &reflection_stream);
        }
        else
        {
            std::cerr << "If no output files are provided, --msl or --wgsl must be provided.\n";
            return EXIT_FAILURE;
        }
        if (should_print_reflection)
        {
            std::cout << "/* reflection info:\n" << reflection_stream.str() << "*/\n";
        }

        parser::free_ast(ast);
        return EXIT_SUCCESS;
    }

    {
        using namespace tungsten;

        std::string code = utility::read_file(input_filepath);
        error::init_error_info(input_filepath, code);

        std::string_view msl_output_filepath;
        std::string_view wgsl_output_filepath;
        std::string_view reflection_output_filepath;
        for (std::string_view filepath : output_filepaths)
        {
            if (filepath.ends_with(".metal"))
            {
                if (!msl_output_filepath.empty())
                {
                    std::cerr << "There should only be one .metal output.\n";
                    return EXIT_FAILURE;
                }
                msl_output_filepath = filepath;
                continue;
            }
            if (filepath.ends_with(".wgsl"))
            {
                if (!wgsl_output_filepath.empty())
                {
                    std::cerr << "There should only be one .wgsl output.\n";
                    return EXIT_FAILURE;
                }
                wgsl_output_filepath = filepath;
                continue;
            }
            if (filepath.ends_with(".txt"))
            {
                if (!reflection_output_filepath.empty())
                {
                    std::cerr << "There should only be one .txt output.\n";
                    return EXIT_FAILURE;
                }
                reflection_output_filepath = filepath;
                continue;
            }
            std::cerr << "Invalid file extension: '" << filepath << "'\n";
            return EXIT_FAILURE;
        }

        parser::Ast* ast = parser::generate_ast(code);

        std::stringstream reflection_stream;
        bool fail = false;
        if (!msl_output_filepath.empty())
        {
            // NOTE: reflection_stream may be filled twice if both MSL and WGSL are being outputted
            std::stringstream msl_stream;
            converter::convert(ast, msl_stream, converter::LanguageTargetMSL, &reflection_stream);
            fail = fail || !utility::write_file(msl_output_filepath, msl_stream.str());
        }
        if (!wgsl_output_filepath.empty())
        {
            reflection_stream.str("");

            std::stringstream wgsl_stream;
            converter::convert(ast, wgsl_stream, converter::LanguageTargetWGSL, &reflection_stream);
            fail = fail || !utility::write_file(wgsl_output_filepath, wgsl_stream.str());
        }
        if (!reflection_output_filepath.empty())
        {
            fail = fail || !utility::write_file(reflection_output_filepath, reflection_stream.str());
        }

        parser::free_ast(ast);

        if (fail)
        {
            return EXIT_FAILURE;
        }
    }

    return EXIT_SUCCESS;
}
