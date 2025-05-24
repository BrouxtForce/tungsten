#include "error.hpp"

#include <iostream>
#include <string>
#include <cassert>

namespace tungsten::error
{
    static std::string s_filename = "[unknown file]";
    static std::string s_source;
    void init_error_info(std::string_view filename, std::string_view source)
    {
        s_filename = filename;
        s_source = source;
    }

    static bool s_had_error = false;
    void report(std::string_view message, uint32_t byte_offset)
    {
        s_had_error = true;

        assert(byte_offset < s_source.size());
        int current_line = 1;
        int current_column = 1;
        for (int i = 0; i < byte_offset; i++)
        {
            if (s_source[i] == '\n')
            {
                current_line++;
                current_column = 1;
                continue;
            }
            current_column++;
        }

        std::cout << "Error " << s_filename << ":" << current_line << ":" << current_column << ":\n";
        std::cout << message << "\n";
    }

    bool had_error()
    {
        return s_had_error;
    }
}
