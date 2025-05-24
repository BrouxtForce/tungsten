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

    static uint32_t s_num_errors = 0;
    static constexpr uint32_t MAX_ERRORS = 32;
    void report(std::string_view message, uint32_t byte_offset, uint32_t byte_length)
    {
        if (++s_num_errors > MAX_ERRORS)
        {
            std::cerr << "Max errors emitted. Stopping now.\n";
            std::abort();
        }

        assert(byte_offset < s_source.size());
        int line_start = 0;
        int line_end = s_source.size();
        int current_line = 1;
        int current_column = 1;
        for (uint32_t i = 0; i < byte_offset; i++)
        {
            if (s_source[i] == '\n')
            {
                line_start = i + 1;
                current_line++;
                current_column = 1;
                continue;
            }
            current_column++;
        }
        for (uint32_t i = byte_offset; i < s_source.size(); i++)
        {
            if (s_source[i] == '\n')
            {
                line_end = i;
                break;
            }
        }

        // TODO: Normalize indents?
        std::cerr << "Error " << s_filename << ":" << current_line << ":" << current_column << ": " << message << "\n\n";
        std::cerr << std::string_view(s_source).substr(line_start, line_end - line_start) << '\n';
        std::cerr << std::string(byte_offset - line_start, ' ') << std::string(byte_length, '~') << "\n\n";
        std::cerr << std::flush;
    }

    bool had_error()
    {
        return s_num_errors > 0;
    }
}
