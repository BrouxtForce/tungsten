#include "tungsten/error.hpp"

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

    struct SourceLocation
    {
        uint32_t line;
        uint32_t column;
    };

    static uint32_t s_num_errors = 0;
    static constexpr uint32_t MAX_ERRORS = 32;
    void report(std::string_view message, uint32_t byte_offset, uint32_t byte_length)
    {
        if (++s_num_errors > MAX_ERRORS)
        {
            std::cerr << "Max errors emitted. Stopping now.\n";
            std::abort();
        }

        assert(byte_offset < s_source.size() && byte_offset + byte_length <= s_source.size());

        uint32_t current_line_byte_offset = 0;
        uint32_t current_line = 1;
        uint32_t current_column = 1;
        for (uint32_t i = 0; i < byte_offset; i++)
        {
            if (s_source[i] == '\n')
            {
                current_line_byte_offset = i + 1;
                current_line++;
                current_column = 1;
                continue;
            }
            current_column++;
        }

        // TODO: Normalize indents?

        std::cerr << "Error " << s_filename << ":" << current_line << ":" << current_column << ": " << message << "\n\n";
        for (uint32_t i = current_line_byte_offset;; i++)
        {
            bool should_break = i >= s_source.size();

            char next_char = should_break ? '\n' : s_source[i];
            if (next_char == '\n')
            {
                std::cerr << std::string_view(s_source).substr(current_line_byte_offset, i - current_line_byte_offset) << '\n';

                if (byte_offset + byte_length <= i)
                {
                    should_break = true;
                }

                // Error starts after the beginning of this line
                if (current_line_byte_offset < byte_offset)
                {
                    std::cerr << std::string(byte_offset - current_line_byte_offset, ' ') << std::string(std::min(i - byte_offset, byte_length), '~') << '\n';
                }
                // Error ends before the end of the line
                else if (byte_offset + byte_length <= i)
                {
                    assert(byte_offset + byte_length >= current_line_byte_offset);
                    std::cerr << std::string(byte_offset + byte_length - current_line_byte_offset, '~') << '\n';
                }
                // Error exists on the entire line
                else
                {
                    std::cerr << std::string(i - current_line_byte_offset, '~') << '\n';
                }

                current_line_byte_offset = i + 1;
            }

            if (should_break)
            {
                break;
            }
        }
        std::cerr << '\n';
    }

    void check(bool condition, std::string_view message, uint32_t byte_offset, uint32_t byte_length)
    {
        if (condition)
        {
            return;
        }
        error::report(message, byte_offset, byte_length);
    }

    bool had_error()
    {
        return s_num_errors > 0;
    }
}
