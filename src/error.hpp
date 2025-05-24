#pragma once

#include <string_view>

namespace tungsten::error
{
    void init_error_info(std::string_view filename, std::string_view source);

    void report(std::string_view message, uint32_t byte_offset);

    bool had_error();
}
