#pragma once

#include <string>
#include <string_view>

namespace tungsten::utility
{
    [[nodiscard]] std::string read_file(std::string_view filepath);

    bool write_file(std::string_view filepath, std::string_view data);
}
