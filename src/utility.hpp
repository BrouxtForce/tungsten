#pragma once

#include <string>
#include <string_view>

namespace tungsten::utility
{
    std::string read_file(std::string_view filepath);
}
