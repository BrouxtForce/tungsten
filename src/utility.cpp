#include "utility.hpp"

#include <filesystem>
#include <fstream>
#include <iostream>

namespace tungsten::utility
{
    std::string read_file(std::string_view filepath)
    {
        if (!std::filesystem::is_regular_file(filepath))
        {
            std::cerr << "Failed to open '" << filepath << "'\n";
            return {};
        }

        size_t file_size = std::filesystem::file_size(filepath);
        if (file_size == 0)
        {
            return {};
        }

        std::string buffer(file_size, '\0');

        std::ifstream file(filepath, std::ios_base::binary);
        file.read(buffer.data(), file_size);

        return buffer;
    }
}
