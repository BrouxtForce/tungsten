#include "tungsten/utility.hpp"

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

        std::ifstream file(std::string(filepath), std::ios_base::binary);
        file.read(buffer.data(), file_size);

        return buffer;
    }

    bool write_file(std::string_view filepath, std::string_view data)
    {
        std::ofstream file((std::string)filepath);
        file << data;
        file.close();

        bool success = (bool)file;
        if (!success)
        {
            std::cerr << "Failed to write to file '" << filepath << "'\n";
        }
        return success;
    }
}
