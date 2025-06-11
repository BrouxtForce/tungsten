#pragma once

#include <ostream>
#include "parser.hpp"

namespace tungsten::converter
{
    void to_msl(const parser::Ast* ast, std::ostream& stream);
}
