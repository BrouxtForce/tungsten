#pragma once

#include <ostream>
#include "tungsten/parser.hpp"

namespace tungsten::converter
{
    void to_msl(const parser::Ast* ast, std::ostream& stream);
    void to_wgsl(const parser::Ast* ast, std::ostream& stream);
    void to_reflection(const parser::Ast* ast, std::ostream& stream);
}
