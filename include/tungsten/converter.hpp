#pragma once

#include <ostream>
#include "tungsten/parser.hpp"

namespace tungsten::converter
{
    void msl_assign_bindings(parser::Ast* ast);
    void wgsl_assign_bindings(parser::Ast* ast);

    void to_msl(const parser::Ast* ast, std::ostream& stream);
    void to_wgsl(const parser::Ast* ast, std::ostream& stream);
}
