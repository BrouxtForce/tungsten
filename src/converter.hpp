#pragma once

#include <ostream>
#include "parser.hpp"

namespace tungsten::converter
{
    enum LanguageTarget
    {
        LanguageTargetMSL,
        LanguageTargetWGSL
    };

    void convert(const parser::Ast* ast, std::ostream& stream, LanguageTarget output_target);
}
