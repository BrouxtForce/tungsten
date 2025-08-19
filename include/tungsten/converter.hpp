#pragma once

#include <ostream>
#include "tungsten/parser.hpp"

namespace tungsten::converter
{
    enum LanguageTarget
    {
        LanguageTargetMSL,
        LanguageTargetWGSL
    };

    void convert(const parser::Ast* ast, std::ostream& stream, LanguageTarget output_target, std::ostream* reflection_stream = nullptr);

    void to_msl(const parser::Ast* ast, std::ostream& stream);
    void to_wgsl(const parser::Ast* ast, std::ostream& stream);
    void to_reflection(const parser::Ast* ast, std::ostream& stream);
}
