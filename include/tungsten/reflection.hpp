#pragma once

#include "tungsten/parser.hpp"

#include <vector>

namespace tungsten::reflection
{
    struct StructMemberReflection
    {
        std::string type_name;
        std::string name;
        std::vector<parser::Attribute> attributes;
    };

    struct UniformGroupReflection
    {
        std::string name;
        uint32_t binding;
        std::vector<StructMemberReflection> members;
    };

    struct VertexGroupReflection
    {
        std::string name;
        std::vector<StructMemberReflection> members;
        std::vector<parser::Attribute> attributes;
    };

    struct FunctionReflection
    {
        bool is_vertex_function;
        bool is_fragment_function;
        bool is_compute_function;
        std::string name;
        std::string vertex_input;
        std::vector<parser::Attribute> attributes;
    };

    struct ReflectionInfo
    {
        std::vector<UniformGroupReflection> uniform_groups;
        std::vector<VertexGroupReflection> vertex_groups;
        std::vector<FunctionReflection> functions;
    };

    ReflectionInfo get_reflection_info(const parser::Ast* ast);
}
