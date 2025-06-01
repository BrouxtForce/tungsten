#pragma once

#include <string_view>
#include <array>
#include <vector>
#include <memory>

#include "lexer.hpp"

namespace tungsten::parser
{
    enum class AstNodeType : uint8_t
    {
        None,

        Struct,
        StructMember,

        UniformGroup,
        UniformGroupMember,

        Macro,

        Function,
        FunctionArg
    };

    struct Attribute
    {
        std::string_view name;
        std::vector<std::string_view> arguments;
    };

    struct AstNode
    {
        inline AstNode() {
            std::memset(this, 0, sizeof(*this));
        }

        AstNodeType node_type = AstNodeType::None;
        std::vector<Attribute> attributes;

        union {
            std::string_view type;
            std::string_view macro_name;
        };
        union {
            std::string_view name;
            std::string_view macro_arg;
        };

        uint16_t child_offset = 0;
        uint16_t num_children = 0;
    };

    struct Ast
    {
        lexer::LexerInfo* lexer_info;

        std::vector<AstNode> root_nodes;
        std::vector<AstNode> child_nodes;
    };

    Ast* generate_ast(std::string_view code);
    void free_ast(Ast* ast);
    void print_ast(const Ast* ast);
}
