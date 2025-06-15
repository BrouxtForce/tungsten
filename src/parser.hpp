#pragma once

#include <string_view>
#include <array>
#include <deque>
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
        FunctionArg,

        Scope,

        VariableDeclaration,
        VariableAssignment,
        Expression,
        NumericLiteral,
        UnaryOperation,
        BinaryOperation,
        Variable,
        FunctionCall,

        IfStatement,
        ElseIfStatement,
        ElseStatement,

        ForLoop,
        WhileLoop,

        ReturnStatement
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
            std::string_view num_str;
            std::string_view operation;
        };
        union {
            std::string_view name;
            std::string_view macro_arg;
        };

        // TODO: These should probably be uint32_t's
        uint16_t child_offset = 0;
        uint16_t num_children = 0;
    };

    struct Ast
    {
        lexer::LexerInfo* lexer_info;

        // We use a deque here in order to keep references to AstNode's valid as more nodes
        // are appended to the list
        std::deque<AstNode> nodes;
    };

    Ast* generate_ast(std::string_view code);
    void free_ast(Ast* ast);
    void print_ast(const Ast* ast);
}
