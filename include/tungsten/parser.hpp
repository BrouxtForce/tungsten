#pragma once

#include <string_view>
#include <deque>
#include <cstring>
#include <vector>
#include <cassert>

#include "tungsten/lexer.hpp"

namespace tungsten::parser
{
    enum class AstNodeType : uint8_t
    {
        None,

        Struct,
        StructMember,
        UniformGroup,
        UniformGroupMember,
        VertexGroup,
        VertexGroupMember,

        FunctionDeclaration,
        FunctionArgument,
        FunctionBody,

        VariableDeclaration,
        VariableAssignment,
        ArrayIndex,
        NumericLiteral,
        BooleanLiteral,
        UnaryOperation,
        BinaryOperation,
        Variable,
        PropertyAccess,
        FunctionCall,

        IfStatement,
        ElseIfStatement,
        ElseStatement,

        ForLoop,
        WhileLoop,

        ReturnStatement,
        KeywordStatement
    };

    template<typename T>
    struct IndexedSpan
    {
        std::vector<T>* vector;
        uint32_t index;
        uint32_t size;

        inline const T& operator[](uint32_t element_index) const
        {
            assert(vector != nullptr && index + element_index < vector->size());
            return (*vector)[index + element_index];
        }

        inline std::vector<T>::iterator begin() const
        {
            if (vector == nullptr)
            {
                return {};
            }
            return vector->begin() + index;
        }
        inline std::vector<T>::iterator end() const
        {
            if (vector == nullptr)
            {
                return {};
            }
            return vector->begin() + index + size;
        }
    };

    struct Attribute
    {
        std::string_view name;
    };

    struct AstNodeStruct
    {
        std::string_view name;
        IndexedSpan<uint32_t> member_nodes;
        uint32_t binding;
    };

    struct AstNodeStructMember
    {
        std::string_view type_name;
        std::string_view name;
    };

    struct AstNodeFunctionArgument
    {
        std::string_view type_name;
        std::string_view name;
    };

    struct AstNodeFunctionDeclaration
    {
        std::string_view return_type_name;
        std::string_view name;
        IndexedSpan<uint32_t> argument_nodes;
        uint32_t body;
    };

    struct AstNodeFunctionBody
    {
        IndexedSpan<uint32_t> statements;
    };

    struct AstNodeVariableDeclaration
    {
        std::string_view type_name;
        std::string_view name;
        bool is_const;
        bool is_array_declaration;
        bool is_top_level;
        std::string_view array_size_str;
        uint32_t expression;
        IndexedSpan<uint32_t> array_expressions;
    };

    struct AstNodeVariableAssignment
    {
        lexer::Operator operation;
        uint32_t variable_node;
        uint32_t expression;
    };

    struct AstNodeArrayIndex
    {
        uint32_t left;
        uint32_t right;
    };

    struct AstNodeNumericLiteral
    {
        std::string_view str;
    };

    struct AstNodeBooleanLiteral
    {
        bool value;
    };

    struct AstNodeUnaryOperation
    {
        lexer::Operator operation;
        uint32_t operand;
    };

    struct AstNodeBinaryOperation
    {
        lexer::Operator operation;
        uint32_t left;
        uint32_t right;
    };

    struct AstNodeVariable
    {
        std::string_view name;
    };

    struct AstNodePropertyAccess
    {
        uint32_t left;
        std::string_view name;
    };

    struct AstNodeFunctionCall
    {
        std::string_view name;
        IndexedSpan<uint32_t> argument_nodes;
    };

    struct AstNodeIfStatement
    {
        uint32_t expression;
        uint32_t body;
    };

    struct AstNodeForLoop
    {
        uint32_t init_expression;
        uint32_t comp_expression;
        uint32_t loop_expression;
        uint32_t body;
    };

    struct AstNodeWhileLoop
    {
        uint32_t expression;
        uint32_t body;
    };

    struct AstNodeReturnStatement
    {
        uint32_t expression;
    };

    struct AstNodeStatement
    {
        std::string_view str;
    };

    struct AstNode
    {
        constexpr AstNode() {
            std::memset(this, 0, sizeof(*this));
        }

        AstNodeType node_type;
        IndexedSpan<Attribute> attributes;
        uint8_t num_parenthesis;

        uint32_t byte_offset;
        uint32_t byte_length;

        union {
            AstNodeStruct struct_declaration;
            AstNodeStructMember struct_member;
            AstNodeFunctionDeclaration function_declaration;
            AstNodeFunctionArgument function_argument;
            AstNodeFunctionBody function_body;
            AstNodeVariableDeclaration variable_declaration;
            AstNodeVariableAssignment variable_assignment;
            AstNodeArrayIndex array_index;
            AstNodeNumericLiteral numeric_literal;
            AstNodeBooleanLiteral boolean_literal;
            AstNodeUnaryOperation unary_operation;
            AstNodeBinaryOperation binary_operation;
            AstNodeVariable variable;
            AstNodePropertyAccess property_access;
            AstNodeFunctionCall function_call;
            AstNodeIfStatement if_statement;
            AstNodeForLoop for_loop;
            AstNodeWhileLoop while_loop;
            AstNodeReturnStatement return_statement;
            AstNodeStatement statement;
        };
    };

    struct Ast
    {
        lexer::LexerInfo* lexer_info;

        // We use a deque here in order to keep references to AstNode's valid as more nodes
        // are appended to the list
        std::deque<AstNode> nodes;
        std::vector<uint32_t> root_nodes;

        std::vector<Attribute> node_attributes;
        std::vector<uint32_t> node_children;
    };

    Ast* generate_ast(std::string_view code);
    void free_ast(Ast* ast);
    void print_ast(const Ast* ast);

    bool has_attribute(const AstNode* node, std::string_view attribute_name);
}
