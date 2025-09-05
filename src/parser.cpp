#include "tungsten/parser.hpp"
#include "tungsten/error.hpp"
#include "tungsten/lexer.hpp"

#include <array>
#include <iostream>
#include <optional>
#include <cassert>
#include <charconv>

namespace tungsten::parser
{
    template<size_t N>
    lexer::Keyword consume_keyword(lexer::LexerInfo* info, std::array<lexer::Keyword, N> keywords)
    {
        lexer::Token token = lexer::peek_next_token(info);
        if (token.type == lexer::TokenType::Keyword)
        {
            for (size_t i = 0; i < N; i++)
            {
                if (token.keyword == keywords[i])
                {
                    lexer::get_next_token(info);
                    return token.keyword;
                }
            }
        }

        error::report("Unexpected token", token.byte_offset, token.byte_length);

        return lexer::Keyword::None;
    }

    bool try_consume_keyword(lexer::LexerInfo* info, lexer::Keyword keyword)
    {
        lexer::Token token = lexer::peek_next_token(info);
        if (token.type == lexer::TokenType::Keyword && token.keyword == keyword)
        {
            lexer::get_next_token(info);
            return true;
        }
        return false;
    }

    void consume_keyword(lexer::LexerInfo* info, lexer::Keyword keyword)
    {
        if (!try_consume_keyword(info, keyword))
        {
            lexer::Token token = lexer::peek_next_token(info);
            error::report("Expected keyword", token.byte_offset, token.byte_length);
        }
    }

    bool try_consume_punctuation(lexer::LexerInfo* info, char punc)
    {
        lexer::Token token = lexer::peek_next_token(info);
        if (token.type == lexer::TokenType::Punctuation && token.punc == punc)
        {
            lexer::get_next_token(info);
            return true;
        }
        return false;
    }

    void consume_punctuation(lexer::LexerInfo* info, char punc)
    {
        if (!try_consume_punctuation(info, punc))
        {
            lexer::Token token = lexer::peek_next_token(info);
            error::report("Unexpected token", token.byte_offset, token.byte_length);
        }
    }

    bool try_consume_operator(lexer::LexerInfo* info, lexer::Operator op)
    {
        lexer::Token token = lexer::peek_next_token(info);
        if (token.type == lexer::TokenType::Operator && token.op == op)
        {
            lexer::get_next_token(info);
            return true;
        }
        return false;
    }

    void consume_operator(lexer::LexerInfo* info, lexer::Operator op)
    {
        if (try_consume_operator(info, op))
        {
            return;
        }
        lexer::Token token = lexer::peek_next_token(info);
        error::report("Unexpected token", token.byte_offset, token.byte_length);
    }

    std::string_view consume_number(lexer::LexerInfo* info)
    {
        lexer::Token token = lexer::peek_next_token(info);
        if (token.type == lexer::TokenType::Number)
        {
            return lexer::get_next_token(info).str;
        }

        error::report("Unexpected token", token.byte_offset, token.byte_length);
        return {};
    }

    std::string_view consume_name(lexer::LexerInfo* info)
    {
        lexer::Token token = lexer::peek_next_token(info);
        if (token.type == lexer::TokenType::Name)
        {
            lexer::get_next_token(info);
            return token.str;
        }

        error::report("Unexpected token", token.byte_offset, token.byte_length);
        return "__unknown";
    }

    std::optional<std::string_view> try_consume_string(lexer::LexerInfo* info)
    {
        lexer::Token token = lexer::peek_next_token(info);
        if (token.type == lexer::TokenType::String)
        {
            lexer::get_next_token(info);
            return token.str;
        }
        return std::nullopt;
    }

    void consume_attribute(lexer::LexerInfo* lexer_info, std::vector<Attribute>& attributes)
    {
        consume_punctuation(lexer_info, '[');
        consume_punctuation(lexer_info, '[');

        std::string_view attribute_name = consume_name(lexer_info);

        consume_punctuation(lexer_info, ']');
        consume_punctuation(lexer_info, ']');

        if (error::had_error()) return;

        attributes.emplace_back(Attribute{ attribute_name });
    }

    void try_consume_attributes(lexer::LexerInfo* lexer_info, std::vector<Attribute>& attributes)
    {
        while (true)
        {
            lexer::Token token = lexer::peek_next_token(lexer_info);
            if (token.type != lexer::TokenType::Punctuation || token.punc != '[')
            {
                break;
            }

            consume_attribute(lexer_info, attributes);
        }
    }

    static std::vector<uint32_t> source_location_stack;
    static std::vector<uint32_t> node_index_stack;

    void push_source_location(uint32_t byte_offset)
    {
        source_location_stack.push_back(byte_offset);
        node_index_stack.push_back(0);
    }

    void push_source_location(Ast* ast)
    {
        uint32_t source_location_begin = lexer::peek_next_token(ast->lexer_info).byte_offset;
        push_source_location(source_location_begin);
    }

    void pop_source_location(Ast* ast)
    {
        assert(source_location_stack.size() > 0);
        source_location_stack.pop_back();

        lexer::Token prev_token = lexer::peek_prev_token(ast->lexer_info);
        uint32_t byte_end_offset = prev_token.byte_offset + prev_token.byte_length;

        while (true)
        {
            uint32_t final_node_index = node_index_stack.back();
            node_index_stack.pop_back();

            if (final_node_index == 0)
            {
                break;
            }

            ast->nodes[final_node_index].byte_length = byte_end_offset - ast->nodes[final_node_index].byte_offset;
        }
    }

    AstNode& ast_push_node(Ast* ast)
    {
        AstNode& node = ast->nodes.emplace_back();
        node.byte_offset = source_location_stack.back();

        node_index_stack.push_back(ast->nodes.size() - 1);

        return node;
    }

    AstNode& ast_push_node(Ast* ast, uint32_t byte_offset, uint32_t byte_length)
    {
        AstNode& node = ast->nodes.emplace_back();
        node.byte_offset = byte_offset;
        node.byte_length = byte_length;

        return node;
    }

    [[nodiscard]]
    uint32_t parse_struct(Ast* ast)
    {
        push_source_location(ast);

        AstNode& struct_node = ast_push_node(ast);
        uint32_t struct_node_index = ast->nodes.size() - 1;

        lexer::Keyword keyword = consume_keyword<3>(ast->lexer_info, {
            lexer::Keyword::Struct, lexer::Keyword::UniformGroup, lexer::Keyword::VertexGroup }
        );
        switch (keyword)
        {
            case lexer::Keyword::Struct:       struct_node.node_type = AstNodeType::Struct; break;
            case lexer::Keyword::UniformGroup: struct_node.node_type = AstNodeType::UniformGroup; break;
            case lexer::Keyword::VertexGroup:  struct_node.node_type = AstNodeType::VertexGroup; break;
            default: assert(false);
        }
        struct_node.struct_declaration.name = consume_name(ast->lexer_info);
        consume_punctuation(ast->lexer_info, '{');

        uint32_t node_children_offset = ast->node_children.size();
        while (true)
        {
            lexer::Token next_token = lexer::peek_next_token(ast->lexer_info);
            if (next_token.type == lexer::TokenType::Punctuation && next_token.punc == '}')
            {
                break;
            }

            push_source_location(ast);

            uint32_t node_attributes_offset = ast->node_attributes.size();
            try_consume_attributes(ast->lexer_info, ast->node_attributes);

            AstNode& member_node = ast_push_node(ast);
            switch (keyword)
            {
                case lexer::Keyword::Struct:       member_node.node_type = AstNodeType::StructMember; break;
                case lexer::Keyword::UniformGroup: member_node.node_type = AstNodeType::UniformGroupMember; break;
                case lexer::Keyword::VertexGroup:  member_node.node_type = AstNodeType::VertexGroupMember; break;
                default: assert(false);
            }

            member_node.struct_member.type_name = consume_name(ast->lexer_info);
            member_node.struct_member.name = consume_name(ast->lexer_info);

            ast->node_children.push_back(ast->nodes.size() - 1);

            try_consume_attributes(ast->lexer_info, ast->node_attributes);

            member_node.attributes = {
                .vector = &ast->node_attributes,
                .index = node_attributes_offset,
                .size = static_cast<uint32_t>(ast->node_attributes.size() - node_attributes_offset)
            };

            consume_punctuation(ast->lexer_info, ';');

            pop_source_location(ast);
        }
        consume_punctuation(ast->lexer_info, '}');
        consume_punctuation(ast->lexer_info, ';');

        struct_node.struct_declaration.member_nodes = {
            .vector = &ast->node_children,
            .index = node_children_offset,
            .size = static_cast<uint32_t>(ast->node_children.size() - node_children_offset)
        };

        pop_source_location(ast);

        return struct_node_index;
    }

    [[nodiscard]]
    uint32_t parse_numeric_literal(Ast* ast)
    {
        push_source_location(ast);

        AstNode& literal_node = ast_push_node(ast);
        literal_node.node_type = AstNodeType::NumericLiteral;
        literal_node.numeric_literal.str = consume_number(ast->lexer_info);

        pop_source_location(ast);

        return ast->nodes.size() - 1;
    }

    [[nodiscard]]
    uint32_t parse_boolean_literal(Ast* ast)
    {
        push_source_location(ast);

        AstNode& literal_node = ast_push_node(ast);
        literal_node.node_type = AstNodeType::BooleanLiteral;

        lexer::Keyword keyword = consume_keyword<2>(ast->lexer_info, { lexer::Keyword::True, lexer::Keyword::False });
        literal_node.boolean_literal.value = (keyword == lexer::Keyword::True);

        pop_source_location(ast);

        return ast->nodes.size() - 1;
    }

    [[nodiscard]]
    uint32_t parse_binary_operation(Ast* ast, lexer::Token operation_token, uint32_t left_operand, uint32_t right_operand)
    {
        assert(operation_token.type == lexer::TokenType::Operator);
        lexer::Operator operation = operation_token.op;

        using enum lexer::Operator;
        constexpr std::array<lexer::Operator, 17> legal_binary_operations {
            Add, Sub, Mul, Div, Mod,
            Less, LessEqual, Equal, GreaterEqual, Greater,
            LogicalAnd, LogicalOr, BitwiseAnd, BitwiseOr, BitwiseXor,
            BitwiseLeftShift, BitwiseRightShift
        };

        bool is_legal_operation = false;
        for (lexer::Operator legal_operation : legal_binary_operations)
        {
            if (operation == legal_operation)
            {
                is_legal_operation = true;
            }
        }
        if (!is_legal_operation)
        {
            error::report(
                "Illegal operation '" + std::string(operator_to_string(operation)) + "'",
                operation_token.byte_offset, operation_token.byte_length
            );
        }

        uint32_t byte_offset = ast->nodes[left_operand].byte_offset;
        uint32_t byte_length = ast->nodes[right_operand].byte_offset + ast->nodes[right_operand].byte_length - byte_offset;

        AstNode& operation_node = ast_push_node(ast, byte_offset, byte_length);
        operation_node.node_type = AstNodeType::BinaryOperation;
        operation_node.binary_operation.operation = operation;
        operation_node.binary_operation.left = left_operand;
        operation_node.binary_operation.right = right_operand;

        return ast->nodes.size() - 1;
    }

    [[nodiscard]]
    uint32_t parse_unary_operation(Ast* ast, lexer::Token operation_token, uint32_t operand)
    {
        constexpr std::array<lexer::Operator, 4> legal_unary_operations {
            lexer::Operator::Add, lexer::Operator::Sub,
            lexer::Operator::BitwiseNot, lexer::Operator::LogicalNot
        };

        assert(operation_token.type == lexer::TokenType::Operator);
        lexer::Operator operation = operation_token.op;

        bool is_legal_operation = false;;
        for (lexer::Operator legal_operation : legal_unary_operations)
        {
            if (operation == legal_operation)
            {
                is_legal_operation = true;
            }
        }
        if (!is_legal_operation)
        {
            error::report(
                "Illegal operation '" + std::string(operator_to_string(operation)) + "'",
                operation_token.byte_offset, operation_token.byte_length
            );
        }

        uint32_t byte_offset = operation_token.byte_offset;
        uint32_t byte_length = ast->nodes[operand].byte_offset + ast->nodes[operand].byte_length - byte_offset;

        AstNode& operation_node = ast_push_node(ast, byte_offset, byte_length);
        operation_node.node_type = AstNodeType::UnaryOperation;
        operation_node.unary_operation.operation = operation;
        operation_node.unary_operation.operand = operand;

        return ast->nodes.size() - 1;
    }

    [[nodiscard]]
    uint32_t parse_variable_properties(Ast* ast, uint32_t parent_node)
    {
        push_source_location(ast->nodes[parent_node].byte_offset);
        while (true)
        {
            lexer::Token next_token = lexer::peek_next_token(ast->lexer_info);
            if (next_token.type != lexer::TokenType::Punctuation || next_token.punc != '.')
            {
                break;
            }
            consume_punctuation(ast->lexer_info, '.');

            AstNode& property_node = ast_push_node(ast);
            property_node.node_type = AstNodeType::PropertyAccess;
            property_node.property_access.left = parent_node;
            property_node.property_access.name = consume_name(ast->lexer_info);

            parent_node = ast->nodes.size() - 1;
        }
        pop_source_location(ast);
        return parent_node;
    }

    [[nodiscard]]
    uint32_t parse_variable_or_function_call(Ast* ast);

    enum class Precedence : uint32_t
    {
        None,
        LogicalOr,
        LogicalAnd,
        BitwiseOr,
        BitwiseXor,
        BitwiseAnd,
        Relational,
        BitwiseShift,
        Sum,
        Product,
        Unary
    };

    Precedence get_operator_precedence(lexer::Operator operation, bool is_unary_operator)
    {
        // TODO: Replace string comparisons with switch statement
        std::string_view op = operator_to_string(operation);

        if (is_unary_operator) return Precedence::Unary;

        if (op == "||") return Precedence::LogicalOr;
        if (op == "&&") return Precedence::LogicalAnd;
        if (op == "|") return Precedence::BitwiseOr;
        if (op == "^") return Precedence::BitwiseXor;
        if (op == "&") return Precedence::BitwiseAnd;
        if (op == "<" || op == "<=" || op == "==" || op == ">=" || op == ">") return Precedence::Relational;
        if (op == "<<" || op == ">>") return Precedence::BitwiseShift;
        if (op == "+" || op == "-") return Precedence::Sum;
        if (op == "*" || op == "/" || op == "%") return Precedence::Product;

        error::report("Invalid operation '" + std::string(op) + "'", 0, 0);

        return Precedence::None;
    }

    [[nodiscard]]
    uint32_t parse_expression(Ast* ast, Precedence precedence = Precedence::None)
    {
        uint32_t left_operand = 0;

        while (true)
        {
            bool should_consume_binary_operation = (left_operand != 0);

            lexer::Token token = lexer::peek_next_token(ast->lexer_info);
            switch (token.type)
            {
                case lexer::TokenType::Number:
                    error::check(!should_consume_binary_operation, "Expected operator", token.byte_offset, token.byte_length);
                    left_operand = parse_numeric_literal(ast);
                    continue;
                case lexer::TokenType::Name:
                    error::check(!should_consume_binary_operation, "Expected operator", token.byte_offset, token.byte_length);
                    left_operand = parse_variable_or_function_call(ast);
                    continue;
                case lexer::TokenType::Operator: {
                    Precedence operator_precedence = get_operator_precedence(token.op, !should_consume_binary_operation);
                    if (operator_precedence <= precedence)
                    {
                        return left_operand;
                    }
                    lexer::get_next_token(ast->lexer_info);

                    left_operand = should_consume_binary_operation ?
                        parse_binary_operation(ast, token, left_operand, parse_expression(ast, operator_precedence)) :
                        parse_unary_operation(ast, token, parse_expression(ast, operator_precedence));

                    continue;
                }
                case lexer::TokenType::Keyword: {
                    left_operand = parse_boolean_literal(ast);
                    continue;
                };
                case lexer::TokenType::Punctuation: {
                    if (token.punc == '(')
                    {
                        consume_punctuation(ast->lexer_info, '(');
                        left_operand = parse_expression(ast);
                        ast->nodes[left_operand].num_parenthesis++;
                        consume_punctuation(ast->lexer_info, ')');
                        left_operand = parse_variable_properties(ast, left_operand);
                        continue;
                    }
                    if (token.punc == '[')
                    {
                        push_source_location(ast->nodes[left_operand].byte_offset);

                        AstNode& array_index_node = ast_push_node(ast);
                        array_index_node.node_type = AstNodeType::ArrayIndex;
                        array_index_node.array_index.left = left_operand;
                        uint32_t array_index_node_index = ast->nodes.size() - 1;

                        consume_punctuation(ast->lexer_info, '[');
                        array_index_node.array_index.right = parse_expression(ast);
                        consume_punctuation(ast->lexer_info, ']');
                        left_operand = parse_variable_properties(ast, array_index_node_index);

                        pop_source_location(ast);

                        continue;
                    }
                    if (token.punc == ';' || token.punc == ')' || token.punc == ',' || token.punc == '}' || token.punc == ']')
                    {
                        break;
                    }
                    [[fallthrough]];
                }
                default:
                    error::report("Unexpected token", token.byte_offset, token.byte_length);
                    lexer::get_next_token(ast->lexer_info);
                    continue;
            }
            if (left_operand == 0)
            {
                error::report("Unexpected end of expression", token.byte_offset, token.byte_length);
            }
            break;
        }
        return left_operand;
    }

    [[nodiscard]]
    uint32_t parse_variable_or_function_call(Ast* ast)
    {
        uint32_t out_node_index = 0;

        push_source_location(ast);

        std::string_view name = consume_name(ast->lexer_info);

        lexer::Token peeked_token = lexer::peek_next_token(ast->lexer_info);
        if (peeked_token.type == lexer::TokenType::Punctuation && peeked_token.punc == '(')
        {
            // Consume function call
            AstNode& function_call_node = ast_push_node(ast);
            function_call_node.node_type = AstNodeType::FunctionCall;
            function_call_node.function_call.name = name;

            uint32_t function_call_node_index = ast->nodes.size() - 1;

            std::vector<uint32_t> argument_nodes;

            consume_punctuation(ast->lexer_info, '(');
            bool is_first_iteration = true;
            while (true)
            {
                lexer::Token token = lexer::peek_next_token(ast->lexer_info);
                if (token.type == lexer::TokenType::Punctuation)
                {
                    if (token.punc == ')')
                    {
                        break;
                    }
                    if (!is_first_iteration)
                    {
                        consume_punctuation(ast->lexer_info, ',');
                    }
                }
                argument_nodes.push_back(parse_expression(ast));

                is_first_iteration = false;
            }
            consume_punctuation(ast->lexer_info, ')');

            function_call_node.function_call.argument_nodes = {
                .vector = &ast->node_children,
                .index = static_cast<uint32_t>(ast->node_children.size()),
                .size = static_cast<uint32_t>(argument_nodes.size())
            };
            ast->node_children.append_range(argument_nodes);

            out_node_index = function_call_node_index;
        }
        else
        {
            AstNode& variable_node = ast_push_node(ast);
            variable_node.node_type = AstNodeType::Variable;
            variable_node.variable.name = name;

            out_node_index = parse_variable_properties(ast, ast->nodes.size() - 1);
        }

        pop_source_location(ast);

        out_node_index = parse_variable_properties(ast, out_node_index);

        return out_node_index;
    }

    [[nodiscard]]
    uint32_t parse_variable_declaration(Ast* ast, std::string_view type)
    {
        push_source_location(lexer::peek_prev_token(ast->lexer_info).byte_offset);

        AstNode& decl_node = ast_push_node(ast);
        decl_node.node_type = AstNodeType::VariableDeclaration;
        decl_node.variable_declaration.type_name = type;
        decl_node.variable_declaration.name = consume_name(ast->lexer_info);

        uint32_t decl_node_index = ast->nodes.size() - 1;

        lexer::Token next_token = lexer::peek_next_token(ast->lexer_info);
        if (next_token.type == lexer::TokenType::Punctuation && next_token.punc == '[')
        {
            lexer::get_next_token(ast->lexer_info);
            std::string_view array_size_str = consume_number(ast->lexer_info);

            decl_node.variable_declaration.is_array_declaration = true;
            decl_node.variable_declaration.array_size_str = array_size_str;

            int num = 0;
            std::from_chars_result result = std::from_chars(array_size_str.begin(), array_size_str.end(), num);
            if (result.ec != std::errc() || result.ptr != array_size_str.end())
            {
                error::report("Invalid number in array initializer", 0, 0);
            }

            consume_punctuation(ast->lexer_info, ']');
            consume_punctuation(ast->lexer_info, '{');

            std::vector<uint32_t> expressions;
            for (int i = 0; i < num; i++)
            {
                expressions.push_back(parse_expression(ast));
                if (!try_consume_punctuation(ast->lexer_info, ','))
                {
                    // TODO: Error if extraneous comma?
                    break;
                }
            }

            decl_node.variable_declaration.array_expressions = {
                .vector = &ast->node_children,
                .index = static_cast<uint32_t>(ast->node_children.size()),
                .size = static_cast<uint32_t>(expressions.size())
            };
            ast->node_children.append_range(expressions);

            consume_punctuation(ast->lexer_info, '}');
        }
        else if (try_consume_operator(ast->lexer_info, lexer::Operator::Assign))
        {
            decl_node.variable_declaration.expression = parse_expression(ast);
        }

        pop_source_location(ast);

        return decl_node_index;
    }

    [[nodiscard]]
    uint32_t parse_variable_declaration(Ast* ast)
    {
        bool is_variable_const = try_consume_keyword(ast->lexer_info, lexer::Keyword::Const);
        uint32_t decl_node_index = parse_variable_declaration(ast, consume_name(ast->lexer_info));

        AstNode& decl_node = ast->nodes[decl_node_index];
        assert(decl_node.node_type == AstNodeType::VariableDeclaration);
        decl_node.variable_declaration.is_const = is_variable_const;

        return decl_node_index;
    }

    [[nodiscard]]
    uint32_t parse_variable_assignment(Ast* ast, std::string_view name)
    {
        uint32_t prev_token_byte_offset = lexer::peek_prev_token(ast->lexer_info).byte_offset;

        push_source_location(prev_token_byte_offset);
        push_source_location(prev_token_byte_offset);

        AstNode& variable_node = ast_push_node(ast);
        variable_node.node_type = AstNodeType::Variable;
        variable_node.variable.name = name;

        uint32_t variable_node_index = ast->nodes.size() - 1;
        variable_node_index = parse_variable_properties(ast, variable_node_index);

        pop_source_location(ast);

        AstNode& assignment_node = ast_push_node(ast);
        assignment_node.node_type = AstNodeType::VariableAssignment;
        assignment_node.variable_assignment.variable_node = variable_node_index;
        uint32_t assignment_node_index = ast->nodes.size() - 1;

        lexer::Token token = lexer::get_next_token(ast->lexer_info);
        bool was_valid_operator = false;
        if (token.type != lexer::TokenType::Operator)
        {
            error::report("Expected operator", token.byte_offset, token.byte_length);
        }
        else
        {
            using enum lexer::Operator;
            std::array<lexer::Operator, 11> valid_assignment_operators {
                AssignAdd, AssignSub, AssignMul, AssignDiv, AssignMod,
                AssignBitwiseAnd, AssignBitwiseOr, AssignBitwiseXor,
                AssignBitwiseLeftShift, AssignBitwiseRightShift, Assign
            };
            for (lexer::Operator op : valid_assignment_operators)
            {
                if (op == token.op)
                {
                    assignment_node.variable_assignment.operation = token.op;
                    assignment_node.variable_assignment.expression = parse_expression(ast);
                    was_valid_operator = true;
                }
            }

            if (token.op == PostfixIncrement || token.op == PostfixDecrement)
            {
                assignment_node.variable_assignment.operation = token.op;
                was_valid_operator = true;
            }
        }

        if (!was_valid_operator)
        {
            error::report("Invalid operator", token.byte_offset, token.byte_length);
        }

        pop_source_location(ast);

        return assignment_node_index;
    }

    [[nodiscard]]
    uint32_t parse_function_body(Ast* ast);

    void parse_if_statement(Ast* ast, std::vector<uint32_t>& output_nodes)
    {
        push_source_location(ast);

        AstNode& if_node = ast_push_node(ast);
        if_node.node_type = AstNodeType::IfStatement;

        output_nodes.push_back(ast->nodes.size() - 1);

        consume_keyword(ast->lexer_info, lexer::Keyword::If);
        consume_punctuation(ast->lexer_info, '(');
        if_node.if_statement.expression = parse_expression(ast);
        consume_punctuation(ast->lexer_info, ')');

        if_node.if_statement.body = parse_function_body(ast);

        pop_source_location(ast);

        while (true)
        {
            push_source_location(ast);

            if (!try_consume_keyword(ast->lexer_info, lexer::Keyword::Else))
            {
                break;
            }

            bool is_else_if = try_consume_keyword(ast->lexer_info, lexer::Keyword::If);

            AstNode& else_node = ast_push_node(ast);
            else_node.node_type = is_else_if ? AstNodeType::ElseIfStatement : AstNodeType::ElseStatement;

            output_nodes.push_back(ast->nodes.size() - 1);

            if (is_else_if)
            {
                consume_punctuation(ast->lexer_info, '(');
                else_node.if_statement.expression = parse_expression(ast);
                consume_punctuation(ast->lexer_info, ')');
            }
            else_node.if_statement.body = parse_function_body(ast);

            pop_source_location(ast);

            if (!is_else_if)
            {
                break;
            }
        }
    }

    [[nodiscard]]
    uint32_t parse_for_loop(Ast* ast)
    {
        push_source_location(ast);

        AstNode& for_node = ast_push_node(ast);
        for_node.node_type = AstNodeType::ForLoop;

        uint32_t for_node_index = ast->nodes.size() - 1;

        consume_keyword(ast->lexer_info, lexer::Keyword::For);
        consume_punctuation(ast->lexer_info, '(');
        for_node.for_loop.init_expression = parse_variable_declaration(ast, consume_name(ast->lexer_info));
        consume_punctuation(ast->lexer_info, ';');
        for_node.for_loop.comp_expression = parse_expression(ast);
        consume_punctuation(ast->lexer_info, ';');
        for_node.for_loop.loop_expression = parse_variable_assignment(ast, consume_name(ast->lexer_info));
        consume_punctuation(ast->lexer_info, ')');

        for_node.for_loop.body = parse_function_body(ast);

        pop_source_location(ast);

        return for_node_index;
    }

    [[nodiscard]]
    uint32_t parse_while_loop(Ast* ast)
    {
        push_source_location(ast);

        AstNode& while_node = ast_push_node(ast);
        while_node.node_type = AstNodeType::WhileLoop;

        uint32_t while_node_index = ast->nodes.size() - 1;

        consume_keyword(ast->lexer_info, lexer::Keyword::While);
        consume_punctuation(ast->lexer_info, '(');
        while_node.while_loop.expression = parse_expression(ast);
        consume_punctuation(ast->lexer_info, ')');

        while_node.while_loop.body = parse_function_body(ast);

        pop_source_location(ast);

        return while_node_index;
    }

    [[nodiscard]]
    uint32_t parse_return_statement(Ast* ast)
    {
        push_source_location(ast);

        AstNode& return_node = ast_push_node(ast);
        return_node.node_type = AstNodeType::ReturnStatement;

        uint32_t return_node_index = ast->nodes.size() - 1;

        consume_keyword(ast->lexer_info, lexer::Keyword::Return);
        return_node.return_statement.expression = parse_expression(ast);
        consume_punctuation(ast->lexer_info, ';');

        pop_source_location(ast);

        return return_node_index;
    }

    [[nodiscard]]
    uint32_t parse_keyword_statement(Ast* ast)
    {
        push_source_location(ast);

        lexer::Keyword keyword = consume_keyword<3>(ast->lexer_info, { lexer::Keyword::Discard, lexer::Keyword::Continue, lexer::Keyword::Break });
        consume_punctuation(ast->lexer_info, ';');

        AstNode& statement_node = ast_push_node(ast);
        statement_node.node_type = AstNodeType::KeywordStatement;

        pop_source_location(ast);

        if (keyword == lexer::Keyword::Discard)
        {
            statement_node.statement.str = "discard";
        }
        if (keyword == lexer::Keyword::Continue)
        {
            statement_node.statement.str = "continue";
        }
        if (keyword == lexer::Keyword::Break)
        {
            statement_node.statement.str = "break";
        }

        return ast->nodes.size() - 1;
    }

    [[nodiscard]]
    uint32_t parse_function_body(Ast* ast)
    {
        std::vector<uint32_t> statements;

        push_source_location(ast);

        consume_punctuation(ast->lexer_info, '{');
        while (true)
        {
            lexer::Token token = lexer::peek_next_token(ast->lexer_info);
            switch (token.type)
            {
                case lexer::TokenType::Name: {
                    std::string_view word = consume_name(ast->lexer_info);
                    if (lexer::peek_next_token(ast->lexer_info).type == lexer::TokenType::Name)
                    {
                        statements.push_back(parse_variable_declaration(ast, word));
                        consume_punctuation(ast->lexer_info, ';');
                    }
                    else {
                        statements.push_back(parse_variable_assignment(ast, word));
                        consume_punctuation(ast->lexer_info, ';');
                    }
                    continue;
                }
                case lexer::TokenType::Keyword:
                    if (token.keyword == lexer::Keyword::If)
                    {
                        parse_if_statement(ast, statements);
                        continue;
                    }
                    if (token.keyword == lexer::Keyword::For)
                    {
                        statements.push_back(parse_for_loop(ast));
                        continue;
                    }
                    if (token.keyword == lexer::Keyword::While)
                    {
                        statements.push_back(parse_while_loop(ast));
                        continue;
                    }
                    if (token.keyword == lexer::Keyword::Return)
                    {
                        statements.push_back(parse_return_statement(ast));
                        continue;
                    }
                    if (token.keyword == lexer::Keyword::Discard || token.keyword == lexer::Keyword::Continue ||
                        token.keyword == lexer::Keyword::Break)
                    {
                        statements.push_back(parse_keyword_statement(ast));
                        continue;
                    }
                    if (token.keyword == lexer::Keyword::Const)
                    {
                        statements.push_back(parse_variable_declaration(ast));
                        consume_punctuation(ast->lexer_info, ';');
                        continue;
                    }
                    goto unexpected_token;
                case lexer::TokenType::Punctuation:
                    if (token.punc == '{')
                    {
                        statements.push_back(parse_function_body(ast));
                        continue;
                    }
                    if (token.punc == '}')
                    {
                        break;
                    }
                    [[fallthrough]];
                default:
                unexpected_token:
                    lexer::get_next_token(ast->lexer_info);
                    error::report("Unexpected token", token.byte_offset, token.byte_length);
                    continue;
            }
            break;
        }
        consume_punctuation(ast->lexer_info, '}');

        AstNode& function_body_node = ast_push_node(ast);
        function_body_node.node_type = AstNodeType::FunctionBody;
        function_body_node.function_body.statements = {
            .vector = &ast->node_children,
            .index = static_cast<uint32_t>(ast->node_children.size()),
            .size = static_cast<uint32_t>(statements.size())
        };

        pop_source_location(ast);

        ast->node_children.append_range(statements);

        return ast->nodes.size() - 1;
    }

    [[nodiscard]]
    uint32_t parse_function(Ast* ast)
    {
        push_source_location(ast);

        AstNode& function_node = ast_push_node(ast);
        function_node.node_type = AstNodeType::FunctionDeclaration;
        function_node.function_declaration.return_type_name = consume_name(ast->lexer_info);
        function_node.function_declaration.name = consume_name(ast->lexer_info);

        uint32_t function_node_index = ast->nodes.size() - 1;
        uint32_t node_children_offset = ast->node_children.size();

        consume_punctuation(ast->lexer_info, '(');
        while (true)
        {
            lexer::Token token = lexer::peek_next_token(ast->lexer_info);
            if (token.type == lexer::TokenType::Punctuation && token.punc == ')')
            {
                break;
            }

            push_source_location(ast);

            uint32_t node_attributes_offset = ast->node_attributes.size();
            try_consume_attributes(ast->lexer_info, ast->node_attributes);

            AstNode& function_arg_node = ast_push_node(ast);
            function_arg_node.node_type = AstNodeType::FunctionArgument;
            function_arg_node.function_argument.type_name = consume_name(ast->lexer_info);
            function_arg_node.function_argument.name = consume_name(ast->lexer_info);

            try_consume_attributes(ast->lexer_info, ast->node_attributes);

            pop_source_location(ast);

            function_arg_node.attributes = {
                .vector = &ast->node_attributes,
                .index = node_attributes_offset,
                .size = static_cast<uint32_t>(ast->node_attributes.size() - node_attributes_offset)
            };

            ast->node_children.push_back(ast->nodes.size() - 1);

            token = lexer::peek_next_token(ast->lexer_info);
            if (token.type == lexer::TokenType::Punctuation && token.punc == ',')
            {
                lexer::get_next_token(ast->lexer_info);
                continue;
            }
            break;
        }
        consume_punctuation(ast->lexer_info, ')');

        function_node.function_declaration.argument_nodes = {
            .vector = &ast->node_children,
            .index = node_children_offset,
            .size = static_cast<uint32_t>(ast->node_children.size() - node_children_offset)
        };
        function_node.function_declaration.body = parse_function_body(ast);

        pop_source_location(ast);

        return function_node_index;
    }

    Ast* generate_ast(std::string_view code)
    {
        Ast* ast = new Ast;
        ast->lexer_info = lexer::init_lexer_info(code);

        // Ensures that index zero is invalid so that it can be treated like a null pointer
        ast->nodes.emplace_back().node_type = AstNodeType::None;

        std::vector<Attribute> attributes;

        bool eof = false;
        while (!eof)
        {
            lexer::Token token = lexer::peek_next_token(ast->lexer_info);
            bool accepts_attributes = false;
            uint32_t next_node_index = ast->nodes.size();
            switch (token.type)
            {
                case lexer::TokenType::Keyword:
                    if (token.keyword == lexer::Keyword::Const)
                    {
                        uint32_t variable_declaration_node_index = parse_variable_declaration(ast);
                        consume_punctuation(ast->lexer_info, ';');

                        ast->nodes[variable_declaration_node_index].variable_declaration.is_top_level = true;
                        ast->root_nodes.push_back(variable_declaration_node_index);

                        accepts_attributes = true;
                        break;
                    }
                    ast->root_nodes.push_back(parse_struct(ast));
                    accepts_attributes = true;
                    break;
                case lexer::TokenType::Punctuation:
                    if (token.punc == '[')
                    {
                        consume_attribute(ast->lexer_info, attributes);
                        break;
                    }
                    goto unexpected_token;
                case lexer::TokenType::Name:
                    ast->root_nodes.push_back(parse_function(ast));
                    accepts_attributes = true;
                    break;
                case lexer::TokenType::None:
                    eof = true;
                    break;
                default: unexpected_token:
                    lexer::get_next_token(ast->lexer_info);
                    error::report("Unexpected token", token.byte_offset, token.byte_length);
            }
            if (accepts_attributes && attributes.size() > 0)
            {
                assert(next_node_index < ast->nodes.size());
                AstNode& next_node = ast->nodes[next_node_index];

                if (next_node.attributes.size == 0)
                {
                    next_node.attributes = {
                        .vector = &ast->node_attributes,
                        .index = static_cast<uint32_t>(ast->node_attributes.size()),
                        .size = static_cast<uint32_t>(attributes.size())
                    };
                    ast->node_attributes.append_range(attributes);
                }
                else
                {
                    error::report("Could not append additional attributes", 0, 0);
                }

                attributes.clear();
            }
        }

        return ast;
    }

    void free_ast(Ast* ast)
    {
        lexer::free_lexer_info(ast->lexer_info);
        delete ast;
    }

    void print_ast_node(const Ast* ast, uint32_t node_index, int indent)
    {
        const AstNode& node = ast->nodes[node_index];

        std::cout << '\n' << std::string(indent * 4, ' ');
        for (const Attribute& attribute : node.attributes)
        {
            std::cout << "[attribute " << attribute.name << "] ";
        }

        const auto& print_ast_nodes = [&ast, &indent](IndexedSpan<uint32_t> node_indices)
        {
            for (uint32_t node_index : node_indices)
            {
                print_ast_node(ast, node_index, indent + 1);
            }
        };

        switch (node.node_type)
        {
            case AstNodeType::None:
                std::cout << "NULL";
                break;

            case AstNodeType::Struct:
            case AstNodeType::UniformGroup:
            case AstNodeType::VertexGroup:
                if (node.node_type == AstNodeType::Struct)       std::cout << "struct ";
                if (node.node_type == AstNodeType::UniformGroup) std::cout << "uniform_group ";
                if (node.node_type == AstNodeType::VertexGroup)  std::cout << "vertex_group ";
                std::cout << node.struct_declaration.name;
                print_ast_nodes(node.struct_declaration.member_nodes);
                break;
            case AstNodeType::StructMember:
            case AstNodeType::UniformGroupMember:
            case AstNodeType::VertexGroupMember:
                if (node.node_type == AstNodeType::StructMember)       std::cout << "struct_member ";
                if (node.node_type == AstNodeType::UniformGroupMember) std::cout << "uniform_group_member ";
                if (node.node_type == AstNodeType::VertexGroupMember)  std::cout << "vertex_group_member ";
                std::cout << node.struct_member.type_name << ' ' << node.struct_member.name;
                break;

            case AstNodeType::FunctionDeclaration:
                std::cout << "function_declaration " << node.function_declaration.return_type_name << ' ' << node.function_declaration.name;
                print_ast_nodes(node.function_declaration.argument_nodes);
                print_ast_node(ast, node.function_declaration.body, indent + 1);
                break;
            case AstNodeType::FunctionArgument:
                std::cout << "function_argument " << node.function_argument.type_name << ' ' << node.function_argument.name;
                break;
            case AstNodeType::FunctionBody:
                std::cout << "function_body";
                print_ast_nodes(node.function_body.statements);
                break;

            case AstNodeType::VariableDeclaration:
                std::cout << "variable_declaration ";
                if (node.variable_declaration.is_top_level) std::cout << "top_level ";
                if (node.variable_declaration.is_const)     std::cout << "const ";
                if (node.variable_declaration.is_array_declaration)
                {
                    std::cout << node.variable_declaration.type_name;
                    std::cout << "[" << node.variable_declaration.array_size_str << "] ";
                    std::cout << node.variable_declaration.name;
                    print_ast_nodes(node.variable_declaration.array_expressions);
                }
                else
                {
                    std::cout << node.variable_declaration.type_name << ' ' << node.variable_declaration.name;
                    print_ast_node(ast, node.variable_declaration.expression, indent + 1);
                }
                break;
            case AstNodeType::VariableAssignment:
                std::cout << "variable_assignment " << operator_to_string(node.variable_assignment.operation);
                print_ast_node(ast, node.variable_assignment.variable_node, indent + 1);
                print_ast_node(ast, node.variable_assignment.expression, indent + 1);
                break;
            case AstNodeType::ArrayIndex:
                std::cout << "array_index";
                print_ast_node(ast, node.array_index.left, indent + 1);
                print_ast_node(ast, node.array_index.right, indent + 1);
                break;
            case AstNodeType::NumericLiteral:
                std::cout << "numeric_literal " << node.numeric_literal.str;
                break;
            case AstNodeType::UnaryOperation:
                std::cout << "unary_operation " << operator_to_string(node.unary_operation.operation);
                print_ast_node(ast, node.unary_operation.operand, indent + 1);
                break;
            case AstNodeType::BinaryOperation:
                std::cout << "binary_operation " << operator_to_string(node.binary_operation.operation);
                print_ast_node(ast, node.binary_operation.left, indent + 1);
                print_ast_node(ast, node.binary_operation.right, indent + 1);
                break;
            case AstNodeType::Variable:
                std::cout << "variable " << node.variable.name;
                break;
            case AstNodeType::PropertyAccess:
                std::cout << "property_access " << node.property_access.name;
                print_ast_node(ast, node.property_access.left, indent + 1);
                break;
            case AstNodeType::FunctionCall:
                std::cout << "function_call " << node.function_call.name;
                print_ast_nodes(node.function_call.argument_nodes);
                break;

            case AstNodeType::IfStatement:
                std::cout << "if_statement";
                print_ast_node(ast, node.if_statement.expression, indent + 1);
                print_ast_node(ast, node.if_statement.body, indent + 1);
                break;
            case AstNodeType::ElseIfStatement:
                std::cout << "else_if_statement";
                print_ast_node(ast, node.if_statement.expression, indent + 1);
                print_ast_node(ast, node.if_statement.body, indent + 1);
                break;
            case AstNodeType::ElseStatement:
                std::cout << "else_statement";
                print_ast_node(ast, node.if_statement.body, indent + 1);
                break;

            case AstNodeType::ForLoop:
                std::cout << "for_loop";
                print_ast_node(ast, node.for_loop.init_expression, indent + 1);
                print_ast_node(ast, node.for_loop.comp_expression, indent + 1);
                print_ast_node(ast, node.for_loop.loop_expression, indent + 1);
                print_ast_node(ast, node.for_loop.body, indent + 1);
                break;
            case AstNodeType::WhileLoop:
                std::cout << "while_loop";
                print_ast_node(ast, node.while_loop.expression, indent + 1);
                print_ast_node(ast, node.while_loop.body, indent + 1);
                break;

            case AstNodeType::ReturnStatement:
                std::cout << "return_statement";
                print_ast_node(ast, node.return_statement.expression, indent + 1);
                break;
            case AstNodeType::KeywordStatement:
                std::cout << node.statement.str << "_statement";
                break;

            default:
                // TODO: Get byte offset
                error::report("Invalid AstNodeType", 0, 0);
        }
    }

    void print_ast(const Ast* ast)
    {
        for (uint32_t root_node_index : ast->root_nodes)
        {
            print_ast_node(ast, root_node_index, 0);
        }
        std::cout << "\n\n";
    }

    bool has_attribute(const AstNode* node, std::string_view attribute_name)
    {
        for (const Attribute& attribute : node->attributes)
        {
            if (attribute.name == attribute_name)
            {
                return true;
            }
        }
        return false;
    }
}
