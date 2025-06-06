#include "parser.hpp"
#include "error.hpp"
#include "lexer.hpp"

#include <array>
#include <iostream>
#include <optional>
#include <cassert>

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

    void consume_punctuation(lexer::LexerInfo* info, char punc)
    {
        lexer::Token token = lexer::peek_next_token(info);
        if (token.type == lexer::TokenType::Punctuation && token.punc == punc)
        {
            lexer::get_next_token(info);
            return;
        }

        error::report("Unexpected token", token.byte_offset, token.byte_length);
    }

    bool try_consume_operator(lexer::LexerInfo* info, std::string_view op)
    {
        lexer::Token token = lexer::peek_next_token(info);
        if (token.type == lexer::TokenType::Operator && token.str == op)
        {
            lexer::get_next_token(info);
            return true;
        }
        return false;
    }

    void consume_operator(lexer::LexerInfo* info, std::string_view op)
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

    void consume_attribute(Ast* ast, std::vector<Attribute>& attributes)
    {
        consume_punctuation(ast->lexer_info, '[');
        consume_punctuation(ast->lexer_info, '[');

        std::string_view attribute_name = consume_name(ast->lexer_info);

        std::vector<std::string_view> arguments;
        while (true)
        {
            lexer::TokenType next_token_type = lexer::peek_next_token(ast->lexer_info).type;

            if (next_token_type == lexer::TokenType::Name || next_token_type == lexer::TokenType::Number)
            {
                arguments.push_back(lexer::get_next_token(ast->lexer_info).str);
                continue;
            }
            break;
        }

        consume_punctuation(ast->lexer_info, ']');
        consume_punctuation(ast->lexer_info, ']');

        if (error::had_error()) return;

        attributes.emplace_back(Attribute{ attribute_name, std::move(arguments) });
    }

    void try_consume_attributes(Ast* ast, std::vector<Attribute>& attributes)
    {
        while (true)
        {
            lexer::Token token = lexer::peek_next_token(ast->lexer_info);
            if (token.type != lexer::TokenType::Punctuation || token.punc != '[')
            {
                break;
            }

            consume_attribute(ast, attributes);
        }
    }

    void consume_struct(Ast* ast)
    {
        AstNode& struct_node = ast->root_nodes.emplace_back();
        struct_node.child_offset = ast->child_nodes.size();

        lexer::Keyword keyword = consume_keyword<2>(ast->lexer_info, { lexer::Keyword::Struct, lexer::Keyword::UniformGroup });
        struct_node.node_type = keyword == lexer::Keyword::Struct ? AstNodeType::Struct : AstNodeType::UniformGroup;
        struct_node.name = consume_name(ast->lexer_info);
        consume_punctuation(ast->lexer_info, '{');

        std::vector<Attribute> attributes;
        while (true)
        {
            if (lexer::peek_next_token(ast->lexer_info).type == lexer::TokenType::Punctuation)
            {
                char punctuation = lexer::peek_next_token(ast->lexer_info).punc;
                if (punctuation == '[')
                {
                    consume_attribute(ast, attributes);
                    continue;
                }
                break;
            }

            AstNode& member_node = ast->child_nodes.emplace_back();
            member_node.node_type = keyword == lexer::Keyword::Struct ? AstNodeType::StructMember : AstNodeType::UniformGroupMember;
            member_node.type = consume_name(ast->lexer_info);
            member_node.name = consume_name(ast->lexer_info);

            lexer::Token peeked_token = lexer::peek_next_token(ast->lexer_info);
            if (peeked_token.type == lexer::TokenType::Punctuation && peeked_token.punc == '[')
            {
                consume_attribute(ast, attributes);
            }
            std::swap(attributes, member_node.attributes);

            consume_punctuation(ast->lexer_info, ';');
        }
        consume_punctuation(ast->lexer_info, '}');
        consume_punctuation(ast->lexer_info, ';');

        struct_node.num_children = ast->child_nodes.size() - struct_node.child_offset;
    }

    void consume_macro(Ast* ast)
    {
        AstNode& macro_node = ast->root_nodes.emplace_back();
        macro_node.node_type = AstNodeType::Macro;

        consume_punctuation(ast->lexer_info, '#');
        macro_node.macro_name = consume_name(ast->lexer_info);

        std::optional<std::string_view> macro_arg = try_consume_string(ast->lexer_info);
        // TODO: Special handling to ensure string and non-string arguments are paired together with macros properly
        if (macro_arg.has_value())
        {
            macro_node.macro_arg = macro_arg.value();
        }
        else {
            macro_node.macro_arg = consume_name(ast->lexer_info);
        }
    }

    void consume_numeric_literal(Ast* ast)
    {
        AstNode& literal_node = ast->child_nodes.emplace_back();
        literal_node.node_type = AstNodeType::NumericLiteral;

        literal_node.num_str = consume_number(ast->lexer_info);
    }

    // Does not include assignment operations
    void consume_operation(Ast* ast, bool should_consume_binary_operation)
    {
        lexer::Token token = lexer::get_next_token(ast->lexer_info);
        if (token.type != lexer::TokenType::Operator)
        {
            error::report("Expected operator", token.byte_offset, token.byte_length);
            return;
        }

        constexpr std::array<std::string_view, 14> legal_binary_operations {
            "+", "-", "*", "/",
            "<", "<=", "==", ">=", ">",
            "&&", "||", "&", "|", "^"
        };
        if (should_consume_binary_operation)
        {
            for (std::string_view operation : legal_binary_operations)
            {
                if (token.str == operation)
                {
                    AstNode& operation_node = ast->child_nodes.emplace_back();
                    operation_node.node_type = AstNodeType::BinaryOperation;
                    operation_node.operation = token.str;
                    return;
                }
            }
        }

        constexpr std::array<std::string_view, 15> legal_unary_operations {
            "+", "-", "~", "++", "--", "!"
        };
        for (std::string_view operation : legal_unary_operations)
        {
            if (token.str == operation)
            {
                AstNode& operation_node = ast->child_nodes.emplace_back();
                operation_node.node_type = AstNodeType::UnaryOperation;
                operation_node.operation = token.str;
                return;
            }
        }

        error::report("Unexpected operator", token.byte_offset, token.byte_length);
    }

    void consume_variable_or_function_call(Ast* ast);

    void consume_expression(Ast* ast)
    {
        AstNode& expression_node = ast->child_nodes.emplace_back();
        expression_node.node_type = AstNodeType::Expression;
        expression_node.child_offset = ast->child_nodes.size();

        bool should_consume_binary_operation = false;
        while (true)
        {
            lexer::Token token = lexer::peek_next_token(ast->lexer_info);
            switch (token.type)
            {
                case lexer::TokenType::Number:
                    error::check(!should_consume_binary_operation, "Expected operator", token.byte_offset, token.byte_length);
                    consume_numeric_literal(ast);
                    should_consume_binary_operation = true;
                    continue;
                case lexer::TokenType::Name:
                    error::check(!should_consume_binary_operation, "Expected operator", token.byte_offset, token.byte_length);
                    consume_variable_or_function_call(ast);
                    should_consume_binary_operation = true;
                    continue;
                case lexer::TokenType::Operator:
                    consume_operation(ast, should_consume_binary_operation);
                    should_consume_binary_operation = false;
                    continue;
                case lexer::TokenType::Punctuation: {
                    if (token.punc == '(')
                    {
                        consume_punctuation(ast->lexer_info, '(');
                        consume_expression(ast);
                        consume_punctuation(ast->lexer_info, ')');
                        should_consume_binary_operation = true;
                        continue;
                    }
                    if (token.punc == ';' || token.punc == ')' || token.punc == ',')
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
            error::check(should_consume_binary_operation, "Expression cannot end on operator", token.byte_offset, token.byte_length);
            break;
        }

        expression_node.num_children = ast->child_nodes.size() - expression_node.child_offset;
    }

    void consume_variable_or_function_call(Ast* ast)
    {
        std::string_view name = consume_name(ast->lexer_info);

        lexer::Token peeked_token = lexer::peek_next_token(ast->lexer_info);
        if (peeked_token.type == lexer::TokenType::Punctuation && peeked_token.punc == '(')
        {
            // Consume function call
            AstNode& function_call_node = ast->child_nodes.emplace_back();
            function_call_node.node_type = AstNodeType::FunctionCall;
            function_call_node.child_offset = ast->child_nodes.size();
            function_call_node.name = name;

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
                consume_expression(ast);

                is_first_iteration = false;
            }
            consume_punctuation(ast->lexer_info, ')');

            function_call_node.num_children = ast->child_nodes.size() - function_call_node.child_offset;
            return;
        }

        // Consume variable
        AstNode& variable_node = ast->child_nodes.emplace_back();
        variable_node.node_type = AstNodeType::Variable;
        variable_node.name = name;
    }

    void consume_variable_declaration(Ast* ast, std::string_view type)
    {
        AstNode& decl_node = ast->child_nodes.emplace_back();
        decl_node.node_type = AstNodeType::VariableDeclaration;
        decl_node.child_offset = ast->child_nodes.size();

        decl_node.type = type;
        decl_node.name = consume_name(ast->lexer_info);

        if (!try_consume_operator(ast->lexer_info, "="))
        {
            // Uninitialized variable
            consume_punctuation(ast->lexer_info, ';');
            return;
        }
        consume_expression(ast);

        decl_node.num_children = ast->child_nodes.size() - decl_node.child_offset;
    }

    void consume_variable_assignment(Ast* ast, std::string_view name)
    {
        AstNode& assignment_node = ast->child_nodes.emplace_back();
        assignment_node.node_type = AstNodeType::VariableAssignment;
        assignment_node.name = name;
        assignment_node.child_offset = ast->child_nodes.size();

        lexer::Token token = lexer::get_next_token(ast->lexer_info);
        if (token.type != lexer::TokenType::Operator)
        {
            error::report("Expected operator", token.byte_offset, token.byte_length);
            return;
        }

        std::array<std::string_view, 9> valid_assignment_operators {
            "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "="
        };
        for (std::string_view op : valid_assignment_operators)
        {
            if (op == token.str)
            {
                assignment_node.type = token.str;
                consume_expression(ast);
                assignment_node.num_children = ast->child_nodes.size() - assignment_node.child_offset;
                return;
            }
        }

        if (token.str == "++" || token.str == "--")
        {
            assignment_node.type = token.str;
            assignment_node.num_children = ast->child_nodes.size() - assignment_node.child_offset;
            return;
        }

        error::report("Invalid operator", token.byte_offset, token.byte_length);
    }

    void consume_function_body(Ast* ast);

    void consume_if_statement(Ast* ast)
    {
        AstNode& if_node = ast->child_nodes.emplace_back();
        if_node.node_type = AstNodeType::IfStatement;
        if_node.child_offset = ast->child_nodes.size();

        consume_keyword(ast->lexer_info, lexer::Keyword::If);
        consume_punctuation(ast->lexer_info, '(');
        consume_expression(ast);
        consume_punctuation(ast->lexer_info, ')');

        consume_function_body(ast);

        if_node.num_children = ast->child_nodes.size() - if_node.child_offset;

        while (try_consume_keyword(ast->lexer_info, lexer::Keyword::Else))
        {
            bool is_else_if = try_consume_keyword(ast->lexer_info, lexer::Keyword::If);

            AstNode& else_node = ast->child_nodes.emplace_back();
            else_node.node_type = is_else_if ? AstNodeType::ElseIfStatement : AstNodeType::ElseStatement;
            else_node.child_offset = ast->child_nodes.size();

            if (is_else_if)
            {
                consume_punctuation(ast->lexer_info, '(');
                consume_expression(ast);
                consume_punctuation(ast->lexer_info, ')');
            }
            consume_function_body(ast);

            else_node.num_children = ast->child_nodes.size() - else_node.child_offset;

            if (!is_else_if)
            {
                break;
            }
        }
    }

    void consume_for_loop(Ast* ast)
    {
        AstNode& for_node = ast->child_nodes.emplace_back();
        for_node.node_type = AstNodeType::ForLoop;
        for_node.child_offset = ast->child_nodes.size();

        consume_keyword(ast->lexer_info, lexer::Keyword::For);
        consume_punctuation(ast->lexer_info, '(');
        consume_variable_declaration(ast, consume_name(ast->lexer_info));
        consume_punctuation(ast->lexer_info, ';');
        consume_expression(ast);
        consume_punctuation(ast->lexer_info, ';');
        consume_variable_assignment(ast, consume_name(ast->lexer_info));
        consume_punctuation(ast->lexer_info, ')');

        consume_function_body(ast);

        for_node.num_children = ast->child_nodes.size() - for_node.child_offset;
    }

    void consume_while_loop(Ast* ast)
    {
        AstNode& while_node = ast->child_nodes.emplace_back();
        while_node.node_type = AstNodeType::WhileLoop;
        while_node.child_offset = ast->child_nodes.size();

        consume_keyword(ast->lexer_info, lexer::Keyword::While);
        consume_punctuation(ast->lexer_info, '(');
        consume_expression(ast);
        consume_punctuation(ast->lexer_info, ')');

        consume_function_body(ast);

        while_node.num_children = ast->child_nodes.size() - while_node.child_offset;
    }

    void consume_function_body(Ast* ast)
    {
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
                        consume_variable_declaration(ast, word);
                        consume_punctuation(ast->lexer_info, ';');
                    }
                    else {
                        consume_variable_assignment(ast, word);
                        consume_punctuation(ast->lexer_info, ';');
                    }
                    continue;
                }
                case lexer::TokenType::Keyword:
                    if (token.keyword == lexer::Keyword::If)
                    {
                        consume_if_statement(ast);
                        continue;
                    }
                    if (token.keyword == lexer::Keyword::For)
                    {
                        consume_for_loop(ast);
                        continue;
                    }
                    if (token.keyword == lexer::Keyword::While)
                    {
                        consume_while_loop(ast);
                        continue;
                    }
                    if (token.keyword == lexer::Keyword::Return)
                    {
                        consume_keyword(ast->lexer_info, lexer::Keyword::Return);
                        AstNode& return_node = ast->child_nodes.emplace_back();
                        return_node.node_type = AstNodeType::ReturnStatement;
                        return_node.child_offset = ast->child_nodes.size();
                        consume_expression(ast);
                        consume_punctuation(ast->lexer_info, ';');
                        return_node.num_children = ast->child_nodes.size() - return_node.child_offset;
                        continue;
                    }
                    goto unexpected_token;
                case lexer::TokenType::Punctuation:
                    if (token.punc == '{')
                    {
                        AstNode& scope_node = ast->child_nodes.emplace_back();
                        scope_node.node_type = AstNodeType::Scope;
                        scope_node.child_offset = ast->child_nodes.size();

                        consume_function_body(ast);

                        scope_node.num_children = ast->child_nodes.size() - scope_node.child_offset;
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
    }

    void consume_function(Ast* ast)
    {
        AstNode& function_node = ast->root_nodes.emplace_back();
        function_node.node_type = AstNodeType::Function;
        function_node.child_offset = ast->child_nodes.size();

        function_node.type = consume_name(ast->lexer_info);
        function_node.name = consume_name(ast->lexer_info);

        consume_punctuation(ast->lexer_info, '(');
        while (true)
        {
            lexer::Token token = lexer::peek_next_token(ast->lexer_info);
            if (token.type == lexer::TokenType::Punctuation && token.punc == ')')
            {
                break;
            }

            AstNode& arg_node = ast->child_nodes.emplace_back();
            arg_node.node_type = AstNodeType::FunctionArg;

            try_consume_attributes(ast, arg_node.attributes);
            arg_node.type = consume_name(ast->lexer_info);
            arg_node.name = consume_name(ast->lexer_info);
            try_consume_attributes(ast, arg_node.attributes);

            token = lexer::peek_next_token(ast->lexer_info);
            if (token.type == lexer::TokenType::Punctuation && token.punc == ',')
            {
                lexer::get_next_token(ast->lexer_info);
                continue;
            }
            break;
        }
        consume_punctuation(ast->lexer_info, ')');

        consume_function_body(ast);

        function_node.num_children = ast->child_nodes.size() - function_node.child_offset;
    }

    Ast* generate_ast(std::string_view code)
    {
        Ast* ast = new Ast;
        ast->lexer_info = lexer::init_lexer_info(code);

        std::vector<Attribute> attributes;

        bool eof = false;
        while (!eof)
        {
            lexer::Token token = lexer::peek_next_token(ast->lexer_info);
            bool accepts_attributes = false;
            switch (token.type)
            {
                case lexer::TokenType::Keyword:
                    consume_struct(ast);
                    accepts_attributes = true;
                    break;
                case lexer::TokenType::Punctuation:
                    if (token.punc == '#')
                    {
                        consume_macro(ast);
                        break;
                    }
                    if (token.punc == '[')
                    {
                        consume_attribute(ast, attributes);
                        break;
                    }
                    goto unexpected_token;
                case lexer::TokenType::Name:
                    consume_function(ast);
                    accepts_attributes = true;
                    break;
                case lexer::TokenType::None:
                    eof = true;
                    break;
                default: unexpected_token:
                    lexer::get_next_token(ast->lexer_info);
                    error::report("Unexpected token", token.byte_offset, token.byte_length);
            }
            if (accepts_attributes)
            {
                std::swap(attributes, ast->root_nodes.back().attributes);
            }
        }

        return ast;
    }

    void free_ast(Ast* ast)
    {
        lexer::free_lexer_info(ast->lexer_info);
        delete ast;
    }

    void print_ast_node(const Ast* ast, const AstNode* node, int indent)
    {
        std::cout << std::string(indent * 4, ' ');
        for (const Attribute& attribute : node->attributes)
        {
            std::cout << "[attribute " << attribute.name;
            for (std::string_view arg : attribute.arguments)
            {
                std::cout << ' ' << arg;
            }
            std::cout << "] ";
        }

        switch (node->node_type)
        {
            case AstNodeType::Struct:
                std::cout << "struct " << node->name;
                break;
            case AstNodeType::StructMember:
                std::cout << "struct_member " << node->type << ' ' << node->name;
                break;

            case AstNodeType::UniformGroup:
                std::cout << "uniform_group " << node->name;
                break;
            case AstNodeType::UniformGroupMember:
                std::cout << "uniform_group_member " << node->type << ' ' << node->name;
                break;

            case AstNodeType::Macro:
                std::cout << "macro " << node->macro_name << ' ' << node->macro_arg;
                break;

            case AstNodeType::Function:
                std::cout << "function " << node->type << ' ' << node->name;
                break;
            case AstNodeType::FunctionArg:
                std::cout << "function_arg " << node->type << ' ' << node->name;
                break;

            case AstNodeType::Scope:
                std::cout << "scope";
                break;

            case AstNodeType::VariableDeclaration:
                std::cout << "variable_declaration " << node->type << ' ' << node->name;
                break;
            case AstNodeType::VariableAssignment:
                std::cout << "variable_assignment " << node->name << ' ' << node->type;
                break;
            case AstNodeType::Expression:
                std::cout << "expression";
                break;
            case AstNodeType::NumericLiteral:
                std::cout << "numeric_literal " << node->num_str;
                break;
            case AstNodeType::UnaryOperation:
                std::cout << "unary_operation " << node->operation;
                break;
            case AstNodeType::BinaryOperation:
                std::cout << "binary_operation " << node->operation;
                break;
            case AstNodeType::Variable:
                std::cout << "variable " << node->name;
                break;
            case AstNodeType::FunctionCall:
                std::cout << "function_call " << node->name;
                break;

            case AstNodeType::IfStatement:
                std::cout << "if_statment";
                break;
            case AstNodeType::ElseIfStatement:
                std::cout << "else_if_statement";
                break;
            case AstNodeType::ElseStatement:
                std::cout << "else_statement";
                break;

            case AstNodeType::ForLoop:
                std::cout << "for_loop";
                break;
            case AstNodeType::WhileLoop:
                std::cout << "while_loop";
                break;

            case AstNodeType::ReturnStatement:
                std::cout << "return_statement";
                break;

            default:
                // TODO: Get byte offset
                error::report("Invalid AstNodeType", 0, 0);
        }
        std::cout << '\n';

        // Note: This assumes that all children of a parent are found immediatley after that parent
        for (uint16_t i = node->child_offset; i < node->child_offset + node->num_children; i++)
        {
            const AstNode* child_node = &ast->child_nodes[i];
            print_ast_node(ast, child_node, indent + 1);
            i += child_node->num_children;
        }
    }

    void print_ast(const Ast* ast)
    {
        std::string indent;
        for (const AstNode& node : ast->root_nodes)
        {
            print_ast_node(ast, &node, 1);
        }
    }
}
