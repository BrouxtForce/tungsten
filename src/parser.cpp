#include "parser.hpp"
#include "error.hpp"
#include "lexer.hpp"

#include <array>
#include <iostream>
#include <optional>

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
                    if (token.keyword == lexer::Keyword::Struct || token.keyword == lexer::Keyword::UniformGroup)
                    {
                        consume_struct(ast);
                        accepts_attributes = true;
                    }
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
                case lexer::TokenType::None:
                    eof = true;
                    break;
                default: unexpected_token:
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
        std::string attributes_string;
        for (const Attribute& attribute : node->attributes)
        {
            attributes_string += "[attribute ";
            attributes_string += attribute.name;
            for (std::string_view arg : attribute.arguments)
            {
                attributes_string += ' ';
                attributes_string += arg;
            }
            attributes_string += "] ";
        }

        std::string indent_string(indent * 4, ' ');
        switch (node->node_type)
        {
            case AstNodeType::Struct:
                std::cout << indent_string << attributes_string << "struct " << node->name << '\n';
                break;
            case AstNodeType::StructMember:
                std::cout << indent_string << attributes_string << "struct_member " << node->type << ' ' << node->name << '\n';
                break;

            case AstNodeType::UniformGroup:
                std::cout << indent_string << attributes_string << "uniform_group " << node->name << '\n';
                break;
            case AstNodeType::UniformGroupMember:
                std::cout << indent_string << attributes_string << "uniform_group_member " << node->type << ' ' << node->name << '\n';
                break;

            case AstNodeType::Macro:
                std::cout << indent_string << "macro " << node->macro_name << ' ' << node->macro_arg << '\n';
                break;

            default:
                // TODO: Get byte offset
                error::report("Invalid AstNodeType", 0, 0);
        }

        for (uint16_t i = node->child_offset; i < node->child_offset + node->num_children; i++)
        {
            const AstNode* child_node = &ast->child_nodes[i];
            print_ast_node(ast, child_node, indent + 1);
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
