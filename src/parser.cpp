#include "parser.hpp"
#include "error.hpp"
#include "lexer.hpp"

#include <array>
#include <iostream>

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

        // TODO: Get byte offset
        error::report("Unexpected token", 0);

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

        // TODO: Get byte offset
        error::report("Unexpected token", 0);
    }

    std::string_view consume_name(lexer::LexerInfo* info)
    {
        lexer::Token token = lexer::peek_next_token(info);
        if (token.type == lexer::TokenType::Name)
        {
            lexer::get_next_token(info);
            return token.str;
        }

        // TODO: Get byte offset
        error::report("Unexpected token", 0);
        return "__unknown";
    }

    void consume_struct(Ast* ast)
    {
        AstNode& struct_node = ast->root_nodes.emplace_back();
        struct_node.child_offset = ast->child_nodes.size();

        lexer::Keyword keyword = consume_keyword<2>(ast->lexer_info, { lexer::Keyword::Struct, lexer::Keyword::UniformGroup });
        struct_node.node_type = keyword == lexer::Keyword::Struct ? AstNodeType::Struct : AstNodeType::UniformGroup;
        struct_node.name = consume_name(ast->lexer_info);
        consume_punctuation(ast->lexer_info, '{');
        while (true)
        {
            if (lexer::peek_next_token(ast->lexer_info).type == lexer::TokenType::Punctuation)
            {
                break;
            }

            AstNode& member_node = ast->child_nodes.emplace_back();
            member_node.node_type = keyword == lexer::Keyword::Struct ? AstNodeType::StructMember : AstNodeType::UniformGroupMember;
            member_node.type = consume_name(ast->lexer_info);
            member_node.name = consume_name(ast->lexer_info);
            consume_punctuation(ast->lexer_info, ';');
        }
        consume_punctuation(ast->lexer_info, '}');
        consume_punctuation(ast->lexer_info, ';');

        struct_node.num_children = ast->child_nodes.size() - struct_node.child_offset;
    }

    Ast* generate_ast(std::string_view code)
    {
        Ast* ast = new Ast;
        ast->lexer_info = lexer::init_lexer_info(code);

        bool eof = false;
        while (!eof)
        {
            lexer::Token token = lexer::peek_next_token(ast->lexer_info);
            switch (token.type)
            {
                case lexer::TokenType::Keyword:
                    if (token.keyword == lexer::Keyword::Struct || token.keyword == lexer::Keyword::UniformGroup)
                    {
                        consume_struct(ast);
                    }
                    break;
                case lexer::TokenType::None:
                    eof = true;
                    break;
                default:
                    // TODO: Get location of token
                    error::report("Unexpected token", 0);
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
        std::string indent_string(indent * 4, ' ');
        switch (node->node_type)
        {
            case AstNodeType::Struct:
                std::cout << indent_string << "struct " << node->name << '\n';
                break;
            case AstNodeType::StructMember:
                std::cout << indent_string << "struct_member " << node->type << ' ' << node->name << '\n';
                break;

            case AstNodeType::UniformGroup:
                std::cout << indent_string << "uniform_group " << node->name << '\n';
                break;
            case AstNodeType::UniformGroupMember:
                std::cout << indent_string << "uniform_group_member " << node->type << ' ' << node->name << '\n';
                break;

            default:
                // TODO: Get byte offse
                error::report("Invalid AstNodeType", 0);
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
