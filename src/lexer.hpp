#pragma once

#include <string_view>

namespace tungsten::lexer
{
    enum class TokenType
    {
        None,

        String,
        Number,
        Name,
        Comment,
        Whitespace,
        Keyword,
        Punctuation,
        Operator
    };

    enum class Keyword
    {
        None,

        Struct,
        UniformGroup,

        If,
        Else,
        For,
        While,
        Continue,
        Break,
        Return
    };

    struct Token
    {
        union
        {
            std::string_view str;
            Keyword keyword;
            char punc;
        };
        TokenType type;

        std::string to_string();
    };

    struct LexerInfo;

    LexerInfo* init_lexer_info(std::string_view code);
    void free_lexer_info(LexerInfo* info);

    Token get_next_token(LexerInfo* info);
    Token peek_next_token(LexerInfo* info);
    bool eof(LexerInfo* info);
}
