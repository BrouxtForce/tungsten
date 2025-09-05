#pragma once

#include <string_view>

namespace tungsten::lexer
{
    enum class TokenType : uint8_t
    {
        None,

        String,
        Number,
        Name,
        Keyword,
        Punctuation,
        Operator
    };

    enum class Keyword : uint8_t
    {
        None,

        Struct,
        UniformGroup,
        VertexGroup,

        If,
        Else,
        For,
        While,
        Continue,
        Break,
        Return,
        Discard,

        True,
        False,

        Const
    };

    enum class Operator : uint8_t
    {
        Add, Sub, Mul, Div, Mod,

        AssignAdd, AssignSub, AssignMul, AssignDiv, AssignMod, Assign,

        Less, LessEqual, Equal, GreaterEqual, Greater,

        LogicalAnd, LogicalOr, LogicalNot,

        BitwiseAnd, BitwiseOr, BitwiseNot, BitwiseXor,
        BitwiseLeftShift, BitwiseRightShift,

        AssignBitwiseAnd, AssignBitwiseOr, AssignBitwiseXor,
        AssignBitwiseLeftShift, AssignBitwiseRightShift,

        PostfixIncrement, PostfixDecrement,

        EnumMemberCount
    };

    std::string_view operator_to_string(Operator op);

    struct Token
    {
        uint32_t byte_offset = 0;
        uint32_t byte_length = 0;

        union
        {
            std::string_view str;
            Keyword keyword;
            Operator op;
            char punc;
        };
        TokenType type = TokenType::None;

        std::string to_string();
    };

    struct LexerInfo;

    LexerInfo* init_lexer_info(std::string_view code);
    void free_lexer_info(LexerInfo* info);

    Token get_next_token(LexerInfo* info);
    Token peek_next_token(LexerInfo* info);
    Token peek_prev_token(LexerInfo* info);
    bool eof(LexerInfo* info);
}
