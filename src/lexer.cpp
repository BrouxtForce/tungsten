#include "tungsten/lexer.hpp"
#include "tungsten/error.hpp"

#include <string>
#include <cassert>
#include <array>

namespace tungsten::lexer
{
    std::string Token::to_string()
    {
        switch (type)
        {
            case TokenType::None:
                return "[None]";
            case TokenType::Number:
                return (std::string)"[Number " + std::string(str) + "]";
            case TokenType::String:
                return (std::string)"[String \"" + std::string(str) + "\"]";
            case TokenType::Name:
                return (std::string)"[Name \"" + std::string(str) + "\"]";
            case TokenType::Keyword: {
                std::string out =  "[Keyword ";
                switch (keyword)
                {
                    case Keyword::Struct:       out += "struct"; break;
                    case Keyword::UniformGroup: out += "uniform_group"; break;

                    case Keyword::If:       out += "if"; break;
                    case Keyword::Else:     out += "else"; break;
                    case Keyword::For:      out += "for"; break;
                    case Keyword::While:    out += "while"; break;
                    case Keyword::Continue: out += "continue"; break;
                    case Keyword::Break:    out += "break"; break;
                    case Keyword::Return:   out += "return"; break;
                    case Keyword::Discard:  out += "discard"; break;

                    default: out += "invalid keyword " + std::to_string((int)keyword);
                }
                return out + "]";
            }
            case TokenType::Punctuation:
                return (std::string)"[Punctuation '" + std::string(1, punc) + "']";
            case TokenType::Operator: {
                return "[Operator " + (std::string)str + "]";
            };
        }
        return "[Invalid Token]";
    }

    struct CharacterStream
    {
        std::string input;
        uint32_t byte = 0;

        CharacterStream(const std::string& code)
            : input(code) {}

        char peek()
        {
            assert(byte < input.size());
            return input[byte];
        }

        char peek(uint32_t offset)
        {
            assert(byte + offset < input.size());
            return input[byte + offset];
        }

        char read()
        {
            assert(byte < input.size());
            return input[byte++];
        }

        bool eof()
        {
            return byte >= input.size();
        }

        std::string_view string_view()
        {
            return std::string_view(input);
        }
    };

    struct LexerInfo
    {
        CharacterStream stream;

        Token peeked_token;
        bool has_peeked_token = false;
    };

    LexerInfo* init_lexer_info(std::string_view code)
    {
        return new LexerInfo {
            .stream = CharacterStream(std::string(code))
        };
    }

    void free_lexer_info(LexerInfo* info)
    {
        delete info;
    }

    bool is_name_or_keyword(char c)
    {
        return (c >= 'a' && c <= 'z') ||
               (c >= 'A' && c <= 'Z') ||
                c == '_';
    }

    bool is_string(char c)
    {
        return c == '"';
    }

    bool is_number(char c)
    {
        return c >= '0' && c <= '9';
    }

    bool is_maybe_comment(char c)
    {
        return c == '/';
    }

    bool is_punctuation(char c)
    {
        return c == ';' || c == ',' || c == '(' || c == ')' || c == '{' ||
               c == '}' || c == '#' || c == '[' || c == ']' || c == '.';
    }

    bool is_operator(char c)
    {
        return c == '+' || c == '-' || c == '*' || c == '/' || c == '<' || c == '>' || c == '%' ||
               c == '=' || c == '&' || c == '!' || c == '|' || c == '^' || c == '~';
    }

    bool is_whitespace(char c)
    {
        return std::isspace(c);
    }

    Token read_name_or_keyword(LexerInfo* info)
    {
        Token out{};

        out.byte_offset = info->stream.byte;
        do
        {
            info->stream.read();
        }
        while (!info->stream.eof() && (is_name_or_keyword(info->stream.peek()) || std::isdigit(info->stream.peek())));

        out.byte_length = info->stream.byte - out.byte_offset;
        std::string_view name = info->stream.string_view().substr(out.byte_offset, out.byte_length);

        Keyword keyword = Keyword::None;

        if (name == "struct")        keyword = Keyword::Struct;
        if (name == "uniform_group") keyword = Keyword::UniformGroup;
        if (name == "vertex_group")  keyword = Keyword::VertexGroup;

        if (name == "if")       keyword = Keyword::If;
        if (name == "else")     keyword = Keyword::Else;
        if (name == "for")      keyword = Keyword::For;
        if (name == "while")    keyword = Keyword::While;
        if (name == "continue") keyword = Keyword::Continue;
        if (name == "break")    keyword = Keyword::Break;
        if (name == "return")   keyword = Keyword::Return;
        if (name == "discard")  keyword = Keyword::Discard;

        if (name == "const") keyword = Keyword::Const;

        if (keyword == Keyword::None)
        {
            out.str = name;
            out.type = TokenType::Name;
            return out;
        }

        out.keyword = keyword;
        out.type = TokenType::Keyword;
        return out;
    }

    Token read_string(LexerInfo* info)
    {
        Token out{};
        out.byte_offset = info->stream.byte;

        info->stream.read();
        do
        {
            char c = info->stream.read();
            if (c == '"')
            {
                break;
            }
            if (c == '\n')
            {
                error::report("Unexpected newline in string", info->stream.byte, 1);
            }
        }
        while (!info->stream.eof());

        out.byte_length = info->stream.byte - out.byte_offset;
        out.str = info->stream.string_view().substr(out.byte_offset + 1, out.byte_length - 2);
        out.type = TokenType::String;

        return out;
    }

    Token read_number(LexerInfo* info)
    {
        // TODO: Support leading decimal point (e.g. .1) and float suffixes (i.e. f and h)

        Token out{};
        out.byte_offset = info->stream.byte;

        // The + or - sign will be eaten as a unary operator, so it only becomes relevant in the exponent part of a number
        bool processed_sign = true;
        bool processed_decimal_point = false;
        bool processed_exponent = false;

        do
        {
            char c = info->stream.peek();
            if (!processed_sign && (c == '+' || c == '-'))
            {
                info->stream.read();
                processed_sign = true;
                continue;
            }
            if (!processed_decimal_point && c == '.')
            {
                info->stream.read();
                processed_decimal_point = true;
                continue;
            }
            if (!processed_exponent && c == 'e')
            {
                info->stream.read();
                processed_sign = false;
                processed_decimal_point = true;
                processed_exponent = true;
                continue;
            }
            if (std::isdigit(c))
            {
                info->stream.read();
                processed_sign = true;
                continue;
            }
            if (processed_decimal_point && (c == 'f' || c == 'h'))
            {
                info->stream.read();
                break;
            }
            if (!processed_decimal_point && c == 'u')
            {
                info->stream.read();
                break;
            }

            // TODO: Check for valid characters immediately following the number
            break;
        }
        while (!info->stream.eof());

        out.byte_length = info->stream.byte - out.byte_offset;
        out.str = info->stream.string_view().substr(out.byte_offset, out.byte_length);
        out.type = TokenType::Number;

        return out;
    }

    bool try_read_comment(LexerInfo* info)
    {
        char comment_type = info->stream.peek(1);
        if (comment_type == '/')
        {
            info->stream.read();
            info->stream.read();
            while (true)
            {
                char c = info->stream.read();
                if (c == '\n')
                {
                    break;
                }
            }
            return true;
        }
        else if (comment_type == '*')
        {
            info->stream.read();
            info->stream.read();
            while (true)
            {
                char c = info->stream.read();
                if (c == '*')
                {
                    if (info->stream.peek() == '/')
                    {
                        info->stream.read();
                        break;
                    }
                }
            }
            return true;
        }
        return false;
    }

    Token read_punctuation(LexerInfo* info)
    {
        return Token {
            .byte_offset = info->stream.byte,
            .byte_length = 1,
            .type = TokenType::Punctuation,
            .punc = info->stream.read()
        };
    }

    Token read_operator(LexerInfo* info)
    {
        Token out{};

        out.byte_offset = info->stream.byte;
        do
        {
            info->stream.read();
        }
        while (!info->stream.eof() && is_operator(info->stream.peek()));

        out.byte_length = info->stream.byte - out.byte_offset;
        std::string_view view = info->stream.string_view().substr(out.byte_offset, out.byte_length);

        constexpr std::array<std::string_view, 32> valid_operators {
            "+", "-", "*", "/", "%", "+=", "-=", "*=", "/=", "%=", "=",
            "<", "<=", "==", ">=", ">",
            "&&", "||", "!",
            "&", "|", "~", "^", "<<", ">>", "&=", "|=", "^=", "<<=", ">>=",
            "++", "--"
        };

        bool is_valid_operator = false;
        for (std::string_view valid_operator : valid_operators)
        {
            if (view == valid_operator)
            {
                is_valid_operator = true;
                break;
            }
        }

        if (!is_valid_operator)
        {
            error::report("Invalid operator '" + (std::string)view + "'", out.byte_offset, out.byte_length);
        }

        out.str = view;
        out.type = TokenType::Operator;

        return out;
    }

    void read_whitespace(LexerInfo* info)
    {
        do
        {
            info->stream.read();
        }
        while (!info->stream.eof() && is_whitespace(info->stream.peek()));
    }

    Token get_next_token(LexerInfo* info)
    {
        if (info->has_peeked_token)
        {
            info->has_peeked_token = false;
            return info->peeked_token;
        }

        if (info->stream.eof())
        {
            return { .type = TokenType::None };
        }

        char c = info->stream.peek();
        if (is_whitespace(c))
        {
            read_whitespace(info);
            return get_next_token(info);
        }
        if (is_maybe_comment(c))
        {
            if (try_read_comment(info))
            {
                return get_next_token(info);
            }
        }
        if (is_string(c))
        {
            return read_string(info);
        }
        if (is_number(c))
        {
            return read_number(info);
        }
        if (is_name_or_keyword(c))
        {
            return read_name_or_keyword(info);
        }
        if (is_punctuation(c))
        {
            return read_punctuation(info);
        }
        if (is_operator(c))
        {
            return read_operator(info);
        }

        char invalid_character = info->stream.read();
        error::report("Invalid character: '" + std::string(1, invalid_character) + "'", info->stream.byte, 1);

        return Token { .type = TokenType::None };
    }

    Token peek_next_token(LexerInfo* info)
    {
        if (info->has_peeked_token)
        {
            return info->peeked_token;
        }

        info->peeked_token = get_next_token(info);
        info->has_peeked_token = true;

        return info->peeked_token;
    }

    bool eof(LexerInfo* info)
    {
        return info->stream.eof();
    }
}
