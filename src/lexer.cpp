#include "lexer.hpp"

#include <cctype>
#include <string>
#include <cassert>
#include <iostream>

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
            case TokenType::Comment:
                return (std::string)"[Comment \"" + std::string(str) + "\"";
            case TokenType::Whitespace:
                return "[Whitespace]";
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

                    default: out += "invalid keyword " + std::to_string((int)keyword);
                }
                return out + "]";
            }
            case TokenType::Punctuation:
                return (std::string)"[Punctuation '" + std::string(1, punc) + "']";
            case TokenType::Operator: {
                std::string out = "[Operator ";
                switch (op)
                {
                    case Operator::Assignment:         out += "assign"; break;
                    case Operator::AssignmentAdd:      out += "assign_add"; break;
                    case Operator::AssignmentSubtract: out += "assign_subtract"; break;
                    case Operator::AssignmentMultiply: out += "assign_multiply"; break;
                    case Operator::AssignmentDivide:   out += "assign_divide"; break;

                    case Operator::Add:      out += "add"; break;
                    case Operator::Subtract: out += "sub"; break;
                    case Operator::Multiply: out += "mul"; break;
                    case Operator::Divide:   out += "div"; break;

                    case Operator::LessThan:           out += "lt"; break;
                    case Operator::LessThanOrEqual:    out += "lte"; break;
                    case Operator::Equal:              out += "eq"; break;
                    case Operator::GreaterThan:        out += "gt"; break;
                    case Operator::GreaterThanOrEqual: out += "gte"; break;

                    case Operator::And: out += "and"; break;
                    case Operator::Or:  out += "or"; break;
                    case Operator::Not: out += "not"; break;

                    case Operator::BitAnd: out += "bitand"; break;
                    case Operator::BitOr:  out += "bitor"; break;
                    case Operator::BitNot: out += "bitnot"; break;
                    case Operator::BitXor: out += "bitxor"; break;

                    default: out += "invalid " + std::to_string((int)op);
                }
                return out + "]";
            };
        }
        return "[Invalid Token]";
    }

    struct CharacterStream
    {
        std::string input;
        uint32_t byte = 0;
        uint32_t line = 1;
        uint32_t column = 1;

        CharacterStream(const std::string& code)
            : input(code) {}

        char peek()
        {
            assert(byte < input.size());
            return input[byte];
        }

        char read()
        {
            assert(byte < input.size());
            char out = input[byte];

            byte++;
            if (out == '\n')
            {
                line++;
                column = 0;
            }
            column++;

            return out;
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
        return c == '+' || c == '-' || c == '*' || c == '/' || c == '<' ||
               c == '>' || c == '=' || c == '&' || c == '!' || c == '|';
    }

    bool is_whitespace(char c)
    {
        return std::isspace(c);
    }

    Token read_name_or_keyword(LexerInfo* info)
    {
        uint32_t start_index = info->stream.byte;
        do
        {
            info->stream.read();
        }
        while (!info->stream.eof() && (is_name_or_keyword(info->stream.peek()) || std::isdigit(info->stream.peek())));

        uint32_t end_index = info->stream.byte;
        std::string_view name = info->stream.string_view().substr(start_index, end_index - start_index);

        Keyword keyword = Keyword::None;

        if (name == "struct")        keyword = Keyword::Struct;
        if (name == "uniform_group") keyword = Keyword::UniformGroup;

        if (name == "if")       keyword = Keyword::If;
        if (name == "else")     keyword = Keyword::Else;
        if (name == "for")      keyword = Keyword::For;
        if (name == "while")    keyword = Keyword::While;
        if (name == "continue") keyword = Keyword::Continue;
        if (name == "break")    keyword = Keyword::Break;
        if (name == "return")   keyword = Keyword::Return;

        if (keyword == Keyword::None)
        {
            return Token {
                .str = name,
                .type = TokenType::Name
            };
        }
        return Token {
            .keyword = keyword,
            .type = TokenType::Keyword
        };
    }

    Token read_string(LexerInfo* info)
    {
        info->stream.read();
        uint32_t start_index = info->stream.byte;

        do
        {
            char c = info->stream.read();
            if (c == '"')
            {
                break;
            }
            if (c == '\n')
            {
                // TODO: Handle error
                std::cerr << "Invalid newline in string\n";
            }
        }
        while (!info->stream.eof());

        uint32_t end_index = info->stream.byte - 1;
        return Token {
            .str = info->stream.string_view().substr(start_index, end_index - start_index),
            .type = TokenType::String
        };
    }

    Token read_number(LexerInfo* info)
    {
        // TODO: Support leading decimal point (e.g. .1) and float suffixes (i.e. f and h)

        uint32_t start_index = info->stream.byte;

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
            }

            // TODO: Check for valid characters immediately following the number
            break;
        }
        while (!info->stream.eof());

        uint32_t end_index = info->stream.byte;
        return Token {
            .str = info->stream.string_view().substr(start_index, end_index - start_index),
            .type = TokenType::Number
        };
    }

    Token try_read_comment(LexerInfo* info)
    {
        // TODO
        return { .type = TokenType::None };
    }

    Token read_punctuation(LexerInfo* info)
    {
        return Token {
            .type = TokenType::Punctuation,
            .punc = info->stream.read()
        };
    }

    Token read_operator(LexerInfo* info)
    {
        uint32_t start_index = info->stream.byte;
        do
        {
            info->stream.read();
        }
        while (!info->stream.eof() && is_operator(info->stream.peek()));

        uint32_t end_index = info->stream.byte;
        std::string_view view = info->stream.string_view().substr(start_index, end_index - start_index);

        Operator op = Operator::None;

        if (view == "+") op = Operator::Add;
        if (view == "-") op = Operator::Subtract;
        if (view == "*") op = Operator::Multiply;
        if (view == "/") op = Operator::Divide;

        if (view == "=")  op = Operator::Assignment;
        if (view == "+=") op = Operator::AssignmentAdd;
        if (view == "-=") op = Operator::AssignmentSubtract;
        if (view == "*=") op = Operator::AssignmentMultiply;
        if (view == "/=") op = Operator::AssignmentDivide;

        if (view == "<")  op = Operator::LessThan;
        if (view == "<=") op = Operator::LessThanOrEqual;
        if (view == "==") op = Operator::Equal;
        if (view == ">")  op = Operator::GreaterThan;
        if (view == ">=") op = Operator::GreaterThanOrEqual;

        if (view == "&&") op = Operator::Add;
        if (view == "||") op = Operator::Or;
        if (view == "!")  op = Operator::Not;

        if (view == "&") op = Operator::BitAnd;
        if (view == "|") op = Operator::BitOr;
        if (view == "~") op = Operator::BitNot;
        if (view == "^") op = Operator::BitXor;

        if (op == Operator::None)
        {
            std::cerr << "Invalid operator '" << view << "'\n";
            // TODO: Graceful error handling
            assert(false);
        }

        return Token { .op = op, .type = TokenType::Operator };
    }

    Token read_whitespace(LexerInfo* info)
    {
        uint32_t start_index = info->stream.byte;
        do
        {
            info->stream.read();
        }
        while (!info->stream.eof() && is_whitespace(info->stream.peek()));

        uint32_t end_index = info->stream.byte;
        return Token {
            .str = info->stream.string_view().substr(start_index, end_index - start_index),
            .type = TokenType::Whitespace
        };
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
            return read_whitespace(info);
        }
        if (is_maybe_comment(c))
        {
            Token token = try_read_comment(info);
            if (token.type == TokenType::Comment)
            {
                return token;
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

        std::cerr << "Invalid character: '" << info->stream.read() << "'\n";

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
