#pragma once

#include <tungsten/parser.hpp>

namespace tungsten::types
{
    enum ScalarType : uint8_t
    {
        None, Bool, Half, Float, Uint, Int, MaxValue = Int
    };

    struct Type;

    struct BuiltinType
    {
        ScalarType scalar;
        uint8_t count_x;
        uint8_t count_y;
    };

    struct TypeNamePair
    {
        const Type* type;
        std::string_view name;
    };

    enum class TypeKind : uint8_t
    {
        None,
        Builtin,
        Struct,
        UniformGroup,
        VertexGroup,
        Function,
    };

    struct UserType
    {
        const Type* return_type;
        std::string_view name;
        parser::IndexedSpan<TypeNamePair> members;
    };

    struct Type
    {
        TypeKind kind;
        union {
            BuiltinType builtin_type;
            UserType user_type;
        };

        bool is_valid_builtin() const;
        bool is_scalar() const;
        bool is_vector() const;
        bool is_matrix() const;

        std::string name() const;
    };

    const Type* get_type(std::string_view type_name);

    void type_check(const parser::Ast* ast);
}
