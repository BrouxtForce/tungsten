#pragma once

#include <tungsten/parser.hpp>

namespace tungsten::types
{
    enum ScalarType : uint8_t
    {
        ScalarType_None  = 0,
        ScalarType_Bool  = 1,
        ScalarType_Half  = 2,
        ScalarType_Float = 4,
        ScalarType_Uint  = 8,
        ScalarType_Int   = 16,
        ScalarType_MaxValue = ScalarType_Int
    };

    struct Type;

    struct BuiltinType
    {
        ScalarType scalar;
        uint8_t count_x;
        uint8_t count_y;
    };

    struct ScalarLiteral
    {
        ScalarType possible_scalar_types;
        ScalarType preferred_scalar_type;
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
        ScalarLiteral,
        Struct,
        UniformGroup,
        VertexGroup,
        Function,
        LibraryFunction
    };

    struct UserType
    {
        const Type* return_type;
        std::string_view name;
        parser::IndexedSpan<TypeNamePair> members;
    };

    struct TypeTemplate
    {
        uint8_t allowed_types;
        uint8_t template_index;
        uint8_t vector_components_mask;

        inline bool operator==(const TypeTemplate& other) const
        {
            return allowed_types == other.allowed_types &&
                template_index == other.template_index &&
                vector_components_mask == other.vector_components_mask;
        }
    };

    struct LibraryFunctionType
    {
        TypeTemplate return_type_template;
        std::string_view name;
        parser::IndexedSpan<TypeTemplate> parameters;
    };

    struct Type
    {
        TypeKind kind;
        bool is_const;
        bool is_array;

        union {
            BuiltinType builtin_type;
            ScalarLiteral scalar_literal;
            UserType user_type;
            LibraryFunctionType library_function_type;
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
