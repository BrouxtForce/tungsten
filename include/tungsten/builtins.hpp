#pragma once

#include <array>
#include <string_view>

namespace tungsten::builtins
{
    enum PrimitiveType : uint8_t
    {
        PrimitiveType_Bool  = 1 << 0,
        PrimitiveType_Half  = 1 << 1,
        PrimitiveType_Float = 1 << 2,
        PrimitiveType_Uint  = 1 << 3,
        PrimitiveType_Int   = 1 << 4
    };

    static constexpr uint16_t create_type(PrimitiveType type, uint8_t num_components)
    {
        return ((uint16_t)type << 8) | (uint16_t)num_components;
    }

    constexpr static size_t TYPE_IS_MASK_BIT = 0b1000'0000;
    static constexpr uint16_t create_type_mask(uint8_t type_mask, uint8_t num_components_mask)
    {
        return create_type((PrimitiveType)type_mask, num_components_mask | TYPE_IS_MASK_BIT);
    }

    struct Type
    {
        PrimitiveType primitive_type = PrimitiveType_Float;
        uint8_t num_components = 1;

        constexpr Type() {}
        constexpr Type(uint16_t value)
        {
            primitive_type = (PrimitiveType)(value >> 8);
            num_components = value & 0xff;
        }

        constexpr bool is_mask() const
        {
            return num_components & TYPE_IS_MASK_BIT;
        }

        static constexpr uint16_t Float3 = create_type(PrimitiveType_Float, 3);
        static constexpr uint16_t Uint   = create_type(PrimitiveType_Uint, 1);
        static constexpr uint16_t Bool   = create_type(PrimitiveType_Bool, 1);

        static constexpr uint16_t Numerical       = create_type_mask(PrimitiveType_Half | PrimitiveType_Float | PrimitiveType_Uint | PrimitiveType_Int, 0b1111);
        static constexpr uint16_t ScalarNumerical = create_type_mask(PrimitiveType_Half | PrimitiveType_Float | PrimitiveType_Uint | PrimitiveType_Int, 0b0001);

        static constexpr uint16_t Floating       = create_type_mask(PrimitiveType_Half | PrimitiveType_Float, 0b1111);
        static constexpr uint16_t ScalarFloating = create_type_mask(PrimitiveType_Half | PrimitiveType_Float, 0b0001);
        static constexpr uint16_t VectorFloating = create_type_mask(PrimitiveType_Half | PrimitiveType_Float, 0b1110);

        static constexpr uint16_t Integer = create_type_mask(PrimitiveType_Int | PrimitiveType_Uint, 0b1111);
        static constexpr uint16_t Boolean = create_type_mask(PrimitiveType_Bool, 0b0001);
    };

    struct Function
    {
        std::string_view name;
        Type return_type;
        std::array<Type, 4> parameter_types;
    };

    // TODO: frexp, modf, texture functions, atomic functions, synchronization functions, simdgroup/quadgroup functions, matrix functions
    static constexpr std::array<Function, 53> functions
    {
        // Floating-point/integer functions
        Function{ "abs",   Type::Numerical, { Type::Numerical } },
        Function{ "clamp", Type::Numerical, { Type::Numerical, Type::Numerical, Type::Numerical } },
        Function{ "max", Type::Numerical, { Type::Numerical, Type::Numerical } },
        Function{ "min", Type::Numerical, { Type::Numerical, Type::Numerical } },

        // Trigonometric functions
        Function{ "sin",  Type::Floating, { Type::Floating } },
        Function{ "cos",  Type::Floating, { Type::Floating } },
        Function{ "tan",  Type::Floating, { Type::Floating } },
        Function{ "asin", Type::Floating, { Type::Floating } },
        Function{ "acos", Type::Floating, { Type::Floating } },
        Function{ "atan", Type::Floating, { Type::Floating } },

        // Hyperbolic functions
        Function{ "sinh",  Type::Floating, { Type::Floating } },
        Function{ "cosh",  Type::Floating, { Type::Floating } },
        Function{ "tanh",  Type::Floating, { Type::Floating } },
        Function{ "acosh", Type::Floating, { Type::Floating } },
        Function{ "asinh", Type::Floating, { Type::Floating } },
        Function{ "atanh", Type::Floating, { Type::Floating } },

        // Other floating-point functions
        Function{ "log",  Type::Floating, { Type::Floating } },
        Function{ "log2", Type::Floating, { Type::Floating } },
        Function{ "exp",  Type::Floating, { Type::Floating } },
        Function{ "exp2", Type::Floating, { Type::Floating } },

        Function{ "sign",  Type::Floating, { Type::Floating } },
        Function{ "fract", Type::Floating, { Type::Floating } },
        Function{ "trunc", Type::Floating, { Type::Floating } },
        Function{ "ceil",  Type::Floating, { Type::Floating } },
        Function{ "floor", Type::Floating, { Type::Floating } },
        Function{ "round", Type::Floating, { Type::Floating } },

        Function{ "sqrt",  Type::Floating, { Type::Floating } },
        Function{ "rsqrt", Type::Floating, { Type::Floating } },

        Function{ "saturate",   Type::Floating, { Type::Floating } },
        Function{ "mix",        Type::Floating, { Type::Floating, Type::Floating, Type::Floating } },
        Function{ "fma",        Type::Floating, { Type::Floating, Type::Floating } },
        Function{ "smoothstep", Type::Floating, { Type::Floating, Type::Floating, Type::Floating } },
        Function{ "step",       Type::Floating, { Type::Floating, Type::Floating } },
        Function{ "ldexp",      Type::Floating, { Type::Floating, Type::Integer } },

        // Derivative functions
        Function{ "dfdx",   Type::Floating, { Type::Floating } },
        Function{ "dfdy",   Type::Floating, { Type::Floating } },
        Function{ "fwidth", Type::Floating, { Type::Floating } },

        // Vector functions
        Function{ "cross",     Type::Float3, { Type::Float3, Type::Float3 } },
        Function{ "distance",  Type::ScalarFloating, { Type::Floating, Type::Floating } },
        Function{ "dot",       Type::ScalarNumerical, { Type::Numerical, Type::Numerical } },
        Function{ "length",    Type::ScalarFloating, { Type::Floating, Type::Floating } },
        Function{ "normalize", Type::VectorFloating, { Type::VectorFloating } },
        Function{ "reflect",   Type::VectorFloating, { Type::VectorFloating, Type::VectorFloating } },
        Function{ "refract",   Type::VectorFloating, { Type::VectorFloating, Type::VectorFloating } },

        // Integer functions
        Function{ "clz",      Type::Integer, { Type::Integer } },
        Function{ "ctz",      Type::Integer, { Type::Integer } },
        Function{ "popcount", Type::Integer, { Type::Integer } },

        Function{ "reverse_bits", Type::Integer, { Type::Integer } },
        Function{ "extract_bits", Type::Integer, { Type::Integer, Type::Uint, Type::Uint } },
        Function{ "insert_bits",  Type::Integer, { Type::Integer, Type::Integer, Type::Uint, Type::Uint } },

        // Logical functions
        Function{ "all",    Type::Bool, { Type::Boolean } },
        Function{ "any",    Type::Bool, { Type::Boolean } },
        Function{ "select", Type::Bool, { Type::Floating, Type::Floating, Type::Boolean } }
    };
}
