#include "tungsten/types.hpp"
#include "tungsten/error.hpp"
#include "tungsten/lexer.hpp"
#include "tungsten/parser.hpp"

#include <cassert>
#include <cstddef>
#include <unordered_map>
#include <array>
#include <ranges>
#include <bit>
#include <utility>

namespace tungsten::types
{
    using parser::Ast, parser::AstNode, parser::AstNodeType;

    static std::unordered_map<std::string, const Type*> name_type_map;
    static std::deque<Type> type_list;
    static std::vector<TypeNamePair> type_name_pairs;
    static std::vector<TypeNamePair> variable_stack;
    static std::vector<TypeTemplate> type_templates;
    static const Type NULL_TYPE { .kind = TypeKind::None };

    bool Type::is_valid_builtin() const
    {
        if (kind != TypeKind::Builtin)
        {
            return false;
        }

        if (!std::has_single_bit(std::to_underlying(builtin_type.scalar)) || builtin_type.scalar > ScalarType::MaxValue)
        {
            return false;
        }

        // Scalar types
        if (builtin_type.count_x == 1 && builtin_type.count_y == 1)
        {
            return true;
        }

        // Vector types
        if (builtin_type.count_x > 1 && builtin_type.count_x <= 4 && builtin_type.count_y == 1)
        {
            return true;
        }

        // Matrix types (only square matrices are supported)
        if (builtin_type.count_x > 1 && builtin_type.count_x <= 4 && builtin_type.count_x == builtin_type.count_y)
        {
            return builtin_type.scalar == ScalarType::Half || builtin_type.scalar == ScalarType::Float;
        }

        return false;
    }

    bool Type::is_scalar() const
    {
        assert(is_valid_builtin());
        return builtin_type.count_x == 1;
    }

    bool Type::is_vector() const
    {
        assert(is_valid_builtin());
        return builtin_type.count_x > 1 && builtin_type.count_y == 1;
    }

    bool Type::is_matrix() const
    {
        assert(is_valid_builtin());
        return builtin_type.count_y > 1;
    }

    std::string Type::name() const
    {
        if (kind == TypeKind::None)
        {
            return "null";
        }

        std::string output;
        if (kind == TypeKind::Builtin)
        {
            assert(is_valid_builtin());

            switch (builtin_type.scalar)
            {
                case ScalarType::Bool:  output = "bool";  break;
                case ScalarType::Half:  output = "half";  break;
                case ScalarType::Float: output = "float"; break;
                case ScalarType::Uint:  output = "uint";  break;
                case ScalarType::Int:   output = "int";   break;
                default:
                    assert(false);
            }
            if (builtin_type.count_x > 1)
            {
                output += static_cast<char>(builtin_type.count_x + '0');
            }
            if (builtin_type.count_y > 1)
            {
                output += 'x';
                output += static_cast<char>(builtin_type.count_y + '0');
            }
        }
        else
        {
            output = std::string(user_type.name);
        }

        if (is_const)
        {
            output = "const " + output;
        }
        if (is_array)
        {
            output += "[]";
        }

        return output;
    }

    std::string scalar_to_string(ScalarType scalar_type)
    {
        Type type {
            .kind = TypeKind::Builtin,
            .builtin_type = {
                .scalar = scalar_type,
                .count_x = 1,
                .count_y = 1
            }
        };
        return type.name();
    }

    void populate_library_functions()
    {
        struct LibraryFunctionDefinition
        {
            std::string_view name;
            TypeTemplate return_type_template;
            std::array<TypeTemplate, 4> parameter_type_templates;
        };

        constexpr TypeTemplate None {};
        constexpr TypeTemplate ScalarUint {
            .allowed_types = ScalarType::Uint,
            .vector_components_mask = 0b0001
        };
        constexpr TypeTemplate ScalarBoolean {
            .allowed_types = ScalarType::Bool,
            .vector_components_mask = 0b0001
        };
        constexpr TypeTemplate Numerical {
            .allowed_types = ScalarType::Half | ScalarType::Float | ScalarType::Uint | ScalarType::Int,
            .vector_components_mask = 0b1111
        };
        constexpr TypeTemplate ScalarNumerical {
            .allowed_types = Numerical.allowed_types,
            .vector_components_mask = 0b0001
        };
        constexpr TypeTemplate Floating {
            .allowed_types = ScalarType::Half | ScalarType::Float,
            .vector_components_mask = 0b1111
        };
        constexpr TypeTemplate ScalarFloating {
            .allowed_types = Floating.allowed_types,
            .vector_components_mask = 0b0001
        };
        constexpr TypeTemplate VectorFloating {
            .allowed_types = Floating.allowed_types,
            .vector_components_mask = 0b1110
        };
        constexpr TypeTemplate VectorFloating3 {
            .allowed_types = ScalarType::Float | ScalarType::Half,
            .vector_components_mask = 0b0100
        };
        constexpr TypeTemplate Integer {
            .allowed_types = ScalarType::Uint | ScalarType::Int,
            .vector_components_mask = 0b1111
        };
        constexpr TypeTemplate Boolean {
            .allowed_types = ScalarType::Bool,
            .vector_components_mask = 0b1111
        };

        constexpr static std::array<LibraryFunctionDefinition, 54> library_function_definitions {
            LibraryFunctionDefinition
            { "abs",   Numerical, { Numerical } },
            { "clamp", Numerical, { Numerical, Numerical, Numerical } },
            { "max",   Numerical, { Numerical, Numerical } },
            { "min",   Numerical, { Numerical, Numerical } },

            // Trigonometric functions
            { "sin",  Floating, { Floating } },
            { "cos",  Floating, { Floating } },
            { "tan",  Floating, { Floating } },
            { "asin", Floating, { Floating } },
            { "acos", Floating, { Floating } },
            { "atan", Floating, { Floating } },

            // Hyperbolic functions
            { "sinh",  Floating, { Floating } },
            { "cosh",  Floating, { Floating } },
            { "tanh",  Floating, { Floating } },
            { "acosh", Floating, { Floating } },
            { "asinh", Floating, { Floating } },
            { "atanh", Floating, { Floating } },

            // Other floating-point functions
            { "log",  Floating, { Floating } },
            { "log2", Floating, { Floating } },
            { "exp",  Floating, { Floating } },
            { "exp2", Floating, { Floating } },
            { "pow",  Floating, { Floating, Floating } },

            { "sign",  Floating, { Floating } },
            { "fract", Floating, { Floating } },
            { "trunc", Floating, { Floating } },
            { "ceil",  Floating, { Floating } },
            { "floor", Floating, { Floating } },
            { "round", Floating, { Floating } },

            { "sqrt",  Floating, { Floating } },
            { "rsqrt", Floating, { Floating } },

            { "saturate",   Floating, { Floating } },
            { "mix",        Floating, { Floating, Floating, Floating } },
            { "fma",        Floating, { Floating, Floating, Floating } },
            { "smoothstep", Floating, { Floating, Floating, Floating } },
            { "step",       Floating, { Floating, Floating } },
            { "ldexp",      Floating, { Floating, Integer } },

            // Derivative functions
            { "dfdx",   Floating, { Floating } },
            { "dfdy",   Floating, { Floating } },
            { "fwidth", Floating, { Floating } },

            // Vector functions
            { "cross",     VectorFloating3, { VectorFloating3, VectorFloating3 } },
            { "distance",  ScalarFloating, { Floating, Floating } },
            { "dot",       ScalarNumerical, { Numerical, Numerical } },
            { "length",    ScalarFloating, { Floating } },
            { "normalize", VectorFloating, { VectorFloating } },
            { "reflect",   VectorFloating, { VectorFloating, VectorFloating } },
            { "refract",   VectorFloating, { VectorFloating, VectorFloating } },

            // Integer functions
            { "clz",      Integer, { Integer } },
            { "ctz",      Integer, { Integer } },
            { "popcount", Integer, { Integer } },

            { "reverse_bits", Integer, { Integer } },
            { "extract_bits", Integer, { Integer, ScalarUint, ScalarUint } },
            { "insert_bits",  Integer, { Integer, Integer, ScalarUint, ScalarUint } },

            // Logical functions
            { "all",    ScalarBoolean, { Boolean } },
            { "any",    ScalarBoolean, { Boolean } },
            { "select", Floating, { Floating, Floating, Boolean } }
        };

        for (const LibraryFunctionDefinition& definition : library_function_definitions)
        {
            Type* type = &type_list.emplace_back(Type{});
            type->kind = TypeKind::LibraryFunction;
            type->library_function_type.name = definition.name;

            type->library_function_type.parameters = {
                .vector = &type_templates,
                .index = static_cast<uint32_t>(type_templates.size())
            };

            std::array<TypeTemplate, 2> used_templates{};
            for (const TypeTemplate& type_template : definition.parameter_type_templates)
            {
                if (type_template == None)
                {
                    break;
                }

                TypeTemplate& out_type_template = type_templates.emplace_back();
                out_type_template = type_template;

                if (used_templates[0] == None || (out_type_template.allowed_types == used_templates[0].allowed_types &&
                    out_type_template.vector_components_mask == used_templates[0].vector_components_mask))
                {
                    out_type_template.template_index = 0;
                    used_templates[0] = out_type_template;
                }
                else
                {
                    if (used_templates[1] != None)
                    {
                        assert(
                            out_type_template.allowed_types == used_templates[1].allowed_types &&
                            out_type_template.vector_components_mask == used_templates[1].vector_components_mask
                        );
                    }
                    out_type_template.template_index = 1;
                    used_templates[1] = out_type_template;
                }
            }

            type->library_function_type.parameters.size = type_templates.size() - type->library_function_type.parameters.index;

            type->library_function_type.return_type_template_index = 1;
            if (definition.return_type_template.allowed_types == used_templates[0].allowed_types)
            {
                type->library_function_type.return_type_template_index = 0;
            }

            type->library_function_type.return_type_is_scalar = (definition.return_type_template.vector_components_mask == 0b0001);

            name_type_map.insert({ std::string(definition.name), type });
        }
    }

    const Type* get_builtin_type(std::string_view type_name)
    {
        auto it = name_type_map.find(std::string(type_name));
        if (it != name_type_map.end())
        {
            if (it->second->kind == TypeKind::Builtin)
            {
                return it->second;
            }
            return &NULL_TYPE;
        }

        Type type {
            .kind = TypeKind::Builtin,
            .builtin_type = {
                .scalar = ScalarType::None,
                .count_x = 1,
                .count_y = 1
            }
        };

        static constexpr std::array<std::pair<std::string_view, ScalarType>, 5> name_scalar_type_pairs {
            std::pair{ "bool",  ScalarType::Bool  },
            std::pair{ "half",  ScalarType::Half  },
            std::pair{ "float", ScalarType::Float },
            std::pair{ "uint",  ScalarType::Uint  },
            std::pair{ "int",   ScalarType::Int   }
        };

        std::string_view vector_count;
        for (const std::pair<std::string_view, ScalarType>& pair : name_scalar_type_pairs)
        {
            if (type_name.starts_with(pair.first))
            {
                vector_count = type_name.substr(pair.first.size());
                type.builtin_type.scalar = pair.second;
                break;
            }
        }

        if (type.builtin_type.scalar == ScalarType::None)
        {
            return &NULL_TYPE;
        }

        bool is_valid_type = false;
        if (vector_count.size() == 0)
        {
            is_valid_type = true;
        }
        else if (vector_count.size() == 1)
        {
            int num_components = vector_count[0] - '0';
            if (num_components >= 2 && num_components <= 4)
            {
                type.builtin_type.count_x = num_components;
                is_valid_type = true;
            }
        }
        else if (vector_count.size() == 3)
        {
            // NOTE: This language currently only supports square matrices
            int num_components_x = vector_count[0] - '0';
            int num_components_y = vector_count[2] - '0';
            if (vector_count[1] == 'x' && num_components_x == num_components_y &&
                num_components_x >= 2 && num_components_x <= 4)
            {
                type.builtin_type.count_x = num_components_x;
                type.builtin_type.count_y = num_components_y;
                is_valid_type = true;
            }
        }

        if (!is_valid_type)
        {
            return &NULL_TYPE;
        }

        assert(type.is_valid_builtin());

        const Type* output_type = &type_list.emplace_back(type);
        name_type_map.insert({ std::string(type_name), output_type });
        return output_type;
    }

    const Type* get_type(std::string_view type_name)
    {
        auto it = name_type_map.find(std::string(type_name));
        if (it != name_type_map.end())
        {
            return it->second;
        }
        return get_builtin_type(type_name);
    }

    const Type* create_modified_type(const Type& type)
    {
        std::string key = type.name();

        auto it = name_type_map.find(key);
        if (it != name_type_map.end())
        {
            return it->second;
        }

        const Type* out = &type_list.emplace_back(type);
        name_type_map.insert({ out->name(), out });

        return out;
    }

    const Type* remove_const(const Type* type)
    {
        if (!type->is_const)
        {
            return type;
        }

        Type new_type = *type;
        new_type.is_const = false;
        return create_modified_type(new_type);
    }

    const Type* create_struct_type(const Ast* ast, const AstNode& node)
    {
        Type& struct_type = type_list.emplace_back(Type{});

        switch (node.node_type)
        {
            case AstNodeType::Struct:
                struct_type.kind = TypeKind::Struct;
                break;
            case AstNodeType::UniformGroup:
                struct_type.kind = TypeKind::UniformGroup;
                break;
            case AstNodeType::VertexGroup:
                struct_type.kind = TypeKind::VertexGroup;
                break;
            default:
                assert(false);
        }

        if (get_builtin_type(node.struct_declaration.name) != &NULL_TYPE)
        {
            error::report("Structure name cannot be the same as a builtin type", node.byte_offset, node.byte_length);
            return &NULL_TYPE;
        }

        struct_type.user_type.name = node.struct_declaration.name;
        struct_type.user_type.members = {
            .vector = &type_name_pairs,
            .index = static_cast<uint32_t>(type_name_pairs.size()),
            .size = node.struct_declaration.member_nodes.size
        };

        for (uint32_t member_node_index : node.struct_declaration.member_nodes)
        {
            const AstNode& member_node = ast->nodes[member_node_index];
            const TypeNamePair& member_type = type_name_pairs.emplace_back(
                get_builtin_type(member_node.struct_member.type_name),
                member_node.struct_member.name
            );
            error::check(member_type.type != &NULL_TYPE, "Member type must be a builtin type", member_node.byte_offset, member_node.byte_length);
        }

        if (name_type_map.contains(std::string(struct_type.user_type.name)))
        {
            error::report("Another user-defined type already has this name", node.byte_offset, node.byte_length);
            return &NULL_TYPE;
        }
        return &struct_type;
    }

    void create_and_insert_struct_type(const Ast* ast, const AstNode& node)
    {
        assert(node.node_type == AstNodeType::Struct || node.node_type == AstNodeType::VertexGroup);

        const Type* type = create_struct_type(ast, node);
        if (type != &NULL_TYPE)
        {
            name_type_map.insert({ std::string(type->user_type.name), type });
        }
    }

    const Type* create_and_insert_function_type(const Ast* ast, const AstNode& node)
    {
        Type& function_type = type_list.emplace_back(Type{});
        function_type.kind = TypeKind::Function;
        function_type.user_type.return_type = get_type(node.function_declaration.return_type_name);

        if (function_type.user_type.return_type == &NULL_TYPE)
        {
            error::report(
                "Unknown return type '" + std::string(node.function_declaration.return_type_name) + "'",
                node.byte_offset, node.byte_length
            );
            return &NULL_TYPE;
        }

        function_type.user_type.name = node.function_declaration.name;
        function_type.user_type.members = {
            .vector = &type_name_pairs,
            .index = static_cast<uint32_t>(type_name_pairs.size()),
            .size = node.function_declaration.argument_nodes.size
        };

        for (uint32_t parameter_node_index : node.function_declaration.argument_nodes)
        {
            const AstNode& parameter_node = ast->nodes[parameter_node_index];
            assert(parameter_node.node_type == AstNodeType::FunctionArgument);

            TypeNamePair& parameter_type = type_name_pairs.emplace_back();
            parameter_type.type = get_type(parameter_node.struct_member.type_name);
            parameter_type.name = parameter_node.struct_member.name;

            if (parameter_type.type == &NULL_TYPE)
            {
                error::report("Unknown type", parameter_node.byte_offset, parameter_node.byte_length);
            }
        }

        name_type_map.insert({ std::string(node.function_declaration.name), &function_type });
        return &function_type;
    }

    void push_variable_scope()
    {
        variable_stack.push_back({ .type = nullptr });
    }

    void pop_variable_scope()
    {
        while (variable_stack.size() > 0)
        {
            const Type* final_type = variable_stack.back().type;
            variable_stack.pop_back();

            if (final_type == nullptr)
            {
                break;
            }
        }
    }

    void scope_add_variable(const Type* type, std::string_view name)
    {
        variable_stack.push_back({
            .type = type,
            .name = name
        });
    }

    const Type* scope_get_variable_type(std::string_view name)
    {
        for (TypeNamePair& pair : std::views::reverse(variable_stack))
        {
            if (pair.type == nullptr)
            {
                continue;
            }
            if (name == pair.name)
            {
                return pair.type;
            }
        }
        return &NULL_TYPE;
    }

    const Type* current_scope_get_variable_type(std::string_view name)
    {
        for (TypeNamePair& pair : std::views::reverse(variable_stack))
        {
            if (pair.type == nullptr)
            {
                break;
            }
            if (name == pair.name)
            {
                return pair.type;
            }
        }
        return &NULL_TYPE;
    }

    void scope_add_function_parameters(const Type* function_type)
    {
        assert(function_type != &NULL_TYPE && function_type->kind == TypeKind::Function);

        variable_stack.append_range(
            std::ranges::subrange(
                function_type->user_type.members.begin(),
                function_type->user_type.members.end()
            )
        );
    }

    bool is_equivalent_type(const Type* a, const Type* b)
    {
        // TODO: Allow implicit casts rather than expecting the exact type
        return remove_const(a) == remove_const(b) && a != &NULL_TYPE;
    }

    const Type* get_binary_operator_return_type(const Type* left_type, const Type* right_type, lexer::Operator operation)
    {
        using enum lexer::Operator;

        assert(left_type != &NULL_TYPE && right_type != &NULL_TYPE);

        if (operation == Assign)
        {
            return is_equivalent_type(left_type, right_type) ? left_type : &NULL_TYPE;
        }
        if (!left_type->is_valid_builtin() || !right_type->is_valid_builtin())
        {
            return &NULL_TYPE;
        }

        const auto has_operator = [](std::span<lexer::Operator> span, lexer::Operator operation)
        {
            return std::find(span.begin(), span.end(), operation) != span.end();
        };

        std::array comparison_operators {
            Less, LessEqual, Equal, GreaterEqual, Greater
        };
        if (has_operator(comparison_operators, operation))
        {
            return is_equivalent_type(left_type, right_type) ? get_builtin_type("bool") : &NULL_TYPE;
        }

        std::array logical_operators { LogicalAnd, LogicalOr, LogicalNot };
        if (has_operator(logical_operators, operation))
        {
            const Type* bool_type = get_builtin_type("bool");
            if (is_equivalent_type(left_type, bool_type) && is_equivalent_type(right_type, bool_type))
            {
                return bool_type;
            }
            return &NULL_TYPE;
        }

        std::array integer_operators {
            Mod, AssignMod, BitwiseAnd, BitwiseOr, BitwiseNot, BitwiseXor, BitwiseLeftShift, BitwiseRightShift,
            AssignBitwiseAnd, AssignBitwiseOr, AssignBitwiseXor, AssignBitwiseLeftShift, AssignBitwiseRightShift
        };
        if (has_operator(integer_operators, operation))
        {
            if (!is_equivalent_type(left_type, right_type))
            {
                return &NULL_TYPE;
            }

            constexpr std::array possible_type_names {
                "uint", "uint2", "uint3", "uint4", "int", "int2", "int3", "int4"
            };
            for (std::string_view type_name : possible_type_names)
            {
                const Type* type = get_builtin_type(type_name);
                if (is_equivalent_type(left_type, type))
                {
                    return type;
                }
            }
            return &NULL_TYPE;
        }

        std::array mul_operators { Mul, AssignMul };
        if (has_operator(mul_operators, operation) && (left_type->is_matrix() || right_type->is_matrix()))
        {
            // TODO: Support scalar * matrix?
            if (left_type->is_matrix() && right_type->is_matrix())
            {
                return is_equivalent_type(left_type, right_type) ? remove_const(left_type) : &NULL_TYPE;
            }
            if (!left_type->is_vector() && !right_type->is_vector())
            {
                return &NULL_TYPE;
            }
            if (left_type->builtin_type.scalar == right_type->builtin_type.scalar &&
                left_type->builtin_type.count_x == right_type->builtin_type.count_x)
            {
                return remove_const(left_type->is_vector() ? left_type : right_type);
            }
            return &NULL_TYPE;
        }

        std::array mul_div_operators { Mul, Div, AssignMul, AssignDiv };
        if (has_operator(mul_div_operators, operation))
        {
            if (is_equivalent_type(left_type, right_type))
            {
                return remove_const(left_type);
            }
            if (!left_type->is_scalar() && !right_type->is_scalar())
            {
                return &NULL_TYPE;
            }
            if (!left_type->is_vector() && !right_type->is_vector())
            {
                return &NULL_TYPE;
            }
            if (left_type->builtin_type.scalar == right_type->builtin_type.scalar)
            {
                return remove_const(left_type->is_vector() ? left_type : right_type);
            }
            return &NULL_TYPE;
        }

        std::array other_operators {
            Add, Sub, AssignAdd, AssignSub
        };
        if (has_operator(other_operators, operation))
        {
            return is_equivalent_type(left_type, right_type) ? remove_const(left_type) : &NULL_TYPE;
        }

        assert(false);
    }


    const Type* get_numeric_literal_type(std::string_view literal)
    {
        if (literal.ends_with('f')) return get_builtin_type("float");
        if (literal.ends_with('h')) return get_builtin_type("half");
        if (literal.ends_with('u')) return get_builtin_type("uint");
        if (literal.contains('.')) return get_builtin_type("float");
        return get_builtin_type("int");
    }

    const Type* type_check_expression_node(const Ast* ast, const AstNode& node);

    const Type* type_check_binary_operation(const Ast* ast, const AstNode& node)
    {
        assert(node.node_type == AstNodeType::BinaryOperation);

        // TODO: Check operation

        const Type* left_type = type_check_expression_node(ast, ast->nodes[node.binary_operation.left]);
        const Type* right_type = type_check_expression_node(ast, ast->nodes[node.binary_operation.right]);

        if (left_type == &NULL_TYPE || right_type == &NULL_TYPE)
        {
            return &NULL_TYPE;
        }

        const Type* return_type = get_binary_operator_return_type(left_type, right_type, node.binary_operation.operation);
        if (return_type == &NULL_TYPE)
        {
            error::report(
                "No matching operator for types '" + left_type->name() + "' and '" + right_type->name() + "'",
                node.byte_offset, node.byte_length
            );
        }
        return return_type;
    }

    const Type* type_check_variable_or_property_access(const Ast* ast, const AstNode& node)
    {
        if (node.node_type == AstNodeType::Variable)
        {
            const Type* variable_type = scope_get_variable_type(node.variable.name);
            if (variable_type == &NULL_TYPE)
            {
                error::report("Use of undeclared variable", node.byte_offset, node.byte_length);
            }
            return variable_type;
        }
        if (node.node_type != AstNodeType::PropertyAccess)
        {
            return type_check_expression_node(ast, node);
        }

        const Type* left_type = type_check_variable_or_property_access(ast, ast->nodes[node.property_access.left]);

        // Access structure members
        if (left_type->kind == TypeKind::Struct || left_type->kind == TypeKind::VertexGroup || left_type->kind == TypeKind::UniformGroup)
        {
            for (const TypeNamePair& pair : left_type->user_type.members)
            {
                if (pair.name == node.property_access.name)
                {
                    return pair.type;
                }
            }
            error::report("Property does not exist", node.byte_offset, node.byte_length);
            return &NULL_TYPE;
        }

        // Access vector components
        if (left_type->kind == TypeKind::Builtin && left_type->builtin_type.count_x > 1)
        {
            std::string_view swizzle = node.property_access.name;

            bool contains_xyzw = swizzle.contains('x') || swizzle.contains('y') || swizzle.contains('z') || swizzle.contains('w');
            bool contains_rgba = swizzle.contains('r') || swizzle.contains('g') || swizzle.contains('b') || swizzle.contains('a');
            if (contains_xyzw && contains_rgba)
            {
                error::report("xyzw and rgba swizzle components must not be mixed", node.byte_offset, node.byte_length);
                return &NULL_TYPE;
            }
            if (!contains_xyzw && !contains_rgba)
            {
                error::report("Invalid swizzle components", node.byte_offset, node.byte_length);
                return &NULL_TYPE;
            }

            Type scalar_type {
                .kind = TypeKind::Builtin,
                .builtin_type = {
                    .scalar = left_type->builtin_type.scalar,
                    .count_x = 1,
                    .count_y = 1
                }
            };

            // We construct a string of the builtin type so that the code for builtin type generation and map insertion stays in one place
            // Otherwise, we would have to check the map ourselves because type equality is based on pointer equality
            if (swizzle.size() == 1)
            {
                return get_builtin_type(scalar_type.name());
            }
            return get_builtin_type(scalar_type.name() + std::to_string(swizzle.size()));
        }

        error::report("Cannot access property of type '" + left_type->name() + "'", node.byte_offset, node.byte_length);
        return &NULL_TYPE;
    }

    const Type* type_check_array_index(const Ast* ast, const AstNode& node)
    {
        assert(node.node_type == AstNodeType::ArrayIndex);

        const Type* array_type = type_check_variable_or_property_access(ast, ast->nodes[node.array_index.left]);
        if (!array_type->is_array)
        {
            const AstNode& indexed_node = ast->nodes[node.array_index.left];
            error::report("Type being indexed is not an array", indexed_node.byte_offset, indexed_node.byte_length);
            return &NULL_TYPE;
        }

        const Type* index_type = type_check_expression_node(ast, ast->nodes[node.array_index.right]);
        if (!is_equivalent_type(index_type, get_builtin_type("int")) && !is_equivalent_type(index_type, get_builtin_type("uint")))
        {
            const AstNode& index_node = ast->nodes[node.array_index.right];
            error::report("Index type must be int or uint", index_node.byte_offset, index_node.byte_length);
        }

        Type output_type = *array_type;
        output_type.is_array = false;
        return create_modified_type(output_type);
    }

    void type_check_expression(const Ast* ast, const AstNode& node, const Type* return_type);

    const Type* type_check_library_function_call(const Ast* ast, const AstNode& node, const Type* function_type)
    {
        assert(node.node_type == AstNodeType::FunctionCall);

        struct ScalarVectorType
        {
            ScalarType scalar_type;
            uint8_t num_components;
        };

        constexpr uint32_t MAX_TEMPLATE_INDICES = 2;
        std::array<ScalarVectorType, MAX_TEMPLATE_INDICES> template_index_type_map{};

        if (function_type->library_function_type.parameters.size != node.function_call.argument_nodes.size)
        {
            error::report("Unexpected number of arguments", node.byte_offset, node.byte_length);
            return &NULL_TYPE;
        }

        bool had_error = false;
        for (uint32_t i = 0; i < node.function_call.argument_nodes.size; i++)
        {
            const AstNode& argument_node = ast->nodes[node.function_call.argument_nodes[i]];
            const Type* argument_type = type_check_expression_node(ast, argument_node);

            if (!argument_type->is_valid_builtin() || argument_type->is_matrix())
            {
                error::report("Library function argument must be a scalar or vector type", argument_node.byte_offset, argument_node.byte_length);
                had_error = true;
                continue;
            }

            const TypeTemplate& parameter_template = function_type->library_function_type.parameters[i];
            if ((parameter_template.allowed_types & argument_type->builtin_type.scalar) &&
                (parameter_template.vector_components_mask & (1 << (argument_type->builtin_type.count_x - 1))))
            {
                ScalarVectorType& argument_type_template = template_index_type_map[parameter_template.template_index];
                if (argument_type_template.scalar_type == ScalarType::None)
                {
                    argument_type_template.scalar_type = argument_type->builtin_type.scalar;
                    argument_type_template.num_components = argument_type->builtin_type.count_x;
                    continue;
                }
                if (argument_type_template.scalar_type != argument_type->builtin_type.scalar ||
                    argument_type_template.num_components != argument_type->builtin_type.count_x)
                {
                    Type expected_type {
                        .kind = TypeKind::Builtin,
                        .builtin_type = {
                            .scalar = argument_type_template.scalar_type,
                            .count_x = argument_type_template.num_components,
                            .count_y = 1
                        }
                    };

                    error::report(
                        "Expected '" + expected_type.name() + "', but got '" + argument_type->name() + "'",
                        argument_node.byte_offset, argument_node.byte_length
                    );
                    had_error = true;
                }
                continue;
            }

            // TODO: List allowed types
            error::report("Invalid type", argument_node.byte_offset, argument_node.byte_length);
            had_error = true;
        }

        if (had_error)
        {
            return &NULL_TYPE;
        }

        uint8_t return_template_index = function_type->library_function_type.return_type_template_index;

        Type output_type {
            .kind = TypeKind::Builtin,
            .builtin_type = {
                .scalar = template_index_type_map[return_template_index].scalar_type,
                .count_x = function_type->library_function_type.return_type_is_scalar ?
                    static_cast<uint8_t>(1) :
                    template_index_type_map[return_template_index].num_components,
                .count_y = 1
            }
        };
        assert(output_type.is_valid_builtin());

        return get_builtin_type(output_type.name());
    }

    const Type* type_check_function_call(const Ast* ast, const AstNode& node)
    {
        assert(node.node_type == AstNodeType::FunctionCall);

        const Type* function_type = get_type(node.function_call.name);
        if (function_type->kind == TypeKind::Builtin && function_type->is_vector())
        {
            uint32_t num_components_consumed = 0;
            bool had_invalid_type = false;
            for (uint32_t argument_node_index : node.function_call.argument_nodes)
            {
                const AstNode& expression_node = ast->nodes[argument_node_index];
                const Type* expression_type = type_check_expression_node(ast, expression_node);

                if (!expression_type->is_valid_builtin())
                {
                    error::report(
                        "Expected scalar or vector type, but got '" + expression_type->name() + "'",
                        expression_node.byte_offset, expression_node.byte_length
                    );
                    had_invalid_type = true;
                    continue;
                }
                if (function_type->builtin_type.scalar != expression_type->builtin_type.scalar || expression_type->is_matrix())
                {
                    std::string left_scalar = scalar_to_string(function_type->builtin_type.scalar);

                    error::report(
                        "Expected scalar or vector of '" + left_scalar + "', but got '" + expression_type->name() + "'",
                        expression_node.byte_offset, expression_node.byte_length
                    );
                    had_invalid_type = true;
                    continue;
                }

                num_components_consumed += expression_type->builtin_type.count_x;
            }

            if (!had_invalid_type && num_components_consumed != function_type->builtin_type.count_x && num_components_consumed != 1)
            {
                error::report(
                    "Invalid number of components passed to '" + function_type->name() + "': Expected " +
                    std::to_string((int)function_type->builtin_type.count_x) + ", but got " + std::to_string(num_components_consumed),
                    node.byte_offset, node.byte_length
                );
            }

            return function_type;
        }

        if (function_type->kind == TypeKind::LibraryFunction)
        {
            return type_check_library_function_call(ast, node, function_type);
        }

        if (function_type->kind != TypeKind::Function)
        {
            error::report("Function does not exist", node.byte_offset, node.byte_length);
            return &NULL_TYPE;
        }

        if (function_type->user_type.members.size != node.function_call.argument_nodes.size)
        {
            error::report("Unexpected number of arguments", node.byte_offset, node.byte_length);
            return function_type->user_type.return_type;
        }

        for (uint32_t i = 0; i < function_type->user_type.members.size; i++)
        {
            const Type* parameter_type = function_type->user_type.members[i].type;
            const AstNode& expression_node = ast->nodes[node.function_call.argument_nodes[i]];

            type_check_expression(ast, expression_node, parameter_type);
        }

        return function_type->user_type.return_type;
    }

    const Type* type_check_expression_node(const Ast* ast, const AstNode& node)
    {
        switch (node.node_type)
        {
            case AstNodeType::ArrayIndex:
                return type_check_array_index(ast, node);
            case AstNodeType::NumericLiteral:
                return get_numeric_literal_type(node.numeric_literal.str);
            case AstNodeType::BooleanLiteral:
                return get_builtin_type("bool");
            case AstNodeType::UnaryOperation:
                // TODO: Check operation
                return type_check_expression_node(ast, ast->nodes[node.unary_operation.operand]);
            case AstNodeType::BinaryOperation:
                return type_check_binary_operation(ast, node);
            case AstNodeType::Variable:
            case AstNodeType::PropertyAccess:
                return type_check_variable_or_property_access(ast, node);
            case AstNodeType::FunctionCall:
                return type_check_function_call(ast, node);
            default:
                assert(false);
        }
    }

    void type_check_expression(const Ast* ast, const AstNode& node, const Type* return_type)
    {
        const Type* actual_return_type = type_check_expression_node(ast, node);
        if (!is_equivalent_type(return_type, actual_return_type))
        {
            error::report(
                "Expected type '" + return_type->name() + "', but got '" + std::string(actual_return_type->name()) + "'",
                node.byte_offset, node.byte_length
            );
        }
    }

    void scope_add_variable_declaration_and_type_check(const Ast* ast, const AstNode& node)
    {
        assert(node.node_type == AstNodeType::VariableDeclaration);

        if (current_scope_get_variable_type(node.variable_declaration.name) != &NULL_TYPE)
        {
            error::report("Variable redefinition", node.byte_offset, node.byte_length);
            return;
        }

        const Type* variable_type = get_type(node.variable_declaration.type_name);
        if (node.variable_declaration.is_array_declaration || node.variable_declaration.is_const)
        {
            Type modified_variable_type = *variable_type;
            modified_variable_type.is_array = node.variable_declaration.is_array_declaration;
            modified_variable_type.is_const = node.variable_declaration.is_const;
            variable_type = create_modified_type(modified_variable_type);
        }

        if (variable_type == &NULL_TYPE)
        {
            error::report("Unknown variable type", node.byte_offset, node.byte_length);
            return;
        }
        scope_add_variable(variable_type, node.variable_declaration.name);

        if (!node.variable_declaration.is_array_declaration)
        {
            if (node.variable_declaration.expression != 0)
            {
                type_check_expression(ast, ast->nodes[node.variable_declaration.expression], variable_type);
            }
        }
        else
        {
            const Type* underlying_type = get_type(node.variable_declaration.type_name);
            for (uint32_t expression_node_index : node.variable_declaration.array_expressions)
            {
                type_check_expression(ast, ast->nodes[expression_node_index], underlying_type);
            }
        }
    }

    void type_check_variable_assignment(const Ast* ast, const AstNode& node)
    {
        assert(node.node_type == AstNodeType::VariableAssignment);

        const Type* left_type = type_check_variable_or_property_access(ast, ast->nodes[node.variable_assignment.variable_node]);

        if (left_type->is_const)
        {
            const AstNode& left_node = ast->nodes[node.variable_assignment.variable_node];
            error::report("Cannot assign to const variable", left_node.byte_offset, left_node.byte_length);
        }

        if (node.variable_assignment.expression == 0)
        {
            return;
        }

        const Type* right_type = type_check_expression_node(ast, ast->nodes[node.variable_assignment.expression]);

        if (left_type == &NULL_TYPE || right_type == &NULL_TYPE)
        {
            return;
        }

        if (!is_equivalent_type(left_type, right_type))
        {
            const AstNode& expression_node = ast->nodes[node.variable_assignment.expression];
            error::report(
                "Expected type '" + left_type->name() + "' but got '" + right_type->name() + "'",
                expression_node.byte_offset, expression_node.byte_length
            );
        }

        // TODO: Check operation
    }

    void type_check_for_loop(const Ast* ast, const AstNode& node)
    {
        push_variable_scope();

        scope_add_variable_declaration_and_type_check(ast, ast->nodes[node.for_loop.init_expression]);
        type_check_expression(ast, ast->nodes[node.for_loop.comp_expression], get_builtin_type("bool"));
        type_check_variable_assignment(ast, ast->nodes[node.for_loop.loop_expression]);

        pop_variable_scope();
    }

    void type_check_function_body(const Ast* ast, const AstNode& node, const Type* function_type)
    {
        assert(node.node_type == AstNodeType::FunctionBody);

        push_variable_scope();
        if (function_type != nullptr)
        {
            scope_add_function_parameters(function_type);
        }

        const Type* bool_type = get_builtin_type("bool");

        for (uint32_t statement_node_index : node.function_body.statements)
        {
            const AstNode& statement_node = ast->nodes[statement_node_index];
            switch (statement_node.node_type)
            {
                case AstNodeType::FunctionBody:
                    type_check_function_body(ast, statement_node, nullptr);
                    break;
                case AstNodeType::VariableDeclaration:
                    scope_add_variable_declaration_and_type_check(ast, statement_node);
                    break;
                case AstNodeType::VariableAssignment:
                    type_check_variable_assignment(ast, statement_node);
                    break;
                case AstNodeType::IfStatement:
                case AstNodeType::ElseIfStatement:
                    type_check_expression(ast, ast->nodes[statement_node.if_statement.expression], bool_type);
                    [[fallthrough]];
                case AstNodeType::ElseStatement:
                    type_check_function_body(ast, ast->nodes[statement_node.if_statement.body], nullptr);
                    break;
                case AstNodeType::ForLoop:
                    type_check_for_loop(ast, statement_node);
                    break;
                case AstNodeType::WhileLoop:
                    type_check_expression(ast, ast->nodes[statement_node.while_loop.expression], bool_type);
                    type_check_function_body(ast, ast->nodes[statement_node.while_loop.body], nullptr);
                    break;
                case AstNodeType::ReturnStatement:
                    type_check_expression(ast, ast->nodes[statement_node.return_statement.expression], function_type->user_type.return_type);
                    break;
                default:
                    assert(false);
            }
        }

        pop_variable_scope();
    }

    void type_check_root_node(const Ast* ast, const AstNode& node)
    {
        switch (node.node_type)
        {
            case AstNodeType::Struct:
            case AstNodeType::VertexGroup:
                create_and_insert_struct_type(ast, node);
                break;
            case AstNodeType::UniformGroup: {
                const Type* type = create_struct_type(ast, node);
                if (type != &NULL_TYPE)
                {
                    scope_add_variable(type, type->user_type.name);
                }
                break;
            }
            case AstNodeType::FunctionDeclaration: {
                const Type* function_type = create_and_insert_function_type(ast, node);
                if (function_type == &NULL_TYPE) break;
                type_check_function_body(ast, ast->nodes[node.function_declaration.body], function_type);
                break;
            }
            case AstNodeType::VariableDeclaration:
                scope_add_variable_declaration_and_type_check(ast, node);
                break;
            default:
                assert(false);
        }
    }

    void type_check(const Ast *ast)
    {
        if (type_list.empty())
        {
            populate_library_functions();
        }

        for (uint32_t root_node_index : ast->root_nodes)
        {
            type_check_root_node(ast, ast->nodes[root_node_index]);
        }
        pop_variable_scope();
        assert(variable_stack.empty());
    }
}
