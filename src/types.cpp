#include "tungsten/types.hpp"
#include "tungsten/error.hpp"
#include "tungsten/parser.hpp"

#include <cassert>
#include <cstddef>
#include <unordered_map>
#include <array>
#include <ranges>

namespace tungsten::types
{
    using parser::Ast, parser::AstNode, parser::AstNodeType;

    static std::unordered_map<std::string, const Type*> name_type_map;
    static std::deque<Type> type_list;
    static std::vector<TypeNamePair> type_name_pairs;
    static std::vector<TypeNamePair> variable_stack;
    static const Type NULL_TYPE { .kind = TypeKind::None };

    bool Type::is_valid_builtin() const
    {
        if (kind != TypeKind::Builtin)
        {
            return false;
        }

        if (builtin_type.scalar <= ScalarType::None || builtin_type.scalar > ScalarType::MaxValue)
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

    void create_and_insert_struct_type(const Ast* ast, const AstNode& node)
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
            return;
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
            return;
        }
        name_type_map.insert({ std::string(struct_type.user_type.name), &struct_type });
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

        if (!is_equivalent_type(left_type, right_type))
        {
            error::report(
                "No matching operator for types '" + left_type->name() + "' and '" + right_type->name() + "'",
                node.byte_offset, node.byte_length
            );
            return &NULL_TYPE;
        }
        return left_type;
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
        assert(node.node_type == AstNodeType::PropertyAccess);

        const Type* left_type = type_check_variable_or_property_access(ast, ast->nodes[node.property_access.left]);

        // Access structure members
        if (left_type->kind == TypeKind::Struct || left_type->kind == TypeKind::VertexGroup)
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

    const Type* type_check_function_call(const Ast* ast, const AstNode& node)
    {
        assert(node.node_type == AstNodeType::FunctionCall);

        const Type* function_type = get_type(node.function_call.name);
        if (function_type->kind != TypeKind::Function)
        {
            error::report("Function does not exist", node.byte_offset, node.byte_length);
            return function_type->user_type.return_type;
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
            case AstNodeType::UniformGroup:
            case AstNodeType::VertexGroup:
                create_and_insert_struct_type(ast, node);
                break;
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
        for (uint32_t root_node_index : ast->root_nodes)
        {
            type_check_root_node(ast, ast->nodes[root_node_index]);
        }
        pop_variable_scope();
        assert(variable_stack.empty());
    }
}
