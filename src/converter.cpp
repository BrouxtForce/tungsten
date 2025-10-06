#include "tungsten/converter.hpp"
#include "tungsten/lexer.hpp"
#include "tungsten/parser.hpp"
#include "tungsten/types.hpp"

#include <cassert>

namespace tungsten::converter
{
    using parser::Ast, parser::AstNode, parser::AstNodeType, parser::Attribute, parser::has_attribute;

    enum Backend { MSL, WGSL };

    std::string_view get_indent(int indent)
    {
        static std::string indent_string;

        int target_indent = indent * 4;
        if ((int)indent_string.size() < target_indent)
        {
            int next_size = std::max<int>(target_indent, indent_string.size() * 2);
            indent_string.resize(next_size);
            std::memset(indent_string.data(), ' ', indent_string.size());
        }

        return std::string_view(indent_string).substr(0, target_indent);
    }

    std::string get_msl_type_name(parser::TypeDescriptor type_descriptor)
    {
        if (type_descriptor.type_name == "texture_2d")
        {
            return "texture2d<" + std::string(type_descriptor.template_type_name) + '>';
        }
        assert(type_descriptor.template_type_name.empty());
        return std::string(type_descriptor.type_name);
    }

    std::string get_wgsl_type_name(const Ast* ast, parser::TypeDescriptor type_descriptor)
    {
        if (type_descriptor.type_name == "texture_2d")
        {
            return "texture_2d<" + get_wgsl_type_name(ast, { .type_name = type_descriptor.template_type_name }) + ">";
        }
        assert(type_descriptor.template_type_name.empty());

        const types::Type* type = types::get_type(ast->type_check_info, type_descriptor);
        if (type->kind != types::TypeKind::Builtin)
        {
            return type->name();
        }

        if (type->is_scalar())
        {
            switch (type->builtin_type.scalar)
            {
                case types::ScalarType_Bool:  return "bool";
                case types::ScalarType_Half:  return "f16";
                case types::ScalarType_Float: return "f32";
                case types::ScalarType_Uint:  return "u32";
                case types::ScalarType_Int:   return "i32";
                default:
                    assert(false);
            }
        }

        if (type->is_vector())
        {
            std::string num_components_str = std::to_string(static_cast<int>(type->builtin_type.count_x));
            switch (type->builtin_type.scalar)
            {
                case types::ScalarType_Bool:  return "vec" + num_components_str + "<bool>";
                case types::ScalarType_Half:  return "vec" + num_components_str + "h";
                case types::ScalarType_Float: return "vec" + num_components_str + "f";
                case types::ScalarType_Uint:  return "vec" + num_components_str + "u";
                case types::ScalarType_Int:   return "vec" + num_components_str + "i";
                default:
                    assert(false);
            }
        }

        if (type->is_matrix())
        {
            std::string matrix_size_str = std::to_string(static_cast<int>(type->builtin_type.count_x));
            switch (type->builtin_type.scalar)
            {
                case types::ScalarType_Half:  return "mat" + matrix_size_str + "x" + matrix_size_str + "h";
                case types::ScalarType_Float: return "mat" + matrix_size_str + "x" + matrix_size_str + "f";
                default:
                    assert(false);
            }
        }

        assert(false);
    }

    std::string get_wgsl_function_name(const Ast* ast, std::string_view function_name)
    {
        if (function_name == "texture_sample") return "textureSample";

        const types::Type* function_type = get_type(ast->type_check_info, { .type_name = function_name });
        if (function_type->is_valid_builtin())
        {
            return get_wgsl_type_name(ast, { .type_name = function_name });
        }
        if (function_type->kind == types::TypeKind::LibraryFunction)
        {
            // TODO: More function conversions
            if (function_type->library_function_type.name == "unpack_unorm4x8_to_float")
            {
                return "unpack4x8unorm";
            }
        }
        return function_type->name();
    }

    bool msl_is_builtin_attribute(const Attribute& attribute, AstNodeType node_type)
    {
        switch (node_type)
        {
            case AstNodeType::StructMember:
                return attribute.name == "position";
            case AstNodeType::FunctionDeclaration:
                return attribute.name == "vertex" || attribute.name == "fragment" || attribute.name == "compute";
            case AstNodeType::FunctionArgument:
                return attribute.name == "vertex_id" || attribute.name == "instance_id";
            default: break;
        }
        return false;
    }

    bool msl_output_attributes(const AstNode& node, std::ostream& stream)
    {
        bool did_output_attribute = false;
        for (const Attribute& attribute : node.attributes)
        {
            if (msl_is_builtin_attribute(attribute, node.node_type))
            {
                stream << "[[" << attribute.name << "]] ";
                did_output_attribute = true;
            }
        }
        return did_output_attribute;
    }

    std::string wgsl_convert_builtin_attribute(const Attribute& attribute, AstNodeType node_type)
    {
        bool is_wgsl_builtin = false;
        std::string_view wgsl_attribute_name = attribute.name;
        switch (node_type)
        {
            case AstNodeType::StructMember:
                is_wgsl_builtin = attribute.name == "position";
                break;
            case AstNodeType::FunctionDeclaration:
                if (attribute.name == "vertex" || attribute.name == "fragment" || attribute.name == "compute")
                {
                    return std::string(attribute.name);
                }
                break;
            case AstNodeType::FunctionArgument:
                if (attribute.name == "vertex_id")
                {
                    is_wgsl_builtin = true;
                    wgsl_attribute_name = "vertex_index";
                }
                if (attribute.name == "instance_id")
                {
                    is_wgsl_builtin = true;
                    wgsl_attribute_name = "instance_index";
                }
                break;
            default: break;
        }
        if (is_wgsl_builtin)
        {
            return "builtin(" + std::string(wgsl_attribute_name) + ")";
        }
        return {};
    }

    bool wgsl_output_attributes(const AstNode& node, std::ostream& stream)
    {
        bool did_output_attribute = false;
        for (const Attribute& attribute : node.attributes)
        {
            std::string builtin_attribute = wgsl_convert_builtin_attribute(attribute, node.node_type);
            if (!builtin_attribute.empty())
            {
                stream << '@' << builtin_attribute << ' ';
                did_output_attribute = true;
            }
        }
        return did_output_attribute;
    }

    void msl_output_struct(const Ast* ast, const AstNode& node, std::ostream& stream)
    {
        assert(node.node_type == AstNodeType::Struct);

        stream << "struct " << node.struct_declaration.name << " {\n";
        for (uint32_t child_node_index : node.struct_declaration.member_nodes)
        {
            const AstNode& child_node = ast->nodes[child_node_index];
            stream << get_indent(1);
            msl_output_attributes(child_node, stream);
            stream << get_msl_type_name(child_node.struct_member.type_descriptor) << ' ' << child_node.struct_member.name << ";\n";
        }
        stream << "};\n";
    }

    void wgsl_output_struct(const Ast* ast, const AstNode& node, std::ostream& stream)
    {
        assert(node.node_type == AstNodeType::Struct);

        // TODO: Only the structs passed as vertex outputs or fragment inputs need the location attribute
        uint32_t location_index = 0;
        stream << "struct " << node.struct_declaration.name << " {\n";
        for (uint32_t child_node_index : node.struct_declaration.member_nodes)
        {
            const AstNode& child_node = ast->nodes[child_node_index];
            stream << get_indent(1);
            wgsl_output_attributes(child_node, stream);
            if (!has_attribute(&child_node, "position"))
            {
                stream << "@location(" << (location_index++) << ") ";
            }
            stream << child_node.struct_member.name << ": " << get_wgsl_type_name(ast, child_node.struct_member.type_descriptor) << ",\n";
        }
        stream << "};\n";
    }

    bool uniform_group_has_primitive_members(const Ast* ast, const AstNode& node)
    {
        assert(node.node_type == AstNodeType::UniformGroup);

        for (uint32_t member_node_index : node.struct_declaration.member_nodes)
        {
            const AstNode& member_node = ast->nodes[member_node_index];
            if (!member_node.struct_member.is_texture && !member_node.struct_member.is_sampler)
            {
                return true;
            }
        }
        return false;
    }

    void msl_output_uniform_group(const Ast* ast, const AstNode& node, std::ostream& stream)
    {
        assert(node.node_type == AstNodeType::UniformGroup);

        if (uniform_group_has_primitive_members(ast, node))
        {
            stream << "device struct {\n";
            for (uint32_t member_node_index : node.struct_declaration.member_nodes)
            {
                const AstNode& member_node = ast->nodes[member_node_index];
                if (member_node.struct_member.is_texture || member_node.struct_member.is_sampler)
                {
                    continue;
                }

                stream << get_indent(1);
                stream << get_msl_type_name(member_node.struct_member.type_descriptor) << ' ' << member_node.struct_member.name << ";\n";
            }
            stream << "}& constant " << node.struct_declaration.name;
            stream << " [[buffer(" << node.struct_declaration.binding << ")]];\n";
        }

        for (uint32_t member_node_index : node.struct_declaration.member_nodes)
        {
            const AstNode& member_node = ast->nodes[member_node_index];

            if (!member_node.struct_member.is_texture && !member_node.struct_member.is_sampler)
            {
                continue;
            }
            assert(!member_node.struct_member.global_name.empty());

            stream << get_msl_type_name(member_node.struct_member.type_descriptor) << " constant " << member_node.struct_member.global_name;
            if (member_node.struct_member.is_texture) stream << " [[texture(";
            if (member_node.struct_member.is_sampler) stream << " [[sampler(";
            stream << member_node.struct_member.binding << ")]];\n";
        }
    }

    void wgsl_output_uniform_group(const Ast* ast, const AstNode& node, std::ostream& stream)
    {
        assert(node.node_type == AstNodeType::UniformGroup);

        if (uniform_group_has_primitive_members(ast, node))
        {
            stream << "struct _" << node.struct_declaration.name << " {\n";
            for (uint32_t child_node_index : node.struct_declaration.member_nodes)
            {
                const AstNode& member_node = ast->nodes[child_node_index];
                if (member_node.struct_member.is_texture || member_node.struct_member.is_sampler)
                {
                    continue;
                }

                wgsl_output_attributes(member_node, stream);
                stream << get_indent(1) << member_node.struct_member.name << ": " << get_wgsl_type_name(ast, member_node.struct_member.type_descriptor) << ",\n";
            }
            stream << "};\n@group(" << node.struct_declaration.binding << ") @binding(0) var<uniform> ";
            stream << node.struct_declaration.name << ": _" << node.struct_declaration.name << ";\n";
        }

        for (uint32_t child_node_index : node.struct_declaration.member_nodes)
        {
            const AstNode& member_node = ast->nodes[child_node_index];
            if (!member_node.struct_member.is_texture && !member_node.struct_member.is_sampler)
            {
                continue;
            }
            assert(!member_node.struct_member.global_name.empty());

            stream << "@group(" << node.struct_declaration.binding << ") @binding(" << member_node.struct_member.binding;
            stream << ") var " << member_node.struct_member.global_name << ": ";
            stream << get_wgsl_type_name(ast, member_node.struct_member.type_descriptor) << ";\n";
        }
    }

    void msl_output_vertex_group(const Ast* ast, const AstNode& node, std::ostream& stream)
    {
        assert(node.node_type == AstNodeType::VertexGroup);

        stream << "struct " << node.struct_declaration.name << " {\n";
        int attribute = 0;
        for (uint32_t member_node_index : node.struct_declaration.member_nodes)
        {
            const AstNode& member_node = ast->nodes[member_node_index];

            stream << get_indent(1);
            stream << get_msl_type_name(member_node.struct_member.type_descriptor) << ' ' << member_node.struct_member.name;
            stream << " [[attribute(" << attribute << ")]];\n";
            attribute++;
        }
        stream << "};\n";
    }

    void wgsl_output_vertex_group(const Ast* ast, const AstNode& node, std::ostream& stream)
    {
        assert(node.node_type == AstNodeType::VertexGroup);

        stream << "struct " << node.struct_declaration.name << " {\n";

        uint32_t member_location = 0;
        for (uint32_t child_node_index : node.struct_declaration.member_nodes)
        {
            const AstNode& child_node = ast->nodes[child_node_index];
            wgsl_output_attributes(child_node, stream);
            stream << get_indent(1) << "@location(" << member_location << ") ";
            stream << child_node.struct_member.name << ": " << get_wgsl_type_name(ast, child_node.struct_member.type_descriptor) << ",\n";
            member_location++;
        }
        stream << "};\n";
    }

    void output_expression(const Ast* ast, const AstNode& node, std::ostream& stream, Backend backend);

    void output_property_access(const Ast* ast, const AstNode& node, std::ostream& stream, Backend backend)
    {
        if (node.node_type == AstNodeType::Variable)
        {
            stream << node.variable.name;
            return;
        }
        if (node.node_type == AstNodeType::PropertyAccess)
        {
            if (!node.property_access.global_name.empty())
            {
                stream << node.property_access.global_name;
                return;
            }

            output_property_access(ast, ast->nodes[node.property_access.left], stream, backend);
            stream << '.' << node.property_access.name;
            return;
        }
        output_expression(ast, node, stream, backend);
    }

    void output_function_call(const Ast* ast, const AstNode& node, std::ostream& stream, Backend backend)
    {
        if (backend == Backend::MSL)
        {
            if (node.function_call.name == "texture_sample")
            {
                assert(node.function_call.argument_nodes.size >= 1);

                const AstNode& texture_node = ast->nodes[node.function_call.argument_nodes[0]];
                output_expression(ast, texture_node, stream, backend);
                stream << ".sample(";

                bool is_first_argument = true;
                for (size_t i = 1; i < node.function_call.argument_nodes.size; i++)
                {
                    if (!is_first_argument) stream << ", ";
                    is_first_argument = false;

                    uint32_t argument_node_index = node.function_call.argument_nodes[i];
                    output_expression(ast, ast->nodes[argument_node_index], stream, backend);
                }
                stream << ')';

                return;
            }
            else
            {
                stream << node.function_call.name;
            }
        }
        else
        {
            stream << get_wgsl_function_name(ast, node.function_call.name);
        }
        stream << '(';
        bool is_first_argument = true;
        for (uint32_t argument_node_index : node.function_call.argument_nodes)
        {
            if (!is_first_argument) stream << ", ";
            is_first_argument = false;
            output_expression(ast, ast->nodes[argument_node_index], stream, backend);
        }
        stream << ')';
    }

    void output_expression(const Ast* ast, const AstNode& node, std::ostream& stream, Backend backend)
    {
        for (int i = 0; i < node.num_parenthesis; i++)
        {
            stream << '(';
        }
        switch (node.node_type)
        {
            case AstNodeType::ArrayIndex:
                output_expression(ast, ast->nodes[node.array_index.left], stream, backend);
                stream << '[';
                output_expression(ast, ast->nodes[node.array_index.right], stream, backend);
                stream << ']';
                break;
            case AstNodeType::NumericLiteral:
                stream << node.numeric_literal.str;
                break;
            case AstNodeType::BooleanLiteral:
                stream << (node.boolean_literal.value ? "true" : "false");
                break;
            case AstNodeType::UnaryOperation:
                stream << lexer::operator_to_string(node.unary_operation.operation);
                output_expression(ast, ast->nodes[node.unary_operation.operand], stream, backend);
                break;
            case AstNodeType::BinaryOperation:
                output_expression(ast, ast->nodes[node.binary_operation.left], stream, backend);
                stream << ' ' << operator_to_string(node.binary_operation.operation) << ' ';
                output_expression(ast, ast->nodes[node.binary_operation.right], stream, backend);
                break;
            case AstNodeType::Variable:
            case AstNodeType::PropertyAccess:
                output_property_access(ast, node, stream, backend);
                break;
            case AstNodeType::FunctionCall:
                output_function_call(ast, node, stream, backend);
                break;
            default: assert(false);
        }
        for (int i = 0; i < node.num_parenthesis; i++)
        {
            stream << ')';
        }
    }

    void msl_output_variable_declaration(const Ast* ast, const AstNode& node, std::ostream& stream, int indent)
    {
        assert(node.node_type == AstNodeType::VariableDeclaration);

        stream << get_indent(indent);

        if (node.variable_declaration.is_top_level) stream << "constant ";
        if (node.variable_declaration.is_const) stream << "const ";

        stream << get_msl_type_name(node.variable_declaration.type_descriptor) << ' ' << node.variable_declaration.name;

        if (node.variable_declaration.is_array_declaration)
        {
            stream << '[' << node.variable_declaration.array_size_str << "] = {\n";

            bool is_first_expression = true;
            for (uint32_t expression_node_index : node.variable_declaration.array_expressions)
            {
                if (!is_first_expression) stream << ",\n";
                is_first_expression = false;

                stream << get_indent(indent + 1);
                output_expression(ast, ast->nodes[expression_node_index], stream, Backend::MSL);
            }

            stream << '\n' << get_indent(indent) << '}';
            return;
        }

        if (node.variable_declaration.expression != 0)
        {
            stream << " = ";
            output_expression(ast, ast->nodes[node.variable_declaration.expression], stream, Backend::MSL);
            return;
        }
        stream << "{}";
    }

    void wgsl_output_variable_declaration(const Ast* ast, const AstNode& node, std::ostream& stream, int indent)
    {
        assert(node.node_type == AstNodeType::VariableDeclaration);

        stream << get_indent(indent);

        if (node.variable_declaration.is_const)
        {
            stream << "const ";
        }
        else
        {
            stream << "var ";
        }

        if (node.variable_declaration.is_array_declaration)
        {
            stream << node.variable_declaration.name << " = array<" << get_wgsl_type_name(ast, node.variable_declaration.type_descriptor);
            stream << ", " << node.variable_declaration.array_size_str << ">(\n";

            bool is_first_expression = true;
            for (uint32_t expression_node_index : node.variable_declaration.array_expressions)
            {
                if (!is_first_expression) stream << ",\n";
                is_first_expression = false;

                stream << get_indent(indent + 1);
                output_expression(ast, ast->nodes[expression_node_index], stream, Backend::WGSL);
            }

            stream << '\n' << get_indent(indent) << ')';
            return;
        }

        stream << node.variable_declaration.name << ": " << get_wgsl_type_name(ast, node.variable_declaration.type_descriptor);

        if (node.variable_declaration.expression != 0)
        {
            stream << " = ";
            output_expression(ast, ast->nodes[node.variable_declaration.expression], stream, Backend::WGSL);
        }
    }

    void msl_output_variable_assignment(const Ast* ast, const AstNode& node, std::ostream& stream, int indent)
    {
        assert(node.node_type == AstNodeType::VariableAssignment);

        stream << get_indent(indent);
        output_property_access(ast, ast->nodes[node.variable_assignment.variable_node], stream, Backend::MSL);

        if (node.variable_assignment.expression == 0)
        {
            // Increment/decrement operations
            stream << operator_to_string(node.variable_assignment.operation);
            return;
        }

        stream << ' ' << operator_to_string(node.variable_assignment.operation) << ' ';
        output_expression(ast, ast->nodes[node.variable_assignment.expression], stream, Backend::MSL);
    }

    void wgsl_output_variable_assignment(const Ast* ast, const AstNode& node, std::ostream& stream, int indent)
    {
        assert(node.node_type == AstNodeType::VariableAssignment);

        stream << get_indent(indent);
        output_property_access(ast, ast->nodes[node.variable_assignment.variable_node], stream, Backend::WGSL);

        if (node.variable_assignment.expression == 0)
        {
            // Increment/decrement operations
            stream << operator_to_string(node.variable_assignment.operation);
            return;
        }

        stream << ' ' << operator_to_string(node.variable_assignment.operation) << ' ';
        output_expression(ast, ast->nodes[node.variable_assignment.expression], stream, Backend::WGSL);
    }


    void output_function_body(const Ast* ast, const AstNode& node, std::ostream& stream, int indent, Backend backend);

    void output_if_statement(const Ast* ast, const AstNode& node, std::ostream& stream, int indent, Backend backend)
    {
        stream << get_indent(indent);
        switch (node.node_type)
        {
            case AstNodeType::IfStatement:     stream << "if "; break;
            case AstNodeType::ElseIfStatement: stream << "else if "; break;
            case AstNodeType::ElseStatement:   stream << "else "; break;
            default: assert(false);
        }

        if (node.node_type != AstNodeType::ElseStatement)
        {
            stream << '(';
            output_expression(ast, ast->nodes[node.if_statement.expression], stream, backend);
            stream << ") ";
        }

        output_function_body(ast, ast->nodes[node.if_statement.body], stream, indent, backend);
    }

    void output_for_loop(const Ast* ast, const AstNode& node, std::ostream& stream, int indent, Backend backend)
    {
        assert(node.node_type == AstNodeType::ForLoop);

        stream << get_indent(indent) << "for (";
        backend == Backend::MSL ?
            msl_output_variable_declaration(ast, ast->nodes[node.for_loop.init_expression], stream, 0) :
            wgsl_output_variable_declaration(ast, ast->nodes[node.for_loop.init_expression], stream, 0);
        stream << "; ";

        output_expression(ast, ast->nodes[node.for_loop.comp_expression], stream, backend);
        stream << "; ";

        backend == Backend::MSL ?
            msl_output_variable_assignment(ast, ast->nodes[node.for_loop.loop_expression], stream, 0) :
            wgsl_output_variable_assignment(ast, ast->nodes[node.for_loop.loop_expression], stream, 0);
        stream << ") ";

        output_function_body(ast, ast->nodes[node.for_loop.body], stream, indent, backend);
    }

    void output_while_loop(const Ast* ast, const AstNode& node, std::ostream& stream, int indent, Backend backend)
    {
        assert(node.node_type == AstNodeType::WhileLoop);

        stream << get_indent(indent) << "while (";
        output_expression(ast, ast->nodes[node.while_loop.expression], stream, backend);
        stream << ") ";
        output_function_body(ast, ast->nodes[node.while_loop.body], stream, indent, backend);
    }

    void output_return_statement(const Ast* ast, const AstNode& node, std::ostream& stream, int indent, Backend backend)
    {
        assert(node.node_type == AstNodeType::ReturnStatement);

        stream << get_indent(indent) << "return ";
        output_expression(ast, ast->nodes[node.return_statement.expression], stream, backend);
    }

    void msl_output_keyword_statement(const AstNode& node, std::ostream& stream, int indent)
    {
        assert(node.node_type == AstNodeType::KeywordStatement);

        stream << get_indent(indent);
        if (node.statement.str == "discard")
        {
            stream << "discard_fragment()";
            return;
        }
        stream << node.statement.str;
    }

    void wgsl_output_keyword_statement(const AstNode& node, std::ostream& stream, int indent)
    {
        assert(node.node_type == AstNodeType::KeywordStatement);
        stream << get_indent(indent) << node.statement.str;
    }

    void output_function_body_statement(const Ast* ast, const AstNode& node, std::ostream& stream, int indent, Backend backend)
    {
        switch (node.node_type)
        {
            case AstNodeType::FunctionBody:
                stream << get_indent(indent);
                output_function_body(ast, node, stream, indent, backend);
                break;
            case AstNodeType::VariableDeclaration:
                backend == Backend::MSL ?
                    msl_output_variable_declaration(ast, node, stream, indent) :
                    wgsl_output_variable_declaration(ast, node, stream, indent);
                stream << ';';
                break;
            case AstNodeType::VariableAssignment:
                backend == Backend::MSL ?
                    msl_output_variable_assignment(ast, node, stream, indent) :
                    wgsl_output_variable_assignment(ast, node, stream, indent);
                stream << ';';
                break;
            case AstNodeType::IfStatement:
            case AstNodeType::ElseIfStatement:
            case AstNodeType::ElseStatement:
                output_if_statement(ast, node, stream, indent, backend);
                break;
            case AstNodeType::ForLoop:
                output_for_loop(ast, node, stream, indent, backend);
                break;
            case AstNodeType::WhileLoop:
                output_while_loop(ast, node, stream, indent, backend);
                break;
            case AstNodeType::ReturnStatement:
                output_return_statement(ast, node, stream, indent, backend);
                stream << ';';
                break;
            case AstNodeType::KeywordStatement:
                backend == Backend::MSL ?
                    msl_output_keyword_statement(node, stream, indent) :
                    wgsl_output_keyword_statement(node, stream, indent);
                stream << ';';
                break;
            default:
                assert(false && "Invalid AstNodeType");
        }
        stream << '\n';
    }

    void output_function_body(const Ast* ast, const AstNode& node, std::ostream& stream, int indent, Backend backend)
    {
        assert(node.node_type == AstNodeType::FunctionBody);

        stream << "{\n";
        for (uint32_t statement_node_index : node.function_body.statements)
        {
            output_function_body_statement(ast, ast->nodes[statement_node_index], stream, indent + 1, backend);
        }
        stream << get_indent(indent) << '}';
    }

    void msl_output_function_declaration(const Ast* ast, const AstNode& node, std::ostream& stream)
    {
        assert(node.node_type == AstNodeType::FunctionDeclaration);
        if (msl_output_attributes(node, stream))
        {
            stream << '\n';
        }
        stream << get_msl_type_name(node.function_declaration.return_type_descriptor) << ' ' << node.function_declaration.name << "(";

        bool is_first_argument = true;
        for (uint32_t argument_node_index : node.function_declaration.argument_nodes)
        {
            if (!is_first_argument) stream << ", ";
            is_first_argument = false;

            const AstNode& argument_node = ast->nodes[argument_node_index];
            msl_output_attributes(argument_node, stream);

            bool is_entry_point = has_attribute(&node, "vertex") || has_attribute(&node, "fragment");
            if (is_entry_point && argument_node.attributes.size == 0)
            {
                stream << "[[stage_in]] ";
            }
            stream << get_msl_type_name(argument_node.function_argument.type_descriptor) << ' ' << argument_node.function_argument.name;
        }

        stream << ") ";
        output_function_body(ast, ast->nodes[node.function_declaration.body], stream, 0, Backend::MSL);
        stream << '\n';
    }

    void wgsl_output_function_declaration(const Ast* ast, const AstNode& node, std::ostream& stream)
    {
        assert(node.node_type == AstNodeType::FunctionDeclaration);
        if (wgsl_output_attributes(node, stream))
        {
            stream << '\n';
        }
        stream << "fn " << node.function_declaration.name << '(';

        bool is_first_argument = true;
        for (uint32_t argument_node_index : node.function_declaration.argument_nodes)
        {
            if (!is_first_argument) stream << ", ";
            is_first_argument = false;

            const AstNode& argument_node = ast->nodes[argument_node_index];
            wgsl_output_attributes(argument_node, stream);
            stream << argument_node.function_argument.name << ": " << get_wgsl_type_name(ast, argument_node.function_argument.type_descriptor);
        }

        stream << ") -> ";
        // TODO: Be more flexible with fragment function return type
        if (has_attribute(&node, "fragment"))
        {
            stream << "@location(0) ";
        }
        stream << get_wgsl_type_name(ast, node.function_declaration.return_type_descriptor) << ' ';
        output_function_body(ast, ast->nodes[node.function_declaration.body], stream, 0, Backend::WGSL);
        stream << '\n';
    }

    void output_root_node(const Ast* ast, const AstNode& node, std::ostream& stream, Backend backend)
    {
        switch (node.node_type)
        {
            case AstNodeType::Struct:
                backend == Backend::MSL ?
                    msl_output_struct(ast, node, stream) :
                    wgsl_output_struct(ast, node, stream);
                break;
            case AstNodeType::UniformGroup:
                backend == Backend::MSL ?
                    msl_output_uniform_group(ast, node, stream) :
                    wgsl_output_uniform_group(ast, node, stream);
                break;
            case AstNodeType::VertexGroup:
                backend == Backend::MSL ?
                    msl_output_vertex_group(ast, node, stream) :
                    wgsl_output_vertex_group(ast, node, stream);
                break;
            case AstNodeType::FunctionDeclaration:
                backend == Backend::MSL ?
                    msl_output_function_declaration(ast, node, stream) :
                    wgsl_output_function_declaration(ast, node, stream);
                break;
            case AstNodeType::VariableDeclaration:
                backend == Backend::MSL ?
                    msl_output_variable_declaration(ast, node, stream, 0) :
                    wgsl_output_variable_declaration(ast, node, stream, 0);
                stream << ";\n";
                break;
            default:
                assert(false && "Invalid AstNodeType");
        }
    }

    void to_msl(const Ast* ast, std::ostream& stream)
    {
        stream << "#include <metal_stdlib>\n\nusing namespace metal;\n";

        for (uint32_t root_node_index : ast->root_nodes)
        {
            stream << '\n';
            output_root_node(ast, ast->nodes[root_node_index], stream, Backend::MSL);
        }
    }

    void to_wgsl(const Ast* ast, std::ostream& stream)
    {
        bool is_first_node = true;
        for (uint32_t root_node_index : ast->root_nodes)
        {
            if (!is_first_node) stream << '\n';
            is_first_node = false;
            output_root_node(ast, ast->nodes[root_node_index], stream, Backend::WGSL);
        }
    }

    void assign_bindings(Ast* ast, Backend backend)
    {
        uint32_t next_uniform_group_binding = 0;
        uint32_t next_texture_binding = 0;
        uint32_t next_sampler_binding = 0;
        // NOTE: This loop takes advantage of the fact that Ast nodes are consistently sorted in the node list;
        //       the uniform group parent nodes will always appear before their respective child nodes.
        for (AstNode& node : ast->nodes)
        {
            if (node.node_type == AstNodeType::UniformGroup)
            {
                node.struct_declaration.binding = next_uniform_group_binding++;
                if (backend == Backend::WGSL)
                {
                    next_texture_binding = 0;
                    next_sampler_binding = 0;
                }
            }
            if (node.node_type == AstNodeType::UniformGroupMember)
            {
                next_texture_binding += node.struct_member.is_texture;
                next_sampler_binding += node.struct_member.is_sampler;
                if (backend == Backend::WGSL && (node.struct_member.is_texture || node.struct_member.is_sampler))
                {
                    node.struct_member.binding = next_texture_binding + next_sampler_binding;
                }
                if (backend == Backend::MSL && node.struct_member.is_texture)
                {
                    node.struct_member.binding = next_texture_binding - 1;
                }
                if (backend == Backend::MSL && node.struct_member.is_sampler)
                {
                    node.struct_member.binding = next_sampler_binding - 1;
                }
            }
        }
    }

    void msl_assign_bindings(Ast* ast)
    {
        assign_bindings(ast, Backend::MSL);
    }

    void wgsl_assign_bindings(Ast* ast)
    {
        assign_bindings(ast, Backend::WGSL);
    }
}
