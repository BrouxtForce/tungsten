#include "tungsten/converter.hpp"
#include "tungsten/parser.hpp"

#include <cstring>
#include <iostream>
#include <functional>
#include <cassert>
#include <algorithm>

namespace tungsten::converter
{
    using namespace parser;

    static uint32_t language_target = 0;
    static uint32_t next_binding = 0;
    static std::ostream* reflection_info = nullptr;

    void output_node(const Ast* ast, const AstNode* node, std::ostream& stream, int indent);

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

    void iterate_node_children(const Ast* ast, const AstNode* node, std::function<bool(const AstNode*)> callback, int num_to_skip = 0)
    {
        for (uint32_t i = node->index + 1; i <= node->index + node->num_children; i++)
        {
            const AstNode* child_node = &ast->nodes[i];
            if (num_to_skip > 0)
            {
                --num_to_skip;
            }
            else if (!callback(child_node))
            {
                return;
            }
            i += child_node->num_children;
        }
    }

    const AstNode* get_nth_child(const Ast* ast, const AstNode* node, int n)
    {
        int num_iterations = 0;
        for (uint32_t i = node->index + 1; i <= node->index + node->num_children; i++)
        {
            const AstNode* child_node = &ast->nodes[i];
            i += child_node->num_children;

            num_iterations++;
            if (num_iterations == n)
            {
                return child_node;
            }
        }
        return nullptr;
    }

    std::string convert_type_wgsl(std::string_view type)
    {
        // TODO: Support all WGSL types
        std::array<std::string_view, 5> scalar_types { "bool", "half", "float", "uint", "int" };
        std::array<std::string_view, 5> wgsl_scalar_types { "bool", "f16", "f32", "u32", "i32" };
        std::array<std::string_view, 5> scalar_type_suffixes { "b", "h", "f", "u", "i" };
        for (size_t i = 0; i < scalar_types.size(); i++)
        {
            if (!type.starts_with(scalar_types[i]))
            {
                continue;
            }

            std::string_view count = type.substr(scalar_types[i].size());
            if (count.empty())
            {
                return (std::string)wgsl_scalar_types[i];
            }

            if (i == 0)
            {
                // Boolean vector types do not seem to have predeclared aliases
                if (count == "2") return "vec2<bool>";
                if (count == "3") return "vec3<bool>";
                if (count == "4") return "vec4<bool>";
                break;
            }

            if (count == "2") return "vec2" + (std::string)scalar_type_suffixes[i];
            if (count == "3") return "vec3" + (std::string)scalar_type_suffixes[i];
            if (count == "4") return "vec4" + (std::string)scalar_type_suffixes[i];

            // TODO: Exclude int and uint from matrix types
            if (count == "4x4")
            {
                return "mat4x4" + std::string(1, wgsl_scalar_types[i][0]);
            }
            if (count == "3x3")
            {
                return "mat3x3" + std::string(1, wgsl_scalar_types[i][0]);
            }
            break;
        }
        return (std::string)type;
    }

    std::string convert_type(std::string_view type)
    {
        if (language_target == LanguageTargetMSL)
        {
            return (std::string)type;
        }
        if (language_target == LanguageTargetWGSL)
        {
            return convert_type_wgsl(type);
        }
        assert(false);
        return {};
    }

    bool output_wgsl_attribute(const Attribute& attribute, std::ostream& stream)
    {
        // TODO: Have conversions to all WGSL supported attributes
        std::array<std::pair<std::string_view, std::string_view>, 6> attribute_conversions {
            std::pair{ "vertex", "@vertex" },
            std::pair{ "fragment", "@fragment" },
            std::pair{ "compute", "@compute" },

            std::pair{ "vertex_id", "@builtin(vertex_index)" },
            std::pair{ "instance_id", "@builtin(instance_index)" },
            std::pair{ "position", "@builtin(position)" }
        };

        for (const auto& [from, to] : attribute_conversions)
        {
            if (attribute.name == from)
            {
                stream << to;
                return true;
            }
        }
        return false;
    }

    bool output_attributes(const std::vector<Attribute>& attributes, std::ostream& stream)
    {
        if (language_target == LanguageTargetWGSL)
        {
            bool did_output_attribute = false;
            for (const Attribute& attribute : attributes)
            {
                if (output_wgsl_attribute(attribute, stream))
                {
                    did_output_attribute = true;
                }
            }
            return did_output_attribute;
        }

        for (const Attribute& attribute : attributes)
        {
            stream << "[[" << attribute.name;
            if (attribute.arguments.size() > 0)
            {
                stream << '(' << attribute.arguments[0];
                for (size_t i = 1; i < attribute.arguments.size(); i++)
                {
                    stream << ", " << attribute.arguments[i];
                }
                stream << ')';
            }
            stream << "]]";
        }
        return attributes.size() > 0;
    }

    void output_struct(const Ast* ast, const AstNode* node, std::ostream& stream, int indent)
    {
        assert(node->node_type == AstNodeType::Struct);

        bool needs_wgsl_location = false;
        if (language_target == LanguageTargetWGSL)
        {
            // NOTE: Currently, only the [[position]] attribute is used to determine if a struct
            //       is as the output of the vertex function / input of the fragment function.
            iterate_node_children(ast, node, [&needs_wgsl_location](const AstNode* child_node) {
                if (has_attribute(child_node, "position"))
                {
                    needs_wgsl_location = true;
                    return false;
                }
                return true;
            });
        }

        stream << get_indent(indent);
        if (output_attributes(node->attributes, stream))
        {
            stream << '\n';
        }
        stream << "struct " << node->name << " {\n";
        int location = 0;
        iterate_node_children(ast, node, [&needs_wgsl_location, &location, &indent, &stream](const AstNode* child_node) {
            assert(child_node->node_type == AstNodeType::StructMember);

            stream << get_indent(indent + 1);
            if (output_attributes(child_node->attributes, stream))
            {
                stream << ' ';
            }
            if (language_target == LanguageTargetMSL)
            {
                stream << convert_type(child_node->type) << ' ' << child_node->name << ";\n";
            }
            if (language_target == LanguageTargetWGSL)
            {
                bool has_position_attribute = has_attribute(child_node, "position");
                if (needs_wgsl_location && !has_position_attribute)
                {
                    stream << "@location(" << (location++) << ") ";
                }
                stream << child_node->name << ": " << convert_type(child_node->type) << ",\n";
            }
            return true;
        });
        stream << "};\n\n";
    }

    void output_uniform_group(const Ast* ast, const AstNode* node, std::ostream& stream, int indent)
    {
        assert(node->node_type == AstNodeType::UniformGroup);
        // TODO: This should not be an assert
        assert(!node->name.starts_with('_') && "Uniform group names must not start with an underscore");

        if (reflection_info != nullptr) {
            *reflection_info << "uniform_group " << node->name << ' ' << next_binding << " { ";
            bool is_first_child = true;
            iterate_node_children(ast, node, [&is_first_child](const AstNode* child_node) {
                assert(child_node->node_type == AstNodeType::UniformGroupMember);
                if (!is_first_child) *reflection_info << ", ";
                is_first_child = false;
                *reflection_info << child_node->type << ' ' << child_node->name;
                return true;
            });
            *reflection_info << " }\n";
        }

        if (language_target == LanguageTargetMSL)
        {
            // TODO: Use attributes to declare the address space
            constexpr std::string_view address_space = "device";

            stream << get_indent(indent);
            if (output_attributes(node->attributes, stream))
            {
                stream << '\n';
            }
            stream << address_space << " struct {\n";
            iterate_node_children(ast, node, [&indent, &stream](const AstNode* child_node) {
                assert(child_node->node_type == AstNodeType::UniformGroupMember);

                stream << get_indent(indent + 1);
                if (output_attributes(child_node->attributes, stream))
                {
                    stream << ' ';
                }
                stream << convert_type(child_node->type) << ' ' << child_node->name << ";\n";
                return true;
            });
            stream << "}& constant " << node->name << " [[buffer(" << (next_binding++) << ")]];\n\n";
            return;
        }
        if (language_target == LanguageTargetWGSL)
        {
            stream << get_indent(indent) << "struct _" << node->name << "_t {\n";
            // TODO: Output attributes
            iterate_node_children(ast, node, [&indent, &stream](const AstNode* child_node) {
                assert(child_node->node_type == AstNodeType::UniformGroupMember);

                stream << get_indent(indent + 1);
                if (output_attributes(child_node->attributes, stream))
                {
                    stream << ' ';
                }
                stream << child_node->name << ": " << convert_type(child_node->type) << ",\n";

                return true;
            });
            stream << "};\n\n@group(" << (next_binding++) << ") @binding(0) var<uniform> " << node->name << ": _" << node->name << "_t;\n\n";
            return;
        }
        assert(false);
    }

    void output_vertex_group(const Ast* ast, const AstNode* node, std::ostream& stream, int indent)
    {
        assert(node->node_type == AstNodeType::VertexGroup);
        // TODO: This should not be an assert
        assert(!node->name.starts_with('_') && "Vertex group names must not start with an underscore");

        if (reflection_info != nullptr) {
            *reflection_info << "vertex_group " << node->name << " { ";
            bool is_first_child = true;
            iterate_node_children(ast, node, [&is_first_child](const AstNode* child_node) {
                assert(child_node->node_type == AstNodeType::VertexGroupMember);
                if (!is_first_child) *reflection_info << ", ";
                is_first_child = false;
                *reflection_info << child_node->type << ' ' << child_node->name;
                return true;
            });
            *reflection_info << " }\n";
        }

        if (language_target == LanguageTargetMSL)
        {
            stream << get_indent(indent);
            if (output_attributes(node->attributes, stream))
            {
                stream << '\n';
            }
            stream << "struct " << node->name << " {\n";
            int attribute = 0;
            iterate_node_children(ast, node, [&attribute, &indent, &stream](const AstNode* child_node) {
                assert(child_node->node_type == AstNodeType::VertexGroupMember);

                stream << get_indent(indent + 1);
                if (output_attributes(child_node->attributes, stream))
                {
                    stream << ' ';
                }
                stream << convert_type(child_node->type) << ' ' << child_node->name << " [[attribute(" << (attribute++) << ")]];\n";
                return true;
            });
            stream << "};\n\n";
            return;
        }
        if (language_target == LanguageTargetWGSL)
        {
            stream << get_indent(indent) << "struct " << node->name << " {\n";
            int location = 0;
            iterate_node_children(ast, node, [&location, &indent, &stream](const AstNode* child_node) {
                assert(child_node->node_type == AstNodeType::VertexGroupMember);

                stream << get_indent(indent + 1);
                stream << "@location(" << (location++) << ") " << child_node->name << ": " << convert_type(child_node->type) << ",\n";

                return true;
            });
            stream << "};\n\n";
            return;
        }
        assert(false);
    }

    void output_function(const Ast* ast, const AstNode* node, std::ostream& stream, int indent)
    {
        assert(node->node_type == AstNodeType::Function);

        bool is_entry_point = false;
        if (reflection_info)
        {
            if (has_attribute(node, "vertex"))
            {
                *reflection_info << "vertex_function " << node->name << '\n';
                is_entry_point = true;
            }
            if (has_attribute(node, "fragment"))
            {
                *reflection_info << "fragment_function " << node->name << '\n';
                is_entry_point = true;
            }
        }

        stream << get_indent(indent);
        if (output_attributes(node->attributes, stream))
        {
            stream << '\n';
        }
        if (language_target == LanguageTargetMSL)
        {
            stream << convert_type(node->type) << ' ';
        }
        if (language_target == LanguageTargetWGSL)
        {
            stream << "fn ";
        }
        stream << node->name << '(';

        bool is_first_child = true;
        iterate_node_children(ast, node, [&stream, &is_first_child, &is_entry_point](const AstNode* child_node) {
            if (child_node->node_type == AstNodeType::FunctionArg)
            {
                if (!is_first_child)
                {
                    stream << ", ";
                }
                if (output_attributes(child_node->attributes, stream))
                {
                    stream << ' ';
                }
                else if (language_target == LanguageTargetMSL && is_entry_point)
                {
                    // TODO: Validation
                    stream << "[[stage_in]] ";
                }
                if (language_target == LanguageTargetMSL)
                {
                    stream << convert_type(child_node->type) << ' ';
                }
                stream << child_node->name;
                if (language_target == LanguageTargetWGSL)
                {
                    stream << ": " << convert_type(child_node->type);
                }
                is_first_child = false;
                return true;
            }
            return false;
        });

        if (language_target == LanguageTargetMSL)
        {
            stream << ") {\n";
        }
        if (language_target == LanguageTargetWGSL)
        {
            bool is_fragment_shader = has_attribute(node, "fragment");
            stream << ") -> ";
            if (is_fragment_shader)
            {
                stream << "@location(0) ";
            }
            stream << convert_type(node->type) << " {\n";
        }

        iterate_node_children(ast, node, [&ast, &indent, &stream](const AstNode* child_node) {
            if (child_node->node_type == AstNodeType::FunctionArg)
            {
                return true;
            }

            output_node(ast, child_node, stream, indent + 1);

            return true;
        });

        stream << "}\n\n";
    }

    void output_expression(const Ast* ast, const AstNode* node, std::ostream& stream, bool is_root_expression);

    void output_function_call(const Ast* ast, const AstNode* node, std::ostream& stream)
    {
        assert(node->node_type == AstNodeType::FunctionCall);

        // TODO: convert_function_call() that also converts stdlib functions
        stream << convert_type(node->name) << '(';
        bool is_first_argument = true;
        iterate_node_children(ast, node, [&ast, &stream, &is_first_argument](const AstNode* child_node) {
            if (!is_first_argument)
            {
                stream << ", ";
            }
            is_first_argument = false;
            output_expression(ast, child_node, stream, true);
            return true;
        });
        stream << ')';
    }

    void output_expression(const Ast* ast, const AstNode* node, std::ostream& stream, bool is_root_expression)
    {
        if (!is_root_expression)
        {
            stream << '(';
        }

        bool needs_spacing = false;
        uint32_t property_node_index = 0;
        iterate_node_children(ast, node, [&ast, &stream, &needs_spacing, &property_node_index](const AstNode* child_node) {
            if (child_node->node_type == AstNodeType::Property)
            {
                property_node_index = child_node->index;
                return false;
            }
            if (needs_spacing && child_node->node_type != AstNodeType::ArrayIndex)
            {
                stream << ' ';
            }
            switch (child_node->node_type)
            {
                case AstNodeType::ArrayIndex:
                    stream << '[';
                    assert(child_node->index + 1 < ast->nodes.size());
                    output_expression(ast, &ast->nodes[child_node->index + 1], stream, true);
                    stream << ']';
                    break;
                case AstNodeType::NumericLiteral:
                    stream << child_node->num_str;
                    needs_spacing = true;
                    break;
                case AstNodeType::Variable:
                    stream << child_node->name;
                    needs_spacing = true;
                    for (size_t offset = 0; offset < child_node->num_children; offset++)
                    {
                        const AstNode* property_node = &ast->nodes[child_node->index + offset + 1];
                        assert(property_node->node_type == AstNodeType::Property);
                        stream << '.' << property_node->name;
                    }
                    break;
                case AstNodeType::FunctionCall:
                    output_function_call(ast, child_node, stream);
                    needs_spacing = true;
                    break;
                case AstNodeType::BinaryOperation:
                    stream << child_node->operation;
                    needs_spacing = true;
                    break;
                case AstNodeType::UnaryOperation:
                    stream << child_node->operation;
                    needs_spacing = false;
                    break;
                case AstNodeType::Expression:
                    output_expression(ast, child_node, stream, false);
                    needs_spacing = true;
                    break;
                default:
                    std::cerr << "Invalid node type " << (int)child_node->node_type << " in output_expression()\n";
            }

            return true;
        });

        if (property_node_index != 0)
        {
            for (uint32_t i = property_node_index; i <= node->index + node->num_children; i++)
            {
                const AstNode* property_node = &ast->nodes[i];
                assert(property_node->node_type == AstNodeType::Property);

                stream << '.' << property_node->name;
            }
        }

        if (!is_root_expression)
        {
            stream << ")";
        }
    }

    void output_variable_declaration(const Ast* ast, const AstNode* node, std::ostream& stream, int indent, bool output_semicolon = true)
    {
        assert(node->node_type == AstNodeType::VariableDeclaration);

        bool is_variable_const = has_attribute(node, "const");
        bool is_top_level = has_attribute(node, "top_level");
        bool is_array = has_attribute(node, "array_size");
        std::string_view array_count = is_array ? get_attribute(node, "array_size") : "";

        stream << get_indent(indent);
        if (language_target == LanguageTargetMSL)
        {
            if (is_top_level) stream << "constant ";
            // TODO: constexpr is not outputted due to many of MSL's stdlib functions not being constexpr.
            //       Perhaps such expressions can be computed at compile time.
            if (is_variable_const) stream << "const ";
            stream << convert_type(node->type) << ' ' << node->name;
            if (is_array) stream << '[' << array_count << ']';
        }
        if (language_target == LanguageTargetWGSL)
        {
            stream << (is_variable_const ? "const " : "var ") << node->name;
            if (!is_array)
            {
                stream << ": " << convert_type(node->type);
            }
        }

        if (node->num_children > 0)
        {
            if (is_array)
            {
                if (language_target == LanguageTargetWGSL)
                {
                    stream << " = array<" << convert_type(node->type) << ", " << array_count << ">(";
                }
                if (language_target == LanguageTargetMSL)
                {
                    stream << " {";
                }

                bool is_first_child = true;
                iterate_node_children(ast, node, [&ast, &stream, &is_first_child, &indent](const AstNode* child_node) {
                    assert(child_node->node_type == AstNodeType::Expression);
                    if (!is_first_child) stream << ',';
                    is_first_child = false;
                    stream << '\n' << get_indent(indent + 1);
                    output_expression(ast, child_node, stream, true);
                    return true;
                });

                if (language_target == LanguageTargetWGSL)
                {
                    stream << "\n)";
                }
                if (language_target == LanguageTargetMSL)
                {
                    stream << "\n}";
                }
            }
            else
            {
                stream << " = ";

                const AstNode* expression_node = get_nth_child(ast, node, 1);
                assert(expression_node->node_type == AstNodeType::Expression);
                output_expression(ast, expression_node, stream, true);
            }
        }
        else
        {
            if (language_target == LanguageTargetMSL)
            {
                // Uninitialized variable
                stream << "{}";
            }
        }

        if (output_semicolon)
        {
            stream << ";\n";
            if (is_top_level) stream << '\n';
        }
    }

    void output_variable_assignment(const Ast* ast, const AstNode* node, std::ostream& stream, int indent, bool output_semicolon = true)
    {
        assert(node->node_type == AstNodeType::VariableAssignment);

        stream << get_indent(indent);

        stream << node->name;
        for (size_t i = node->index + 1; i < ast->nodes.size(); i++)
        {
            if (ast->nodes[i].node_type != AstNodeType::Property)
            {
                break;
            }
            stream << '.' << ast->nodes[i].name;
        }

        if (node->operation == "++" || node->operation == "--")
        {
            stream << node->operation;
        }
        else
        {
            stream << ' ' << node->operation << ' ';
        }

        iterate_node_children(ast, node, [&ast, &stream](const AstNode* child_node) {
            if (child_node->node_type == AstNodeType::Expression)
            {
                output_expression(ast, child_node, stream, true);
                return false;
            }
            return true;
        });

        if (output_semicolon) stream << ";\n";
    }

    void output_scope(const Ast* ast, const AstNode* node, std::ostream& stream, int indent)
    {
        stream << get_indent(indent) << "{\n";
        iterate_node_children(ast, node, [&ast, &stream, &indent](const AstNode* child_node) {
            output_node(ast, child_node, stream, indent + 1);
            return true;
        });
        stream << get_indent(indent) << "}\n";
    }

    void output_if_else_for_while(const Ast* ast, const AstNode* node, std::ostream& stream, int indent)
    {
        assert(node->node_type == AstNodeType::IfStatement || node->node_type == AstNodeType::ElseIfStatement ||
               node->node_type == AstNodeType::ElseStatement || node->node_type == AstNodeType::ForLoop ||
               node->node_type == AstNodeType::WhileLoop);

        stream << get_indent(indent);
        switch (node->node_type)
        {
            case AstNodeType::IfStatement:     stream << "if"; break;
            case AstNodeType::ElseIfStatement: stream << "else if"; break;
            case AstNodeType::ElseStatement:   stream << "else"; break;
            case AstNodeType::ForLoop:         stream << "for"; break;
            case AstNodeType::WhileLoop:       stream << "while"; break;
            default: assert(false);
        }

        if (node->node_type == AstNodeType::ForLoop)
        {
            stream << " (";

            const AstNode* initialization_node = get_nth_child(ast, node, 1);
            const AstNode* expression_node     = get_nth_child(ast, node, 2);
            const AstNode* update_node         = get_nth_child(ast, node, 3);

            assert(initialization_node != nullptr && expression_node != nullptr && update_node != nullptr);

            output_variable_declaration(ast, initialization_node, stream, 0, false);
            stream << "; ";
            output_expression(ast, expression_node, stream, true);
            stream << "; ";
            output_variable_assignment(ast, update_node, stream, 0, false);

            stream << ')';
        }
        else if (node->node_type != AstNodeType::ElseStatement)
        {
            stream << " (";
            const AstNode* expression_node = get_nth_child(ast, node, 1);
            assert(expression_node != nullptr);
            output_expression(ast, expression_node, stream, true);
            stream << ')';
        }

        stream << " {\n";
        iterate_node_children(ast, node, [&ast, &stream, &indent](const AstNode* child_node) {
            output_node(ast, child_node, stream, indent + 1);
            return true;
        }, node->node_type == AstNodeType::ForLoop ? 3 : 1);

        stream << get_indent(indent) << "}\n";
    }

    void output_return_statement(const Ast* ast, const AstNode* node, std::ostream& stream, int indent)
    {
        assert(node->node_type == AstNodeType::ReturnStatement);

        stream << get_indent(indent) << "return ";

        const AstNode* expression_node = get_nth_child(ast, node, 1);
        assert(expression_node != nullptr);
        output_expression(ast, expression_node, stream, true);

        stream << ";\n";
    }

    void output_node(const Ast* ast, const AstNode* node, std::ostream& stream, int indent)
    {
        switch (node->node_type)
        {
            case AstNodeType::Struct:
                output_struct(ast, node, stream, indent);
                break;
            case AstNodeType::UniformGroup:
                output_uniform_group(ast, node, stream, indent);
                break;
            case AstNodeType::VertexGroup:
                output_vertex_group(ast, node, stream, indent);
                break;

            case AstNodeType::Function:
                output_function(ast, node, stream, indent);
                break;

            case AstNodeType::Scope:
                output_scope(ast, node, stream, indent);
                break;

            case AstNodeType::VariableDeclaration:
                output_variable_declaration(ast, node, stream, indent);
                break;
            case AstNodeType::VariableAssignment:
                output_variable_assignment(ast, node, stream, indent);
                break;

            case AstNodeType::IfStatement:
            case AstNodeType::ElseIfStatement:
            case AstNodeType::ElseStatement:
            case AstNodeType::ForLoop:
            case AstNodeType::WhileLoop:
                output_if_else_for_while(ast, node, stream, indent);
                break;

            case AstNodeType::ReturnStatement:
                output_return_statement(ast, node, stream, indent);
                break;

            default:
                std::cerr << "unsupported node type " << (int)node->node_type << '\n';
        }
    }

    void convert(const Ast* ast, std::ostream& stream, LanguageTarget output_target, std::ostream* reflection_stream)
    {
        assert(output_target == LanguageTargetMSL || output_target == LanguageTargetWGSL);
        language_target = output_target;
        next_binding = 0;
        reflection_info = reflection_stream;

        if (language_target == LanguageTargetMSL)
        {
            stream << "#include <metal_stdlib>\n\nusing namespace metal;\n\n";
        }

        for (uint32_t i = 0; i < ast->nodes.size(); i++)
        {
            const AstNode* child_node = &ast->nodes[i];
            output_node(ast, child_node, stream, 0);
            i += child_node->num_children;
        }

        if (next_binding > 4)
        {
            std::cerr << "Warning: WGSL implementations have a default max bind group count of 4, and this shader exceeds this limit.\n";
        }
    }
}
