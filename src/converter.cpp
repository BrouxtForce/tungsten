#include "converter.hpp"
#include "parser.hpp"

#include <cstring>
#include <sstream>
#include <iostream>
#include <functional>
#include <cassert>
#include <algorithm>

namespace tungsten::converter
{
    using namespace parser;

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

    void iterate_node_children(const Ast* ast, const AstNode* node, std::function<bool(const AstNode*)> callback)
    {
        for (uint16_t i = node->child_offset; i < node->child_offset + node->num_children; i++)
        {
            const AstNode* child_node = &ast->child_nodes[i];
            if (!callback(child_node))
            {
                return;
            }
            i += child_node->num_children;
        }
    }

    bool output_attributes(const std::vector<Attribute>& attributes, std::ostream& stream)
    {
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

        stream << get_indent(indent);
        if (output_attributes(node->attributes, stream))
        {
            stream << '\n';
        }
        stream << "struct " << node->name << " {\n";
        iterate_node_children(ast, node, [&indent, &stream](const AstNode* child_node) {
            assert(child_node->node_type == AstNodeType::StructMember);

            stream << get_indent(indent + 1);
            if (output_attributes(child_node->attributes, stream))
            {
                stream << ' ';
            }
            stream << child_node->type << ' ' << child_node->name << ";\n";
            return true;
        });
        stream << "};\n\n";
    }

    void output_function(const Ast* ast, const AstNode* node, std::ostream& stream, int indent)
    {
        assert(node->node_type == AstNodeType::Function);

        stream << get_indent(indent);
        if (output_attributes(node->attributes, stream))
        {
            stream << '\n';
        }
        stream << node->type << ' ' << node->name << '(';

        bool is_first_child = true;
        iterate_node_children(ast, node, [&stream, &is_first_child](const AstNode* child_node) {
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
                stream << child_node->type << ' ' << child_node->name;
                is_first_child = false;
                return true;
            }
            return false;
        });

        stream << ") {\n";

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

        stream << node->name << '(';
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
        iterate_node_children(ast, node, [&ast, &stream, &needs_spacing](const AstNode* child_node) {
            if (needs_spacing)
            {
                stream << ' ';
            }
            switch (child_node->node_type)
            {
                case AstNodeType::NumericLiteral:
                    stream << child_node->num_str;
                    needs_spacing = true;
                    break;
                case AstNodeType::Variable:
                    stream << child_node->name;
                    needs_spacing = true;
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

        if (!is_root_expression)
        {
            stream << ")";
        }
    }

    void output_variable_declaration(const Ast* ast, const AstNode* node, std::ostream& stream, int indent)
    {
        assert(node->node_type == AstNodeType::VariableDeclaration);

        stream << get_indent(indent);
        stream << node->type << ' ' << node->name << " = ";

        assert(node->num_children > 0);
        const AstNode* expression_node = &ast->child_nodes[node->child_offset];

        assert(expression_node->node_type == AstNodeType::Expression);
        output_expression(ast, expression_node, stream, true);

        stream << ";\n";
    }

    void output_variable_assignment(const Ast* ast, const AstNode* node, std::ostream& stream, int indent)
    {
        assert(node->node_type == AstNodeType::VariableAssignment);

        stream << get_indent(indent);

        if (node->operation == "++" || node->operation == "--")
        {
            stream << node->name << node->operation;
        }
        else
        {
            stream << node->name << ' ' << node->operation << ' ';
        }

        iterate_node_children(ast, node, [&ast, &stream](const AstNode* child_node) {
            if (child_node->node_type == AstNodeType::Expression)
            {
                output_expression(ast, child_node, stream, true);
                return false;
            }
            return true;
        });

        stream << ";\n";
    }

    void output_node(const Ast* ast, const AstNode* node, std::ostream& stream, int indent)
    {
        switch (node->node_type)
        {
            case AstNodeType::Struct:
                output_struct(ast, node, stream, indent);
                break;

            case AstNodeType::Function:
                output_function(ast, node, stream, indent);
                break;

            // case AstNodeType::Scope:
            //     break;

            case AstNodeType::VariableDeclaration:
                output_variable_declaration(ast, node, stream, indent);
                break;
            case AstNodeType::VariableAssignment:
                output_variable_assignment(ast, node, stream, indent);
                break;

            // case AstNodeType::IfStatement:
            //     break;
            // case AstNodeType::ElseIfStatement:
            //     break;
            // case AstNodeType::ElseStatement:
            //     break;

            // case AstNodeType::ForLoop:
            //     break;
            // case AstNodeType::WhileLoop:
            //     break;

            // case AstNodeType::ReturnStatement:
            //     break;

            default:
                std::cerr << "unsupported node type " << (int)node->node_type << '\n';
        }
    }

    void to_msl(const Ast* ast, std::ostream& stream)
    {
        for (const AstNode& node : ast->root_nodes)
        {
            output_node(ast, &node, stream, 0);
        }
    }
}
