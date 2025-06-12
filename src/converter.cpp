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

    void iterate_node_children(const Ast* ast, const AstNode* node, std::function<bool(const AstNode*)> callback, int num_to_skip = 0)
    {
        for (uint16_t i = node->child_offset; i < node->child_offset + node->num_children; i++)
        {
            const AstNode* child_node = &ast->child_nodes[i];
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
        for (uint16_t i = node->child_offset; i < node->child_offset + node->num_children; i++)
        {
            const AstNode* child_node = &ast->child_nodes[i];
            i += child_node->num_children;

            num_iterations++;
            if (num_iterations == n)
            {
                return child_node;
            }
        }
        return nullptr;
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

    void output_variable_declaration(const Ast* ast, const AstNode* node, std::ostream& stream, int indent, bool output_semicolon = true)
    {
        assert(node->node_type == AstNodeType::VariableDeclaration);

        stream << get_indent(indent);
        stream << node->type << ' ' << node->name;

        if (node->num_children > 0)
        {
            stream << " = ";

            const AstNode* expression_node = &ast->child_nodes[node->child_offset];
            assert(expression_node->node_type == AstNodeType::Expression);
            output_expression(ast, expression_node, stream, true);
        }
        else
        {
            // Uninitialized variable
            stream << "{}";
        }

        if (output_semicolon) stream << ";\n";
    }

    void output_variable_assignment(const Ast* ast, const AstNode* node, std::ostream& stream, int indent, bool output_semicolon = true)
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

    void to_msl(const Ast* ast, std::ostream& stream)
    {
        for (const AstNode& node : ast->root_nodes)
        {
            output_node(ast, &node, stream, 0);
        }
    }
}
