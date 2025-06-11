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

        stream << "\n}\n\n";
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

            // case AstNodeType::VariableDeclaration:
            //     break;
            // case AstNodeType::VariableAssignment:
            //     break;

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
