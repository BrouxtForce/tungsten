#include "tungsten/reflection.hpp"

namespace tungsten::reflection
{
    using parser::Ast, parser::AstNode, parser::AstNodeType, parser::has_attribute;

    void add_uniform_group_reflection(const Ast* ast, const AstNode& node, ReflectionInfo& info)
    {
        assert(node.node_type == AstNodeType::UniformGroup);

        UniformGroupReflection& reflection = info.uniform_groups.emplace_back();
        reflection.name = node.struct_declaration.name;
        reflection.binding = node.struct_declaration.binding;

        reflection.members.reserve(node.struct_declaration.member_nodes.size);
        for (uint32_t member_node_index : node.struct_declaration.member_nodes)
        {
            const AstNode& member_node = ast->nodes[member_node_index];
            assert(member_node.node_type == AstNodeType::UniformGroupMember);

            StructMemberReflection& member_reflection = reflection.members.emplace_back();
            member_reflection.type_name = member_node.struct_member.type_descriptor.to_string();
            member_reflection.name = member_node.struct_member.name;
            member_reflection.attributes.assign_range(
                std::ranges::subrange(member_node.attributes.begin(), member_node.attributes.end())
            );
        }
    }

    void add_vertex_group_reflection(const Ast* ast, const AstNode& node, ReflectionInfo& info)
    {
        assert(node.node_type == AstNodeType::VertexGroup);

        VertexGroupReflection& reflection = info.vertex_groups.emplace_back();
        reflection.name = node.struct_declaration.name;
        reflection.attributes.assign_range(
            std::ranges::subrange(node.attributes.begin(), node.attributes.end())
        );

        // TODO: This is basically the same as the uniform group implementation, and even outputs to the same type
        reflection.members.reserve(node.struct_declaration.member_nodes.size);
        for (uint32_t member_node_index : node.struct_declaration.member_nodes)
        {
            const AstNode& member_node = ast->nodes[member_node_index];
            assert(member_node.node_type == AstNodeType::VertexGroupMember);

            StructMemberReflection& member_reflection = reflection.members.emplace_back();
            member_reflection.type_name = member_node.struct_member.type_descriptor.to_string();
            member_reflection.name = member_node.struct_member.name;
            member_reflection.attributes.assign_range(
                std::ranges::subrange(member_node.attributes.begin(), member_node.attributes.end())
            );
        }
    }

    void add_function_reflection(const Ast* ast, const AstNode& node, ReflectionInfo& info)
    {
        assert(node.node_type == AstNodeType::FunctionDeclaration);

        FunctionReflection& reflection = info.functions.emplace_back();
        reflection.is_vertex_function = has_attribute(&node, "vertex");
        reflection.is_fragment_function = has_attribute(&node, "fragment");
        reflection.is_compute_function = has_attribute(&node, "compute");
        reflection.name = node.function_declaration.name;
        reflection.attributes.assign_range(
            std::ranges::subrange(node.attributes.begin(), node.attributes.end())
        );

        for (uint32_t parameter_node_index : node.function_declaration.argument_nodes)
        {
            const AstNode& parameter_node = ast->nodes[parameter_node_index];
            assert(parameter_node.node_type == AstNodeType::FunctionArgument);

            if (parameter_node.attributes.size == 0)
            {
                reflection.vertex_input = parameter_node.function_argument.type_descriptor.to_string();
                break;
            }
        }
    }

    ReflectionInfo get_reflection_info(const parser::Ast* ast)
    {
        ReflectionInfo info;

        for (const parser::AstNode& node : ast->nodes)
        {
            switch (node.node_type)
            {
                case AstNodeType::UniformGroup:
                    add_uniform_group_reflection(ast, node, info);
                    break;
                case AstNodeType::VertexGroup:
                    add_vertex_group_reflection(ast, node, info);
                    break;
                case AstNodeType::FunctionDeclaration:
                    add_function_reflection(ast, node, info);
                    break;
                default: break;
            }
        }

        return info;
    }
}
