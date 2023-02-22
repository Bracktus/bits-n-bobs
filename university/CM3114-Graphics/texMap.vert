#version 330 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec3 vNormal;
layout(location = 2) in vec2 vTexture;

out vec3 position;
out vec3 normal;
out vec2 textureCoord;

uniform mat4 ModelView;
uniform mat4 NormalTransform;
uniform mat4 Projection;

void main()
{
    // Transform vertex position into eye coordinates
    position = (ModelView * vPosition).xyz;

    // Transform vertex normal into eye coordinates
    normal = normalize(mat3(NormalTransform) * vNormal);

    // Vertex position after projection
    gl_Position = Projection * ModelView * vPosition;

    // Pass in texture coordinates
    textureCoord = vTexture;
}
