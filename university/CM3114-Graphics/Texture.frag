#version 330 core

in vec4 color;
in vec2 texCoord;
out vec4 fColor;

uniform sampler2D tex;
void main()
{
    fColor = texture(tex, texCoord);
}