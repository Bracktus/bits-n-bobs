#version 330 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec3 vNormal;

out vec3 position;
out vec3 normal;
out vec4 color;

uniform mat4 ModelView;
uniform mat4 NormalTransform;
uniform mat4 Projection;
uniform vec4 LightPosition;
uniform vec4 AmbientProduct, DiffuseProduct, SpecularProduct;
uniform float Shininess;


void main()
{
    vec3 ecPosition = (ModelView * vPosition).xyz;
    vec3 L = normalize(LightPosition.xyz - ecPosition);
    vec3 E = normalize(-ecPosition);
    vec3 H = normalize(L + E);
    vec3 N = normalize((NormalTransform * vec4(vNormal, 0)).xyz);

    vec4 ambient = AmbientProduct;

    float Kd = max(dot(L,N), 0.0);
    vec4 diffuse = Kd * DiffuseProduct;

    float Ks = pow(max(dot(N,H), 0.0), Shininess);
    vec4 specular = Ks * SpecularProduct;

    if (dot(L,N) < 0.0) {
        specular = vec4(0.0, 0.0, 0.0, 1.0);
    }

    gl_Position = (Projection * ModelView * vPosition);
    color = (ambient + diffuse + specular);
    color.a = 1.0;
}
