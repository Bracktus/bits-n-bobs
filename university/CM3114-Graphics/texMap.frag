#version 330 core

in vec3 position;
in vec3 normal;
in  vec2 textureCoord;
out vec4 fColor;

uniform vec4 LightPosition;
uniform vec4 AmbientProduct, DiffuseProduct, SpecularProduct;
uniform float Shininess;

uniform sampler2D tex;
uniform sampler2D map;

void main()
{
	vec3 normal_2 = texture(map, textureCoord).rgb;
	normal_2 = normalize(normal_2 * 2.0 - 1.0);
	// Here light position is defined in eye coordinates
	vec3 lightPos = normalize( LightPosition.xyz - position );

	// View direction
	vec3 viewDir = normalize( -position );

	// Compute terms in the illumination equation
	vec4 ambient = AmbientProduct;

	// Compute the diffuse direction
	float Kd = max( dot(lightPos, normal_2), 0.0 );
	vec4 diffuse = Kd*DiffuseProduct;

	// Compute reflect direction
	vec3 reflectDir = normalize( -reflect(lightPos, normal_2) );

	//For original Phong model
	float Ks = pow( max(dot(viewDir, reflectDir), 0.0), Shininess);

	vec4 specular = Ks * SpecularProduct;
	specular = clamp(specular, 0.0, 1.0);

	vec4 colour = texture(tex, textureCoord);

	fColor = (ambient + diffuse + specular) * colour;
	fColor.a = 1.0;
}
