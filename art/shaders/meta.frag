#ifdef GL_ES
precision mediump float;
#endif

#define ss(a, b, t)  smoothstep(a, b, t)
uniform vec2 u_resolution;
uniform float u_time;
uniform vec2 u_mouse;

vec3 charge(vec2 uv, vec2 pos, float rad){
    //offset rgb to create chromatic abberation
    vec2 r = pos + vec2(sin(u_time/2.)*0.003, 0.001);
    vec2 g = pos + vec2(0.0, cos(u_time/2.)*0.002);
    vec2 b = pos + vec2(cos(u_time/2.)*0.001, sin(u_time/2.)*0.003);
    
    //generate charge levels for each colour
    float chargeR = rad / length(uv - r); 
    float chargeG = rad / length(uv - g); 
    float chargeB = rad / length(uv - b); 

    return vec3(chargeR, chargeG, chargeB);
}

void main() {
    //resolution stuff
    vec2 uv = gl_FragCoord.xy/u_resolution.xy;
    uv -= 0.5;
    uv.x *= u_resolution.x/u_resolution.y;

    //mouse stuff
    
    //defining ball positions
    vec2 b1 = vec2(0.0, 0.0);
    vec2 b2 = vec2(.3*cos(u_time*0.5), .4*sin(u_time*0.5));
    vec2 b3 = vec2(-0.1, 0.4*sin(u_time*.2));
    vec2 b4 = vec2(0.5*(cos(u_time*.3)), 0.01);
    
    //get charge levels for balls
    vec3 c1 = charge(uv, b1, 0.05);
    vec3 c2 = charge(uv, b2, 0.08);
    vec3 c3 = charge(uv, b3, 0.1);
    vec3 c4 = charge(uv, b4, 0.05);

    vec3 total = c1 + c2 + c3 + c4;

    //apply chromatic abberation offsets
    vec3 bg = vec3(0.0); 
    bg.r += ss(0.9, 0.92, total.r) - ss(0.93, 0.945, total.r);
    bg.g += ss(0.9, 0.92, total.g) - ss(0.93, 0.945, total.g);
    bg.b += ss(0.9, 0.92, total.b) - ss(0.93, 0.945, total.b);

    gl_FragColor = vec4(bg, 1.0);
}


