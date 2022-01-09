#ifdef GL_ES
precision mediump float;
#endif

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

#define ss(a, b, t) smoothstep(a, b, t)

vec3 tail(float i, float i_max, vec2 uv){
    float x = 0.5 * tan(u_time*2.5 - i*0.01);
    float y = 0.5 * sin(u_time*2.5 - i*0.01);
    float d = length(uv - vec2(x,y));
    float val = (i_max - i)/d*0.0009; 


    /* float x = 3.5 * sin(u_time*2.5 - i*0.05); */
    /* float y = sin((u_time*2.5 - i*0.05) * 2.0); */
    /* x *= 0.2; */
    /* y *= 0.2; */
    /* float d = length(uv - vec2(x,y)); */
    /* float val = (i_max - i)/d*0.002; */ 

    //x + sin(at + delta)
    //y + sin(bt)
    /* float x = 0.5 * sin(1.*(u_time - i*0.005) + 3.1462 * 0.75); */
    /* float y = 0.5 * sin(5.*(u_time - i*0.005)); */ 
    /* float d = length(uv - vec2(x,y)); */
    /* float val = (i_max - i)/d*0.001; */ 

    float r = val*0.5;
    float g = val*0.5;
    float b = val;


    //x + sin(at + delta)
    //y + sin(bt)
    /* x = 0.5 * sin(2.*(u_time - i*0.008) + 3.1462 * 0.75); */
    /* y = 0.5 * sin(3.2*(u_time - i*0.008)); */ 
    /* d = length(uv - vec2(x,y)); */
    /* val = (i_max - i)/d*0.001; */
    /* r += val*0.5; */
    /* g += val*0.5; */

    return vec3(r,g,b);
}

void main() {
    vec2 uv = gl_FragCoord.xy/u_resolution.xy;
    uv -= 0.5;
    uv.x *= u_resolution.x/u_resolution.y;

    vec3 col = vec3(0., 0., 0.); 
    for (float i = 0.0; i < 10.0; i++){
        col += tail(i, 10.0, uv);
    }
    gl_FragColor = vec4(col, 1.0);
}


