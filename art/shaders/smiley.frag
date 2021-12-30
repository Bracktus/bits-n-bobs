#ifdef GL_ES
precision mediump float;
#endif

#define TAU 6.28318530718
#define ss(a, b, t) smoothstep(a, b, t)

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

float remap01(float t, float a, float b){
    //remaps t from range <a,b> to range <0,1>
    return clamp((t-a)/(b-a), 0.0, 1.0);
}

float remap(float t, float a, float b, float c, float d){
   //remaps t from range <a,b> to <c,d>
   return clamp(((t-a)/(b-a)) * c+(d-c), 0.0, 1.0);
}

vec2 within(vec2 uv, vec4 rect){
    //rect.xy = bot left corner
    //rect.zw = top right corner
    return (uv-rect.xy)/(rect.zw - rect.xy);
}

vec4 eye(vec2 uv){
    uv -= 0.5;
    float d = length(uv);
    
    //edge gradient
    vec4 irisCol = vec4(0.3, 0.5, 1.0, 1.0); //baby blue
    vec4 col = mix(vec4(1.0), irisCol, ss(0.1, 0.7, d) * 0.5);
    
    //edge shadow
    col.rgb *= 1.0 - ss(0.45, 0.5, d) * 0.5 * clamp(-uv.y-uv.x, 0.0, 1.0);

    //pupil
    col.rgb = mix(col.rgb, vec3(0.0), ss(0.3, 0.28, d));

    //iris
    irisCol.rgb *= 1. + ss(0.3, 0.05, d);
    col.rgb = mix(col.rgb, irisCol.rgb, ss(0.28, 0.25, d));

    //pupil 
    col.rgb = mix(col.rgb, vec3(0.0), ss(0.16, 0.14, d));

    //highlights
    float highlight = ss(0.1, 0.09, length(uv-vec2(-0.15, 0.15)));
    highlight += ss(0.07, 0.05, length(uv+vec2(-0.08, 0.08)));
    col.rgb = mix(col.rgb, vec3(1.0), highlight);

    col.a = ss(0.5, 0.49, d); //radius
    return col;
}

vec4 mouth(vec2 uv){
    uv -= 0.5;
    vec4 col = vec4(0.5, 0.18, 0.05, 1.0);

    /* //shape of mouth */
    uv.y *= 1.5;
    uv.y -= uv.x * uv.x * 2.0;
    float d = length(uv);
    col.a = ss(0.5, 0.48, d);

    //teeth
    float td = length(uv - vec2(0.0, 0.6));
    vec3 toothCol = vec3(1.0)*ss(0.6, 0.35, d);
    col.rgb = mix(col.rgb, toothCol, ss(0.4, 0.37, td));

    //tongue
    td = length(uv + vec2(0.0, 0.5));
    col.rgb = mix(col.rgb, vec3(1.0, 0.5, 0.5), ss(0.5, 0.2, td));

    return col;
}

vec4 head(vec2 uv){
    vec4 col = vec4(0.9, 0.65, 0.1, 1.0); //yellow
    float d = length(uv);

    //define the head with radius 0.5
    col.a = ss(0.5, 0.49, d);

    //add a gradient shading the edge of the head
    float edgeShade = ss(0.4, 0.5, d);
    col.rgb *= 1.0 - edgeShade * .5;
    
    //add an outline to the edge of the head
    float outline = ss(0.475, 0.48, d);
    col.rgb = mix(col.rgb, vec3(0.6, 0.3, 0.1), outline);

    //add a white semi-circle to middle of head
    float highlight = ss(0.41, 0.405, d);
    highlight *= remap(0.41, 0.0, 0.75, 0.0, uv.y);
    col.rgb = mix(col.rgb, vec3(1.), highlight);

    //add 2 blushing cheeks
    d = length(uv - vec2(0.25, -0.20));
    float cheek = ss(0.2, 0.01, d) * 0.6;
    col.rgb = mix(col.rgb, vec3(1.0, 0.1, 0.1), cheek);
    return col;
}

vec4 smiley(vec2 uv){
    vec4 col = vec4(0.0);
    uv.x = abs(uv.x);
    vec4 head = head(uv);
    vec4 eye = eye(within(uv, vec4(0.03, -0.1, 0.37, 0.25)));
    vec4 mouth = mouth(within(uv, vec4(-0.3, -0.4, 0.3, -0.1)));

    col = mix(col, head, head.a);
    col = mix(col, eye, eye.a);
    col = mix(col, mouth, mouth.a);
    //a as in rgba
    
    return col;
}

void main(){
    vec2 uv = gl_FragCoord.xy/u_resolution.xy;
    uv -= 0.5;
    uv.x *= u_resolution.x/u_resolution.y;
    gl_FragColor = smiley(uv);
}
