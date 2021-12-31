#ifdef GL_ES
precision mediump float;
#endif

#define TAU 6.28318530718
#define ss(a, b, t) smoothstep(a, b, t)

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

/* float plot(vec2 uv, float center, float blur){ */
/*     return ss(center - blur, center, uv.y)- */ 
/*            ss(center, center + blur, uv.y); */
/* } */

float remap(float t, float a, float b, float c, float d){
   //remaps t from range <a,b> to <c,d>
   return clamp(((t-a)/(b-a)) * c+(d-c), 0.0, 1.0);
}

float angDiff(vec2 p1, vec2 p2){
    float aDiff = atan(p2.y, p2.x) - atan(p1.y, p1.x);
    if (aDiff < 0.0){
         aDiff += TAU;
    }
    return aDiff;
}

float scanner(vec2 uv, float a, float rad){
    if (length(uv) > rad){
        return 0.0; 
    }
    float aDiff = angDiff(uv, vec2(cos(a), sin(a)));
    float fin = ss(TAU/4.0, 0.0, aDiff);
    return fin;
}

float grid(vec2 uv, float rad){
    if (length(uv) > rad){
        return 0.0; 
    }
    vec2 tl = vec2(-rad, rad);
    vec2 br = vec2(rad, -rad);
    uv = uv-tl/(br-tl); 
    //remapping our coords

    
    float fin = 0.0;
    for (float i = -0.5; i < 4.0; i+= 0.125){
        //xLine
        fin += ss(i,i-0.001, uv.x) -
               ss(i+0.001, i, uv.x);

        fin += ss(i,i-0.001, uv.y) -
               ss(i+0.001, i, uv.y);
    }
    return fin;
}

float outline(vec2 uv, float rad, float lSize){
    //the outline of the circle
    float d = length(uv);
    float larger = ss(rad + lSize, rad, d);
    float smaller = ss(rad, rad-lSize, d);
    return larger - smaller;
}


vec4 radar(vec2 uv, float t){
    vec4 col = vec4(0.0, 0.0, 0.0, 1.0);
    float r = 0.4;

    float grid = grid(uv, r);
    float outline = outline(uv, r, 0.003);
    float scanner = scanner(uv, u_time, r);

    col -= vec4(0.05, 0.36, 0.95, 1.0) * grid;
    col.rgb = mix(col.rgb, vec3(0.125, 0.775, 0.0), outline);
    col.rgb = mix(col.rgb, vec3(0.0, 0.8, 0.1), scanner * 0.5);
    return col;
}

void main(){
    // COLOURS
    vec3 lGreen = vec3(0.4, 1.0, 0.2);

    // NORMALISATION
    vec2 uv = gl_FragCoord.xy/u_resolution.xy;
    uv -= 0.5; //<-0.5, 0.5>
    uv.x *= u_resolution.x/u_resolution.y;

    // DRAWING
    gl_FragColor = radar(uv, u_time); 
}
