// Butterfly
void setup(){
  size(600,600);
  
}

float a = 100;
float off = 0.5;
float step = 0.01;
float xoff = 0;
float yoff = 0;

void draw(){
  translate(width/2, height/2);
  background(0);
  stroke(255);
  beginShape();
  for (int i = 0; i < 75; i++){
    float theta = map(i, 0, 50, 0, 2*PI);
    float r = 100 * sin(2 * theta);
    float flap = sin(off);
    float x = flap * r * cos(theta);
    float y = r * sin(theta);
    vertex(x,y);
  }
  endShape(CLOSE);
  
  //constrain offset
  if (off > PI - 0.4 || off < 0.4){
    step = -step;
  }
  off+= step;
}
