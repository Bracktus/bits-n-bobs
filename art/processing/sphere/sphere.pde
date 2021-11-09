import queasycam.*;

/* Sphere Equation (spiral ver)

x = sin(t) * cos(nt)
y = sin(t) * sin(nt)
z = cos(t)

0 <= t <= pi
*/

QueasyCam camera;
void setup(){
  size(400, 400, P3D);
  camera = new QueasyCam(this);
}

int numPoints = 1000;
float n = 100;
float r = 1000;
float sqang;

void draw(){
  
  //camera
  background(0);
  lights();
  translate(width/2, height/2);
  
  //rendering
  float t, x, y, z;
  for(int i = 0; i < numPoints; i++){
    t = map(i, 0, numPoints, 0, PI);
    x = sin(t) * cos(n*t);
    y = sin(t) * sin(n*t);
    z = cos(t);
    //point(x *r, y * r, z * r);
    drawCube(x * r, y * r, z * r, 5);
  }
}

void drawCube(float x, float y, float z, float sqrtLen){
  
  rotate(sqang / 4);
  stroke(255);
  pushMatrix();
  sqang += 0.0000005;
  rotate(sqang * 2);
  float len = sq(sqrtLen)/2;
  
  PVector a = new PVector(x - len, y + len, z + len);
  PVector b = new PVector(x - len, y + len, z - len);
  PVector c = new PVector(x + len, y + len, z - len);
  PVector d = new PVector(x + len, y + len, z + len);
  PVector e = new PVector(x - len, y - len, z + len);
  PVector f = new PVector(x + len, y - len, z + len);
  PVector g = new PVector(x + len, y - len, z - len);
  PVector h = new PVector(x - len, y - len, z - len);
  
  drawEdge(a,b);
  drawEdge(a,d);
  drawEdge(a,e);
  drawEdge(b,c);
  drawEdge(b,h);
  drawEdge(c,d);
  drawEdge(c,g);
  drawEdge(d,f);
  drawEdge(e,h);
  drawEdge(e,f);
  drawEdge(f,g);
  drawEdge(g,h);
  popMatrix();
}

void drawEdge(PVector p1, PVector p2){
  line(p1.x, p1.y, p1.z, p2.x, p2.y, p2.z);
}
