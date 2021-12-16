ArrayList<Ball> balls = new ArrayList<Ball>();

color yellow = color(255, 253, 130);
color orange = color(255, 155, 113);
color red = color(232, 72, 85);

color[] colScheme = {yellow, orange, red};

float getRadius(float h, float w, float num){
  return 2;
}

void setup(){
  fullScreen();
  
  float radius = 80;
  int lines = 15;
  float off;
  for (float x = radius/2; x < width; x+= 80){
    for (float y = radius/2 ; y < height; y+= 80){
      off = (x+y)/2;
      //off = cos(x + y) / 2;
      Ball temp = new Ball(new PVector(x,y), colScheme, radius, lines, off);
      balls.add(temp);
    }
  }
}

void draw(){
  background(43, 26, 81);
  for (Ball b: balls){
    b.render(0.1, true);
    //b.render2();
  }
  //noLoop();
}
