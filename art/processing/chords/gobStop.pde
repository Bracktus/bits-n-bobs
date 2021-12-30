import processing.javafx.*;

ArrayList<Ball> balls = new ArrayList<Ball>();

color yellow = color(255, 253, 130);
color orange = color(255, 155, 113);
color red = color(232, 72, 85);

color[] colScheme = {yellow, orange, red};

void setup(){
  size(900,900, FX2D);
  
  float radius = 60;
  int lines = 50;
  float off;
  for (float x = radius/2; x < width; x+= 45){
    for (float y = radius/2; y < height; y+= 45){
      //off = (x+y)/2;
      off = cos(x + y) / 2;
      Ball temp = new Ball(new PVector(x,y), colScheme, radius, lines, off);
      balls.add(temp);
    }
  }
}

void draw(){
  background(43, 26, 81);
  for (Ball b: balls){
    b.render(0.05, false);
    //b.render2(0.05, false);
  }
  println(frameRate);
  //noLoop();
}
