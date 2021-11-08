PVector start1 = new PVector(0, 0);
Circle circle1 = new Circle(start1, 450, 100);
Circle circle2 = new Circle(start1, 400, 100);
Circle circle3 = new Circle(start1, 350, 100);
Circle circle4 = new Circle(start1, 300, 100);
Circle circle5 = new Circle(start1, 250, 100);
Circle circle6 = new Circle(start1, 200, 100);
Circle circle7 = new Circle(start1, 150, 100);

int numCirc = 7;
Circle[] circleList = new Circle[numCirc];

void setup() {
  //frameRate(60);
  size(1000,1000);
  
  circleList[0] = circle1;
  circleList[1] = circle2;
  circleList[2] = circle3;
  circleList[3] = circle4;
  circleList[4] = circle5;
  circleList[5] = circle6;
  circleList[6] = circle7;
}

void draw() {
  translate(width/2, height/2);
  background(236, 233, 231, 0.8);
  fill(255);
  for (int i = 0; i < numCirc; i++){
      circleList[i].update();
      circleList[i].display();
  }
  //saveFrame("frames/####.tiff"); 
}

void mouseClicked() {
  float power;
  for (int times = 0; times < 20; times++){
    for (int i = 0; i < numCirc; i++){
      power = circleList[i].radius / 2;
      circleList[i].pullRandom(power);
    }
  }
}
