PVector start1 = new PVector(0, 0);
Circle circle1 = new Circle(start1, 300, 40);

PVector start2 = new PVector(10, 0);
Circle circle2 = new Circle(start2, 300, 40);

void setup() {
  frameRate(60);
  size(1000,1000);
}

void draw() {
  translate(width/2, height/2);
  background(236, 233, 231, 0.8);
  fill(255);

  circle1.update();
  circle1.display();
  
  circle2.update();
  circle2.display();
}

void mouseClicked() {
  for (int i = 0; i < 1; i++){
    circle1.pullRandom(50);
  }
}
