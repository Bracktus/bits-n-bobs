void setup() {
  //size(800, 800);
  fullScreen();
  colorMode(HSB, 100);
}
int num = 30;
PVector mPrev[] = new PVector[num];

void draw() {
  background(0, 0, 10);
  stroke(255);
  strokeWeight(3);

  int which = frameCount % num;
  mPrev[which] = new PVector(mouseX, mouseY);

  //PVector mPos = new PVector(mouseX, mouseY);
  PVector part;
  PVector diff;
  for (int x = 5; x < width; x += 15) {
    for (int y = 5; y < height; y += 15) {

      part = new PVector(x, y);
      if (frameCount > num) {
        for (int i = 0; i < num; i++) {
          // which+1 is the smallest (the oldest in the array)
          int index = (which+1 + i) % num;

          if (PVector.dist(mPrev[index], part) < i*3) {
            diff = PVector.sub(part, mPrev[index]);
            //part.add(PVector.mult(diff, index/2));
            part.add(PVector.mult(diff, 0.07));
          }
        }
      }
      //stroke((frameCount+y+0.1*x)*0.1 % 100, 80, 100);
      //fill((frameCount+y+0.1*x)*0.1 % 100, 80, 100);
      stroke(0,0,100);
      point(part.x, part.y);
    }
  }
}
