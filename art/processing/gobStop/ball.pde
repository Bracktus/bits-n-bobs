class Ball {
  PVector pos;

  int lines;
  float[] posAngs;
  float[] tVals;
  float dirAng;
  float radius;
  float off; //used to set length of the chords

  color circuCol;
  color[] baseCols;
  color[] colsArr;

  Ball(PVector pos, color[] baseCols, float diameter, int lines, float off) {
    this.pos = pos;
    this.baseCols = baseCols;
    this.circuCol = pickRandom(baseCols);
    this.radius = diameter/2;
    //this.dirAng = noise(pos.x/800, pos.y/800) * PI;
    this.dirAng = noise(pos.y/350, pos.x/350) * PI;
    this.lines = lines;
    this.posAngs = createPosAngs(lines);
    this.off = off;
    this.colsArr = createColArr(baseCols, lines);
    this.tVals = createTVals(lines);
  }

  color pickRandom(color[] cols) {
    int colIdx = int(random(colScheme.length));
    return cols[colIdx];
  }
  
  float[] createTVals(int lines){
    float[] tVals = new float[lines];
    for (int i = 0; i < lines; i++) {
      float posAng = posAngs[i];
      PVector circuPoint = getCircuPoint(posAng);
      float tVal = getIntersection(circuPoint, dirAng);
      tVals[i] = tVal;
    }
    return tVals;
      
  }
  float[] createPosAngs(int num) {
    float[] thisPosAngs = new float[num];
    for (int i = 0; i < num; i++) {
      float posAng = random(PI, 2*PI);
      thisPosAngs[i] = posAng;
    }
    return thisPosAngs;
  }

  color[] createColArr(color[] cols, int lines) {
    color[] thisColArr = new color[lines];
    for (int i = 0; i < lines; i++) {
      thisColArr[i] = pickRandom(cols);
    }
    return thisColArr;
  }

  void render(float offPlus, boolean renderCirc) {
    stroke(circuCol);
    noFill();
    if (renderCirc){
      circle(pos.x, pos.y, radius*2);
    }

    for (int i = 0; i < lines; i++) {
      stroke(colsArr[i]);
      float posAng = posAngs[i];
      PVector circuPoint = getCircuPoint(posAng);

      float tVal = tVals[i];
      //float prop = noise(off);
      //float prop = ((cos(off) + 1) / 2) * 0.86;
      float prop = ((cos(off/2) + 1) / 2);
      drawLine(circuPoint, dirAng, tVal, prop);
    }
    off += offPlus;
  }

  void render2(float offPlus, boolean renderCirc){
    stroke(circuCol);
    noFill();
    if (renderCirc){
      circle(pos.x, pos.y, radius*2);
    }

    beginShape(TRIANGLE_STRIP);
    for (float i = PI; i < 2*PI; i+=0.45) {
      PVector circuPoint = getCircuPoint(i);

      float tVal = getIntersection(circuPoint, dirAng);
      float prop = ((cos(off) + 1) / 2) * 0.8;

      off += offPlus;
      float x2 = circuPoint.x + prop * tVal * cos(dirAng);
      float y2 = circuPoint.y + prop * tVal * sin(dirAng);
      vertex(circuPoint.x, circuPoint.y);
      vertex(x2, y2);
    }
    endShape();
  }

  PVector getCircuPoint(float ang) {
    float x = pos.x + radius * cos(ang);
    float y = pos.y + radius * sin(ang);
    return new PVector(x, y);
  }

  void drawLine(PVector start, float ang, float tValue, float p) {
    float x2 = start.x + p * tValue * cos(ang);
    float y2 = start.y + p * tValue * sin(ang);
    line(start.x, start.y, x2, y2);
  }

  float getIntersection(PVector start, float angle) {
    //returns t value at intersection
    float a = cos(angle);
    float b = sin(angle);
    float c = pow(a, 2) + pow(b, 2);//a^2 + b^2
    float rSq = pow(radius, 2); //r^2
    float xDiff = pos.x - start.x;//xc - x0
    float yDiff = pos.y - start.y;//yc - y0

    float numer1 = a*xDiff; // a*(xc - x0)
    float numer2 = b*yDiff; // b*(yc - y0)
    float numer3 = a*yDiff; // a*(yc - y0)
    float numer4 = b*xDiff; // b*(xc - x0);

    float numer5 = numer1 + numer2;
    float numer6 = rSq * c - pow(numer3 - numer4, 2);

    float finNumer2 = numer5 + sqrt(numer6);
    float ans = finNumer2 / c;
    return ans;
  }
}
