class Circle {
    PVector origin;
    float radius;
    int numSprings;
    Spring[] springs;

    Circle (PVector origin, float radius, int numSprings) {
      this.origin = origin;
      this.radius = radius;
      this.springs = this.generate(origin, radius, numSprings);
    }

    Spring[] generate(PVector origin, float radius, int numSprings) {
      //turn spacing into numSprings instead

      Spring[] springs = new Spring[numSprings];
      
      float angle;
      PVector springOrigin;
      PVector unit;
      
      for (int i = 0; i < numSprings; i++){
        angle = ((float) i / numSprings) * TWO_PI;
        
        springOrigin = new PVector(radius * cos(angle), radius * sin(angle)).add(origin);
        
        unit = PVector.sub(origin, springOrigin);
        
        springs[i] = new Spring(springOrigin, unit.normalize());
      }
      return springs;
    }

    void pullRandom(float dist) {
      int rand = int(random(springs.length));
      springs[rand].pull(dist);
    }

    void update() {
      
      //int rand = random(1) > 0.5? -1 : 1;
      float lDiff;
      float rDiff;
      float spread = 0.005;
      
      PVector rightForce;
      PVector leftForce;
      PVector finalForce;
      
      for (int i = 0; i < springs.length; i++){
        if (i == 0){
          lDiff = springs[springs.length - 1].getLength() - springs[i].getLength();
        }
        else {
          lDiff = springs[i - 1].getLength() - springs[i].getLength();
        }
        
        if (i == springs.length - 1){
          rDiff = springs[0].getLength() - springs[i].getLength();
        }
        else {
          rDiff = springs[i + 1].getLength() - springs[i].getLength();
        }
        
        leftForce = springs[i].magToVector(lDiff * spread);
        rightForce = springs[i].magToVector(rDiff * spread);
        finalForce = PVector.add(leftForce, rightForce);
        springs[i].pull(lDiff * 0.1);
        springs[i].pull(rDiff * 0.1);
        springs[i].update(finalForce, 0.05);
      }
    }

    void display() {
      //PVector pos;
      noFill();
      //beginShape();
      for (int i = 0; i < springs.length; i++) {
        //pos = springs[i].position;
        //vertex(pos.x, pos.y);
        springs[i].display();
      }
      //endShape();
    }
  }
