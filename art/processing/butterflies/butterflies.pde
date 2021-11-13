// Butterfly
void setup(){
  size(600,600);
}

float off = 0.5;
float step = 0.01;

void draw(){
  translate(width/2, height/2);
  background(0);
  stroke(255);
  beginShape();
  for (int i = 0; i < 50; i++){
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

class Butterfly{
  float offset = 0.5;
  float step = 0.01;
  
  PVector acceleration;
  PVector position;
  PVector velocity;
  
  Butterfly(float x, float y){
    this.position = new PVector(x,y);
    this.velocity = PVector.random2D();
    this.acceleration = new PVector(0,0);
  }
  
  void update(ArrayList<Butterfly> others){
    //adapted from https://processing.org/examples/flocking.html
    
    PVector seperate;
    PVector alignment;
    PVector cohesion;
    
    float sepDist = 25;
    PVector sepAvg = new PVector(0,0);
    float sepCount = 0;
    
    float cohDist = 50;
    PVector cohAvg = new PVector(0,0);
    float cohCount = 0;
    
    float aliDist = 50;
    PVector aliAvg = new PVector(0,0);
    float aliCount = 0;
    
    //add these 3 vectors to acceleration and use euler integration
    for (Butterfly b : others){
      float distance = PVector.dist(position, b.position);
      
      //seperation check
      if (distance > 0 && distance < sepDist){
        PVector diff = PVector.sub(position, b.position);
        diff.normalize().div(distance);
        sepAvg.add(diff);
        sepCount++;
      }
      
      //cohesion check
      if (distance > 0 && distance < cohDist){
        cohAvg.add(b.position);
        cohCount++;
      }
      
      // alignment
      if (distance > 0 && distance < aliDist){
        aliAvg.add(b.velocity);
        aliCount++;
      }
      
    }
    
    if (sepCount > 0){
      seperate = sepAvg.div(sepCount);
    }
    
    if (cohCount > 0){
      cohesion = cohAvg.div(cohCount);
      PVector newDir = PVector.sub(position, cohesion);
    } 
    
    if (aliCount > 0){
      alignment = aliAvg.div(aliCount);
    }
  }
  
  PVector steer(PVector point){
    
  }
  
}
