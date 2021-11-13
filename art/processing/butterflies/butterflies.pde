// Butterfly
ArrayList<Fish> flock = new ArrayList<Fish>();
void setup(){
  fullScreen();
  float randX;
  float randY;
  for (int i = 0; i < 30; i++){
    randX = random(width);
    randY = random(height);
    flock.add(new Fish(randX, randY));
  }
}

void draw(){
  //translate(width/2, height/2);
  background(0);
  stroke(255);
  
  for (Fish b: flock){
    b.update(flock);
    b.render();
  }
}

class Fish{
  //float offset = 0.5;
  //float step = 0.01;
  //float rFact = 10;
  
  PVector position;
  PVector velocity;
  
  float maxSpeed = 5;
  float maxSteer = 0.3;
  
  Fish(float x, float y){
    this.position = new PVector(x,y);
    this.velocity = PVector.random2D();
  }
  
  void update(ArrayList<Fish> others){
    PVector acceleration = getForce(others);
    velocity.add(acceleration);
    velocity.limit(maxSpeed);
    position.add(velocity);
    borders();
  }
  
  void borders() {
    if (position.x < 0) position.x = width;
    if (position.y < 0) position.y = height;
    if (position.x > width) position.x = 0;
    if (position.y > height) position.y = 0;
  }
  
  PVector rotatePara(float x, float y, float theta){
    //https://math.stackexchange.com/questions/245859/rotating-parametric-curve
    float newX = x * cos(theta) - y * sin(theta);
    float newY = x * sin(theta) + y * cos(theta);
    return new PVector(newX, newY);
  }
  
  void render(){
    float direction = velocity.heading();
    int points = 30;
    float r = 10;
    beginShape();
    for (int i = 0; i < points; i++){
      //https://en.wikipedia.org/wiki/Fish_curve
      float t = map(i, 0, points, 0, TWO_PI);
      float x = r * cos(t) - (r * sq(sin(t))) / sqrt(2);
      float y = r * cos(t) * sin(t);
      
      PVector rotated = rotatePara(x, y, direction);
      vertex(rotated.x + position.x, rotated.y + position.y);
      
     
      
      //vertex(x + position.x , y + position.y);
      
    }
    endShape(CLOSE);
  }
  
  PVector getForce(ArrayList<Fish> others){
    //adapted from https://processing.org/examples/flocking.html
    
    PVector seperation = new PVector(0,0);
    PVector alignment = new PVector(0,0);
    PVector cohesion = new PVector(0,0);
    
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
    for (Fish b : others){
      float distance = PVector.dist(position, b.position);
      
      //seperation check
      if (distance > 0 && distance < sepDist){
        PVector diff = PVector.sub(position, b.position);
        diff.normalize();
        diff.div(distance);
        sepAvg.add(diff);
        sepCount++;
      }
      
      //cohesion check
      if (distance > 0 && distance < cohDist){
        cohAvg.add(b.position);
        cohCount++;
      }
      
      // alignment check
      if (distance > 0 && distance < aliDist){
        aliAvg.add(b.velocity);
        aliCount++;
      }
    }
    
    // seperation calculations
    if (sepCount > 0){
      sepAvg.div(sepCount);
      sepAvg.setMag(maxSpeed);
      seperation = sepAvg.sub(velocity);
      seperation.limit(maxSteer);
    }
    
    // cohesion calculations
    if (cohCount > 0){
      cohAvg.div(cohCount);
      cohesion = seek(cohAvg);
    } 
    
    // alignment calculations
    if (aliCount > 0){
      aliAvg.div(aliCount);
      aliAvg.setMag(maxSpeed);
      alignment = aliAvg.sub(velocity);
      alignment.limit(maxSteer);
    }
    
    // apply weights
    alignment.mult(1.1);
    cohesion.mult(0.8);
    seperation.mult(1.5);
    
    
    PVector force = PVector.add(alignment, cohesion);
    force.add(seperation);
    return force;
  }
  
  PVector seek(PVector target){
    PVector desired = PVector.sub(target, position);
    desired.setMag(maxSpeed);
    
    PVector steer = PVector.sub(desired, velocity);
    steer.limit(maxSteer);
    return steer;
  }
}
