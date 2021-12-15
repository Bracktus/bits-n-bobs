void addParticle(ArrayList<Particle> particles){
  float xPos = random(width);
  float yPos = random(height);
  
  float xVel = random(5);
  float yVel = random(5);
  
  PVector pos = new PVector(xPos,yPos);
  PVector vel = new PVector(xVel,yVel);
  Particle particle = new Particle(pos, vel);
  particles.add(particle);
}

void removeParticle(ArrayList<Particle> particles){
  int len = particles.size();
  if (len > 0){
     particles.remove(len - 1);    
  }
}

void renderRandom(ArrayList<Particle> particles){
  float maxDist = sqrt(pow(width, 2) + pow(height, 2));
  for (int i = 0; i < particles.size(); i++){
    
    int friendIdx = int(random(0, particles.size() - 1));
    Particle friend = particles.get(friendIdx);
    Particle me = particles.get(i);
    
    float dist = PVector.dist(me.pos, friend.pos);
    float lerpVal = map(dist, 0, maxDist/2, 0, 1);
    color background = color(249, 248, 224);
    color black = color(0,0,0);
    color col = lerpColor(background, black, lerpVal);
    
    stroke(0);
    fill(0);
    circle(me.pos.x, me.pos.y, 3);
    stroke(col);
    line(me.pos.x, me.pos.y, friend.pos.x, friend.pos.y);
  }
}

void renderNearest(ArrayList<Particle> particles){
  float maxDist = sqrt(pow(width, 2) + pow(height, 2));
  for (int i = 0; i < particles.size(); i++){
    Particle me = particles.get(i);
    
    Particle nearest = getNearest(me, particles);
    float dist = PVector.dist(me.pos, nearest.pos);
    float col = map(dist, 0, maxDist, 100, 255);
    stroke(0);
    fill(0);
    circle(me.pos.x, me.pos.y, 3);
    
    stroke(col);
    line(me.pos.x, me.pos.y, nearest.pos.x, nearest.pos.y);
  }
}

Particle getNearest(Particle particle, ArrayList<Particle> neighbours){
  float dist = 100000000;
  float tmpDist;
  Particle nearest = null;
  for (int i = 0; i < neighbours.size(); i++){
    Particle friend = neighbours.get(i);
    Boolean isSelf = (friend == particle);
    tmpDist = PVector.dist(particle.pos, friend.pos);
    
    if (tmpDist < dist && !isSelf){
      nearest = friend;
      dist = tmpDist;
    }
  }
  return nearest;
}

ArrayList<Particle> particles = new ArrayList<>();

void setup(){
  fullScreen();
}

void draw(){
  background(249, 248, 224);
  for (int i = 0; i < particles.size(); i++){
    particles.get(i).update();
  }
  if (particles.size() > 2){
      renderNearest(particles);
  }
}

void keyPressed(){
  if (key == 'a'){
    addParticle(particles);
  }
  if (key == 'd'){
    removeParticle(particles);
  }
}

//connect to nearest neighbour?
//connect to random neightbour?
