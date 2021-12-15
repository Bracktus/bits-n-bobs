class Particle {
  PVector pos;
  PVector vel;
  
  Particle (PVector pos, PVector vel){
    this.pos = pos;
    this.vel = vel;
  }
  
  void update(){
    if (pos.x > width || pos.x < 0){
      vel.x *= -1;
    }
    if (pos.y > height || pos.y < 0){ 
      vel.y *= -1;
    }
    pos.add(vel);
  }
}
