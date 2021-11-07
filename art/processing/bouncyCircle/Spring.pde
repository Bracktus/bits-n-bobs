class Spring {
  PVector origin;
  PVector position;
  PVector velocity;
  PVector unit;
  float restLength;

  Spring (PVector origin, PVector unit) {
    this.origin = origin; //start
    this.position = origin.copy(); //end
    this.unit = unit;
    this.velocity = new PVector(0, 0);
    this.restLength = 0;
    print(origin + "\n");
    //print(unit + "\n");
  }

  void update(PVector extraForce, float tension) {
    PVector direction = PVector.sub(position, origin); //<>//
    float displacement = direction.mag() - restLength;

    direction.normalize();
    PVector acceleration = PVector.mult(direction, -tension * displacement); //hooke's law
    acceleration.add(extraForce);

    velocity.add(acceleration);
    velocity.mult(0.98); //dampening
    position.add(velocity);
  }

  void display() {
    stroke(0);
    fill(0);
    line(origin.x, origin.y, position.x, position.y);
    circle(position.x, position.y, 1);
  }
  
  void pull(float dist) {
    position.add(magToVector(dist));
  }

  float getLength() {
    float len = PVector.sub(position, origin).mag();
    return len;
  }

  PVector magToVector(float magnitude) {
    return PVector.mult(unit, magnitude);
  }
}
