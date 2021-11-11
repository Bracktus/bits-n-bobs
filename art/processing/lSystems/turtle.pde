class Turtle {
  PVector position;
 
  Turtle(PVector position){
    this.position = position;
  }

  void moveForward(float len){
    PVector direction = position.normalize();
    position = position.add(direction.mult(len));
  }

  void drawForward(float len){
    PVector oldPos = position;
    moveForward(len);
    line(oldPos.x, oldPos.y, oldPos.z, position.x, position.y, position.z);
  }
  
  void rotX(float a){
    float[][] mat = {{ cos(a), sin(a), 0},
                     {-sin(a), cos(a), 0},
                     {0      , 0     , 1}};
                     
    matMul(mat);
    
  }
  
  void rotY(float a){
    float[][] mat = {{ cos(a), 0     , -sin(a)},
                     { 0     , 1     , 0},
                     { sin(a), 0     , cos(a)}};
                     
    matMul(mat);
  }
  
  void rotZ(float a){
    float[][] mat = {{ 1     , 0     , 0},
                     { 0     , cos(a), -sin(a)},
                     { 0     , sin(a), cos(a)}};
                     
    matMul(mat);
  }

  void matMul(float[][] matrix){
    //naive matrix multiplication
    //only works for 3x3
    float[] newPos = {0,0,0};
    float[] oldPos = position.array();
    for (int i = 0; i < 3; i++){
      for (int j = 0; j < 3; j++){
        newPos[i] += matrix[i][j] * oldPos[j];
      }
    }
    position.set(newPos[0], newPos[1], newPos[2]);
  }
}
