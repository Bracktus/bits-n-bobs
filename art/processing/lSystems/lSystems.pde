class Turtle {
  PVector position;
  PVector direction;

  Turtle(PVector position, PVector direction){
    this.position = position;
    this.direction = direction;
  }

  void moveForward(float len){
    PVector v = PVector.mult(direction, len);
    position = position.add(v);
  }

  void drawForward(float len){
    PVector oldPos = position.copy();
    moveForward(len);
    line(oldPos.x, oldPos.y, oldPos.z, position.x, position.y, position.z);
  }
 
  void rotX(float a){
    float[][] mat = {{ 1     , 0     , 0},
                     { 0     , cos(a), -sin(a)},
                     { 0     , sin(a), cos(a)}};
    matMul(mat);
  }
  
  void rotY(float a){
    float[][] mat = {{ cos(a), 0     , -sin(a)},
                     { 0     , 1     , 0},
                     { sin(a), 0     , cos(a)}};
    matMul(mat);
  }
  
  void rotZ(float a){
    float[][] mat = {{ cos(a), sin(a), 0},
                     {-sin(a), cos(a), 0},
                     {0      , 0     , 1}};
    matMul(mat);
  }

  void matMul(float[][] matrix){ //<>//
    //naive matrix multiplication
    //only works for 3x3
    float[] newPos = {0,0,0};
    float[] oldPos = direction.array();
    for (int i = 0; i < 3; i++){
      for (int j = 0; j < 3; j++){
        newPos[i] += matrix[i][j] * oldPos[j];
      }
    }
    direction.set(newPos[0], newPos[1], newPos[2]);
  }
  
  void printMatrix(float[][] matrix){
    for (int i = 0; i < 3; i++){
      print("[ ");
      for (int j = 0; j < 3; j++){
        print(matrix[i][j] + " ");
        
      }
      print("] ");
    }
  }
}

class LSystem {
  String initString;
  int depth;
  StringDict rules;
  String finalString;

  LSystem (String initString, int depth){
    this.initString = initString;
    this.rules = new StringDict();
    this.depth = depth;
  }

  void addRule(String symbol, String rule){
    rules.set(symbol, rule);
  }
  
  void preComputeString(){
    finalString = computeString();
  }

  void display(float theta, int len){
    Turtle turtle = new Turtle(new PVector(0,0,0), new PVector(0,0,1));
    String symbols = finalString;
    String c;
    for (int i = 0; i < symbols.length(); i++){
      c = Character.toString(symbols.charAt(i));
      switch(c) {
        case "F":
          turtle.drawForward(len);
          break;
        case "f":
          turtle.moveForward(len);
          break;
        case "y":
          turtle.rotY(theta);
          break;
        case "Y":
          turtle.rotY(-theta);
          break;
        case "x":
          turtle.rotX(theta);
          break;
        case "X":
          turtle.rotX(-theta);
          break;
        case "z":
          turtle.rotZ(theta);
          break;
        case "Z":
          turtle.rotZ(-theta);
          break;
        case "o":
          turtle.rotY(PI);
          break;
      }
    }
  }

  String computeString(){
      String init = initString;
      String finalString = "";
      String c;
      
      for (int i = 0; i < depth; i++){
        finalString = "";
        for (int j = 0; j < init.length(); j++){
          c = Character.toString(init.charAt(j));
          
          if (rules.hasKey(c)){
            finalString += rules.get(c);
          }
          else {
            finalString += c;
          }
        }
        init = finalString;
        print(i + "\n");
      }
      return finalString;
  }
}

import queasycam.*;
QueasyCam camera;
LSystem lsys;
void setup(){
  size(800, 800, P3D);
  camera = new QueasyCam(this);
  lsys = new LSystem("A", 5);
  lsys.addRule("A", "AzFFZAFyFyZAxF");
  lsys.preComputeString();
}

void draw(){
  background(0);
  stroke(255);
  lights();
  lsys.display(QUARTER_PI, 30);
}
