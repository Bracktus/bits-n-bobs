class Turtle {
  PVector position;
  PVector direction;
  int sWeight = 4;

  Turtle(PVector position, PVector direction){
    this.position = position;
    this.direction = direction;
  }
  
  void addSWeight(float toAdd){
    this.sWeight += toAdd;
    strokeWeight(sWeight);
  }
  
  PVector[] getState(){
    PVector[] fin = {position.copy(), direction.copy()};
    return fin;
  }
  
  void setState(PVector[] state){
    position = state[0];
    direction = state[1];
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
 
  void rotH(float a){
    float[][] mat = {{ 1     , 0     , 0},
                     { 0     , cos(a), -sin(a)},
                     { 0     , sin(a), cos(a)}};
    matMul(mat);
  }
  
  void rotL(float a){
    float[][] mat = {{ cos(a), 0     , -sin(a)},
                     { 0     , 1     , 0},
                     { sin(a), 0     , cos(a)}};
    matMul(mat);
  }
  
  void rotU(float a){
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
    Turtle turtle = new Turtle(new PVector(0,0,0), new PVector(1, 0, 0));
    String symbols = finalString;
    String c;
    ArrayList<PVector[]> stack = new ArrayList<PVector[]>();
    for (int i = 0; i < symbols.length(); i++){
      c = Character.toString(symbols.charAt(i));
      switch(c) {
        case "[":
          stack.add(turtle.getState());
          break;
        case "]":
          stack.remove(stack.size() - 1);
          break;
        case "F":
          turtle.drawForward(len);
          break;
        case "f":
          turtle.moveForward(len);
          break;
        case "+":
          turtle.rotU(theta);
          break;
        case "-":
          turtle.rotU(-theta);
          break;
        case "&":
          turtle.rotL(theta);
          break;
        case "^":
          turtle.rotL(-theta);
          break;
        case "\\":
          turtle.rotH(theta);
          break;
        case "/":
          turtle.rotH(-theta);
          break;
        case "|":
          turtle.rotU(PI);
          break;
        case "£":
          turtle.addSWeight(1);
          break;
        case "$":
          turtle.addSWeight(-1);
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
        //print(i + "\n");
      }
      return finalString;
  }
}

import queasycam.*;
QueasyCam camera;
LSystem lsys;
ArrayList<String> prods = new ArrayList<String>();


void setup(){
  size(800, 800, P3D);
  camera = new QueasyCam(this);
  lsys = new LSystem("A", 5);
  
  //lsys.addRule("A", "B-F+CFC+F-D&F^D-F+&&CFC+F+B//");
  //lsys.addRule("B", "A&F^CFB^F^D^^-F-D^|F^B|FC^F^A//");
  //lsys.addRule("C", "|D^|F^B-F+C^F^A&&FA&F^C+F+B^F^D//");
  //lsys.addRule("D", "|CFB-F+B|FA&F^A&&FB-F+B|FC//");
  
  prods.add("A");
  prods.add("B");
  prods.add("C");
  //prods.add("D");
  
  ruleGen(lsys, "A", prods, 5, 15);
  ruleGen(lsys, "B", prods, 5, 15);
  ruleGen(lsys, "C", prods, 10, 11);
  //ruleGen(lsys, "D", prods, 10, 15);
  lsys.preComputeString();
}
float angle = 22.7;
float off = 0;
void draw(){
  background(0);
  stroke(255);
  lights();
  
  lsys.display(radians(angle), 10);
  //lsys.rules.remove("B");
  //ruleGen(lsys, "B", prods, 5, 15);
  //lsys.preComputeString();
 
  //angle += (noise(off) * 2 - 1)/10;
  //off += 0.01;
  //angle += 0.01;

}

void ruleGen(LSystem lsys, String symbol, ArrayList<String> others, int min, int max){
  ArrayList<String> possible = new ArrayList<String>();
  possible.add("+");
  possible.add("-");
  possible.add("&");
  possible.add("^");
  possible.add("\\");
  possible.add("/");
  possible.add("|");
  possible.add("F");
  possible.add("[");
  //possible.add("$");
  //possible.add("£");
  //possible.add("f");
  possible.addAll(others);
  int len = int(random(min, max));
  int rand;
  int rand2 = -1;
  String rule = "";
  for (int i = 0; i < len; i++){
    rand = int(random(possible.size()));
    if (possible.get(rand) == "["){
      rand2 = int(random(i + 1, len));
    }
    
    if (rand2 != -1 && i == rand2){
      rule += "]";
      rand2 = -1;
    }
      
    rule += possible.get(rand);
  }
  print(symbol + " -> " + rule + "\n");
  lsys.addRule(symbol, rule);
  
}
