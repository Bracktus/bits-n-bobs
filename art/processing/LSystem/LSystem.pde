class LSystem {
  String initString;
  float theta;
  int depth;
  StringDict rules; 
  
  LSystem (String initString, float theta, int depth){
    this.initString = initString;
    this.theta = theta;
    this.depth = depth;
    this.rules = new StringDict();
  }
  
  void addRule(String symbol, String rule){
    rules.set(symbol, rule);
  }
  
  String computeString(){
      String init = initString;
      String finalString = "";
      String c;
      
      for (int i = 0; i < depth; i++){
        finalString = "";
        for (int j = 0; j < init.length() - 1; j++){
          c = Character.toString(init.charAt(j));
          
          if (rules.hasKey(c)){
            finalString += rules.get(c);
          }
          else {
            finalString += c;
          }
        }
        init = finalString;
      }
      return finalString;
  }
  
  void display(){
  }
}

String init = "F-F-F-F";
LSystem lsys = new LSystem(init, 0, 4);
lsys.addRule("F", "FF-F");
print(lsys.computeString());
