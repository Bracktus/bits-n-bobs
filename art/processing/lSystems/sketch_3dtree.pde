class LSystem {
  String initString;
  int depth;
  StringDict rules; 
  Turtle turtle;
  
  LSystem (String initString, float theta, int depth){
    this.initString = initString;
    this.depth = depth;
    this.rules = new StringDict();
  }
  
  void addRule(String symbol, String rule){
    rules.set(symbol, rule);
  }
  
  void display(){
    String symbols = computeString();
    String c;
    for (int i = 0; i < symbols.length(); i++){
      c = Character.toString(symbols.charAt(i));
      switch(c) {
        case "F":
          
      }
    }
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
}
