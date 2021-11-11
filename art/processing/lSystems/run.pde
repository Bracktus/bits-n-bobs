LSystem lsys = new LSystem("FF-F", 20, 3);
lsys.addRule("F", "FF-F");
print(lsys.computeString());

Turtle t = new Turtle(new PVector(1,2,3));
float[][] mat = {{1,4,0}, {3,2,0}, {0,0,1}};
t.matMul(mat);
print(t.position);
