import turtle
import random

class LSystem:
    
    def __init__(self, start, theta, depth):
        self.start = start
        self.theta = theta
        self.depth = depth
        self.rules = {}

    def add_rule(self, symbol, string, prob):
        right = (string, prob)
        self.rules[symbol] = right

    def draw(self):
        draw_string = self.get_string()

        screen = turtle.getscreen()
        screen.screensize(2000, 2000)
        turtle.tracer(0, 0)
        turtle.hideturtle()
        turtle.speed(0)
        turtle.left(90)

        stack = []
        for char in draw_string:
            if char == "[":
                pos = turtle.pos()
                angle = turtle.heading()
                stack.append((pos, angle))
            elif char == "]":
                state = stack.pop()
                new_pos = state[0]
                new_angle = state[1]

                turtle.penup()
                turtle.goto(new_pos[0], new_pos[1])
                turtle.setheading(new_angle)
                turtle.pendown()

            elif char == "F":
                turtle.forward(10)
            elif char == "-":
                turtle.right(self.theta)
            elif char == "+":
                turtle.left(self.theta)

        turtle.update()
        turtle.done()
       
    def get_string(self):
        start = self.start

        for i in range(self.depth):
            fin = ""
            num = random.random()
            for char in start:
                if char in self.rules and self.rules[char][1] >= num:
                    fin += self.rules[char][0]
                else:
                    fin += char
            start = fin

        return fin

system = LSystem("X", 22.5, 5)
system.add_rule("X", "F-[[X]+X]+F[+FX]-X", 1)
system.add_rule("F", "FF", 1)
system.draw()


