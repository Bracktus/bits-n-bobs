import random

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

class Rectangle:
    def __init__(self, x, y, height, width):
        self.x = x
        self.y = y
        self.height = height
        self.width = width

    def contains(self, point):
        left = point.x > self.x - self.width/2
        right = point.x < self.x + self.width/2
        bottom = point.y > self.y - self.height/2
        top = point.y < self.y + self.height/2
        return left and right and bottom and top

class QuadTree:
    def __init__(self, boundary, point=None):
        self.boundary = boundary
        self.point = point
        self.divided = False
    
    def subdivide(self):
        x = self.boundary.x
        y = self.boundary.y
        height = self.boundary.height
        width = self.boundary.width

        tl_bound = Rectangle(x - width/4, y + height/4, height/2, width/2)
        tr_bound = Rectangle(x + width/4, y + height/4, height/2, width/2)
        bl_bound = Rectangle(x - width/4, y - height/4, height/2, width/2)
        br_bound = Rectangle(x + width/4, y - height/4, height/2, width/2)

        self.top_left = QuadTree(tl_bound)
        self.top_right = QuadTree(tr_bound)
        self.bot_left = QuadTree(bl_bound)
        self.bot_right = QuadTree(br_bound)

        self.divided = True

    def insert(self, point):
        if not self.boundary.contains(point):
            return

        if self.divided == False:
            if self.point == None:
            #if it's a leaf node insert. 
                self.point = point
            else:
            #if it's not a leaf node but not subdivided
            #push the current point down to the children
            #and push the new point down to it's children
                self.subdivide()
                self.insert(self.point)
                self.point = None
                self.insert(point)
        else:
        #if it's not a leaf node and subdivided, push it down to children 
            self.top_left.insert(point)
            self.top_right.insert(point)
            self.bot_left.insert(point)
            self.bot_right.insert(point)



boundary = Rectangle(50, 50, 100, 100)
quadtree = QuadTree(boundary)

for i in range(100):
    randx = random.randint(0, 100)
    randy = random.randint(0, 100)
    quadtree.insert(Point(randx, randy))

#PLAN:
#   1.) Brighten image
#   2.) Apply dithering
#   3.) For each black pixel during dithering, add to the quadtree
#   4.) Build a graph connecting each pixel
#   5.) Visualise it using turtle (Delaunay?)
