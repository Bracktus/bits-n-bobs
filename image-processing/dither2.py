from PIL import Image
import math

#Floyd-Steinberg Algorithm
image = Image.open("./images/statue.png")
width, height = image.size
old_pixels = image.load()

new_image = Image.new('1', (width, height))
new_pixels = new_image.load()

old_copy = []
for x in range(width):
    temp = []
    for y in range(height):
        temp.append(old_pixels[x,y])
    old_copy.append(temp)

for y in range(height-1):
    for x in range(1, width-1):
        pixel = old_copy[x][y]
        
        print(pixel)
        new_pixel = round(pixel / 128)
        print(new_pixel)
        new_pixels[x,y] = new_pixel
        quant_error = new_pixels[x,y] - old_copy[x][y]

        old_copy[x+1][y  ] = old_copy[x+1][y  ] + quant_error * (7/16)
        old_copy[x-1][y+1] = old_copy[x-1][y+1] + quant_error * (3/16)
        old_copy[x  ][y+1] = old_copy[x  ][y+1] + quant_error * (5/16)
        old_copy[x+1][y+1] = old_copy[x+1][y+1] + quant_error * (1/16)

new_image.save("./images/dither2.png")
