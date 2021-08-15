from PIL import Image
import math

def applyDither(dither_matrix, pixel):
    """Takes in a pixel value and applies a dither matrix
       which returns a 2D array of 1s and 0s based on the 
       pixel's value"""

    applied_matrix = []

    #normalise pixel so it's value is within the dither matrix
    max_val = max(list(map(max, dither_matrix)))
    norm_pixel = math.floor(pixel / max_val + 1)

    for arr in dither_matrix:
        thres_arr = [int(value < norm_pixel) for value in arr]
        applied_matrix.append(thres_arr)
    
    return applied_matrix

def transformImage(trans_mat, image, x, y):
    dither_len = len(trans_mat)

    for i in range(dither_len):
        for j in range(dither_len):
            pixel = trans_mat[i][j]
            image[x + i, y + j] = pixel
            print(f"x,y: ({x + i}, {y+j})")

dither_matrix = [[0, 8, 2, 10 ],
                 [12, 4, 14, 6],
                 [3, 11, 1, 9 ],
                 [15, 7, 13, 5]]

dither_len = len(dither_matrix)

image = Image.open("./images/statue.png")
width, height = image.size
old_pixels = image.load()

new_size = (width * dither_len, height * dither_len)
new_image = Image.new('1', new_size)
new_pixels = new_image.load()


new_x = 0
new_y = 0
for x in range(width):
    for y in range(height):
        old_pixel, a = old_pixels[x, y]
        trans_mat = applyDither(dither_matrix, old_pixel)
        transformImage(trans_mat, new_pixels, x * 4, y * 4)

new_image.save("./images/dither.png")

