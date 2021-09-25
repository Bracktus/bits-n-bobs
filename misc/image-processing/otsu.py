from PIL import Image
import matplotlib.pyplot as plt

image = Image.open("./images/fan.png")
width, height = image.size
size = width * height
histogram = image.histogram()

# g, a = image.split()
# histogram = g.histogram()
# plt.plot([i for i in range(256)], histogram)
# plt.show()

weight_bg = 0
sum_bg = 0
count_bg = 0

sum_total = 0
count_total = 0

variance_list = []
for hist_bin, pix_count in enumerate(histogram):
    sum_total += hist_bin * pix_count
    count_total += pix_count

for hist_bin, pix_count in enumerate(histogram):

    #the sum of the values of the pixels
    sum_bg += hist_bin * pix_count
    sum_fg = sum_total - sum_bg
   
    #the number of pixels
    count_bg += pix_count
    count_fg = count_total - count_bg

    #the probability of a pixel being black/white
    weight_bg = count_bg / count_total
    weight_fg = count_fg / count_total
   
    #the average value of the pixels
    if count_bg != 0:
        mean_bg = sum_bg / count_bg
    else:
        mean_bg = 0

    if count_fg != 0:
        mean_fg = sum_fg / count_fg
    else:
        mean_fg = 0

    mean_diff = mean_bg - mean_fg
    
    #calculating variance
    variance = weight_bg * weight_fg * mean_diff * mean_diff
    variance_list.append(variance)

threshold = variance_list.index(max(variance_list))
new_image = Image.new('1', (width, height))
new_image_pixels = new_image.load()
old_image = image.load()

#recreating the image with the threshold
for x in range(width):
    for y in range(height):
        # g, a = old_image[x, y]
        g = old_image[x, y]
        
        if g > threshold:
            new_image_pixels[x, y] = 1
        else:
            new_image_pixels[x, y] = 0

new_image.save("./images/otsu.png")
