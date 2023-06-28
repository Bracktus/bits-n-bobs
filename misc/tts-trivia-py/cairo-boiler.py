import cairo

# Set up the dimensions and filename for the image
width, height = 400, 300
filename = "ellipse.png"

# Create a new surface with the specified dimensions
surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, width, height)

# Create a new context for drawing
context = cairo.Context(surface)

# Set the background color
context.set_source_rgb(1, 1, 1)  # White
context.rectangle(0, 0, width, height)
context.fill()

# Set the ellipse properties
center_x, center_y = width // 2, height // 2
radius_x, radius_y = 150, 100
rotation = 0

# Translate and rotate the context to the center of the ellipse
context.translate(center_x, center_y)
context.rotate(rotation)

# Scale the context to the ellipse's dimensions
context.scale(radius_x, radius_y)

# Set the ellipse color
context.set_source_rgb(0, 0, 0)  # Black

# Draw the ellipse
context.arc(0, 0, 1, 0, 2 * 3.14159)
context.fill()

# Save the image to a file
surface.write_to_png(filename)

print(f"Ellipse saved to {filename}.")