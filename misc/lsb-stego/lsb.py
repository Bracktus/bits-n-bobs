import base64
import bitarray
from PIL import Image
import argparse

def message_to_bitarray(message):
    ba = bitarray.bitarray()
    ba.frombytes(bytes(message, 'utf-8'))
    return [int(i) for i in ba] 

def transform_image(image, bit_array):
    width, height = image.size
    
    if len(bit_array) > width * height * 3:
        print("Message is too long to encode")
        return

    pixels = image.load()
    bit_idx = 0
    
    for pixel_idx in range(0,width*height):
        
        pixel_col = pixel_idx % width
        pixel_row = pixel_idx // width

        red, green, blue, alpha = pixels[pixel_col, pixel_row]
         
        print(f"[-] rgb=({red},{blue},{green})")
        new_red = red
        new_green = green
        new_blue = blue

        if bit_idx < len(bit_array):
            #the original pixel with the last bit replaced
            new_red = (red & ~1) | bit_array[bit_idx]
            bit_idx += 1

        if bit_idx < len(bit_array):
            new_green = (green & ~1) | bit_array[bit_idx]
            bit_idx += 1

        if bit_idx < len(bit_array):
            new_blue = (blue & ~1) | bit_array[bit_idx]
            bit_idx += 1

        print(f"[+] rgb=({new_red},{new_blue},{new_green})")
        pixels[pixel_col, pixel_row] = (new_red, new_green, new_blue, alpha)

    return image

def decode(image):
    width, height = image.size
    pixels = image.load()
    
    extracted = ""
    for pixel_idx in range(0,width* height):

        pixel_col = pixel_idx % width
        pixel_row = pixel_idx // width
 
        red, green, blue, alpha = pixels[pixel_col, pixel_row]

        extracted += str(red % 2)
        extracted += str(green % 2)
        extracted += str(blue % 2)

    #right pad the string so it's a multiple of 8 
    padding = "".join(["0" for i in range(0, len(extracted) % 8)])
    extracted += padding
    
    chars = []
    #split string into blocks of 8 and convert to utf-8
    for i in range(int(len(extracted)/8)):
        byte = extracted[i*8:(i+1)*8]
        character_val = int(''.join([str(bit) for bit in byte]), 2)
        chars.append(chr(character_val))
    
    return "".join(chars)

parser = argparse.ArgumentParser()
base_opts = parser.add_mutually_exclusive_group()
base_opts.add_argument("--encode", "-e", help="hide a message in a file", action="store_true")
base_opts.add_argument("--decode", "-d", help="extract a message from file", action="store_true")
parser.add_argument("--image", "-i", help="the image file you want to hide/extract your message inside")
parser.add_argument("--message", "-m", help="the message you want to hide inside the image")
parser.add_argument("--output", "-o", help="the output file")

args = parser.parse_args()

if args.encode and (args.image is None or args.message is None):
    parser.error("--encode requires --image and --message")

if args.decode and args.image is None:
    parser.error("--decode requires --image")

if args.encode:
    bitarray = message_to_bitarray(args.message)
    
    #open image, create a copy of it and edit the copy    
    image = Image.open(args.image)
    image.save(args.output)
    image = Image.open(args.output)
    
    image = transform_image(image, bitarray)
    image.save(args.output)

elif args.decode:
    #open the image, decode the message and 
    image = Image.open(args.image)
    message = decode(image)
    print(message)

else:
    parser.print_help()
