from PIL import Image
from PIL import ImageDraw
import numpy as np              
import matplotlib.pyplot as plt

def drawline(x1,y1,x2,y2):
    print ("1")


with open('data') as f:
    content = f.readlines()

    im = Image.new('RGBA',(1000,1000),(255,255,255,255))
    draw = ImageDraw.Draw(im)
    for line in content:
        points = line.split()
        if (len(points) == 4):
            draw.line((int(float(points[0])),int(float(points[1])),int(float(points[2])),int(float(points[3]))), fill=(0,0,0,0),width=3)

    plt.imshow(im)
    plt.show()
