from PIL import Image
from PIL import ImageDraw
import numpy as np              
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.patches import Rectangle

with open('data') as f:
    content = f.readlines()

    #im = Image.new('RGBA',(1000,1000),(255,255,255,255))
    #draw = ImageDraw.Draw(im)
    fig = plt.figure()
    axis = plt.gca()
    axis.set_xlim([0,1000])
    axis.set_ylim([0,1000])
    for line in content:
        points = line.split()
        if (len(points) == 3):
            x = int(float(points[0]))
            y = int(float(points[1]))
            l = int(float(points[2]))
            axis.add_patch(Rectangle((x,y),l,l,alpha=1))

    #plt.imshow(im)
    plt.show()
