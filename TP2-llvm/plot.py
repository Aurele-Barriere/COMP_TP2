import numpy as np              
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.patches import Rectangle

with open('data') as f:
    content = f.readlines()
    fig = plt.figure(figsize=(1,1))
    axis = plt.gca()
    axis.set_xlim([0,1000])
    axis.set_ylim([0,1000])
    for line in content:
        points = line.split()
        if (len(points) == 3):
            x = int(float(points[0]))
            y = int(float(points[1]))
            l = int(float(points[2]))
            axis.add_patch(Rectangle((x,y),l,l,alpha=1,facecolor="#000000"))
    plt.show()
    fig.savefig('sierpinski.png')
