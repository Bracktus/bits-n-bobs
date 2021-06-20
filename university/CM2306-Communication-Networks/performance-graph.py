from mpl_toolkits import mplot3d
from matplotlib import pyplot as plt
from matplotlib import cm 
import numpy as np

def response_time(pps, hps):
    util = hps/pps
    numerator = (util * pps)
    denominator = 2 * (1 - util)
    return numerator/denominator

# need hits/sec to be same len as proc/sec
pps = np.linspace(30, 100, 1000) #jobs processed per sec
hps = np.linspace(0, 15, 1000) #jobs arriving per sec

PPS, HPS = np.meshgrid(pps, hps)
RTIME = response_time(PPS, HPS)

xx,yy = np.meshgrid(pps, hps)
zz = (xx * 0) + 2

ax = plt.axes(projection='3d')
ax.plot_wireframe(PPS, HPS, RTIME, color='black')
ax.plot_surface(xx,yy,zz, color='white')
ax.set_xlabel('Jobs Processed/Sec')
ax.set_ylabel('Jobs Arriving/Sec')
ax.set_zlabel('Response Time');
plt.show()
