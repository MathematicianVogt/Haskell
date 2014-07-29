#Ryan  Vogt
#Differential Geo

import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import matplotlib.pyplot as plt
counter=0



#Cycloid

for xx in range(-1,2):
	for yy in range(-1,2):
		a=xx+yy
		x0=.25*xx
		y0=.25*yy
		label="parametric curve x0= " + str(x0) + " y0 = " + str(y0) + " a = " + str(a)
	
		mpl.rcParams['legend.fontsize'] = 10

		fig = plt.figure()
		ax = fig.gca(projection='3d')
		t = np.linspace(-4 * np.pi, 4 * np.pi, 100)
		z = np.linspace(0, 0, 100)
		x = x0*np.cos(t) + (y0-a)*np.sin(t) + a*t
		y = -x0*np.sin(t) + (y0-a)*np.cos(t) + a
		
		ax.plot(x, y, z, label=label)
		ax.legend()

		fig.savefig("/Users/MathematicianVogt/Desktop/Differential Geomotry/plots/"+ str(counter)+ ".png")
		counter=counter+1
#Epicycloid:
for xx in range(1,5):
	for yy in range(1,3):
		x0=.25*xx
		y0=.25*yy
		bigR=xx
		littleR=yy
		label="parametric curve x0= " + str(x0) + " y0 = " + str(y0) + " R = " + str(bigR) + " r = " + str(littleR)
		mpl.rcParams['legend.fontsize'] = 10

		fig = plt.figure()
		ax = fig.gca(projection='3d')
		t = np.linspace(-4 * np.pi, 4 * np.pi, 100)
		z = np.linspace(0, 0, 100)
		x = (x0 - (bigR+littleR))*np.cos(((bigR+littleR)*t)/littleR) - y0*np.sin(((bigR+littleR)*t)/littleR) + (bigR+littleR)*np.cos(t)
		y = (x0 - (bigR+littleR))*np.sin(((bigR+littleR)*t)/littleR) + y0*np.cos(((bigR+littleR)*t)/littleR) + (bigR+littleR)*np.sin(t)
		
		print label
		ax.plot(x, y, z, label=label)
		ax.legend()

		fig.savefig("/Users/MathematicianVogt/Desktop/Differential Geomotry/plots/"+ str(counter)+ ".png")
		counter=counter+1


#Hypocycloid


for xx in range(1,5):
	for yy in range(1,3):
		x0=.25*xx
		y0=.25*yy
		bigR=xx
		littleR=yy
		mpl.rcParams['legend.fontsize'] = 10
		label="parametric curve x0= " + str(x0) + " y0 = " + str(y0) + " R = " + str(bigR) + " r = " + str(littleR)
	

		fig = plt.figure()
		ax = fig.gca(projection='3d')
		t = np.linspace(-4 * np.pi, 4 * np.pi, 100)
		z = np.linspace(0, 0, 100)
		x = (x0 - (bigR-littleR))*np.cos(((bigR-littleR)*t)/littleR) + y0*np.sin(((bigR-littleR)*t)/littleR) + (bigR-littleR)*np.cos(t)
		y = -(x0 - (bigR-littleR))*np.sin(((bigR-littleR)*t)/littleR) + y0*np.cos(((bigR-littleR)*t)/littleR) + (bigR-littleR)*np.sin(t)
		
		ax.plot(x, y, z, label=label)
		ax.legend()

		fig.savefig("/Users/MathematicianVogt/Desktop/Differential Geomotry/plots/"+ str(counter)+ ".png")
		counter=counter+1
		
