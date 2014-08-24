from mpl_toolkits.mplot3d import Axes3D
from scipy.interpolate import griddata
import matplotlib.pyplot as plt
import numpy as np


def GraphSolution(dataList,title,xLabel,yLabel,zLabel):
	data = dataList



	x, y, z = zip(*data)
	z = map(float, z)
	grid_x, grid_y = np.mgrid[min(x):max(x):100j, min(y):max(y):100j]
	grid_z = griddata((x, y), z, (grid_x, grid_y), method='cubic')

	fig = plt.figure()
	ax = fig.gca(projection='3d')
	ax.plot_surface(grid_x, grid_y, grid_z, cmap=plt.cm.Spectral)
	ax.set_xlabel(xLabel)
	ax.set_ylabel(yLabel)
	ax.set_zlabel(zLabel)
	ax.set_title(title)
	plt.show()