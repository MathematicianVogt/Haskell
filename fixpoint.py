import math


def fixedP(f,x0,h):
	while( (f(x0)-x0)>h):
		x0=f(x0)
	return x0



print str(fixedP(eval("lambda x : (x**2 +4)/4.0"),0,.00011))	
