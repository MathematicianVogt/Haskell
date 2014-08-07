from plot import GraphSolution
import sys


def makeList (start,end,h):
	a=[]
	a.append(start)
	n=(end-start)/h
	
	while(n!=0):	
		new=a[-1] +h
		a.append(a[-1] +h)
		n=n-1
	return a

def generateBase(xList,f):
	
	if(len(xList)==0):
		return []
	elif(xList[0]==0 or len(xList)==1):
		return ([(xList[0],0,0)] + generateBase(xList[1:],f))
	
	else:
		return ([(xList[0],0,(f(xList[0])))] + generateBase(xList[1:],f))

def getThird(a):
	return a[2]

'''def fdm(alpha,startx,endx,startt,endt,dx,dt,f):
	sys.setrecursionlimit(1000000)
	baseSolution=generateBase((makeList(startx,endx,dx)),f)
	totalSoltuion= baseSolution + startCalc(alpha,(makeList(startx,endx,dx)),(makeList(startt,endt,dt))[1:],baseSolution,dx,dt,[],[])
	return totalSoltuion
def startCalc(alpha,xList,tList,phiSolutions,dx,dt,newPhi,newX):
	if(len(tList)==0):
		return []
	elif (len(xList)==1):
		return ([(xList[0],tList[0],0)] + startCalc(alpha,(newX + [xList[0]]),tList[1:],(newPhi + [(xList[0],tList[0],0) ]),dx,dt,[],[]))
	elif (xList[0]==0):
		return ([(xList[0],tList[0],0)] + startCalc(alpha,(xList[1:]),tList,phiSolutions,dx,dt,(newPhi + [(xList[0],tList[0],0)]),(newX + [xList[0]]))) 

	else:
		sol=newPhiSol(xList[0],tList[0],getThird(phiSolutions[0]),getThird(phiSolutions[1]),getThird(phiSolutions[2]),dx,dt,alpha)
		return ([sol] + startCalc(alpha,(xList[1:]),tList,phiSolutions[1:],dx,dt,(newPhi + [sol]),(newX + [xList[0]]))) 
'''

def otherSolve(alpha,startx,endx,startt,endt,dx,dt,f):
	sys.setrecursionlimit(1000000)
	baseSolution=generateBase((makeList(startx,endx,dx)),f)
	totalSoltuion=Solveit(alpha,(makeList(startx,endx,dx)),(makeList(startt,endt,dt))[1:],baseSolution,dx,dt)
	return totalSoltuion

def Solveit(alpha,xList,tList,phiSolutions,dx,dt):
	#print phiSolutions
	totalSoltuion=phiSolutions
	currentLineSolution=phiSolutions
	nextLineSolution=[]
	for t in range(0,len(tList)):
		for x in range(0,len(xList)):
			if(x==0 or x==(len(xList)-1)):
				nextLineSolution.append((xList[x],tList[t],0))
				if(x==(len(xList)-1)):
					totalSoltuion=totalSoltuion+nextLineSolution
					currentLineSolution=nextLineSolution
					nextLineSolution=[]
			else:
				nextLineSolution.append(newPhiSol(xList[x],tList[t],currentLineSolution[0][2],currentLineSolution[1][2],currentLineSolution[2][2],dx,dt,alpha ))
				currentLineSolution=currentLineSolution[1:]
	return totalSoltuion




def newPhiSol(x,t,phiL,phiC,phiR,dx,dt,alpha):
	return (x,t,(phiC + (alpha*(dt/(dx**2)))*(phiR-(2*phiC)+phiL)) )

def showMe(SolutionList):
	GraphSolution(SolutionList)

showMe(otherSolve(50,0,10,0,20,1,.01,(lambda x:(x**2+(5*x)+5))))






