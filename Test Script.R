#Fix bug that occurs when eliminated game is 1x2 or 2x1

install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

#Want:
###Learn&Practice (Include PDF viewer for notes):
######Derivative rules
######MRS
######Budget Line
######Constrained Optimization
######Income Expansion path
######Engle Curve
######Income/Substitution Effect (when price of good,income changes)
######Classify: Complements, Substitutes
######Classify: Giffin, Inferior, Normal, Luxury
######Elasticities

###Test:
######
#Input a function
#
input = "x^3 * y^2"
Ufun = parse(text = input)

MUx=D(Ufun, 'x')
MUy=D(Ufun, 'y')


Px=5
Py=2
I=20

getVars <- function(Ufun, x, y, Px, Py, I) {
  MUx = eval(MUx)
  MUy = eval(MUy)
  MRSxy = MUx/MUy
  Cost = x*Px + y*Py
  U = eval(Ufun)
  slopeBL = Px/Py
  return(c(MUx, MUy, MRSxy, U, Cost, slopeBL, x, y))
}

solveBundle = function(x) crossprod(getVars(Ufun, x[1], x[2], Px, Py, I)[c(3,5)] - c(Px/Py, I))

Py=5

exactBundle = function(){
  precision = .0001
  result = optim(c(1,1), solveBundle)$par
  result1 = optim(result, solveBundle)$par
  diff = abs(result-result1)
  while ((diff>precision)[1]||(diff>precision)[2]){
    result = result1
    result1 = optim(result, solveBundle)$par
    diff = abs(result-result1)
  }
  return(result1)
}

bundle1=exactBundle()
result1=round(getVars(Ufun, bundle1[1], bundle1[2], Px, Py, I),2)
result1

Py=1

solveIntermediate = function(x) crossprod(getVars(Ufun, x[1], x[2], Px, Py, I)[c(3,4)] - c(Px/Py, result1[4]))

exactIntermediate = function(){
  precision = .0001
  result = optim(c(1,1), solveIntermediate)$par
  result1 = optim(result, solveIntermediate)$par
  diff = abs(result-result1)
  while ((diff>precision)[1]||(diff>precision)[2]){
    result = result1
    result1 = optim(result, solveIntermediate)$par
    diff = abs(result-result1)
  }
  return(result1)
}

bundle2=exactIntermediate()
result2=round(getVars(Ufun, bundle2[1], bundle2[2], Px, Py, I),2)
result2


########################################### GRAPHS ######################################################