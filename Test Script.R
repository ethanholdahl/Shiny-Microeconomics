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

######Indifference Curves######

UCurves = 5
Umin = 10
Umax = 300
xmax = 10
ymax = 10
U = seq(Umin, Umax, length.out = UCurves)
x = seq(0,xmax,.01)


fn <- function(Ufun, x, y) {
  U <- eval(Ufun) 
  return(U)
}


yvals = function(x,i){
fn2 <- function(y) crossprod(fn(Ufun,x,y) - c(U[i]))
y = optimize(fn2,c(0:100))$minimum
return(y)
}


for (i in 1:length(U)){
  y = sapply(x, yvals, i=i)
  
  if(i == 1){
    tib=tibble(x=x, y=y, U=as.factor(U[i]))
  } else {
    new_u = tibble(x=x, y=y, U=as.factor(U[i]))
    tib = tib %>%
      bind_rows(new_u, id=NULL)
  }

}


ggplot(data = tib, aes(x=x, y=y, color=U))+
  geom_line()+
  scale_color_viridis_d(begin = .1, end = .9, option="plasma")+
  scale_x_continuous(limits = c(0,xmax))+
  scale_y_continuous(limits = c(0,ymax)) 

######Budget Lines######

######Income Expansion Path######

######Constrained Optimization######

######Substitution/Income/Total Effect######


############################# Demand Curve ###########################################

############################ Engle Curve ############################################
