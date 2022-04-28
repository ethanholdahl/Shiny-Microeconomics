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
inputFunction = "x^2 + 2*x*y"
Ufun = parse(text = inputFunction)
Px=2
Py=2
I=20


getVars = function(Ufun, x, y, Px, Py, I) {
  MUx=D(Ufun, 'x')
  MUy=D(Ufun, 'y')
  MUx = eval(MUx)
  MUy = eval(MUy)
  MRSxy = MUx/MUy
  Cost = x*Px + y*Py
  U = eval(Ufun)
  slopeBL = Px/Py
  return(tibble(MUx, MUy, MRSxy, U, Cost, slopeBL, x, y))
}

solveBundle = function(x, Ufun, Px, Py, I){
  vars = getVars(Ufun, x[1], x[2], Px, Py, I)
  result = crossprod(c(vars$MRSxy, vars$Cost) - c(Px/Py, I))
  return(result)
}

exactBundle = function(Ufun, Px, Py, I, precision = .0001){
  result = optim(c(1,1), solveBundle, Ufun = Ufun, Px = Px, Py = Py, I = I)$par
  result1 = optim(result, solveBundle, Ufun = Ufun, Px = Px, Py = Py, I = I)$par
  diff = abs(result-result1)
  while ((diff>precision)[1]||(diff>precision)[2]){
    result = result1
    result1 = optim(result, solveBundle, Ufun = Ufun, Px = Px, Py = Py, I = I)$par
    diff = abs(result-result1)
  }
  return(result1)
}

optimalBundle = function(Ufun, Px, Py, I) {
  MUx=D(Ufun, 'x')
  MUy=D(Ufun, 'y')
  if (class(MUx) == "numeric" & class(MUy) == "numeric") {
    #linear utility function. Likely want corner Solution
    if (MUx / Px > MUy / Py) {
      #spend all $ on good x
      bundle = c(I / Px, 0)
    } else {
      if (MUx / Px < MUy / Py) {
        #spend all $ on good y
        bundle = c(0, I / Py)
      } else {
        #equality. Any allocation works. Default to spend all $ such that x=y
        bundle = c(I / (Px + Py), I / (Px + Py))
      }
    }
  } else {
    #not linear, use exactBundle()
    bundle = exactBundle(Ufun, Px, Py, I)
  }
}

bundle1 = optimalBundle(Ufun, Px, Py, I)

result1 = round(getVars(Ufun, bundle1[1], bundle1[2], Px, Py, I),2)
result1

Py=1

solveIntermediate = function(x, Ufun, Px, Py, I, U){
  vars = getVars(Ufun, x[1], x[2], Px, Py, I)
  result = crossprod(c(vars$MRSxy, vars$U) - c(Px/Py, U))
  return(result)
}

exactIntermediate = function(Ufun, Px, Py, I, U, precision = .0001){
  result = optim(c(1,1), solveIntermediate, Ufun = Ufun, Px = Px, Py = Py, I = I, U = U)$par
  result1 = optim(result, solveIntermediate, Ufun = Ufun, Px = Px, Py = Py, I = I, U = U)$par
  diff = abs(result-result1)
  while ((diff>precision)[1]||(diff>precision)[2]){
    result = result1
    result1 = optim(result, solveIntermediate, Ufun = Ufun, Px = Px, Py = Py, I = I, U = U)$par
    diff = abs(result-result1)
  }
  return(result1)
}

optimalIntermediate = function(Ufun, Px, Py, I, U) {
  MUx=D(Ufun, 'x')
  MUy=D(Ufun, 'y')
  if (class(MUx) == "numeric" & class(MUy) == "numeric") {
    #linear utility function. Likely want corner Solution
    if (MUx / Px > MUy / Py) {
      #spend all $ on good x
      bundle = c(I / Px, 0)
    } else {
      if (MUx / Px < MUy / Py) {
        #spend all $ on good y
        bundle = c(0, I / Py)
      } else {
        #equality. Any allocation works. Default to spend all $ such that x=y
        bundle = c(I / (Px + Py), I / (Px + Py))
      }
    }
  } else {
    #not linear, use exactIntermediate
    bundle = exactIntermediate(Ufun, Px, Py, I, U)
  }
}


bundle2 = optimalIntermediate(Ufun, Px, Py, I, result1$U)
result2 = round(getVars(Ufun, bundle2[1], bundle2[2], Px, Py, I),2)
result2


########################################### GRAPHS ######################################################

######Indifference Curves######

inputFunction = "x^2+y"
Ufun = parse(text = inputFunction)
Px=2
Py=2
I=20
# UCurves = 5
# Umin = 10
# Umax = 300
xmax = 10
ymax = 10
# U = seq(Umin, Umax, length.out = UCurves)
x = seq(0,xmax,.01)
U = 10

getUValue_U = function(Ufun, x, y) {
  U <- eval(Ufun) 
  return(U)
}

solveYValue_U = function(Ufun, U, x, y) crossprod(getUValue_U(Ufun, x, y) - U)

getYValues_U = function(Ufun, U, x, ymax){
  y = optimize(solveYValue_U, c(0:ymax*5), Ufun = Ufun, U = U, x = x)$minimum
return(y)
}

makeIndifferenceCurve = function(Ufun, U, xmax, ymax, precision = .01, color = "red"){
  x = seq(0, xmax, precision)
  y = sapply(x, getYValues_U, Ufun = Ufun, U = U, ymax = ymax)
  indifferenceCurve=tibble(x = x, y = y, U = as.factor(U))
  if (class(color) == "numeric"){
    color = as.factor(color)
    indifferenceCurveGeom = geom_line(data = indifferenceCurve, aes(x = x, y = y, color = color, group = U))
  } else {
    indifferenceCurveGeom = geom_line(data = indifferenceCurve, aes(x = x, y = y, group = U), color = color)
  }
  return(indifferenceCurveGeom)
}

makeAllIndiffernceCurves = function(Ufun, Ulist, xmax, ymax, precision = .01){
  x = seq(0, xmax, precision)
  for (i in 1:length(Ulist)) {
    y = sapply(x, getYValues_U, Ufun = Ufun, U = Ulist[i], ymax = ymax)
    if (i == 1) {
      indifferenceCurves = tibble(x = x, y = y, U = Ulist[i])
    } else {
      new_u = tibble(x = x, y = y, U = Ulist[i])
      indifferenceCurves = indifferenceCurves %>%
        bind_rows(new_u, id = NULL)
    }
  }
  indifferenceCurves = indifferenceCurves %>%
    arrange(as.numeric(U))
  indifferenceCurvesGeom = geom_line(data = indifferenceCurves, aes(x = x, y = y, color = as.factor(sort(U)), group = U))
}

Ulist = c(20,50,80, 110, 42)
xmax = 1
ggplot()+ makeAllIndiffernceCurves(Ufun, Ulist, xmax, ymax) + scale_color_viridis_d(begin = .1, end = .8, option="plasma")

 
######Budget Lines######

Px=5
Py=2

makeBudgetLine = function(Px, Py, I, color = "blue", linetype = "solid"){
  x = c(0,I/Px)
  y = c(I/Py,0)
  budgetLine = tibble(x,y)
  budgetLineGeom = geom_line(data = budgetLine, aes(x=x, y=y), color = color, linetype = linetype)  
  return(budgetLineGeom)
}

Px=5
Py=2
I=40
xmax = 30
ymax = 30

ggplot() + makeIndifferenceCurve(Ufun, 10, xmax, ymax, .01, 10) + 
  makeIndifferenceCurve(Ufun, 20, xmax, ymax, .01, 20) + 
  makeIndifferenceCurve(Ufun, 30, xmax, ymax, .01, 30) +
  scale_color_viridis_c(begin = .1, end = .8, option="plasma")+
  labs(color = "U")+
  makeBudgetLine(Px, Py, I)+ 
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))

######Income Expansion Path######

#Need: set MRS=slope of budget line, find all y values for a sequence of x values

inputFunction = "x^3 * y^2"
Ufun = parse(text = inputFunction)
Px=5
Py=2
I=20

xmax = 10
ymax = 10
x = seq(0,xmax,.01)

getMRS = function(Ufun, x, y){
  MUx=D(Ufun, 'x')
  MUy=D(Ufun, 'y')
  MRSxy = eval(MUx)/eval(MUy)
  return(MRSxy)
}

solveYValue_IE <- function(Ufun, x, y, Px, Py) crossprod(getMRS(Ufun, x, y) - (Px/Py))

getYValues_IE = function(Ufun, x, ymax, Px, Py){
  y = optimize(solveYValue_IE, c(0:ymax*5), Ufun = Ufun, x = x, Px = Px, Py = Py)$minimum
  return(y)
}

makeIncomeExpansion = function(Ufun, Px, Py, xmax, ymax, precision = .01, color = "darkgreen") {
  MUx = D(Ufun, 'x')
  MUy = D(Ufun, 'y')
  if (class(MUx) == "numeric" & class(MUy) == "numeric") {
    #linear utility function. Likely want corner Solution
    if (MUx / Px > MUy / Py) {
      #spend all $ on good x
      x = c(0, xmax*1.2)
      y = c(0, 0)
    } else {
      if (MUx / Px < MUy / Py) {
        #spend all $ on good y
        x = c(0, 0)
        y = c(0, ymax*1.2)
      } else {
        #equality. Any allocation works. Default to spend all $ such that x=y
        x = c(0, max(xmax*1.2, ymax*1.2))
        y = c(0, max(xmax*1.2, ymax*1.2))
      }
    }
  } else {
    x = seq(precision, xmax*1.2, precision)
    y = sapply(x, getYValues_IE, Ufun = Ufun, ymax = ymax, Px = Px, Py = Py)
  }
  incomeExpansion = tibble(x = x, y = y)
  incomeExpansionGeom = geom_line(data = incomeExpansion, aes(x = x, y = y), color = color)
  return(incomeExpansionGeom)
}


inputFunction = "x^2 +2*x*y"
Ufun = parse(text = inputFunction)
Px=2
Py=1
I=20
xmax = 10
ymax = 10

ggplot() + 
  makeIndifferenceCurve(Ufun, 10, xmax, ymax, .01, 10) + 
  makeIndifferenceCurve(Ufun, 20, xmax, ymax, .01, 20) + 
  makeIndifferenceCurve(Ufun, 30, xmax, ymax, .01, 30) +
  scale_color_viridis_c(begin = .1, end = .8, option="plasma")+
  labs(color = "U")+
  makeBudgetLine(Px, Py, I)+ 
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  makeIncomeExpansion(Ufun, Px, Py, xmax, ymax)+
  coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))

######Constrained Optimization######

#Need: Optimal Bundle, Indifference curve at that utility level, budget line at that utility level

makeOptimalBundle_Point = function(Ufun, Px, Py, I, color = "black", size = 3){
  bundle = optimalBundle(Ufun, Px, Py, I)
  x = bundle[1]
  y = bundle[2]
  optimalBundle_PointGeom = annotate("point", x = x, y = y, color = color, size = size)
  return(optimalBundle_PointGeom)
}

makeOptimalBundle_Indifference = function(Ufun, Px, Py, I, xmax, ymax, precision = .01, color = "red"){
  bundle = optimalBundle(Ufun, Px, Py, I)
  results = round(getVars(Ufun, bundle[1], bundle[2], Px, Py, I),2)
  U = results$U
  x = seq(0, xmax, precision)
  y = sapply(x, getYValues_U, Ufun = Ufun, U = U, ymax = ymax)
  indifferenceCurve=tibble(x = x, y = y, U = as.factor(U))
  if (class(color) == "numeric"){
    color = as.factor(color)
    indifferenceCurveGeom = geom_line(data = indifferenceCurve, aes(x = x, y = y, color = U, group = U))
  } else {
    indifferenceCurveGeom = geom_line(data = indifferenceCurve, aes(x = x, y = y, group = U), color = color)
  }
  return(indifferenceCurveGeom)
}



bundle = optimalBundle(Ufun, Px, Py, I)
results = round(getVars(Ufun, bundle[1], bundle[2], Px, Py, I),2)
U = results$U
inputFunction = "x^2 +2*x*y"
Ufun = parse(text = inputFunction)
Px=2
Py=1
I=20
xmax = 12
ymax = 24
Ulist = c(20,50,80, U)

ggplot() + 
  makeAllIndiffernceCurves(Ufun, Ulist, xmax, ymax)+
  scale_color_viridis_d(begin = .1, end = .8, option="plasma") +
  labs(color = "U(x,y)") +
  makeBudgetLine(Px, Py, I) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  makeIncomeExpansion(Ufun, Px, Py, xmax, ymax) +
  makeOptimalBundle_Point(Ufun, Px, Py, I) +
  coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))


ggplot() + 
  makeOptimalBundle_Indifference(Ufun, Px, Py, I, xmax, ymax, color = 1)+
  scale_color_viridis_d(begin = .6, end = .8, option="plasma") +
  labs(color = "U(x,y)") +
  makeBudgetLine(Px, Py, I) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  makeIncomeExpansion(Ufun, Px, Py, xmax, ymax) +
  makeOptimalBundle_Point(Ufun, Px, Py, I) +
  coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))

######Substitution/Income/Total Effect######

inputFunction = "x^2 +2*x*y"
Ufun = parse(text = inputFunction)
Px=c(2,2)
Py=c(1,2)
I=c(20,20)
xmax = 12
ymax = 24
bundle_1 = optimalBundle(Ufun, Px[1], Py[1], I[1])
results_1 = round(getVars(Ufun, bundle_1[1], bundle_1[2], Px[1], Py[1], I[1]),2)

bundle_2 = optimalIntermediate(Ufun, Px[2], Py[2], I[2], results_1$U)
results_2 = round(getVars(Ufun, bundle_2[1], bundle_2[2], Px[2], Py[2], I[2]),2)

bundle_3 = optimalBundle(Ufun, Px[2], Py[2], I[2])
results_3 = round(getVars(Ufun, bundle_3[1], bundle_3[2], Px[2], Py[2], I[2]),2)

ggplot() + 
  makeOptimalBundle_Indifference(Ufun, Px[1], Py[1], I[1], xmax, ymax, color = 1) +
  makeOptimalBundle_Indifference(Ufun, Px[2], Py[2], I[2], xmax, ymax, color = 1) +
  scale_color_viridis_d(begin = .5, end = .8, option="plasma") +
  labs(color = "U(x,y)") +
  makeBudgetLine(Px[1], Py[1], I[1]) + 
  makeBudgetLine(Px[2], Py[2], I[2], color = "lightblue") + 
  makeBudgetLine(Px[2], Py[2], results_2$Cost, color = "lightblue", linetype = "longdash") + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  makeIncomeExpansion(Ufun, Px[1], Py[1], xmax, ymax) +
  makeIncomeExpansion(Ufun, Px[2], Py[2], xmax, ymax, color = "green") +
  makeOptimalBundle_Point(Ufun, Px[1], Py[1], I[1]) +
  makeOptimalBundle_Point(Ufun, Px[2], Py[2], I[2]) +
  makeOptimalBundle_Point(Ufun, Px[2], Py[2], results_2$Cost) +
  coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))
  

############################# Demand Curve ###########################################


############################ Engle Curve ############################################

