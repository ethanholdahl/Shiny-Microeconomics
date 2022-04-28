###################### INPUTS NEEDED ######################

inputFunction = "x+y" #U(x,y) function input as a string
Px=5 #Could be multiple for multiple budget lines
Py=2 #Could be multiple for multiple budget lines
I=20 #Could be multiple for multiple budget lines
U = 10 #Could be multiple for multiple indifference curves
xmax = 10 #Viewing area for ggplot
ymax = 10 #Viewing area for ggplot
precision = .01 #How smooth curves should be in plots

###########################################################


######################## FUNCTIONS ########################

Ufun = parse(text = input)
MUx=D(Ufun, 'x')
MUy=D(Ufun, 'y')



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

###################### PLOT FUNCTIONS #####################

######### MAKE INDIFFERENCE CURVE ##########

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

############# MAKE BUDGET LINE #############

makeBudgetLine = function(Px, Py, I, color = "blue"){
  x = c(0,I/Px)
  y = c(I/Py,0)
  budgetLine = tibble(x,y)
  budgetLineGeom = geom_line(data = budgetLine, aes(x = x, y = y), color = color)  
  return(budgetLineGeom)
}

########## MAKE INCOME EXPANSION ###########

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
      x = c(0, xmax)
      y = c(0, 0)
    } else {
      if (MUx / Px < MUy / Py) {
        #spend all $ on good y
        x = c(0, 0)
        y = c(0, ymax)
      } else {
        #equality. Any allocation works. Default to spend all $ such that x=y
        x = c(0, max(xmax, ymax))
        y = c(0, max(xmax, ymax))
      }
    }
  } else {
    x = seq(precision, xmax, precision)
    y = sapply(x, getYValues_IE, Ufun = Ufun, ymax = ymax, Px = Px, Py = Py)
  }
  incomeExpansion = tibble(x = x, y = y)
  incomeExpansionGeom = geom_line(data = incomeExpansion, aes(x = x, y = y), color = color)
  return(incomeExpansionGeom)
}