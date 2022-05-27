#Fix bug that occurs when eliminated game is 1x2 or 2x1

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("plotly")
install.packages("ggnewscale")
library(tidyverse)
library(ggplot2)
library(plotly)
library(ggnewscale)
library(Ryacas)

#Want:
###Learn&Practice (Include PDF viewer for notes):
######Derivative rules
######MRS
######Budget Line
######Constrained Optimization
######Income Expansion path
######Engel Curve
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
ymax = 10
xmax = 10
N = 50

########################### INSURANCE ####################################

########### PRODUCTION ##########
## Isoquants

# -> need production function

prodfun = "K^(.5)*L^(.5)"
prodFun = Ryacas::yac_expr(prodfun)

getQValue_Q = function(prodFun, K, L) {
  Q <- eval(prodFun) 
  return(Q)
}

getKValues_Q = function(prodFun, L, Q){
  prodFunK = Ryacas::yac_str(paste0(prodFun," Where L==",L))
  K = try(Ryacas::yac_str(paste0("OldSolve(",prodFunK,"==",Q,", K)")) %>% y_rmvars() %>% yac_expr() %>% eval(), silent = TRUE)
  if (inherits(K, "try-error")){
    K = NA
  }
  return(K)
}

makeIsoquantCurve = function(prodfun, Q, Lmax, precision = .01, color = "red"){
  prodFun = Ryacas::yac_expr(prodfun)
  L = seq(0, Lmax, precision)
  K = sapply(L, getKValues_Q, prodFun = prodFun, Q = Q)
  isoquantCurve=tibble(L = L, K = K, Q = Q)
  if (class(color) == "numeric"){
    color = as.factor(color)
    isoquantCurveGeom = geom_path(data = isoquantCurve, aes(x = L, y = K, color = color))
  } else {
    isoquantCurveGeom = geom_path(data = isoquantCurve, aes(x = L, y = K, group = Q), color = color)
  }
  return(isoquantCurveGeom)
}

makeAllIsoquantCurves = function(prodfun, QList, Lmax, precision = .01){
  prodFun = Ryacas::yac_expr(prodfun)
  L = seq(0, Lmax, precision)
  for (i in 1:length(QList)) {
    K = sapply(L, getKValues_Q, prodFun = prodFun, Q = QList[i])
    if (i == 1) {
      isoquantCurves = tibble(L = L, K = K, Q = QList[i])
    } else {
      new_q = tibble(L = L, K = K, Q = QList[i])
      isoquantCurves = isoquantCurves %>%
        bind_rows(new_q, id = NULL)
    }
  }
  isoquantCurves = isoquantCurves %>%
    arrange(as.numeric(Q)) %>%
    mutate(Q = as.factor(Q))
  isoquantCurvesGeom = geom_path(data = isoquantCurves, aes(x = L, y = K, color = Q))
}


ggplotly(ggplot()+
  makeAllIsoquantCurves(prodfun, c(10,20,30,40,50), 20, precision = .1))




## MRTS

# -> need production function

# Isocosts

# -> need cost of inputs

## Cost Minimization

# -> Need Q to produce

# -> need production function

# -> need cost of inputs

## Firm's SR Expansion Path

# -> need production function

## Firm's LR Expansion Path

# -> need production function

# -> need cost of inputs

## Returns to Scale

# -> need production function

######### COSTS ###########
# Total Cost Curve (TC = VC + FC)
# Average Total Cost Curve (ATC = AVC + AFC)
# MC Curve
# SR vs LR TC Curves
# SR vs LR ATC Curves
# SR vs LR MC Curves
# Economies of Scale


########## PERFECT COMPETITION ##########
# Zoom in animation on supply vs demand -> perfectly elastic; Price takers
# TR, TC, Profit Graphs (with tangent lines and spacers)
# + Option to stack with MC = MR graphs
# MC, P, ATC Profit/Loss Graphs
# AVC, SR Supply Graphs (Individual SR Supply Graph)
# Add together all individual supply Curves -> industry supply
# Producer Surplus
# Perfect Competition in the LR (show price and profit earned by firms, control decision to add/remove firms and have it adjust profit)
# PC in the LR show LR solution and change in total supply
# Solve for N firms in PC in the LR, 


######### MONOPOLY ###########
# Q vs Price (demand curve): Toggle to include revenue, click to keep revenues area
# D, MR, MC graph
# C.S. P.S. D.W.L. in Monopoly 
# Solve for max producer surplus



######### OLIGOPOLY ##########








