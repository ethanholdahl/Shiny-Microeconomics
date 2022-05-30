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

makeIsoquantCurve = function(prodfun, Q, LMax, smooth = 100, color = "red"){
  prodFun = Ryacas::yac_expr(prodfun)
  L = seq(0, LMax, length.out = smooth)
  L = round(L, 2)
  K = sapply(L, getKValues_Q, prodFun = prodFun, Q = Q)
  K = round(K, 2)
  isoquantCurve=tibble(L = L, K = K, Q = Q)
  if (class(color) == "numeric"){
    color = as.factor(color)
    isoquantCurveGeom = geom_path(data = isoquantCurve, aes(x = L, y = K, color = color))
  } else {
    isoquantCurveGeom = geom_path(data = isoquantCurve, aes(x = L, y = K, group = Q), color = color)
  }
  return(isoquantCurveGeom)
}

makeAllIsoquantCurves = function(prodfun, QList, LMax, smooth = 100){
  prodFun = Ryacas::yac_expr(prodfun)
  L = seq(0, LMax, length.out = smooth)
  L = round(L, 2)
  for (i in 1:length(QList)) {
    K = sapply(L, getKValues_Q, prodFun = prodFun, Q = QList[i])
    K = round(K, 2)
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


### TEST ###
prodfun = "K^(.5)*L^(.5)"
prodFun = Ryacas::yac_expr(prodfun)
MaxQ = 10
NumCurves = 10
LMax = 20


QList = seq(from = MaxQ/NumCurves, to = MaxQ,length.out = NumCurves)
plot = ggplot() +
  makeAllIsoquantCurves(prodfun, QList, LMax, 100) +
  scale_color_viridis_d(begin = .25, end = .85, option="plasma") +
  labs(color = "Q = f(K,L)") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  coord_cartesian(xlim = c(0, LMax), ylim = c(0, LMax))

ggplotly(plot)


## MRTS

# -> need production function

makeMRTSCurve = function(prodfun, Q, LMax, smooth = 100){
  prodFun = Ryacas::yac_expr(prodfun)
  L = seq(0, LMax, length.out = smooth)
  L = round(L, 2)
  K = sapply(L, getKValues_Q, prodFun = prodFun, Q = Q)
  K = round(K, 2)
  MPL=Ryacas::yac_expr(Ryacas::yac_str(paste0("D(L) ",prodfun)))
  MPK=Ryacas::yac_expr(Ryacas::yac_str(paste0("D(K) ",prodfun)))
  isoquantCurve = tibble(L = L, K = K, Q = Q, MRTS_LK = eval(MPL)/eval(MPK))
  MRTSCurveGeom = geom_path(data = isoquantCurve, aes(x = L, y = K, color = MRTS_LK))
  MRTSPointsGeom = geom_point(data = isoquantCurve, aes(x = L, y = K, color = MRTS_LK))
  return(list(MRTSCurveGeom, MRTSPointsGeom))
}

### TEST ###
plot = ggplot()+
  makeIsoquantCurve(prodfun, Q, LMax, color = "black") +
  makeMRTSCurve(prodfun, Q, LMax, 1000)[2] +
  makeMRTSCurve(prodfun, Q, LMax, 1000)[1] +
  scale_color_viridis_c(option = "viridis", limits = c(0, 10), direction = -1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  coord_cartesian(xlim = c(0, LMax), ylim = c(0, LMax))
ggplotly(plot)

# Isocosts

# -> need cost of inputs

makeIsocostLine = function(r, w, C, color = "blue", linetype = "solid"){
  L = seq(0, C/w, length.out = 100)
  L = round(L, 2)
  K = seq(C/r, 0, length.out = 100)
  K = round(K, 2)
  isocostLine = tibble(L = L, K = K)
  isocostLineGeom = geom_path(data = isocostLine, aes(x=L, y=K), color = color, linetype = linetype)  
  return(isocostLineGeom)
}

makeIsocostLines = function(wList, rList, CList, color = TRUE, linetype = "solid"){
  #first make all lists the same length (should all be length 1 or n)
  nLines = max(length(wList), length(rList), length(CList))
  if(length(wList) != nLines){
    wList = rep(wList, nLines)
  }
  if(length(rList) != nLines){
    rList = rep(rList, nLines)
  }
  if(length(CList) != nLines){
    CList = rep(CList, nLines)
  }
  L = as.vector(sapply(CList/wList, seq, from = 0, length.out = 100))
  L = round(L, 2)
  K = as.vector(sapply(CList/rList, seq, to = 0, length.out = 100))
  K = round(K, 2)
  Isocost_Lines = L
  for(i in 0:(nLines-1)){
    Isocost_Lines[(i*100+1):(i*100+100)] = rep(paste0(" C = ", CList[(i+1)], ", w = ", wList[(i+1)], ", r = ", rList[(i+1)]), 100)
  }
  isocostLineData = tibble(L = L, K = K, Isocost_Lines = Isocost_Lines)
  isocostLineData$Isocost_Lines = factor(isocostLineData$Isocost_Lines, levels = unique(isocostLineData$Isocost_Lines))
  if(color == TRUE){
    isocostLinesGeom = geom_line(data = isocostLineData, aes(x = L, y = K, color = Isocost_Lines), linetype = linetype)
  } else {
    isocostLinesGeom = geom_line(data = isocostLineData, aes(x = L, y = K, group = Isocost_Lines), color = color, linetype = linetype)
  }
  return(isocostLinesGeom)
}

isocostLineData$Isocost_Lines = factor(isocostLineData$Isocost_Lines, levels = unique(isocostLineData$Isocost_Lines))

### TEST ###
CList = seq(10,100,10)
r = 2
w = 2
C = 100

plot = ggplot() +
  makeIsocostLines(r, w, CList) +
  scale_color_viridis_d("Budget Lines", option = "mako", begin = .3, end = .7, direction = -1) +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)

ggplotly(plot)

## Cost Minimization

# -> Need Q to produce

# -> need production function

# -> need cost of inputs
prodfun = "K + L"
prodfun = "K + L^2"
prodfun = "K + L^2 + L"
prodfun = "K^.5*L^.5"
prodfun = "K^3+K^(.5)*L^(.5)"
prodFun = Ryacas::yac_expr(prodfun)
Q = 100
r = 2
w = 2

makeCostMin_Point = function(prodfun, Q, w, r, color = "black", size = 3){
  bundle = optimalBundle(Ufun, Px, Py, I)
  x = bundle[1]
  y = bundle[2]
  optimalBundle_PointGeom = annotate("point", x = x, y = y, color = color, size = size)
  return(optimalBundle_PointGeom)
}

calculateProductionLR = function(prodfun, w, r) {
  K = caracas::symbol('K')
  L = caracas::symbol('L')
  Q = caracas::symbol('Q')
  prodFun = caracas::as_sym(prodfun)
  MPL = caracas::der(prodFun, L)
  MPK = caracas::der(prodFun, K)
  #Test if K or L do not have constant returns to scale 
  KisVar = caracas::tex(caracas::der(MPL/MPK, K)) != 0
  LisVar = caracas::tex(caracas::der(MPL/MPK, L)) != 0
  prodFunLR = list()
  Kexpansion = list()
  Lexpansion = list()
  if(!LisVar & !KisVar){
    #Perfect substitutes
    if(caracas::as_expr(MPL)/w > caracas::as_expr(MPK)/r){
      #L more economical at all input levels
      Kexpansion[[1]] = caracas::as_sym(0)
      prodFunLR[[1]] = caracas::subs(prodFun, K, 0)
      Lexpansion[[1]] = caracas::solve_sys(prodFunLR[[1]], Q, L)[[1]]$L
    } else if(caracas::as_expr(MPL)/w < caracas::as_expr(MPK)/r){
      #K more economical at all input levels
      Lexpansion[[1]] = caracas::as_sym(0)
      prodFunLR[[1]] = caracas::subs(prodFun, L, 0)
      Kexpansion[[1]] = caracas::solve_sys(prodFunLR[[1]], Q, K)[[1]]$K
    } else {
      # L and K equally economical at all input levels
      # Set K = L (arbitrarily)
      prodFunLR[[1]] = caracas::subs(prodFun, L, K)
      Kexpansion[[1]] = caracas::solve_sys(prodFunLR[[1]], Q, K)[[1]]$K
      Lexpansion[[1]] = Kexpansion[[1]]
    }
  }
  if(LisVar) Lcritical = try(caracas::solve_sys(MPL/MPK, w/r, L))
  if(KisVar) Kcritical = try(caracas::solve_sys(MPL/MPK, w/r, K))
  if(LisVar | KisVar){
    #Not perfect substitutes. Possible interior solution
    #search for critical values to test for corner solutions
    #Lcrit first
    #add 0 to critical value
    Lcrit = list(caracas::as_sym(0))
    if(length(Lcritical>0)){
      for(i in 1:length(Lcritical)){
        Lcrit[[i+1]] = (Lcritical[[i]]$L)
      }
    }
    lenLcrit = length(Lcrit)
    for(i in 1:lenLcrit){
      Lexpansion_try = Lcrit[[i]]
      prodFunLR_try = caracas::subs(prodFun, L, Lcrit[[i]])
      results = caracas::solve_sys(prodFunLR_try, Q, K)
      if(length(results)==0) next
      Q = 10
      j = length(prodFunLR)
      for(k in 1:length(results)){
        if(!is.complex(eval(caracas::as_expr(results[[k]]$K)))){
          j = j + 1
          Lexpansion[[j]] = Lexpansion_try
          prodFunLR[[j]] = prodFunLR_try
          Kexpansion[[j]]= results[[k]]$K
        }
      }
      Q = caracas::symbol('Q')
    }
    #Kcrit second
    #add 0 to critical value
    Kcrit = list(caracas::as_sym(0))
    if(length(Kcritical>0)){
      for(i in 1:length(Kcritical)){
        Kcrit[[i+1]] = (Kcritical[[i]]$K)
      }
    }
    lenKcrit = length(Kcrit)
    for(i in 1:lenKcrit){
      Kexpansion_try = Kcrit[[i]]
      prodFunLR_try = caracas::subs(prodFun, K, Kcrit[[i]])
      results = caracas::solve_sys(prodFunLR_try, Q, L)
      if(length(results)==0) next
      Q = 10
      j = length(prodFunLR)
      for(k in 1:length(results)){
        if(!is.complex(eval(caracas::as_expr(results[[k]]$L)))){
          j = j + 1
          Kexpansion[[j]] = Kexpansion_try
          prodFunLR[[j]] = prodFunLR_try
          Lexpansion[[j]]= results[[k]]$L
        }
      }
      Q = caracas::symbol('Q')
    }
  }
  
  #LR expansion Path
  LRcost = list()
  Qcrit = c()
  
  #Calculate LR cost curve under possible solutions
  for(i in 1:length(prodFunLR)){
    KinL = caracas::tex(caracas::der(Lexpansion[[i]],K))!=0
    LinK = caracas::tex(caracas::der(Kexpansion[[i]],Q))!=0
    if(LinK) Kexpansion[[i]] = caracas::subs(Kexpansion[[i]], L, Lexpansion[[i]])
    if(KinL) Lexpansion[[i]] = caracas::subs(Lexpansion[[i]], K, Kexpansion[[i]])
    LRcost[[i]] = Lexpansion[[i]]*w + Kexpansion[[i]]*r
  }
  
  #Identify all critical points for LR cost curve
  if(length(prodFunLR) > 1){
    for(i in 1:(length(prodFunLR)-1)){
      for(j in (i+1):length(prodFunLR)){
        Qcritical = caracas::solve_sys(LRcost[[i]], LRcost[[j]], Q)
        if (length(Qcritical) == 0) next
        for(k in length(Qcritical)){
          Qcrit = c(Qcrit, caracas::as_expr(Qcritical[[k]]$Q))
        }
      }
      #add Qcrit for levels of Q at which levels of L or K is 0
      LisVar = caracas::tex(caracas::der(Lexpansion[[i]],Q))!=0
      KisVar = caracas::tex(caracas::der(Kexpansion[[i]],Q))!=0
      if(LisVar){
        result = caracas::solve_sys(Lexpansion[[i]], Q)
        if(length(result)>0){
          for(j in length(result)){
            Qcrit = c(Qcrit, caracas::as_expr(result[[j]]$Q))
          }
        }
      }
      if(KisVar){
        result = caracas::solve_sys(Kexpansion[[i]], Q)
        if(length(result)>0){
          for(j in length(result)){
            Qcrit = c(Qcrit, caracas::as_expr(result[[j]]$Q))
          }
        }
      }
    }
  }
  
  #Test which LR cost curve is positive and least expensive for all levels of output
  if(!is.null(Qcrit)){
    Qcrit = sort(unique(Qcrit[Qcrit>0]))
    Qbins = c(0, Qcrit, max(0, Qcrit)+2)
  } else {
    Qbins = c(0, 2)
  }
    Qtests = c()
  for(i in 1:(length(Qbins)-1)){
    Qtests = c(Qtests, (Qbins[i] + Qbins[i+1])/2)
  }
  
  minLRcostIndex = list()
  for(i in 1:length(Qtests)){
    costTest = c()
    Q = Qtests[i]
    for(j in 1:length(LRcost)){
      Klevel = eval(caracas::as_expr(Kexpansion[[j]]))
      Llevel = eval(caracas::as_expr(Lexpansion[[j]]))
      if(Klevel >= 0 && Llevel >=0){
        costTest = c(costTest, eval(caracas::as_expr(LRcost[[j]])))
      } else {
        costTest = c(costTest, Inf)
      }
    }
    minC = min(costTest[costTest>0])
    minLRcostIndex[[i]] = match(minC, costTest)
  }
  
  #Test if cost function is piecewise
  if (length(minLRcostIndex) > 1){
    #Piecewise. consolidate pieces
    remove = c()
    for(i in 1:(length(minLRcostIndex)-1)){
      if(minLRcostIndex[[i]] == minLRcostIndex[[i+1]]){
        remove = c(remove, i)
      }
    }
    Qcrit = Qcrit[-remove]
    Qbins = Qbins[-(remove+1)]
    minLRcostIndex = minLRcostIndex[-remove]
  }
  Qbins[length(Qbins)] = Inf
  
  #return LR cost function, LR expansion paths, LR prodFun, Piecewise Bounds
  LRExpansion = list()
  for(i in 1:length(minLRcostIndex)){
    LRExpansion[[i]] = list(cost = LRcost[[minLRcostIndex[[i]]]], prodFun = prodFunLR[[minLRcostIndex[[i]]]], Lexpansion = Lexpansion[[minLRcostIndex[[i]]]], Kexpansion = Kexpansion[[minLRcostIndex[[i]]]], Qmin = Qbins[i], Qmax = Qbins[i+1])
  }
  return(LRExpansion)
}

calculateProductionLR(prodfun, w, r)
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







install.packages('caracas')
if (!caracas::has_sympy()) {
  caracas::install_sympy() 
}
