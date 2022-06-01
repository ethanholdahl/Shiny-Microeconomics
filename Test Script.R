#Fix bug that occurs when eliminated game is 1x2 or 2x1

#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("ggnewscale")
#install.packages('caracas')
library(caracas)
if (!caracas::has_sympy()) {
  caracas::install_sympy() 
}
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
  K = NA
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
  isoquantCurve=tibble(L = L, K = K, Isoquant_Line = paste0("Q= ", Q))
  if (class(color) == "numeric"){
    color = as.factor(color)
    isoquantCurveGeom = geom_path(data = isoquantCurve, aes(x = L, y = K, color = Isoquant_Line))
  } else {
    isoquantCurveGeom = geom_path(data = isoquantCurve, aes(x = L, y = K, group = Isoquant_Line), color = color)
  }
  return(isoquantCurveGeom)
}

makeAllIsoquantCurves = function(prodfun, QList, LMax, color = TRUE, smooth = 100){
  QList = sort(QList)
  prodFun = Ryacas::yac_expr(prodfun)
  L = seq(0, LMax, length.out = smooth)
  L = round(L, 2)
  for (i in 1:length(QList)) {
    K = sapply(L, getKValues_Q, prodFun = prodFun, Q = QList[i])
    K = round(K, 2)
    if (i == 1) {
      isoquantCurves = tibble(L = L, K = K, Isoquant_Line = paste0("Q= ", QList[i]))
    } else {
      new_q = tibble(L = L, K = K, Isoquant_Line = paste0("Q= ", QList[i]))
      isoquantCurves = isoquantCurves %>%
        bind_rows(new_q, id = NULL)
    }
  }
  isoquantCurves$Isoquant_Line = factor(isoquantCurves$Isoquant_Line, levels = unique(isoquantCurves$Isoquant_Line))
  if(color == TRUE){
    isoquantCurvesGeom = geom_path(data = isoquantCurves, aes(x = L, y = K, color = Isoquant_Line))
  } else {
    isoquantCurvesGeom = geom_path(data = isoquantCurves, aes(x = L, y = K, group = Isoquant_Line), color = color)
  }
  return(isoquantCurvesGeom)
}


### TEST ###
prodfun = "K^(.5)*L^(.5)"
QMax = 10
NumCurves = 10
LMax = 20
smooth = 100

prodFun = Ryacas::yac_expr(prodfun)
QList = seq(from = QMax/NumCurves, to = QMax, length.out = NumCurves)

plot = ggplot() +
  makeAllIsoquantCurves(prodfun, QList, LMax, smooth = 100) +
  scale_color_viridis_d("Q = f(K,L)", begin = .25, end = .85, option="plasma") +
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
  isoquantCurve = tibble(L = L, K = K, Q = Q, MRTS_LK = round(eval(MPL)/eval(MPK), 2))
  MRTSCurveGeom = geom_path(data = isoquantCurve, aes(x = L, y = K, color = MRTS_LK))
  MRTSPointsGeom = geom_point(data = isoquantCurve, aes(x = L, y = K, color = MRTS_LK))
  return(list(MRTSCurveGeom, MRTSPointsGeom))
}

### TEST ###

makeMRTSCurveGraph = function(prodfun, Q, LMax, smooth = 1000){
  MRTSCurve = makeMRTSCurve(prodfun, Q, LMax, smooth = smooth)
  plotly = ggplotly(ggplot() +
                      makeIsoquantCurve(prodfun, Q, LMax, color = "black") +
                      MRTSCurve[[2]] +
                      MRTSCurve[[1]] +
                      scale_color_viridis_c(option = "viridis", limits = c(0, 10), direction = -1) +
                      geom_hline(yintercept = 0) +
                      geom_vline(xintercept = 0) +
                      coord_cartesian(xlim = c(0, LMax), ylim = c(0, LMax))
  )
  return(plotly)
}

prodfun = "K^(.5)*L^(.5)"
LMax = 40
Q = 10
smooth = 100

makeMRTSCurveGraph(prodfun, Q, LMax, smooth = 1000)

# Isocosts

# -> need cost of inputs

makeIsocostLine = function(r, w, C, color = "blue", linetype = "solid"){
  L = seq(0, C/w, length.out = 100)
  L = round(L, 2)
  K = seq(C/r, 0, length.out = 100)
  K = round(K, 2)
  Isocost_Line = rep(paste0(" C = ", C, ", w = ", w, ", r = ", r))
  isocostLine = tibble(L = L, K = K, Isocost_Line = Isocost_Line)
  isocostLineGeom = geom_path(data = isocostLine, aes(x = L, y = K, group = Isocost_Line), color = color, linetype = linetype)  
  return(isocostLineGeom)
}

makeAllIsocostLines = function(rList, wList, CList, color = TRUE, linetype = "solid"){
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
  Isocost_Line = L
  for(i in 0:(nLines-1)){
    Isocost_Line[(i*100+1):(i*100+100)] = rep(paste0(" C = ", CList[(i+1)], ", w = ", wList[(i+1)], ", r = ", rList[(i+1)]), 100)
  }
  isocostLineData = tibble(L = L, K = K, Isocost_Line = Isocost_Line)
  isocostLineData$Isocost_Line = factor(isocostLineData$Isocost_Line, levels = unique(isocostLineData$Isocost_Line))
  if(color == TRUE){
    isocostLinesGeom = geom_line(data = isocostLineData, aes(x = L, y = K, color = Isocost_Line), linetype = linetype)
  } else {
    isocostLinesGeom = geom_line(data = isocostLineData, aes(x = L, y = K, group = Isocost_Line), color = color, linetype = linetype)
  }
  return(isocostLinesGeom)
}



### TEST ###
CList = seq(10,100,10)
r = 2
w = 2
C = 110

plot = ggplot() +
  makeAllIsocostLines(r, w, CList) +
  #makeIsocostLine(r, w, C) +
  scale_color_viridis_d("Isocost Lines", option = "mako", begin = .3, end = .7, direction = 1) +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)

ggplotly(plot)

## Cost Minimization

# -> Need Q to produce

# -> need production function

# -> need cost of inputs
prodfun = "K + L"
prodfun = "K + L^2"
prodfun = "K + L^3 - L^2"
prodfun = "K^.5*L^.5"
Q = 100
r = 2
w = 2

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
  j = 0
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
  Lcritical = c()
  Kcritical = c()
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
      for(k in 1:length(results)){
        j = j + 1
        Lexpansion[[j]] = Lexpansion_try
        prodFunLR[[j]] = prodFunLR_try
        Kexpansion[[j]]= results[[k]]$K
      }
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
      for(k in 1:length(results)){
        j = j + 1
        Kexpansion[[j]] = Kexpansion_try
        prodFunLR[[j]] = prodFunLR_try
        Lexpansion[[j]]= results[[k]]$L
      }
    }
  }
  
  #LR expansion Path
  LRcost = list()
  Qcrit = c()
  
  #Calculate LR cost curve under possible solutions
  for(i in 1:length(prodFunLR)){
    KinL = caracas::tex(caracas::der(Lexpansion[[i]],K))!=0
    LinK = caracas::tex(caracas::der(Kexpansion[[i]],L))!=0
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

makeCostMinPoints = function(ProductionLR, QList, color = "black", size = 3){
  #ProductionLR may be piecewise. Need to find correct piece.
  point = "Cost Minimization Solution"
  L = c()
  K = c()
  for (Q in QList){
    ProductionLR_Q = findVarsQ(ProductionLR, Q)[[1]]
    L = c(L, round(eval(caracas::as_expr(ProductionLR_Q$Lexpansion)),2))
    K = c(K, round(eval(caracas::as_expr(ProductionLR_Q$Kexpansion)),2))
  }
  data = tibble(L = L, K = K, point = point)
  Point_Geom = geom_point(data = data, aes(x = L, y = K, group = point), color = color, size = size)
  return(Point_Geom)
}

findVarsQ = function(ProductionLR, Q){
  for(i in 1:length(ProductionLR)){
    if(ProductionLR[[i]]$Qmin <= Q && ProductionLR[[i]]$Qmax >= Q) break
  }
  ProductionLR_Q = ProductionLR[[i]]
  C = round(eval(caracas::as_expr(ProductionLR_Q$cost)),2)
  L = round(eval(caracas::as_expr(ProductionLR_Q$Lexpansion)),2)
  K = round(eval(caracas::as_expr(ProductionLR_Q$Kexpansion)),2)
  return(list(ProductionLR_Q = ProductionLR_Q, C = C, L = L, K = K))
}


# add try()
ProductionLR = try(calculateProductionLR(prodfun, w, r), silent = TRUE)
if (inherits(ProductionLR, "try-error")){
  #USE seq(), min(), and max() to graph lines
}


makeCostMinGraph = function(prodfun, w, r, Q, smooth = 100){
  ProductionLR = calculateProductionLR(prodfun, w, r)
  ProductionLR_Q = findVarsQ(ProductionLR, Q)[[1]]
  C = findVarsQ(ProductionLR, Q)[[2]]
  L = findVarsQ(ProductionLR, Q)[[3]]
  K = findVarsQ(ProductionLR, Q)[[4]]
  LMax = 1.5*C/w
  KMax = 1.5*C/r
  
  plotly = ggplotly(ggplot() + 
             makeIsocostLine(r, w, C) +
             makeIsoquantCurve(prodfun, Q, LMax, smooth = smooth) +
             makeCostMinPoints(ProductionLR, Q) +
             geom_hline(yintercept = 0) +
             geom_vline(xintercept = 0) +
             coord_cartesian(xlim = c(0, LMax), ylim = c(0, KMax))
  )
  return(plotly)
}

makeCostMinGraph(prodfun, w, r, Q)


## Firm's SR Expansion Path

# -> need production function
# -> need K
calculateSRExpansion = function(prodfun, K, w, r){
  Kval = K
  K = caracas::symbol('K')
  L = caracas::symbol('L')
  Q = caracas::symbol('Q')
  prodFun = caracas::as_sym(prodfun)
  prodFunSR = caracas::subs(prodFun, K, Kval)
  results = caracas::solve_sys(prodFunSR, Q, L)
  j = 0
  LexpansionSR = list()
  costSR = list()
  if (length(results)>0){
   for(i in 1:length(results)){
     if(caracas::subs(results[[i]]$L, Q, 10)[[1]] > 0){
       j = j+1
       LexpansionSR[[j]] = results[[i]]$L
       costSR[[j]] = LexpansionSR[[j]]*w + Kval*r
     }
   }
  }
  return(list(prodFunSR = prodFunSR, LexpansionSR = LexpansionSR, costSR = costSR, K = Kval))
}

makeSRExpansion = function(K, LMax, color = "orange"){
  L = round(seq(0, LMax*1.2, length.out = 100),2)
  Line = "Short Run Expansion Path"
  data = tibble(L = L, K = K, Line = Line)
  geom = geom_line(data = data, aes(x = L, y = K, group = Line), color = color)
  return(geom)
}

makeSRCostMinPoints = function(ProductionSR, QList, color = "black", size = 3){
    #ProductionLR may be piecewise. Need to find correct piece.
    point = "SR Cost Minimization Solution"
    K = ProductionSR$K
    L = c()
    for (Q in QList){
      L = c(L, round(eval(caracas::as_expr(ProductionSR$LexpansionSR[[1]])),2))
    }
    data = tibble(L = L, K = K, point = point)
    Point_Geom = geom_point(data = data, aes(x = L, y = K, group = point), color = color, size = size)
    return(Point_Geom)
}

### test ###

makeSRProductionGraph = function(prodfun, K, w, r, LMax, color = "orange", smooth = 100){
  prodFunSR = calculateSRExpansion(prodfun, K, w, r)$prodFunSR
  L = seq(0, LMax, length.out = smooth) %>%
    round(2)
  Q = eval(caracas::as_expr(prodFunSR)) %>%
    round(2)
  Line = "Short Run Production"
  data = tibble(L = L, Q = Q, Line = Line)
  plotly = ggplotly(ggplot() +
                      geom_line(data = data, aes(x = L, y = Q, group = Line), color = color) +
                      geom_hline(yintercept = 0) +
                      geom_vline(xintercept = 0) +
                      coord_cartesian(xlim = c(0, NA), ylim = c(0, NA))
  )
  return(plotly)
}

K = 10
w = 2
r = 2
prodfun = "K^.5*L^.5"
LMax = 40
smooth = 100

makeSRProductionGraph(prodfun, K, w, r, LMax, smooth = smooth)


## Firm's LR Expansion Path

# -> need production function

# -> need cost of inputs



makeLRExpansion = function(ProductionLR, QMax, color = "darkgreen"){
  Q = round(seq(0, QMax, length.out = 100),2)
  rProductionLR = try(calculateProductionLR(prodfun, w, r), silent = TRUE)
  if (inherits(ProductionLR, "try-error")){
    #USE seq(), min(), and max() to graph lines
  }
  result = sapply(Q, findVarsQ, ProductionLR = ProductionLR)
  L = round(unlist(result[3,]),2)
  K = round(unlist(result[4,]),2)
  Line = "Long Run Expansion Path"
  data = tibble(L = L, K = K, Line = Line)
  geom = geom_line(data = data, aes(x = L, y = K, group = Line), color = color)
  return(geom)
}


makeLRvSRExpansionGraph = function(prodfun, w, r, Q, smooth = 100){
  ProductionLR = calculateProductionLR(prodfun, w, r)
  result = findVarsQ(ProductionLR, Q)
  ProductionLR_Q = result[[1]]
  C = result[[2]]
  L = result[[3]]
  K = result[[4]]
  LMax = 1.5*C/w
  KMax = 1.5*C/r
  QMax = Q*4
  
  plotly = ggplotly(ggplot() + 
                      makeIsocostLine(r, w, C) +
                      makeIsoquantCurve(prodfun, Q, LMax, smooth = smooth) +
                      geom_hline(yintercept = 0) +
                      geom_vline(xintercept = 0) +
                      makeSRExpansion(K, LMax) +
                      makeLRExpansion(ProductionLR, QMax) +
                      makeCostMinPoints(ProductionLR, Q) +
                      coord_cartesian(xlim = c(0, LMax), ylim = c(0, KMax))
  )
  return(plotly)
}

prodfun = "L^.5*K^.5"
r = 2
w = 2
Q = 10
makeLRvSRExpansionGraph(prodfun, w, r, Q, smooth = 100)


makeLRvSRCostExpansionGraph = function(prodfun, w, r, Q, QList, smooth = 100){
  ProductionLR = calculateProductionLR(prodfun, w, r)
  result = findVarsQ(ProductionLR, Q)
  ProductionLR_Q = result[[1]]
  C = result[[2]]
  L = result[[3]]
  K = result[[4]]
  LMax = 1.5*C/w
  KMax = 1.5*C/r
  QList = c(QList, Q)
  QMax = max(QList)
  results = sapply(QList, findVarsQ, ProductionLR = ProductionLR)
  CList = unlist(results[2,])
  ProductionSR = calculateSRExpansion(prodfun, K, w, r)
  costSR = ProductionSR$costSR[[1]]
  for(q in QList){
    Q = q
    CList = c(CList, round(eval(caracas::as_expr(costSR)),2))
  }
  CList = unique(CList)
  plotly = ggplotly(ggplot() + 
             makeAllIsocostLines(r, w, CList, color = "blue") +
             makeAllIsoquantCurves(prodfun, QList, LMax, color = "red", smooth = smooth) +
             geom_hline(yintercept = 0) +
             geom_vline(xintercept = 0) +
             makeSRExpansion(K, LMax) +
             makeLRExpansion(ProductionLR, QMax*2) +
             makeSRCostMinPoints(ProductionSR, QList) +
             makeCostMinPoints(ProductionLR, QList) + 
             coord_cartesian(xlim = c(0, LMax), ylim = c(0, KMax))
  )
  return(plotly)
}

makeLRvSRCostExpansionGraph(prodfun, w, r, Q, QList, smooth = 100)


makeFirmExpansionGraph = function(prodfun, QMax, QNum, w, r, smooth = 100){
  QList = seq(QMax/QNum, QMax, length.out = QNum)
  ProductionLR = calculateProductionLR(prodfun, w, r)
  results = sapply(QList, findVarsQ, ProductionLR = ProductionLR)
  CList = unlist(results[2,])
  LMax = max(CList)/w
  KMax = max(CList)/r
  plotly = ggplotly(ggplot() +
    makeAllIsocostLines(r, w, CList, color = "blue") +
    makeAllIsoquantCurves(prodfun, QList, LMax*1.5, smooth = smooth) +
    scale_color_viridis_d("Q = f(K,L)", begin = .25, end = .85, option="plasma") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    makeLRExpansion(ProductionLR, QMax*2) +
    coord_cartesian(xlim = c(0, LMax), ylim = c(0, KMax))
  )
  plot = ggplot() +
    makeAllIsocostLines(r, w, CList) +
    scale_color_viridis_d("Isocost Lines", option = "mako", begin = .3, end = .7, direction = 1) +
    new_scale("color") +
    makeAllIsoquantCurves(prodfun, QList, LMax*1.5, smooth = smooth) +
    scale_color_viridis_d("Q = f(K,L)", begin = .25, end = .85, option="plasma") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    makeLRExpansion(ProductionLR, QMax*2) +
    coord_cartesian(xlim = c(0, LMax), ylim = c(0, KMax))
  return(list(plotly, plot))
}

test = makeFirmExpansionGraph(prodfun, QMax, QNum, w, r, smooth = 100)
test[[1]]
test[[2]]



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

