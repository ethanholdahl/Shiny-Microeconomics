#Fix bug that occurs when eliminated game is 1x2 or 2x1

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("plotly")
install.packages("ggnewscale")
library(tidyverse)
library(ggplot2)
library(plotly)
library(ggnewscale)

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

############# BUNDLES ###############


makeRandomBundles = function(Px, Py, I, xmax, ymax, N){
  x = runif(N, max = xmax)
  y = runif(N, max = ymax)
  return(list(x, y))
}
  
makeRandomFeasibilityGraphs = function(Px, Py, I, xmax, ymax, x, y){
  bundles = tibble(x, y, Cost = x*Px + y*Py, Feasibility = x)
  for (i in 1:(dim(bundles)[1])){
    if(I>=bundles$Cost[i]){
      bundles$Feasibility[i] = "Feasible"
    } else {
      bundles$Feasibility[i] = "Infeasible"
    }
  }
  bundles = bundles %>%
    arrange(Cost)
  
  bundlesPlot = ggplot()+
    geom_point(data = bundles, aes(x = x, y = y, color = Cost), size = 3) +
    scale_color_viridis_c(guide = "none", option = "inferno", begin = .3, end = .9) +
    makeBudgetLine(Px, Py, I) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))
  bundlesPlot = bundlesPlot %>%
    ggplotly()
  
  feasibilityPlot = ggplot()+
    geom_point(data = bundles, aes(x = x, y = y, alpha = Feasibility, color = Cost), size = 3) +
    scale_color_viridis_c(guide = "none", option = "inferno", begin = .3, end = .9) +
    scale_alpha_discrete(range = c(1,.2)) +
    labs(alpha = c("Bundles")) +
    makeBudgetLine(Px, Py, I) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    coord_cartesian(xlim = c(0,xmax), ylim = c(0,ymax))
  feasibilityPlot = feasibilityPlot %>%
    ggplotly() %>%
    layout(showlegend = TRUE)
  return(list(bundlesPlot, feasibilityPlot))
}


bundles = makeRandomBundles(Px, Py, I, 5, ymax, 50)

makeRandomFeasibilityGraphs(5, Py, I, 5, ymax, bundles[[1]], bundles[[2]])

########################### INSURANCE ####################################


##################### MARKET DEMAND + EQ PRICE & QUANTITY ########################

install.packages("devtools")
devtools::install_github("r-cas/ryacas", 
                         build_opts = c("--no-resave-data", "--no-manual"))
library(Ryacas)

getQValue_Choke = function(Dfun, p) {
  Q <- eval(Dfun) 
  return(Q)
}

solvePValue_Choke = function(Dfun, Q, p) crossprod(getQValue_Choke(Dfun, p) - Q)

makePiecewise = function(demandList, NList){
  num = length(demandList)
  chokeList = list()
  chokeVector = c()
  demandNList = list()
  demandNListExpanded = list()
  for (i in 1:num){
    demadExpr = Ryacas::yac_expr(demandList[[i]])
    p = 0
    chokeList[[i]] = round(optimize(solvePValue_Choke, c(0,eval(demadExpr)), Dfun = demadExpr, Q = 0)$minimum,2)
    chokeVector = c(chokeVector, chokeList[[i]])
    demandNList[[i]] = Ryacas::yac_str(paste0(NList[[i]], "*(", demandList[[i]], ")"))
    demandNListExpanded[[i]] = Ryacas::yac('Expand(%)')
  }
  chokes = sort(unique(c(0,chokeVector)))
  demandMarket = list()
  demandMarketSimp = list()
  demandMarketExpanded = list()
  for (i in 1:(length(chokes)-1)){
    #get index of demand functions not choked out
    demandIndicies = (1:length(chokeVector))[chokeVector>chokes[i]]
    for (j in 1:length(demandIndicies)){
      if (j > 1){
        demandMarket[[i]] = Ryacas::yac_str(paste(demandMarket[[i]], demandNList[[demandIndicies[j]]], sep = " + "))
      } else {
        demandMarket[[i]] = Ryacas::yac_str(demandNList[[demandIndicies[j]]])
      }
    }
    demandMarketSimp[[i]] = demandMarket[[i]] %>%
      Ryacas::yac_symbol() %>%
      Ryacas::simplify() %>%
      Ryacas::yac_str()
    demandMarketExpanded[[i]] = Ryacas::yac('Expand(%)')
  }
  return(list(demandNList, demandNListExpanded, chokeList, chokes, demandMarket, demandMarketSimp, demandMarketExpanded))
}

makePiecewisePlot = function(demandNList, chokeList, chokes, demandMarket, supply = 0){
  for (i in 1:length(demandNList)){
    demadExpr = yac_expr(demandNList[[i]])
    p = seq(0,chokeList[[i]], .01)
    Q = sapply(p, getQValue_Choke, Dfun = demadExpr)
    if(i == 1){
      individualDemandData = tibble("Demand_Function" = as.factor(i), p = p, Q = Q) 
    } else {
      individualDemandData = individualDemandData %>%
        bind_rows(tibble("Demand_Function" = as.factor(i), p = p, Q = Q)) 
    }
  }
  
  for (i in 1:length(demandMarket)){
    demadExpr = yac_expr(demandMarket[[i]])
    p = seq(chokes[i],chokes[i+1], .01)
    Q = sapply(p, getQValue_Choke, Dfun = demadExpr)
    if(i == 1){
      piecewiseDemandData = tibble("Demand_Function" = "Market", p = p, Q = Q) 
    } else {
      piecewiseDemandData = piecewiseDemandData %>%
        bind_rows(tibble("Demand_Function" = "Market", p = p, Q = Q)) 
    }
  }
  supplyExpr = yac_expr(supply)
  p = seq(0, chokes[length(chokes)], .01)
  Q = sapply(p, getQValue_Choke, Dfun = supplyExpr)
  supplyData = tibble("Supply_Function" = "Market", p = p, Q = Q)
  
  plist = list()
  for(i in 1:length(demandMarket)){
    plist[[i]] = yac_str(paste0("Solve(", demandMarket[[i]], " == ", supply, ", p)")) %>%
      y_rmvars() %>% 
      yac_expr() %>% 
      eval() %>%
      round(2)
    if(plist[[i]] < chokes[[i+1]] & plist[[i]] >= chokes[[i]]){
      correctPiece = i
    }
  }
  p = plist[[correctPiece]]
  Q = round(eval(yac_expr(supply)),2)
  equilibriumData = tibble(p = p, Q = Q, Equilibrium = "Market Equilibrium")
  Q = seq(0, max(piecewiseDemandData$Q)*1.2, 1)
  equilibriumPriceLine = tibble(p = p, Q = Q, Equilibrium = "Price Level")
  Q = c()
  Qindividual = c()
  for (i in 1:length(chokeList)){
    Qindividual = c(Qindividual, round(eval(yac_expr(demandNList[[i]])),2))
    if(chokeList[[i]] > p){
      Q = c(Q,Qindividual[i])
    }
  }
  equilibriumIndividual = tibble(p = p, Q = Q, Equilibrium = "Group Equilibrium")
  
  demandPlot = ggplot()+
    geom_line(data = individualDemandData, aes(x = Q, y = p, color = Demand_Function)) +
    scale_color_viridis_d(option = "viridis", begin = 0, end = .9, name = "Demand Function") +
    geom_line(data = piecewiseDemandData, aes(x = Q, y = p, group = Demand_Function), color = "darkgreen") + 
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    coord_cartesian(xlim = c(0,max(piecewiseDemandData$Q)), ylim = c(0,chokes[length(chokes)]))
  demandPlot = ggplotly(demandPlot)
  
  marketPlot = ggplot()+
    geom_line(data = individualDemandData, aes(x = Q, y = p, color = Demand_Function)) +
    scale_color_viridis_d(option = "viridis", begin = 0, end = .9, name = "Demand Function") +
    geom_line(data = piecewiseDemandData, aes(x = Q, y = p, group = Demand_Function), color = "darkgreen") +
    geom_line(data = supplyData, aes(x = Q, y = p, group = Supply_Function), color = "red") +
    geom_point(data = equilibriumData, aes(x = Q, y = p, group = Equilibrium), size = 3) +
    geom_line(data = equilibriumPriceLine, aes(x = Q, y = p, group = Equilibrium), linetype = "dashed") +
    geom_point(data = equilibriumIndividual, aes(x = Q, y = p, group = Equilibrium), size = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    coord_cartesian(xlim = c(0,max(piecewiseDemandData$Q)), ylim = c(0,chokes[length(chokes)]))
  marketPlot = ggplotly(marketPlot)
  return(list(demandPlot, marketPlot, plist, correctPiece, equilibriumData$Q, Qindividual))
}

demand1 = "100 - 5*p"
N1 = 6
demand2 = "50 -5*p"
N2 = 4
demand3 = "60 -2*p"
N3 = 10
demand4 = "70 -7*p"
N4 = 4
demand5 = "150 -5*p"
N5 = 6
demand6 = "60 -6*p"
N6 = 8
supply = "100*p-100"
demandList = list(demand1, demand2, demand3, demand4, demand5, demand6)
NList = list(N1, N2, N3, N4, N5, N6)

a = makePiecewise(demandList, NList)

b = makePiecewisePlot(a[[1]], a[[3]], a[[4]], a[[5]], supply)

b[[2]]
