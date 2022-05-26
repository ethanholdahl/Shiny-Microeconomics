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
# Isoquants
# MRTS
# Isocosts
# Cost Minimization
# Firm's SR Expansion Path
# Firm's LR Expansion Path
# Returns to Scale

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
# Q vs Price (demand curve): Toggle to include revenue, click to keep revenus area
# D, MR, MC graph
# C.S. P.S. D.W.L. in Monopoly 
# Solve for max producer surplus



######### OLIGOPOLY ##########










######### PIECEWISE MARKET DEMAND ##########


D1 = "50 - 12 * p"
D2 = "75 - 5 * p"
D3 = "70 - 8 * p"
N1 = 1
N2 = 1
N3 = 1
demandList = list(D1, D2, D3)
NList = list(N1, N2, N3)
makePiecewise(demandList, NList)



getQValue_Choke = function(Dfun, p) {
  Q <- eval(Dfun) 
  return(Q)
}

makePiecewise = function(demandList, NList){
  num = length(demandList)
  chokeList = list()
  chokeVector = c()
  demandNList = list()
  demandNListExpanded = list()
  for (i in 1:num){
    demandExpr = Ryacas::yac_expr(demandList[[i]])
    p = 0
    chokeList[[i]] = yac_str(paste0("Solve(",D1,"==0, p)")) %>% y_rmvars() %>% yac_expr() %>% eval()
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
    demadExpr = Ryacas::yac_expr(demandNList[[i]])
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
    demadExpr = Ryacas::yac_expr(demandMarket[[i]])
    p = seq(chokes[i],chokes[i+1], .01)
    Q = sapply(p, getQValue_Choke, Dfun = demadExpr)
    if(i == 1){
      piecewiseDemandData = tibble("Demand_Function" = "Market", p = p, Q = Q) 
    } else {
      piecewiseDemandData = piecewiseDemandData %>%
        bind_rows(tibble("Demand_Function" = "Market", p = p, Q = Q)) 
    }
  }
  supplyExpr = Ryacas::yac_expr(supply)
  p = seq(0, chokes[length(chokes)], .01)
  Q = sapply(p, getQValue_Choke, Dfun = supplyExpr)
  supplyData = tibble("Supply_Function" = "Market", p = p, Q = Q)
  
  plist = list()
  for(i in 1:length(demandMarket)){
    plist[[i]] = Ryacas::yac_str(paste0("Solve(", demandMarket[[i]], " == ", supply, ", p)")) %>%
      Ryacas::y_rmvars() %>% 
      Ryacas::yac_expr() %>% 
      eval() %>%
      round(2)
    if(plist[[i]] < chokes[[i+1]] & plist[[i]] >= chokes[[i]]){
      correctPiece = i
    }
  }
  p = plist[[correctPiece]]
  Q = round(eval(Ryacas::yac_expr(supply)),2)
  equilibriumData = tibble(p = p, Q = Q, Equilibrium = "Market Equilibrium")
  Q = seq(0, max(piecewiseDemandData$Q)*1.2, 1)
  equilibriumPriceLine = tibble(p = p, Q = Q, Equilibrium = "Price Level")
  Q = c()
  Qindividual = c()
  for (i in 1:length(chokeList)){
    Qindividual = c(Qindividual, round(eval(Ryacas::yac_expr(demandNList[[i]])),2))
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





