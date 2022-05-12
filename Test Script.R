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



