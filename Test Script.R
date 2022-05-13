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



demand1 = "100 - 5*p"
N1 = 6
demand2 = "50 -5*p"
N2 = 4
demand3 = "60 -2*p"
N3 = 10
test = function(demand1, demand2, demand3, N1, N2, N3){
demand1N = yac_str(paste0(N1, "*(", demand1, ")"))
demand2N = yac_str(paste0(N2, "*(", demand2, ")"))
demand3N = yac_str(paste0(N3, "*(", demand3, ")"))

demand12N_simp = yac_str(paste(demand1N, demand2N, sep = " + ")) %>%
  yac_symbol() %>%
  simplify() %>%
  yac_str()
demand12N_expand = yac('Expand(%)')
return(list(demand12N_simp, demand12N_expand))
}

test("100 - 5*p", "50 -5*p", "60 -2*p", 6, 4, 10)



D1 = parse(text = demand1)
D2 = parse(text = demand2)
D3 = parse(text = demand3)
p = 0

getQValue_Choke = function(Dfun, p) {
  Q <- eval(Dfun) 
  return(Q)
}

solvePValue_Choke = function(Dfun, Q, p) crossprod(getQValue_Choke(Dfun, p) - Q)


choke1 = optimize(solvePValue_Choke, c(0,eval(D1)), Dfun = D1, Q = 0)$minimum
choke2 = optimize(solvePValue_Choke, c(0,eval(D2)), Dfun = D2, Q = 0)$minimum
choke3 = optimize(solvePValue_Choke, c(0,eval(D3)), Dfun = D3, Q = 0)$minimum

D12 = parse(text = paste(demand1, demand2, sep = " + "))
D12



