#Fix bug that occurs when eliminated game is 1x2 or 2x1

install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

S1 = 5
S2 = 2

gameinfo = GenerateGame(S1,S2)
gameinfo
# GAME TABLE GRAPHIC

tabledata = MakeTableData(S1,S2, gameinfo[[1]])
p = ggplot()+
  geom_segment(aes(x = seq(0,S2), y = 0, xend = seq(0,S2), yend = -S1))+
  geom_segment(aes(x = 0, y = seq(0,-S1), xend = S2, yend = seq(0,-S1)))+
  annotate("text", x = tabledata[[1]], y = tabledata[[2]], size = 10, label = tabledata[[3]], color = tabledata[[4]])+
  annotate("text", x = tabledata[[5]], y = tabledata[[6]], size = 10, label = tabledata[[7]], color = tabledata[[8]])+
  theme_void()+
  coord_cartesian(xlim =c(-.3, S2), ylim = c(-S1,.3))

p

# IEDS TABLE GRAPHIC

elimdata = IEDSTableData(S1, S2, gameinfo[[2]])
ggplot()+
  ggtitle("Game after IEDS")+
  geom_segment(aes(x = seq(0,S2), y = 0, xend = seq(0,S2), yend = -S1))+
  geom_segment(aes(x = 0, y = seq(0,-S1), xend = S2, yend = seq(0,-S1)))+
  annotate("text", x = tabledata[[1]], y = tabledata[[2]], size = 10, label = tabledata[[3]], color = tabledata[[4]])+
  annotate("text", x = tabledata[[5]], y = tabledata[[6]], size = 10, label = tabledata[[7]], color = tabledata[[8]])+
  theme_void()+
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 15))+
  coord_cartesian(xlim =c(-.3, S2), ylim = c(-S1,.3)) +
  annotate("rect", xmin = elimdata[[1]], xmax = elimdata[[2]], ymin = elimdata[[3]], ymax = elimdata[[4]], alpha = .6, fill = "black")

BRStars = BRTableData(gameinfo[[1]])


elimdata = IEDSTableData(S1, S2, gameinfo[[2]])
ggplot()+
  ggtitle("Game after IEDS")+
  geom_segment(aes(x = seq(0,S2), y = 0, xend = seq(0,S2), yend = -S1))+
  geom_segment(aes(x = 0, y = seq(0,-S1), xend = S2, yend = seq(0,-S1)))+
  annotate("text", x = tabledata[[1]], y = tabledata[[2]], size = 10, label = tabledata[[3]], color = tabledata[[4]])+
  annotate("text", x = tabledata[[5]], y = tabledata[[6]], size = 10, label = tabledata[[7]], color = tabledata[[8]])+
  theme_void()+
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 15))+
  coord_cartesian(xlim =c(-.3, S2), ylim = c(-S1,.3)) +
  annotate("text", x = BRStars$x, y = BRStars$y, size = 10, label = "*", color = BRStars$color)+
  annotate("rect", xmin = elimdata[[1]], xmax = elimdata[[2]], ymin = elimdata[[3]], ymax = elimdata[[4]], alpha = .6, fill = "black")



MakeExpectedPayoffGraphs(gameinfo)
gameinfo





game = array(c(12,15,9,-7,-1,14,6,4,5,15,14,-10,-7,16,18,3,6,18,-1,6), dim = c(5,2,2))
game = array(c(1,1,4,5),dim=c(2,1,2))
game
IEDSresult = IEDS(game)

#IEDS animation: color row/column bright red + green for dominated + dominant rows/columns. Then bright green fade back to normal and bright red fade to dark red.

dominated = IEDSresult[[1]]
dominators = IEDSresult[[2]]
#game
game[-dominated[[1]],-dominated[[2]], ]


BRAresult = BRA(game, dominated)
p1BRs = BRAresult[[1]]
p2BRs = BRAresult[[2]]
remain1 = BRAresult[[3]]
remain2 = BRAresult[[4]]
elim1 = BRAresult[[5]]
elim2 = BRAresult[[6]]

NE = FindPureNE(game, p1BRs, p2BRs, remain2)
PureNE = NE
if (length(remain1) == 2 & length(remain2) == 2) {
  MixedNE = Find2x2MixedNE(game, remain1, remain2)
  MixedNE
  NE = c(NE, list(MixedNE))
} else {
  if (length(remain1) == 2 | length(remain2) == 2) {
    MixedNE = FindNx2MixedNE(game, remain1, remain2, p1BRs, p2BRs)
    NE = c(NE, MixedNE)
  }
}

NE = NE[which(!(sapply(NE, is.null)))]

for(i in 1:length(NE)){
  NE[[1]][[i]] = round(NE[[1]][[i]],4)
}
  
NE

ContinuousNE = FindContinuous(NE)

#ContinuousNE

#This function finds and returns a list of the 2x2 mixed strategy Nash Equilibrium from this Nx2 or 2xN game (if it exists)
#Possible m>2 x 2 mixed strategy Nash equilibria (if they exist) are linear combinations of these equilibria.

#in 2xN games
MixedNE = list()
if (length(remain1) == 2 & length(remain2) > 2) {
  #Player 2 with more than 2 strategies remaining.
  n = length(remain2)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (!all(p1BRs[[remain2[i]]] == p1BRs[[remain2[j]]]) |
          (length(p1BRs[[remain2[i]]]) + length(p1BRs[[remain2[j]]])) == 4) {
        #find the mixed NE in all 2x2 games that don't share the same BR
        Possible = Find2x2MixedNE(game, remain1, c(remain2[i], remain2[j]))
        #Check to see if the mixed NE in the 2x2 game is also a mixed NE in the 2xN game
        PossiblePayoffs = round(colSums(game[, , 2] * Possible[[1]]),4)
        BR = which(PossiblePayoffs == round(max(PossiblePayoffs),4))
        if (remain2[i] %in% BR & remain2[j] %in% BR) {
          #If true, then  the 2x2 NE is a NE in the 2xN game
          MixedNE[[length(MixedNE) + 1]] = Possible
        }
      }
    }
  }
}

#in Nx2 games
if (length(remain1) > 2 & length(remain2) == 2) {
  #Player 1 with more than 2 strategies remaining.
  n = length(remain1)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (!all(p2BRs[[remain1[i]]] == p2BRs[[remain1[j]]]) |
          (length(p2BRs[[remain1[i]]]) + length(p2BRs[[remain1[j]]])) == 4) {
        #find the mixed NE in all 2x2 games that don't share the same BR
        Possible = Find2x2MixedNE(game, c(remain1[i], remain1[j]), remain2)
        #Check to see if the mixed NE in the 2x2 game is also a mixed NE in the Nx2 game
        PossiblePayoffs = round(rowSums(sweep(game[,,1],2,Possible[[2]],'*')),4)
        BR = which(PossiblePayoffs == round(max(PossiblePayoffs),4))
        if (remain1[i] %in% BR & remain1[j] %in% BR) {
          #If true, then  the 2x2 NE is a NE in the 2xN game
          MixedNE[[length(MixedNE) + 1]] = Possible
        }
      }
    }
  }
}

#test 2