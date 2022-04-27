
## Load and install the packages
library("tidyverse", "shiny", "stringr")
theme_set(theme_minimal())


# Define server logic
function(input, output, session) {
  
  MakeGame = function(S1, S2) {
    #This function creates a 3-dimensional array meant to represent payoffs in a 2 player S1 by S2 normal form game.
    
    #Name strategies
    player1strats = paste("row", 1:S1, sep = "")
    player2strats = paste("col", 1:S2, sep = "")
    players = c("Player 1 Payoffs", "Player 2 Payoffs")
    
    #Randomly generate payoffs, create payoff array (game table).
    payoffs = sample(-10:20, (S1 * S2 * 2), replace = TRUE)
    game = array(
      payoffs,
      dim = c(S1, S2, 2),
      dimnames = list(player1strats, player2strats, players)
    )
    return(game)
  }
  
  
  IEDS = function(game) {
    #This function completes the IEDS proccess using only pure strategies for a given game starting with player 1.
    #The input of the function should be a S1 by S2 by 2 numerical array representing payoffs for 2 players in a normal form game where player 1 has S1 strategies and player 2 has S2 strategies.
    #An output of the makegame function is an intended input for this function
    #The output of this function is a list of the dominated strategies and the strategies they were dominated by.
    
    #Initiate list of dominated strategies and the strategies that dominate them. 777 used as a filler to avoid null indexing errors later on.
    dominated = list(c(777), c(777))
    dominators = list(c(777), c(777))
    
    #extract the number of strategies for each player (S1 and S2) from the game
    S1 = dim(game)[1]
    S2 = dim(game)[2]
    
    #create variable to track if new eliminations are made
    finish = FALSE
    
    #Conditional iteration control flow
    while (finish == FALSE) {
      #Switch finish variable to TRUE (will be reassigned to false if eliminations are made in the current iteration)
      finish = TRUE
      
      #EDS for player 1
      for (s1 in 1:(S1 - 1)) {
        #Go to next strategy if current one already dominated
        if (s1 %in% dominated[[1]]) {
          next
        }
        for (s2 in (s1 + 1):S1) {
          #Go to next strategy if current one already dominated
          if (s2 %in% dominated[[1]]) {
            next
          }
          #check if s1 dominates s2
          if (all(game[s1,-dominated[[2]], 1] > game[s2,-dominated[[2]], 1])) {
            #record domination
            dominated[[1]] = c(dominated[[1]], s2)
            dominators[[1]] = c(dominators[[1]], s1)
            #Set finish to false
            finish = FALSE
          }
          #check if s2 dominates s1
          if (all(game[s1,-dominated[[2]], 1] < game[s2,-dominated[[2]], 1])) {
            #record domination
            dominated[[1]] = c(dominated[[1]], s1)
            dominators[[1]] = c(dominators[[1]], s2)
            #Set finish to false
            finish = FALSE
            #go to new s1 if s1 dominated
            break
          }
        }
      }
      
      #EDS for player 2
      for (s1 in 1:(S2 - 1)) {
        #Go to next strategy if current one already dominated
        if (s1 %in% dominated[[2]]) {
          next
        }
        for (s2 in (s1 + 1):S2) {
          #Go to next strategy if current one already dominated
          if (s2 %in% dominated[[2]]) {
            next
          }
          #check if s1 dominates s2
          if (all(game[-dominated[[1]], s1, 2] > game[-dominated[[1]], s2, 2])) {
            #record domination
            dominated[[2]] = c(dominated[[2]], s2)
            dominators[[2]] = c(dominators[[2]], s1)
            #Set finish to false
            finish = FALSE
          }
          #check if s2 dominates s1
          if (all(game[-dominated[[1]], s1, 2] < game[-dominated[[1]], s2, 2])) {
            #record domination
            dominated[[2]] = c(dominated[[2]], s1)
            dominators[[2]] = c(dominators[[2]], s2)
            #Set finish to false
            finish = FALSE
            #go to new s1 if s1 dominated
            break
          }
        }
      }
      
      #input spacer between elimintions (In case I want to animate the iterated process)
      if (finish == FALSE) {
        dominated[[1]] = c(dominated[[1]], 888)
        dominators[[1]] = c(dominators[[1]], 888)
        dominated[[2]] = c(dominated[[2]], 888)
        dominators[[2]] = c(dominators[[2]], 888)
      }
    }
    returnlist = list(dominated, dominators)
    names(returnlist) = c("dominated", "dominators")
    return(returnlist)
  }
  
  
  BRA = function(game, dominated) {
    #This function preforms best response analysis on the remaining strategies in the game.
    #It's inputs are the game being analyzed and the strategies dominated by IEDS
    #The output of this function are lists of the best response correspondance for each player as well as lists of remaining strategies and lists of eliminated strategies.
    
    #retrieve the list of strategies available to each player
    S1 = 1:dim(game)[1]
    S2 = 1:dim(game)[2]
    
    #create list of eliminated strategies by IEDS
    elim1 = dominated[[1]][dominated[[1]] < 100]
    elim2 = dominated[[2]][dominated[[2]] < 100]
    
    #identify strategies remaining after IEDS
    remain1 = S1[!S1 %in% elim1]
    remain2 = S2[!S2 %in% elim2]
    
    #Player 1 BRs
    #initiate BR list
    p1BRs = list()
    
    for (s in remain2) {
      brval = max(game[remain1, s, 1])
      p1BRs[[s]] = remain1[brval == game[remain1, s, 1]]
    }
    
    #Player 2 BRs
    #initiate BR list
    p2BRs = list()
    
    for (s in remain1) {
      brval = max(game[s, remain2, 2])
      p2BRs[[s]] = remain2[brval == game[s, remain2, 2]]
    }
    returnlist = list(p1BRs, p2BRs, remain1, remain2, elim1, elim2)
    names(returnlist) = c("p1BRs", "p2BRs", "remain1", "remain2", "elim1", "elim2")
    return(returnlist)
  }
  
  
  FindPureNE = function(game, p1BRs, p2BRs, remain2) {
    #This function finds and returns a list of all pure strategy NE in a game
    #Inputs are the original game, best response correspondences, and a list of remaining strategies for player 2.
    
    #create empty list for Nash Equilibrias
    NE = list()
    for (s in remain2) {
      for (strats1 in p1BRs[[s]]) {
        for (strats2 in p2BRs[[strats1]]) {
          #Check if best response to the best response to s is indeed s (a Nash Equilibrium).
          if (strats2 == s) {
            #Pure strategy NE reached at strats1, strats2.
            #Create vector form strategy for player 1 and player 2
            s1 = 1:dim(game)[1]
            s2 = 1:dim(game)[2]
            #set other strategies = 0
            s1[s1[!(s1 %in% strats1)]] = 0
            s2[s2[!(s2 %in% strats2)]] = 0
            #set NE pure strat = 1
            s1[strats1] = 1
            s2[strats2] = 1
            #Add to list of NEs
            NE[[length(NE) + 1]] = list(s1, s2)
          }
        }
      }
    }
    return(NE)
  }
  
  
  Find2x2MixedNE = function(game, remain1, remain2) {
    #This function finds and returns a list of the 2x2 mixed strategy Nash Equilibrium from this 2x2 game (if it exists)
    
    #in 2x2: check to make sure no player has a strictly dominant strategy
    if (!all(game[remain1, remain2[1], 2] > game[remain1, remain2[2], 2]) &
        !all(game[remain1, remain2[2], 2] > game[remain1, remain2[1], 2]) &
        !all(game[remain1[1], remain2, 1] > game[remain1[2], remain2, 1]) &
        !all(game[remain1[2], remain2, 1] > game[remain1[1], remain2, 1])) {
      #calculate mix player 2 needs to play to make player 1 indifferent
      remaingame1 = game[remain1, remain2, 1]
      p = (remaingame1[2, 2] - remaingame1[1, 2]) / (remaingame1[1, 1] + remaingame1[2, 2] -
                                                       remaingame1[1, 2] - remaingame1[2, 1])
      
      #calculate mix player 1 needs to play to make player 2 indifferent
      remaingame2 = game[remain1, remain2, 2]
      q = (remaingame2[2, 2] - remaingame2[2, 1]) / (remaingame2[1, 1] + remaingame2[2, 2] -
                                                       remaingame2[1, 2] - remaingame2[2, 1])
      #Mixed strategy NE reached.
      #Create vector form strategy for player 1 and player 2
      s1 = 1:dim(game)[1]
      s2 = 1:dim(game)[2]
      #set other strategies = 0
      s1[s1[!(s1 %in% remain1)]] = 0
      s2[s2[!(s2 %in% remain2)]] = 0
      
      s1[remain1][1] = q
      s1[remain1][2] = 1 - q
      s2[remain2][1] = p
      s2[remain2][2] = 1 - p
      
      #check for any NaNs (the case where one player is always indifferent) if this is the case the mixed strategy will be picked up later by the Continuous NE function
      if (is.nan(p)) {
        return(NULL)
      } else {
        #return the mixed NE as a list
        return(list(s1, s2))
      }
    }
  }
  
  
  FindNx2MixedNE = function(game, remain1, remain2, p1BRs, p2BRs) {
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
    return(MixedNE)
  }
  
  FindContinuous = function(NE) {
    #This function looks across the Nash Equilibria strategy profiles for duplicates in strategies for either player.
    #If a duplicate is found, then the linear combination of the two equilibria constitutes a continuous equilibria.
    
    for (twice in 1:2) {
      #repeat the process twice in case there is a whole area of indifference
      NEContinouos = list()
      #set 777 for fill value to avoid NULL errors.
      remove = c(777)
      #Test for any continuous NE
      if (length(NE) > 1) {
        for (i in 1:(length(NE) - 1)) {
          for (j in (i + 1):length(NE)) {
            if (all(NE[[i]][[1]] == NE[[j]][[1]])) {
              #Continuous NE found, solution is linear combination.
              a = NE[[i]][[2]]
              b = NE[[j]][[2]]
              c = paste(b, "+", (a - b), "* p")
              if (length(NE[[i]]) == 2) {
                #check if already 3rd element to list added through this process
                NEContinouos[[length(NEContinouos) + 1]] = list(NE[[i]][[1]], c, "where p is in the interval [0,1]")
              } else {
                #already 3rd element: "where p is in the interval [0,1]" add second comment for q.
                NEContinouos[[length(NEContinouos) + 1]] = list(NE[[i]][[1]], c, "where p and q are in the interval [0,1]")
              }
              remove = c(remove, i, j)
            }
            if (all(NE[[i]][[2]] == NE[[j]][[2]])) {
              #Continuous NE found, solution is linear combination.
              a = NE[[i]][[1]]
              b = NE[[j]][[1]]
              c = paste(b, "+", (a - b), "* q")
              if (length(NE[[i]]) == 2) {
                #check if already 3rd element to list added through this process
                NEContinouos[[length(NEContinouos) + 1]] = list(c, NE[[i]][[2]], "where q is in the interval [0,1]")
              } else {
                #will have been picked up above
              }
              remove = c(remove, i, j)
            }
          }
        }
      }
      NE = c(NE[-remove], NEContinouos)
    }
    #return list of Nash Equilibria
    return(NE)
  }
  
  GenerateGame = function(S1,S2){
    #Takes number of Player 1 and Player 2 strategies as inputs
    #Returns a normal form game with randomly generated payoffs, list of dominated strategies, best response analysis, list of pure NE, list of all NE (in Nx2 or 2xN games)
    game = MakeGame(S1, S2)
    
    #game = array(c(0, 0, 0, 0, 0, 0, 0, 0), dim = c(2, 2, 2))
    #game = array(c(5, -1, -10, 3, -5, 3, -6, 19, 2, -2, 9, 11, -5, -1, 14, -4, 8, 4), dim = c(3, 3, 2))
    #game = array(c(3, 10, 12, 4, 15, 17, 7, 6, 10, 4, 4, 8), dim = c(2, 3, 2))
    
    IEDSresult = IEDS(game)
    
    #IEDS animation: color row/column bright red + green for dominated + dominant rows/columns. Then bright green fade back to normal and bright red fade to dark red.
    
    dominated = IEDSresult[[1]]
    dominators = IEDSresult[[2]]
    #game
    #game[-dominated[[1]],-dominated[[2]], ]
    
    
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
      NE[[i]][[1]] = round(NE[[i]][[1]],4)
      NE[[i]][[2]] = round(NE[[i]][[2]],4)
    }
    
    #NE
    
    ContinuousNE = FindContinuous(NE)
    
    #ContinuousNE
    
    return(list(game, dominated, dominators, p1BRs, p2BRs, remain1, remain2, PureNE, ContinuousNE))
  }
  
  MakeTableData = function(S1, S2, game){
    #This function takes game info as inputs and returns ggplot ready inputs as outputs
    
    straty = c(.5-(1:S1), rep(.2, each = S2))
    stratx = c(rep(-.2, each = S1), (1:S2)-.5)
    stratnames = c(paste0("s",1:S1), paste0("s",1:S2))
    stratcolors = c(rep("red", each = S1), rep("blue", each = S2))
    
    payoffs = as.vector(game)
    payoffsy =  c(.25-rep(1:S1, times = S2), .75-rep(1:S1, times = S2))
    payoffsx = c(rep(1:S2, each = S1)-.75, rep(1:S2, each = S1)-.25)
    payoffcolors = rep(c("red","blue"), each = S1*S2)
    
    return(list(stratx, straty, stratnames, stratcolors, payoffsx, payoffsy, payoffs, payoffcolors))
  }
  
  IEDSTableData = function(S1, S2, dominated){
    #This function takes game information about dominated strategies as inputs
    #This function returns ggplot ready inputs to depict eliminations
    
    dominatedstrats1 = dominated[[1]][dominated[[1]]<100]
    dominatedstrats2 = dominated[[2]][dominated[[2]]<100]
    
    #initiate ggplot data with values that will not be seen
    elimxmin = c(10)
    elimxmax = c(10)
    elimymin = c(10)
    elimymax = c(10)
    if(length(dominatedstrats1)>0){
      #some strats were dominated
      
      for(s in dominatedstrats1){
        #create ggplot data
        elimxmin = c(0, elimxmin)
        elimxmax = c(S2, elimxmax)
        elimymin = c(-s, elimymin)
        elimymax = c(1-s, elimymax)
      }
    }
    if(length(dominatedstrats2)>0){
      #some strats were dominated
      
      for(s in dominatedstrats2){
        #create ggplot data
        elimxmin = c(s-1, elimxmin)
        elimxmax = c(s, elimxmax)
        elimymin = c(-S1, elimymin)
        elimymax = c(0, elimymax)
      }
    }
    
    return(list(elimxmax, elimxmin, elimymax, elimymin))
  }
  
  BRAall = function(game) {
    #This function preforms best response analysis on all strategies in the game.
    #It's input is the game being analyzed 
    
    #retrieve the list of strategies available to each player
    S1 = 1:dim(game)[1]
    S2 = 1:dim(game)[2]
    
    #Player 1 BRs
    #initiate BR list
    p1BRsall = list()
    
    for (s in S2) {
      brval = max(game[S1, s, 1])
      p1BRsall[[s]] = S1[brval == game[S1, s, 1]]
    }
    
    #Player 2 BRs
    #initiate BR list
    p2BRsall = list()
    
    for (s in S1) {
      brval = max(game[s, S2, 2])
      p2BRsall[[s]] = S2[brval == game[s, S2, 2]]
    }
    returnlist = list(p1BRsall, p2BRsall)
    names(returnlist) = c("p1BRsall", "p2BRsall")
    return(returnlist)
  }
  
  BRTableData = function(game){
    #This function takes a game as it's input and finds all the coordinates of BRs.
    #This function returns ggplot ready inputs to depict BRs
    
    #retrieve the list of strategies available to each player
    S1 = dim(game)[1]
    S2 = dim(game)[2]
    
    BRStars = tibble(x=numeric(), y=numeric(), color=character())
    BRs = BRAall(game)
    for(i in 1:S2){
      for(j in 1:length(BRs[[1]][[i]])){
        BRStars = BRStars %>%
          add_row(x = i - .55, y =-BRs[[1]][[i]][j] + .35, color = "red")
      }
    }
    for(i in 1:S1){
      for(j in 1:length(BRs[[2]][[i]])){
        BRStars = BRStars %>%
          add_row(x = BRs[[2]][[i]][j]-.05, y = -i+.85, color = "blue")
      }
    }
    return(BRStars)
  }
  
  MakeExpectedPayoffGraphs = function(gameinfo) {
    game = gameinfo[[1]]
    dominated = gameinfo[[2]]
    remain1 = gameinfo[[6]]
    remain2 = gameinfo[[7]]
    remainGame = game[-dominated[[1]],-dominated[[2]],]
    remainGame
    P1Payoffs = NULL
    P2Payoffs = NULL
    if (length(remain2) == 2) {
      #2 strategies remaining for player 2
      #make ggplot data
      payoffs = as.vector(remainGame[, , 1])
      q = rep(c(1, 0), each = length(remain1))
      Strat = rep(paste0("S", remain1), 2)
      expectedpayoffs = tibble(payoffs, q, Strat)
      
      P1Payoffs = ggplot(expectedpayoffs, aes(x = q, y = payoffs, color = Strat)) +
        geom_path(size = 1.5) +
        scale_x_reverse(position = "top") +
        theme(plot.title = element_text(hjust = .5, face = "bold", size = 15),
              plot.subtitle = element_text(hjust=0.5, size = 12))+
        guides(size = "none") +
        ggtitle(
          "Expected Payoffs for Player 1's Strategies Given Player 2's Choice of q",
          subtitle = paste0(
            "where q is the probability player 2 plays S",
            remain2[1],
            " and 1-q is the probability player 2 plays S",
            remain2[2]
          )
        ) +
        ylab("Expected Payoff")
    }
    
    
    if (length(remain1) == 2) {
      #2 strategies remaining for player 1
      #make ggplot data
      payoffs = as.vector(remainGame[, , 2])
      payoffs
      q = rep(c(1, 0), times = length(remain2))
      q
      Strat = paste0("S", rep(remain2, each = 2))
      Strat
      expectedpayoffs = tibble(payoffs, q, Strat)
      
      P2Payoffs = ggplot(expectedpayoffs, aes(x = q, y = payoffs, color = Strat)) +
        geom_path(size = 1.5) +
        scale_x_reverse(position = "top") +
        theme(plot.title = element_text(hjust = .5, face = "bold", size = 15),
              plot.subtitle = element_text(hjust=0.5, size = 12))+
        guides(size = "none") +
        ggtitle(
          "Expected Payoffs for Player 2's Strategies Given Player 1's Choice of p",
          subtitle = paste0(
            "where p is the probability player 1 plays S",
            remain1[1],
            " and 1-p is the probability player 1 plays S",
            remain1[2]
          )
        ) +
        ylab("Expected Payoff")
    }
    
    return(list(P1Payoffs, P2Payoffs))
  }
  
  
  
  gameinfo = reactive({
    GenerateGame(input$S1, input$S2)
  })
  
  allBRs = reactive({
    BRAall(gameinfo()[[1]])
  })
  
  tabledata = reactive({
    MakeTableData(input$S1, input$S2, gameinfo()[[1]])
  })
  
  observeEvent(input$EPayoffs, {
      v$ExpectedPayoffGraphs = MakeExpectedPayoffGraphs(gameinfo())
  })
  
  v <- reactiveValues(elimdata = NULL, BR = NULL, pureNE = NULL, allNE = NULL, 
                      hover_strats = tibble(xmin = 8, xmax = 8, ymin = 8, ymax = 8),
                      annotate_strats = tibble(xmin = 8, xmax = 8, ymin = 8, ymax = 8),
                      hover_payoffs = tibble(x = 8, y = 8, color = "red"),
                      annotate_payoffs = tibble(x = 8, y = 8, color = "red"),
                      ExpectedPayoffGraphs = NULL
  )
  
  observeEvent(input$IEDS, {
    v$elimdata = IEDSTableData(input$S1, input$S2, gameinfo()[[2]])
  })
  
  observeEvent(input$plot_hover, {
    hoverx = round(input$plot_hover$x + .5)
    hovery = round(input$plot_hover$y - .5)
    if(hoverx != 0 & hovery != 0){
      #hovering inside game table
      shade = c(7,7,7,7)
      hoverdata = nearPoints(tibble(x = tabledata()[[5]], y = tabledata()[[6]], color = tabledata()[[8]]), input$plot_hover, xvar = "x", yvar = "y", threshold = 20, maxpoints = 1)
       }
    if (hoverx - hovery == 0) {
      shade = c(7,7,7,7)
      hoverdata = tibble(x = 7, y = 7, color = "red")
    } else {
      #hovering over strategies
      if (hoverx == 0) {
        shade = c(0, input$S2, hovery, hovery + 1)
        hoverdata = tibble(x = 7, y = 7, color = "red")
      }
      if (hovery == 0) {
        shade = c(hoverx - 1, hoverx,-input$S1, 0)
        hoverdata = tibble(x = 7, y = 7, color = "red")
      }
    }
    
    v$hover_strats = v$hover_strats %>%
      add_row(xmin = shade[1], xmax = shade[2], ymin = shade[3], ymax = shade[4])
    
    v$hover_strats = distinct(v$hover_strats) %>%
      slice_tail()
    
    v$hover_payoffs = v$hover_payoffs %>%
      add_row(x = hoverdata$x, y = hoverdata$y, color = hoverdata$color)
    
    v$hover_payoffs = distinct(v$hover_payoffs) %>%
      slice_tail()
  })
  
  observeEvent(input$plot_click,{
    #remove hover shading
    v$hover_strats = tibble(xmin = 8, xmax = 8, ymin = 8, ymax = 8)
    v$hover_payoffs = tibble(x = 8, y = 8, color = "red")
    clickx = round(input$plot_click$x +.5)
    clicky = round(input$plot_click$y - .5)
    if(clickx != 0 & clicky != 0){
      #clicking inside game table
      shade = c(7,7,7,7)
      clickdata = nearPoints(tibble(x = tabledata()[[5]], y = tabledata()[[6]], color = tabledata()[[8]]), input$plot_click, xvar = "x", yvar = "y", threshold = 20, maxpoints = 1)
    }
    if (clickx - clicky == 0) {
      shade = c(7,7,7,7)
      clickdata = tibble(x = 8, y = 8, color = "red")
    } else {
      #clicking on strategies
      if (clickx == 0) {
        shade = c(0, input$S2, clicky, clicky + 1)
        clickdata = tibble(x = 8, y = 8, color = "red")
      }
      if (clicky == 0) {
        shade = c(clickx - 1, clickx,-input$S1, 0)
        clickdata = tibble(x = 8, y = 8, color = "red")
      }
    }
    
    #accumulate eliminations
    v$annotate_strats = v$annotate_strats %>%
      add_row(xmin = shade[1], xmax = shade[2], ymin = shade[3], ymax = shade[4])
    
    #undo eliminations
    v$annotate_strats = v$annotate_strats %>%
      group_by(xmin, xmax, ymin, ymax) %>%
      filter(n()==1)%>%
      ungroup()
    
    #accumulate BRs
    v$annotate_payoffs = v$annotate_payoffs %>%
      add_row(x = clickdata$x, y = clickdata$y, color = clickdata$color)
    
    #undo BRs
    v$annotate_payoffs = v$annotate_payoffs %>%
      group_by(x, y, color) %>%
      filter(n()==1)%>%
      ungroup()
    
  })
  
  observeEvent(input$removeHover, {
    v$hover_strats = tibble(xmin = 8, xmax = 8, ymin = 8, ymax = 8)
    v$hover_payoffs = tibble(x = 8, y = 8, color = "red")
    
    })
  
  observeEvent(input$removeEliminations, {
    v$annotate_strats = tibble(xmin = 8, xmax = 8, ymin = 8, ymax = 8)
  })
  
  observeEvent(input$removeBRs, {
    v$annotate_payoffs = tibble(x = 8, y = 8, color = "red")
  })
  
  observeEvent(input$regenerate, {
    #Trick to generate new game. Not sure if there is a "cleaner" method.
    S1 = input$S1
    if (S1 ==2){
      S11 = 3
    } else {
      S11 = S1-1
    }
    updateSliderInput(session, "S1", value = S11)
    updateSliderInput(session, "S1", value = S1)
    
    #reset reactive values
    v$elimdata = NULL
    v$BR = NULL
    v$pureNE = NULL
    v$allNE = NULL
    v$hover_strats = tibble(xmin = 8, xmax = 8, ymin = 8, ymax = 8)
    v$annotate_strats = tibble(xmin = 8, xmax = 8, ymin = 8, ymax = 8)
    v$hover_payoffs = tibble(x = 8, y = 8, color = "red")
    v$annotate_payoffs = tibble(x = 8, y = 8, color = "red")
    v$ExpectedPayoffGraphs = NULL
  })
  
   observeEvent(input$clear, {
    v$elimdata = NULL
    v$BR = NULL
    v$pureNE = NULL
    v$allNE = NULL
    v$ExpectedPayoffGraphs = NULL
  })
   
   observeEvent(input$S1, {
     v$elimdata = NULL
     v$BR = NULL
     v$pureNE = NULL
     v$allNE = NULL
     v$hover_strats = tibble(xmin = 8, xmax = 8, ymin = 8, ymax = 8)
     v$annotate_strats = tibble(xmin = 8, xmax = 8, ymin = 8, ymax = 8)
     v$hover_payoffs = tibble(x = 8, y = 8, color = "red")
     v$annotate_payoffs = tibble(x = 8, y = 8, color = "red")
     v$ExpectedPayoffGraphs = NULL
   })
   
   observeEvent(input$S2, {
     v$elimdata = NULL
     v$BR = NULL
     v$pureNE = NULL
     v$allNE = NULL
     v$hover_strats = tibble(xmin = 8, xmax = 8, ymin = 8, ymax = 8)
     v$annotate_strats = tibble(xmin = 8, xmax = 8, ymin = 8, ymax = 8)
     v$hover_payoffs = tibble(x = 8, y = 8, color = "red")
     v$annotate_payoffs = tibble(x = 8, y = 8, color = "red")
     v$ExpectedPayoffGraphs = NULL
   })
   
   
  output$gametable = renderPlot({
    # plot
    p = ggplot()+
      geom_segment(aes(x = seq(0,input$S2), y = 0, xend = seq(0,input$S2), yend = -input$S1))+
      geom_segment(aes(x = 0, y = seq(0,-input$S1), xend = input$S2, yend = seq(0,-input$S1)))+
      annotate("text", x = tabledata()[[1]], y = tabledata()[[2]], size = 10, label = tabledata()[[3]], color = tabledata()[[4]])+
      annotate("text", x = tabledata()[[5]], y = tabledata()[[6]], size = 10, label = tabledata()[[7]], color = tabledata()[[8]])+
      theme_void()+
      coord_cartesian(xlim =c(-.3, input$S2), ylim = c(-input$S1,.3)) +
      annotate("text", x = v$hover_payoffs$x+.2, y = v$hover_payoffs$y+.1, size = 10, label = "*", color = v$hover_payoffs$color, alpha = .5) + 
      annotate("text", x = v$annotate_payoffs$x+.2, y = v$annotate_payoffs$y+.1, size = 10, label = "*", color = v$annotate_payoffs$color, alpha = 1)+
      annotate("rect", xmin = v$hover_strats$xmin, xmax = v$hover_strats$xmax, ymin = v$hover_strats$ymin, ymax = v$hover_strats$ymax, alpha = .2, fill = "black")+
      annotate("rect", xmin = v$annotate_strats$xmin, xmax = v$annotate_strats$xmax, ymin = v$annotate_strats$ymin, ymax = v$annotate_strats$ymax, alpha = .6, fill = "black")

    p
    })

  
  output$info = renderPrint({
    print(v$annotate_payoffs)
    print(v$hover_payoffs)
    
  })

  
  output$resultgame = renderPlot({
    p = ggplot()+
      geom_segment(aes(x = seq(0,input$S2), y = 0, xend = seq(0,input$S2), yend = -input$S1))+
      geom_segment(aes(x = 0, y = seq(0,-input$S1), xend = input$S2, yend = seq(0,-input$S1)))+
      annotate("text", x = tabledata()[[1]], y = tabledata()[[2]], size = 10, label = tabledata()[[3]], color = tabledata()[[4]])+
      annotate("text", x = tabledata()[[5]], y = tabledata()[[6]], size = 10, label = tabledata()[[7]], color = tabledata()[[8]])+
      theme_void()+
      coord_cartesian(xlim =c(-.3, input$S2), ylim = c(-input$S1,.3))+
      ggtitle("Solutions")+
      theme(plot.title = element_text(hjust = .5, face = "bold", size = 15))+
      annotate("text", x = v$BR$x, y = v$BR$y, size = 10, label = "*", color = v$BR$color)+
      annotate("rect", xmin = v$elimdata[[1]], xmax = v$elimdata[[2]], ymin = v$elimdata[[3]], ymax = v$elimdata[[4]], alpha = .6, fill = "black")
    
    p
  })
  
  output$P1expectedpayoffs = renderPlot({
    v$ExpectedPayoffGraphs[[1]]
  })
  
  output$P2expectedpayoffs = renderPlot({
    v$ExpectedPayoffGraphs[[2]]
  })
  
  
  
  observeEvent(input$BR, {
    v$BR = BRTableData(gameinfo()[[1]])
      })
  
  observeEvent(input$PureNE, {
    v$pureNE = c("The list of pure strategy Nash Equilibria are:", paste(gameinfo()[[8]]))
             })
  
  observeEvent(input$allNE, {
    v$allNE = c("The list of all (both pure and mixed) strategy Nash Equilibria are:", paste(gameinfo()[[9]]))
  })
  
  output$pureNE = renderText(v$pureNE)
  
  output$allNE = renderText(v$allNE)

}
