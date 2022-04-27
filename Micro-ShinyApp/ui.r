
## Load and install the packages
library(tidyverse) 
#library(shinythemes)
library(shiny)


# Define UI for the application 
fluidPage(
  #theme = shinytheme("sandstone"),
  tags$head(HTML("<title> Game Theory Tools by Ethan Holdahl </title>")),
navbarPage(
  title = HTML("Game Theory Tools by <a href='https://ethanholdahl.com' style='color:darkgreen'>Ethan Holdahl</a>"),
  tabPanel(
    "Normal Form Games",
    h2("Instructions"),
    helpText(
      "I've created this application to help users practice solving normal form games.
      To practice, simply generate a game, solve it, then use the buttons to reveal the solutions."
    ),
    
    helpText(
      "Recall: When solving normal form games there are 4 large steps we want to follow:"
    ),
    
    helpText(
      "1. Use Iterated Elimination of Dominated Strategies to make the game as small as possible without removing any equilibria from the game"
    ),
    
    helpText("2. Use Best Response Analysis (The order of 1 and 2 may be flipped)"),
    
    helpText("3. Identify any pure strategy Nash Equilibria"),
    
    helpText("4. Identify any mixed strategy Nash Equilibria"),
    
    helpText(
      "In the panel below the game you will be able to select how large of a game you want to generate. The payoffs shown in the generated game are randomly sampled from a list of integers from -10 to 20 with replacement.
      After selecting the size of your game you may select the IEDS, Best Response, Pure NE, and All NE buttons to reveal the strategies dominated, the best reponse correspondance, list of pure strategy Nash Equilibria, and list of all Nash Equilibria in the game."
    ),
    helpText(
      "Note: You may select a game as large as a 9x9, however, currently the application will only report mixed strategy Nash Equilibria from Nx2 or 2xN games (when N can be any size)."
      ),
    helpText("More features will be added soon."),
    helpText("New: You can now click on strategy names to eliminate them and click on payoffs to mark them as a best response.
             Clicking on these elements again removes the annotation. The buttons directly below the table can remove all anotations of a given type at once."),
    
    # Sidebar with a slider input for number of bins
    verticalLayout(
      plotOutput("gametable",
                 click = "plot_click",
                 dblclick = "plot_dblclick",
                 hover = "plot_hover"),
      #verbatimTextOutput("info"),
      wellPanel(
        actionButton("removeHover",
                             "Remove Hover Effects"),
        actionButton("removeEliminations",
                     "Remove Eliminations"),
        actionButton("removeBRs",
                     "Remove Best Response Indicators")
                ),
                      wellPanel(
                        sliderInput(
                          "S1",
                          label = "Number of Strategies for Player 1:",
                          min = 2,
                          max = 9,
                          step = 1,
                          value = 3
                        ),
                        sliderInput(
                          "S2",
                          label = "Number of Strategies for Player 2:",
                          min = 2,
                          max = 9,
                          step = 1,
                          value = 2
                        ),
                        actionButton(
                          "IEDS",
                          "IEDS"
                        ),
                        actionButton(
                          "BR",
                          "Best Response"
                        ),
                        actionButton(
                          "EPayoffs",
                          "Graph of Expected Payoffs (reduced to Nx2 or 2xN only)"
                        ),
                        actionButton(
                          "PureNE",
                          "Pure NE"
                        ),
                        actionButton(
                          "allNE",
                          "All NE"
                        ),
                        actionButton(
                          "clear",
                          "Clear"
                        ),
                        actionButton(
                          "regenerate",
                          "New Game"
                        )
                      ),
                      
                      
                      # Show a plot of the generated distribution 
      br(), 
      plotOutput("resultgame"), 
      br(), 
      plotOutput("P1expectedpayoffs"),
      br(),
      plotOutput("P2expectedpayoffs"),
      br(),
      textOutput("pureNE"), 
      br(), 
      textOutput("allNE")
                      )
                      )
)
)