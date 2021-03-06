
# Load and install the packages
library(shiny)
library(tidyverse)
#library(shinythemes)
library(plotly)
library(shinyjs)
library(ggnewscale)
library(shinyWidgets)
library(shinycssloaders)
library(Ryacas)
library(caracas)

# Define UI for the application
fluidPage(
  #theme = shinytheme("sandstone"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom_css.css"),
    HTML("<title>Intermediate Microeconomics by Ethan Holdahl </title>")
  ),
  navbarPage(
    id = "bannerTabs",
    title = HTML(
      "Intermediate Microeconomics by <a href='https://ethanholdahl.com' style='color:darkgreen'>Ethan Holdahl</a>"
    ),
    useShinyjs(),
    tabPanel(
      "Home",
      value = "Home",
      withMathJax(),
      HTML("<h3>More Content Coming Soon</h3>
      <p>Please allow a short time for the interactive assets to load into your browser.</p>
      <p>List of currently published content:</p>
      <h4>
      <ul>
      <li> Study Pages (notes and accompanying interactive graphs)</li>
      <ul>
      <li><a href='https://shiny.ethanholdahl.com/Micro/?url=ConsumerStudy' style='color:darkgreen'>Consumer Behavior</a></li>
      <li><a href='https://shiny.ethanholdahl.com/Micro/?url=DemandStudy' style='color:darkgreen'>Individual and Market Demand</a></li>
      <li><a href='https://shiny.ethanholdahl.com/Micro/?url=ProductionStudy' style='color:darkgreen'>Production</a></li>
      </ul>
      <li> Practice Pages (user defined test-like questions with an answer validator and step by step solutions) </li>
      <ul>
      <li><a href='https://shiny.ethanholdahl.com/Micro/?url=ProductionPractice' style='color:darkgreen'>Production</a></li>
      <li><a href='https://shiny.ethanholdahl.com/Micro/?url=MarketPractice' style='color:darkgreen'>Market Structures</a></li>
      <ul>
      <li> Perfect Competition </li>
      <li> Monopolies </li>
      <li> Oligopolies </li>
      </ul>
      </ul>
      </ul>
      </h4>
      ")
    ),
    ###### Consumer Behavior ######
    navbarMenu(
      "Consumer Behavior",
      tabPanel(
        "Study",
        value = "ConsumerStudy",
        h1("Chapter 4: Consumer Behavior"),
        p(
          "Class Notes and interactive content to help make the concepts more engaging"
        ),
        fluidRow(
          column(6,
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Derivatives.pdf"),
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Utility.pdf")
          ),
          column(6,
          textInput(
            inputId = "UtilityFunction",
            label = "Input a differentiable utility function with arguments x and y. Be sure not to omit the multiplication sign (*).",
            value = "4*x+3*y+x^2*y^3+17"
          ),
          actionButton(inputId = "RunUtilityFunction",
                       label = "Calculate"),
          br(),
          HTML("The Function you entered appears as:"),
          p("U(x,y) =", textOutput(outputId = "UtilityFunction", inline = T)),
          HTML("MUx, the derivative of U(x,y) with respect to x is:"),
          p("MUx =", textOutput(outputId = "UtilityMUx", inline = T)),
          HTML("MUy, the derivative of U(x,y) with respect to y is:"),
          p("MUy =", textOutput(outputId = "UtilityMUy", inline = T)%>% withSpinner(color="#004623")),
        )
        ),
        fluidRow(
          column(6,
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Indifference_Curves.pdf")
          ),
          column(6,
          textInput(
            inputId = "IndifferenceFunction",
            label = "Input a differentiable utility function with arguments x and y. Be sure not to omit the multiplication sign (*).",
            value = "x^.6 * y^.4"
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IndifferenceNumCurves",
              label = "Number of Indifference Curves",
              value = 10,
              min = 1,
              max = 100,
              step = 1,
              width = '220px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IndifferenceMaxU",
              label = "Max Utility Level",
              value = 10,
              min = 1,
              width = '150px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IndifferenceXMax",
              label = "Maximum x value of plot",
              value = 10,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IndifferenceYMax",
              label = "Maximum y value of plot",
              value = 40,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          actionButton(inputId = "RunIndifferencePlot",
                       label = "Draw Graph"),
          plotlyOutput("IndifferencePlot")%>% withSpinner(color="#004623")
          ),
        ),
        fluidRow(
          column(6,
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "MRS.pdf")
          ),
          column(6,
          textInput(
            inputId = "MRSFunction",
            label = "Input a differentiable utility function with arguments x and y. Be sure not to omit the multiplication sign (*).",
            value = "x * y"
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "MRSU",
              label = "Utility Level",
              value = 10,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "MRSXMax",
              label = "Maximum x value of plot",
              value = 9,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "MRSYMax",
              label = "Maximum y value of plot",
              value = 9,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          actionButton(inputId = "RunMRSPlot",
                       label = "Draw Graph"),
          plotlyOutput("MRSPlot")%>% withSpinner(color="#004623")
          )
        ),
        fluidRow(
          column(6,
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Budget_Constraints.pdf"),0
          ),
          column(6,
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "BudgetI",
              label = "Income ($)",
              value = 20,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "BudgetPx",
              label = "Price of good x ($)",
              value = 2,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "BudgetPy",
              label = "Price of good y ($)",
              value = 3,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "BudgetXMax",
              label = "Maximum x value of plot|bundles",
              value = 10,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "BudgetYMax",
              label = "Maximum y value of plot|bundles",
              value = 8,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "BudgetN",
              label = "Number of Bundles",
              value = 50,
              min = 1,
              step = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            materialSwitch(
              inputId = "BudgetFeasibility",
              label = "Show Feasibility",
              value = FALSE,
              status = "success",
              width = '200px'
            )
          ),
          actionButton(inputId = "RunBudgetNewBundles",
                       label = "Create New Set of Bundles"),
          actionButton(inputId = "RunBudgetPlot",
                       label = "Draw Graph"),
          plotlyOutput("BudgetPlot")%>% withSpinner(color="#004623")
          )
        ),
        fluidRow(
          column(6,
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Constrained_Optimization.pdf")
          ),
          column(6,
          div(
            style = "display:inline-block",
            textInput(
              inputId = "ConstrainedUtilityFunction",
              label = "Input a differentiable utility function with arguments x and y. Be sure not to omit the multiplication sign (*).",
              value = "x^2 + 2 * x * y"
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "ConstrainedUtilityXMax",
              label = "Maximum x value of plot",
              value = 12,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "ConstrainedUtilityYMax",
              label = "Maximum y value of plot",
              value = 10,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "ConstrainedUtilityI",
              label = "Income ($)",
              value = 20,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "ConstrainedUtilityPx",
              label = "Price of good x ($)",
              value = 2,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "ConstrainedUtilityPy",
              label = "Price of good y ($)",
              value = 3,
              min = 1,
              width = '200px'
            )
          ),
          actionButton(inputId = "RunConstrainedUtilityPlot",
                       label = "Draw Graph"),
          plotlyOutput("ConstrainedUtilityPlot")%>% withSpinner(color="#004623")
        )
        )
      ),
      
      tabPanel("Practice",
               value = "ConsumerPractice",
               h1("Coming Soon")),
      tabPanel("Graphs",
               value = "ConsumerGraphs",
               h1("Coming Soon"))
    ),
    ###### Individual and Market Demand ######
    navbarMenu(
      "Individual and Market Demand",
      tabPanel(
        "Study",
        value = "DemandStudy",
        h1("Chapter 5: Individual and Market Demand"),
        p(
          "Class Notes and interactive content to help make the concepts more engaging"
        ),
        fluidRow(
          column(6,
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Income_Expansion_Path.pdf")
          ),
          column(6,
          div(
            style = "display:inline-block",
            textInput(
              inputId = "IncomeExpansionFunction",
              label = "Input a differentiable utility function with arguments x and y. Be sure not to omit the multiplication sign (*).",
              value = "x * y"
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncomeExpansionPx",
              label = "Initial Price of good x ($)",
              value = 3,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncomeExpansionPy",
              label = "Initial Price of good y ($)",
              value = 2,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncomeExpansionIMax",
              label = "Largest Level of Income ($)",
              value = 20,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncomeExpansionINum",
              label = "Number of Income Levels",
              value = 10,
              min = 1,
              step = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncomeExpansionXMax",
              label = "Maximum x value of plot",
              value = 10,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncomeExpansionYMax",
              label = "Maximum y value of plot",
              value = 10,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          actionButton(inputId = "RunIncomeExpansionPlot",
                       label = "Draw Graph"),
          plotlyOutput("IncomeExpansionPlot")%>% withSpinner(color="#004623"),
          p("Note that the income expansion path is the collection of all of the utility maximizing bundles subject to a budget constraint at every income level.
            Recall how we find a utility maximizing bundle subject to a budget constraint: we set MRSxy = the slope of the budget line. Then we used systems of equations with the budget line to find the utility maximizing bundle.
            The first step to constrained optimization: setting MRSxy = the slope of the budget line gives us the relationship between x and y that maximizes our utility independant of our budget.
            This means that setting MRSxy = the slople of the budget line is the equation that gives us the income expansion path.")
          )
        ),
        fluidRow(
          column(6,
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Engel_Curves.pdf")
          ),
          column(6,
          p("The paramaters governing the dynamics for the Engel Curves are taken from the Income Expansion Path paramaters since they are so intertwined."),
          actionButton(inputId = "RunEngelPlots",
                       label = "Draw Graph"),
          plotlyOutput("EngelPlots")%>% withSpinner(color="#004623"),
          p("An Engel plot is simply the income expansion path with just one good placed on the x axis and income placed on the y axis. It is useful foor identifying income ranges over which goods are normal or inferior."),
          )
        ),
        fluidRow(
          column(6,
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Derived_Demand.pdf")
        ),
        column(6,
          div(
            style = "display:inline-block",
            textInput(
              inputId = "DerivedDemandFunction",
              label = "Input a differentiable utility function with arguments x and y. Be sure not to omit the multiplication sign (*).",
              value = "x^2 +2*x*y"
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "DerivedDemandXMax",
              label = "Maximum x value of plot",
              value = 10,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "DerivedDemandYMax",
              label = "Maximum y value of plot",
              value = 6,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "DerivedDemandI",
              label = "Income ($)",
              value = 16,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "DerivedDemandPy",
              label = "Price of good y ($)",
              value = 2,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "DerivedDemandPxmin",
              label = "Min price of x used to make demand curve",
              value = 1.5,
              min = .5,
              step = .5,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "DerivedDemandPxmax",
              label = "Max price of x used to make demand curve",
              value = 10,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "DerivedDemandPointMax",
              label = "Max price of x",
              value = 10,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "DerivedDemandPointN",
              label = "Number of prices for x",
              value = 5,
              min = 1,
              width = '200px'
            )
          ),
          actionButton(inputId = "RunDerivedDemandPlot",
                       label = "Draw Graph"),
          plotlyOutput("DerivedDemandPlot")%>% withSpinner(color="#004623"),
        )
        ),
        fluidRow(
          column(6,
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Income&Substitution_Effects.pdf")
          ),
          column(6,
          div(
            style = "display:inline-block",
            textInput(
              inputId = "IncSubEffectsFunction",
              label = "Input a differentiable utility function with arguments x and y. Be sure not to omit the multiplication sign (*).",
              value = "x * y"
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncSubEffectsXMax",
              label = "Maximum x value of plot",
              value = 10,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncSubEffectsYMax",
              label = "Maximum y value of plot",
              value = 10,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncSubEffectsI",
              label = "Initial Income ($)",
              value = 20,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncSubEffectsPx",
              label = "Initial Price of good x ($)",
              value = 4,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncSubEffectsPy",
              label = "Initial Price of good y ($)",
              value = 2,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncSubEffectsI1",
              label = "New Income ($)",
              value = 20,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncSubEffectsPx1",
              label = "New Price of good x ($)",
              value = 4,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IncSubEffectsPy1",
              label = "New Price of good y ($)",
              value = 6,
              min = 1,
              width = '200px'
            )
          ),
          actionButton(inputId = "RunIncSubEffectsPlot",
                       label = "Draw Graph"),
          plotlyOutput("IncSubEffectsPlot")%>% withSpinner(color="#004623"),
          p("In the above graph, the original budget line is given by the dark blue line and the original income expansion path is given by the dark green line.
            After the change in prices and/or income the utility maximizing bundle has shifted. The bright green line is the new income expansion path and the solid light blue line is the new budget line.
            The dashed light blue line is a ficticious budget line parallel to the new budget line. It is tangent to the original indifference curve at the same place the new budget line intersects the old indifference curve.
            That point where those three lines: the ficticious budget line, the new income expansion path, and the indifference curve corresponding the the utility realized in the original bundle is the ficticious point we use to calculate the income and substitution effects.
            The differnce between it and the original bundle is the substitution effect, and the income effect is the difference between the new utility maximizing bundle and it.
            Note: the income effect takes place along the new income expansion path and the substitution effect takes place along the indifference curve corresponding to the original utility.
            When only income changes, there is no substitution effect, only a income effect."),
          p("In the example in the plot above, the substitution effect was:", textOutput(outputId = "IncSubEffectsSub", inline = T),
            "the income effect was:", textOutput(outputId = "IncSubEffectsInc", inline = T), "and the total effect was:", textOutput(outputId = "IncSubEffectsTotal", inline = T)),
        )
        ),
        fluidRow(
          column(6,
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Market_Demand.pdf"),
          ),
          column(6,
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "MarketDemandNFuns",
                     label = "How many different types of demand functions are there?",
                     value = "3",
                     min = 1,
                     max = 5,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   uiOutput(
                     outputId = "MarketDemandFunsList"
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   uiOutput(
                     outputId = "MarketDemandFunsNList"
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   uiOutput(
                     outputId = "MarketDemandSupply"
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   actionButton(
                     inputId = "RunMarketDemandNFuns",
                     label = "Update Number of Demand Functions"
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   actionButton(
                     inputId = "RunMarketDemandSupply",
                     label = "Add a Supply Function"
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   actionButton(
                     inputId = "RunMarketDemandPlot",
                     label = "Draw Graph")
                 ),
                 div(
                   style = "display:inline-block",
                   materialSwitch(
                     inputId = "MarketDemandEquilibrium",
                     label = "Plot Supply and Equilibrium",
                     value = FALSE,
                     status = "success",
                     width = '200px'
                   )
                 ),
                 plotlyOutput("MarketDemandPlot")%>% withSpinner(color="#004623"),
                 actionButton(inputId = "RunMarketDemandPiecewise",
                              label = "Calculate Market Demand Function"),
                 uiOutput("MarketDemandPiecewiseFun"),
                
                 )
        ),
        
      ),
      tabPanel("Practice",
               value = "DemandPractice",
               h1("Coming Soon")),
      tabPanel("Graphs",
               value = "DemandGraphs",
               h1("Coming Soon"))
    ),
    
    ###### Production ######
    navbarMenu(
      "Production",
      tabPanel("Study",
        value = "ProductionStudy",
        h1("Chapter 6: Production"),
        p(
          "Class Notes and interactive content to help make the concepts more engaging"
        ),
        fluidRow(
          column(6,
                 tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                             src = "Isoquants.pdf"),
          ),
          column(6,
                 div(
                   style = "display:inline-block",
                   textInput(
                     inputId = "IsoquantsProdfun",
                     label = "Input a differentiable production function: f(K, L). Be sure not to omit the multiplication sign (*).",
                     value = "K^.5*L^.5",
                     width = '400px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "IsoquantsQMax",
                     label = "Maximum Q value of isoquant curves",
                     value = 120,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "IsoquantsQNum",
                     label = "Number of isoquant Curves",
                     value = 8,
                     min = 1,
                     step = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "IsoquantsLMax",
                     label = "Maximum L value to plot curve's points",
                     value = 200,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "IsoquantsSmooth",
                     label = "Smoothnes Paramater (number of points in each curve)",
                     value = 100,
                     min = 20,
                     step = 1,
                     width = '200px'
                   )
                 ),
                 actionButton(inputId = "RunIsoquantsPlot",
                              label = "Draw Graph"),
                 plotlyOutput("IsoquantsPlot")%>% withSpinner(color="#004623"),
          )
        ),
        fluidRow(
          column(6,
                 tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                             src = "Isocosts.pdf"),
          ),
          column(6,
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "IsocostsR",
                     label = "r: rental rate, the price of capital (K)",
                     value = 20,
                     min = 1,
                     width = '250px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "IsocostsW",
                     label = "w: wages, the price of labor (L)",
                     value = 15,
                     min = 1,
                     width = '250px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "IsocostsCMax",
                     label = "Maximum Cost",
                     value = 10000,
                     min = 1,
                     width = '150px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "IsocostsCNum",
                     label = "Number of isocost Curves",
                     value = 10,
                     min = 1,
                     step = 1,
                     width = '200px'
                   )
                 ),
                 actionButton(inputId = "RunIsocostsPlot",
                              label = "Draw Graph"),
                 plotlyOutput("IsocostsPlot")%>% withSpinner(color="#004623"),
          )
        ),
        fluidRow(
          column(6,
                 tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                             src = "MRTS.pdf"),
          ),
          column(6,
                 textInput(
                   inputId = "MRTSProdfun",
                   label = "Input a differentiable production function: f(K, L). Be sure not to omit the multiplication sign (*).",
                   value = "K^.5*L^.5",
                   width = '400px'
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "MRTSQ",
                     label = "Q: Level of Production",
                     value = 10,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "MRTSLMax",
                     label = "Maximum L value of plot",
                     value = 30,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "MRTSSmooth",
                     label = "Smoothnes Paramater (number of points in each curve)",
                     value = 1000,
                     min = 100,
                     step = 1,
                     width = '200px'
                   )
                 ),
                 actionButton(inputId = "RunMRTSPlot",
                              label = "Draw Graph"),
                 plotlyOutput("MRTSPlot")%>% withSpinner(color="#004623")          
                 )
        ),
        fluidRow(
          column(6,
                 tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                             src = "Firms_SR_Production.pdf"),
          ),
          column(6,
                 div(
                   style = "display:inline-block",
                   textInput(
                     inputId = "SRProductionProdfun",
                     label = "Input a differentiable production function: f(K, L). Be sure not to omit the multiplication sign (*).",
                     value = "K^.5*L^.5",
                     width = '400px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRProductionK",
                     label = span("\\(\\overline{K}\\): Fixed level of capital"),
                     value = 48,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRProductionW",
                     label = "w: wages, the price of labor (L)",
                     value = 10,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRProductionR",
                     label = "r: rental rate, the price of capital (K)",
                     value = 15,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRProductionLMax",
                     label = "Maximum L value to plot curve's points",
                     value = 200,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRProductionSmooth",
                     label = "Smoothnes Paramater (number of points in each curve)",
                     value = 100,
                     min = 20,
                     step = 1,
                     width = '200px'
                   )
                 ),
                 actionButton(inputId = "RunSRProductionPlot",
                              label = "Draw Graph"),
                 plotlyOutput("SRProductionPlot")%>% withSpinner(color="#004623"),
          )
        ),
        fluidRow(
          column(6,
                 tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                             src = "Cost_Minimization.pdf"),
          ),
          column(6,
                 div(
                   style = "display:inline-block",
                   textInput(
                     inputId = "CostMinProdfun",
                     label = "Input a differentiable production function: f(K, L). Be sure not to omit the multiplication sign (*).",
                     value = "K^.5*L^.5",
                     width = '400px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "CostMinQ",
                     label = "Q: Level of production",
                     value = 15,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "CostMinW",
                     label = "w: wages, the price of labor (L)",
                     value = 15,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "CostMinR",
                     label = "r: rental rate, the price of capital (K)",
                     value = 12,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "CostMinSmooth",
                     label = "Smoothnes Paramater (number of points in each curve)",
                     value = 100,
                     min = 20,
                     step = 1,
                     width = '200px'
                   )
                 ),
                 actionButton(inputId = "RunCostMinPlot",
                              label = "Draw Graph"),
                 plotlyOutput("CostMinPlot")%>% withSpinner(color="#004623"),
          )
        ),
        fluidRow(
          column(6,
                 tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                             src = "Firm_LR_vs_SR_Expansion.pdf"),
          ),
          column(6,
                 div(
                   style = "display:inline-block",
                   textInput(
                     inputId = "LRExpansionProdfun",
                     label = "Input a differentiable production function: f(K, L). Be sure not to omit the multiplication sign (*).",
                     value = "K^.5*L^.5",
                     width = '400px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "LRExpansionQMax",
                     label = "Maximum Q level of isoquant curve",
                     value = 100,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "LRExpansionQNum",
                     label = "Number of isoquant curves",
                     value = 10,
                     min = 1,
                     step = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "LRExpansionW",
                     label = "w: wages, the price of labor (L)",
                     value = 2,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "LRExpansionR",
                     label = "r: rental rate, the price of capital (K)",
                     value = 4,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "LRExpansionSmooth",
                     label = "Smoothnes Paramater (number of points in each curve)",
                     value = 100,
                     min = 20,
                     step = 1,
                     width = '200px'
                   )
                 ),
                 actionButton(inputId = "RunLRExpansionPlot",
                              label = "Draw Graph"),
                 plotlyOutput("LRExpansionPlot")%>% withSpinner(color="#004623"),
          )
        ),
        fluidRow(
          column(6,
                 div(
                   style = "display:inline-block",
                   textInput(
                     inputId = "SRLRExpansionProdfun",
                     label = "Input a differentiable production function: f(K, L). Be sure not to omit the multiplication sign (*).",
                     value = "K^.5*L^.5",
                     width = '400px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRLRExpansionQ",
                     label = "Q: Level of production",
                     value = 100,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRLRExpansionW",
                     label = "w: wages, the price of labor (L)",
                     value = 2,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRLRExpansionR",
                     label = "r: rental rate, the price of capital (K)",
                     value = 4,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRLRExpansionSmooth",
                     label = "Smoothnes Paramater (number of points in each curve)",
                     value = 100,
                     min = 20,
                     step = 1,
                     width = '200px'
                   )
                 ),
                 actionButton(inputId = "RunSRLRExpansionPlot",
                              label = "Draw Graph"),
                 plotlyOutput("SRLRExpansionPlot")%>% withSpinner(color="#004623"),
          ),
          column(6,
                 div(
                   style = "display:inline-block",
                   textInput(
                     inputId = "SRLRExpansionCProdfun",
                     label = "Input a differentiable production function: f(K, L). Be sure not to omit the multiplication sign (*).",
                     value = "K^.5*L^.5",
                     width = '400px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRLRExpansionCQ",
                     label = "Q: Level of production",
                     value = 100,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   textInput(
                     inputId = "SRLRExpansionCQList",
                     label = "Comma seperated list of Q to transition to",
                     value = "50, 150",
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRLRExpansionCW",
                     label = "w: wages, the price of labor (L)",
                     value = 2,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRLRExpansionCR",
                     label = "r: rental rate, the price of capital (K)",
                     value = 4,
                     min = 1,
                     width = '200px'
                   )
                 ),
                 div(
                   style = "display:inline-block",
                   numericInput(
                     inputId = "SRLRExpansionCSmooth",
                     label = "Smoothnes Paramater (number of points in each curve)",
                     value = 100,
                     min = 20,
                     step = 1,
                     width = '200px'
                   )
                 ),
                 actionButton(inputId = "RunSRLRExpansionCPlot",
                              label = "Draw Graph"),
                 plotlyOutput("SRLRExpansionCPlot")%>% withSpinner(color="#004623"),
          )
        ),
      ),
      tabPanel("Practice",
               value = "ProductionPractice",
               fluidRow(
                 column(4,
                        actionButton(inputId = "RunNewCostMinQuestion",
                                     label = "Generate a new question"),
                        div(
                          style = "display:inline-block",
                          textInput(
                            inputId = "CostMinStepsProdfun",
                            label = "Input a differentiable production function: f(K, L). Be sure not to omit the multiplication sign (*).",
                            value = "K^.5*L^.5",
                          )
                        ),
                        div(
                          style = "display:inline-block",
                          numericInput(
                            inputId = "CostMinStepsQ",
                            label = "Q: Level of production",
                            value = 15,
                            min = 1,
                            width = '200px'
                          )
                        ),
                        div(
                          style = "display:inline-block",
                          numericInput(
                            inputId = "CostMinStepsW",
                            label = "w: wages, the price of labor (L)",
                            value = 15,
                            min = 1,
                            width = '200px'
                          )
                        ),
                        div(
                          style = "display:inline-block",
                          numericInput(
                            inputId = "CostMinStepsR",
                            label = "r: rental rate, the price of capital (K)",
                            value = 12,
                            min = 1,
                            width = '200px'
                          )
                        ),
                        div(
                          style = "display:inline-block",
                          numericInput(
                            inputId = "CostMinStepsSmooth",
                            label = "Smoothnes Paramater (number of points in each curve)",
                            value = 100,
                            min = 20,
                            step = 1,
                            width = '200px'
                          )
                        ),
                        actionButton(inputId = "RunCostMinStepsPlot",
                                     label = "Draw Graph"),
                        actionButton(inputId = "ClearCostMinStepsPlot",
                                     label = "Clear Plot"),
                        plotlyOutput("CostMinStepsPlot")%>% withSpinner(color="#004623"),
                        p("The following inputs are for part b"),
                        div(
                          style = "display:inline-block",
                          numericInput(
                            inputId = "CostMinStepsSRQ",
                            label = "Q: Level of production",
                            value = 25,
                            min = 1,
                            width = '200px'
                          )
                        ),
                        actionButton(inputId = "RunCostMinStepsSRPlot",
                                     label = "Draw Graph"),
                        actionButton(inputId = "ClearCostMinStepsSRPlot",
                                     label = "Clear Plot"),
                        plotlyOutput("CostMinStepsSRPlot")%>% withSpinner(color="#004623"),
                        ),
                 column(8,
                        uiOutput(
                          outputId = "CostMinStepsQuestion"
                        ),
                        h5("Type your answers to the question in the field below, rounding to the nearest hundredth"),
                        div(
                          style = "display:inline-block",
                          numericInput(
                            inputId = "CostMinStepsAnswerC",
                            label = "How much does the production of 15 units cost?",
                            min = 0,
                            value = 0,
                            step = .01,
                            width = '200px'
                          )
                        ),
                        div(
                          style = "display:inline-block",
                          numericInput(
                            inputId = "CostMinStepsAnswerL",
                            label = "How much labor (L) is used?",
                            min = 0,
                            value = 0,
                            step = .01,
                            width = '200px'
                          )
                        ),
                        div(
                          style = "display:inline-block",
                          numericInput(
                            inputId = "CostMinStepsAnswerK",
                            label = "How much capital (K) is used?",
                            min = 0,
                            value = 0,
                            step = .01,
                            width = '200px'
                          )
                        ),
                        actionButton(inputId = "RunCostMinStepsAnswers",
                                     label = "Submit Solutions"),
                        actionButton(inputId = "RunCostMinStepsSolutions",
                                     label = "Reveal Step-by-Step Solutions"),
                        actionButton(inputId = "ClearCostMinStepsSolutions",
                                     label = "Clear Step-by-Step Solutions"),
                        htmlOutput("CostMinStepsAnswers") %>% withSpinner(color="#004623"),
                        htmlOutput("CostMinStepsSolutions") %>% withSpinner(color="#004623"),
                        uiOutput(
                          outputId = "CostMinStepsSRQuestion"
                        ),
                        h5("Type your answers to the question in the field below, rounding to the nearest hundredth"),
                        div(
                          style = "display:inline-block",
                          numericInput(
                            inputId = "CostMinStepsSRAnswerCSR",
                            label = "How much does the production of 25 units cost in the short run?",
                            min = 0,
                            value = 0,
                            step = .01,
                            width = '200px'
                          )
                        ),
                        div(
                          style = "display:inline-block",
                          numericInput(
                            inputId = "CostMinStepsSRAnswerCLR",
                            label = "How much does the production of 25 units cost in the long run?",
                            min = 0,
                            value = 0,
                            step = .01,
                            width = '200px'
                          )
                        ),
                        actionButton(inputId = "RunCostMinStepsSRAnswers",
                                     label = "Submit Solutions"),
                        actionButton(inputId = "RunCostMinStepsSRSolutions",
                                     label = "Reveal Step-by-Step Solutions"),
                        actionButton(inputId = "ClearCostMinStepsSRSolutions",
                                     label = "Clear Step-by-Step Solutions"),
                        htmlOutput("CostMinStepsSRAnswers") %>% withSpinner(color="#004623"),
                        htmlOutput("CostMinStepsSRSolutions") %>% withSpinner(color="#004623"),
                        )
                 ),
               ),
      tabPanel("Graphs",
               value = "ProductionGraphs",
               h1("Coming Soon"))
    ),
    
    ###### Costs ######
    navbarMenu(
      "Costs",
      tabPanel("Study",
               value = "CostsStudy",
               h1("Chapter 7: Costs"),
               p(
                 "Class Notes and interactive content to help make the concepts more engaging"
               ),
      ),
      tabPanel("Practice",
               value = "CostsPractice",
               h1("Coming Soon")),
      tabPanel("Graphs",
               value = "CostsGraphs",
               h1("Coming Soon"))
    ),
    
    ###### Market Structures ######
    navbarMenu(
      "Market Structures",
      tabPanel("Study",
               value = "MarketStudy",
               h1("Chapter 8,9,11: Market Structures"),
               p(
                 "Class Notes and interactive content to help make the concepts more engaging"
               ),
      ),
      tabPanel("Practice",
               value = "MarketPractice",
               tabsetPanel(
                 tabPanel("Perfect Competition",
                          fluidRow(
                            column(4,
                                   p("Be sure not to omit the multiplication sign (*) when inputting functions"),
                                   actionButton(inputId = "RunNewPerfectCompetitionQuestion",
                                                label = "Generate a new question"),
                                   p("Inputs for part a"),
                                   div(
                                     style = "display:inline-block",
                                     textInput(
                                       inputId = "PerfectCompetitionStepsCostfun",
                                       label = "Firms' total cost function TC(Q):",
                                       value = "25 + Q^2",
                                     )
                                   ),
                                   div(
                                     style = "display:inline-block",
                                     textInput(
                                       inputId = "PerfectCompetitionStepsDemandfun",
                                       label = "Market demand function Q(P):",
                                       value = "101000 - 100*P",
                                     )
                                   ),
                                   p("Inputs for part b"),
                                   radioGroupButtons(
                                     inputId = "PerfectCompetitionStepsSRChoice",
                                     label = "Select a function to change for part b", 
                                     choices = c("Demand", "Cost"),
                                     status = "success"
                                   ),
                                   div(
                                     style = "display:inline-block",
                                     textInput(
                                       inputId = "PerfectCompetitionStepsSRfun",
                                       label = "New Market demand function Q(P):",
                                       value = "200000-2500*P",
                                     )
                                   ),
                                   
                            ),
                            column(8,
                                   uiOutput(
                                     outputId = "PerfectCompetitionStepsQuestion"
                                   ),
                                   h5("Type your answer to the question in the field below. If necessary, round to the nearest integer"),
                                   div(
                                     style = "display:inline-block",
                                     numericInput(
                                       inputId = "PerfectCompetitionStepsAnswerN",
                                       label = "How many firms will be in this market in the long run?",
                                       min = 1,
                                       value = 1,
                                       step = 1,
                                       width = '200px'
                                     )
                                   ),
                                   actionButton(inputId = "RunPerfectCompetitionStepsAnswer",
                                                label = "Submit Solution"),
                                   actionButton(inputId = "RunPerfectCompetitionStepsSolution",
                                                label = "Reveal Step-by-Step Solution"),
                                   actionButton(inputId = "ClearPerfectCompetitionStepsSolution",
                                                label = "Clear Step-by-Step Solution"),
                                   htmlOutput("PerfectCompetitionStepsAnswer") %>% withSpinner(color="#004623"),
                                   htmlOutput("PerfectCompetitionStepsSolution") %>% withSpinner(color="#004623"),
                                   uiOutput(
                                     outputId = "PerfectCompetitionStepsSRQuestion"
                                   ),
                                   h5("Type your answer to the question in the field below. If necessary, round to the nearest hundredth"),
                                   div(
                                     style = "display:inline-block",
                                     numericInput(
                                       inputId = "PerfectCompetitionStepsSRAnswerPi",
                                       label = "How much profit do firms make in this market in the short run?",
                                       value = 0,
                                       step = .01,
                                       width = '200px'
                                     )
                                   ),
                                   actionButton(inputId = "RunPerfectCompetitionStepsSRAnswer",
                                                label = "Submit Solution"),
                                   actionButton(inputId = "RunPerfectCompetitionStepsSRSolution",
                                                label = "Reveal Step-by-Step Solution"),
                                   actionButton(inputId = "ClearPerfectCompetitionStepsSRSolution",
                                                label = "Clear Step-by-Step Solution"),
                                   htmlOutput("PerfectCompetitionStepsSRAnswer") %>% withSpinner(color="#004623"),
                                   htmlOutput("PerfectCompetitionStepsSRSolution") %>% withSpinner(color="#004623"),
                            )
                          ),
                 ), 
                 tabPanel("Monopolies",
                          fluidRow(
                            column(4,
                                   p("Be sure not to omit the multiplication sign (*) when inputting functions"),
                                   actionButton(inputId = "RunNewMonopolyQuestion",
                                                label = "Generate a new question"),
                                   div(
                                     style = "display:inline-block",
                                     textInput(
                                       inputId = "MonopolyStepsCostfun",
                                       label = "Firm's total cost function TC(Q):",
                                       value = "5000 + 10*Q",
                                     )
                                   ),
                                   div(
                                     style = "display:inline-block",
                                     textInput(
                                       inputId = "MonopolyStepsDemandfun",
                                       label = "Market demand function Q(P):",
                                       value = "1000 - 5*P",
                                     )
                                   ),
                            ),
                            column(8,
                                   uiOutput(
                                     outputId = "MonopolyStepsQuestion"
                                   ),
                                   h5("Type your answer to the question in the field below. If necessary, round to the nearest hundredth"),
                                   div(
                                     style = "display:inline-block",
                                     numericInput(
                                       inputId = "MonopolyStepsAnswerPi",
                                       label = "How much profit can the firm make?",
                                       min = 0,
                                       value = 0,
                                       step = .01,
                                       width = '200px'
                                     )
                                   ),
                                   actionButton(inputId = "RunMonopolyStepsAnswer",
                                                label = "Submit Solution"),
                                   actionButton(inputId = "RunMonopolyStepsSolution",
                                                label = "Reveal Step-by-Step Solution"),
                                   actionButton(inputId = "ClearMonopolyStepsSolution",
                                                label = "Clear Step-by-Step Solution"),
                                   htmlOutput("MonopolyStepsAnswer") %>% withSpinner(color="#004623"),
                                   htmlOutput("MonopolyStepsSolution") %>% withSpinner(color="#004623"),
                                   uiOutput(
                                     outputId = "MonopolyStepsDWLQuestion"
                                   ),
                                   h5("Type your answer to the question in the field below. If necessary, round to the nearest hundredth"),
                                   div(
                                     style = "display:inline-block",
                                     numericInput(
                                       inputId = "MonopolyStepsDWLAnswerDWL",
                                       label = "How much dead weight loss is created?",
                                       min = 0,
                                       value = 0,
                                       step = .01,
                                       width = '200px'
                                     )
                                   ),
                                   actionButton(inputId = "RunMonopolyStepsDWLAnswer",
                                                label = "Submit Solution"),
                                   actionButton(inputId = "RunMonopolyStepsDWLSolution",
                                                label = "Reveal Step-by-Step Solution"),
                                   actionButton(inputId = "ClearMonopolyStepsDWLSolution",
                                                label = "Clear Step-by-Step Solution"),
                                   htmlOutput("MonopolyStepsDWLAnswer") %>% withSpinner(color="#004623"),
                                   htmlOutput("MonopolyStepsDWLSolution") %>% withSpinner(color="#004623"),
                            )
                          ),
                          ), 
                 tabPanel("Oligopolies", 
                          fluidRow(
                            column(4,
                                   p("Be sure not to omit the multiplication sign (*) when inputting functions"),
                                   actionButton(inputId = "RunNewOligopolyQuestion",
                                                label = "Generate a new question"),
                                   div(
                                     style = "display:inline-block",
                                     textInput(
                                       inputId = "OligopolyStepsCostfun",
                                       label = "Firms' total cost function TC(Q):",
                                       value = "10*Q",
                                     )
                                   ),
                                   div(
                                     style = "display:inline-block",
                                     textInput(
                                       inputId = "OligopolyStepsDemandfun",
                                       label = "Market demand function Q(P):",
                                       value = "200 - 5*P",
                                     )
                                   ),
                                   p("The following options are for the Cournot Question"),
                                   div(
                                     style = "display:inline-block",
                                     switchInput(
                                       inputId = "CournotStepsN",
                                       label = "Solve for N firms",
                                       labelWidth = "80px",
                                       onStatus = "success", 
                                       offStatus = "danger"
                                     )
                                   ),
                                   div(
                                     style = "display:inline-block",
                                     numericInput(
                                       inputId = "CournotStepsNVal",
                                       label = "How many firms in Cournot competiton?", 
                                       min = 1,
                                       value = 2,
                                       step = 1,
                                       width = '200px'
                                       )
                                   ),
                                   
                            ),
                            column(8,
                                   uiOutput(
                                     outputId = "BertrandStepsQuestion"
                                   ),
                                   h5("Type your answer to the question in the field below. If necessary, round to the nearest hundredth"),
                                   div(
                                     style = "display:inline-block",
                                     numericInput(
                                       inputId = "BertrandStepsAnswerP1",
                                       label = "What is the price firm 1 sets?",
                                       min = 0,
                                       value = 0,
                                       step = .01,
                                       width = '200px'
                                     )
                                   ),
                                   div(
                                     style = "display:inline-block",
                                     numericInput(
                                       inputId = "BertrandStepsAnswerP2",
                                       label = "What is the price firm 2 sets?",
                                       min = 0,
                                       value = 0,
                                       step = .01,
                                       width = '200px'
                                     )
                                   ),
                                   actionButton(inputId = "RunBertrandStepsAnswer",
                                                label = "Submit Solution"),
                                   actionButton(inputId = "RunBertrandStepsSolution",
                                                label = "Reveal Step-by-Step Solution"),
                                   actionButton(inputId = "ClearBertrandStepsSolution",
                                                label = "Clear Step-by-Step Solution"),
                                   htmlOutput("BertrandStepsAnswer") %>% withSpinner(color="#004623"),
                                   htmlOutput("BertrandStepsSolution") %>% withSpinner(color="#004623"),
                                   uiOutput(
                                     outputId = "CournotStepsQuestion"
                                   ),
                                   h5("Type your answer to the question in the field below. If necessary, round to the nearest hundredth"),
                                   div(
                                     style = "display:inline-block",
                                     numericInput(
                                       inputId = "CournotStepsAnswerq",
                                       label = "How much quantity does each firm produce?",
                                       min = 0,
                                       value = 0,
                                       step = .01,
                                       width = '200px'
                                     )
                                   ),
                                   div(
                                     style = "display:inline-block",
                                     numericInput(
                                       inputId = "CournotStepsAnswerPi",
                                       label = "How much profit does each firm make?",
                                       min = 0,
                                       value = 0,
                                       step = .01,
                                       width = '200px'
                                     )
                                   ),
                                   actionButton(inputId = "RunCournotStepsAnswers",
                                                label = "Submit Solution"),
                                   actionButton(inputId = "RunCournotStepsSolution",
                                                label = "Reveal Step-by-Step Solution"),
                                   actionButton(inputId = "ClearCournotStepsSolution",
                                                label = "Clear Step-by-Step Solution"),
                                   htmlOutput("CournotStepsAnswers") %>% withSpinner(color="#004623"),
                                   htmlOutput("CournotStepsSolution") %>% withSpinner(color="#004623"),
                            )
                          ),
                          )
               )
      ),
      tabPanel("Graphs",
               value = "MarketGraphs",
               h1("Coming Soon"))
    ),
    tabPanel("Graphs",
             value = "Graphs",
             h1("Coming Soon")),
    tabPanel("Code",
             value = "Code",
             h3("GitHub Repository"),
             HTML('<a href="https://github.com/ethanholdahl/Shiny-Microeconomics"><img src="https://github-readme-stats.vercel.app/api/pin/?username=ethanholdahl&amp;repo=Shiny-Microeconomics&amp;title_color=004623&amp;text_color=462300&amp;bg_color=FFFFF&amp;border_color=004623&amp;icon_color=330046 alt="Repo Card" align="left"></a>'),
    )
  )
)
