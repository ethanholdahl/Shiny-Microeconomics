

# Load and install the packages
library(shiny)
library(tidyverse)
#library(shinythemes)
library(plotly)
library(shinyjs)
library(ggnewscale)


# Define UI for the application
fluidPage(
  #theme = shinytheme("sandstone"),
  tags$head(
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
      h1("More Content Coming Soon"),
      p(
        "Please allow a short time for the interactive assets to load into your browser. Currently available are the study pages under the Consumer Behavior and Individual and Market Demand tabs. I will be adding content as quickly as I can throughout the weekend"
      )
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
        wellPanel(
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Utility.pdf"),
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Derivatives.pdf"),
          textInput(
            inputId = "UtilityFunction",
            label = "Input a differentiable utility function with arguments x and y. Be sure not to omit the multiplication sign (*).",
            value = "4*x+3*y+x^2*y^3+17"
          ),
          HTML("The Function you entered appears as:"),
          p("U(x,y) =", textOutput(outputId = "UtilityFunction", inline = T)),
          HTML("MUx, the derivative of U(x,y) with respect to x is:"),
          p("MUx =", textOutput(outputId = "UtilityMUx", inline = T)),
          HTML("MUy, the derivative of U(x,y) with respect to y is:"),
          p("MUy =", textOutput(outputId = "UtilityMUy", inline = T)),
        ),
        wellPanel(
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Indifference_Curves.pdf"),
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
          plotlyOutput("IndifferencePlot")
        ),
        wellPanel(
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "MRS.pdf"),
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
              value = 10,
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
              value = 8,
              min = 1,
              max = 100,
              width = '200px'
            )
          ),
          plotlyOutput("MRSPlot")
        ),
        wellPanel(
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Budget_Constraints.pdf"),
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
              inputId = "BudgetN",
              label = "Number of Bundles",
              value = 50,
              min = 6,
              step = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            checkboxInput(
              inputId = "BudgetFeasibility",
              label = "Show feasibility",
              value = FALSE,
              width = '200px'
            )
          ),
          plotlyOutput("BudgetPlot")
        ),
        wellPanel(
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Constrained_Optimization.pdf"),
          div(
            style = "display:inline-block",
            textInput(
              inputId = "ConstrainedFunction",
              label = "Input a differentiable utility function with arguments x and y. Be sure not to omit the multiplication sign (*).",
              value = "x^2 + 2 * x * y"
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "ConstrainedXMax",
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
              inputId = "ConstrainedYMax",
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
              inputId = "ConstrainedI",
              label = "Income ($)",
              value = 20,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "ConstrainedPx",
              label = "Price of good x ($)",
              value = 2,
              min = 1,
              width = '200px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "ConstrainedPy",
              label = "Price of good y ($)",
              value = 3,
              min = 1,
              width = '200px'
            )
          ),
          plotlyOutput("ConstrainedPlot")
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
        wellPanel(
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Income_Expansion_Path.pdf"),
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
          plotlyOutput("IncomeExpansionPlot"),
        ),
        wellPanel(
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Engel_Curves.pdf"),
          p("The paramaters governing the dynamics for the Engel Curves are taken from the Income Expansion Path paramaters since they are so intertwined."),
          plotlyOutput("EngelPlots"),
        ),
        wellPanel(
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Derived_Demand.pdf"),
        ),
        wellPanel(
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Income&Substitution_Effects.pdf"),
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
          plotlyOutput("IncSubEffectsPlot"),
          p("In the above graph, the original budget line is given by the dark blue line and the original income expansion path is given by the dark green line.
            After the change in prices and/or income the utility maximizing bundle has shifted. The bright green line is the new income expansion path and the solid light blue line is the new budget line.
            The dashed light blue line is a ficticious budget line parallel to the new budget line. It is tangent to the original indifference curve at the same place the new budget line intersects the old indifference curve.
            That point where those three lines: the ficticious budget line, the new income expansion path, and the indifference curve corresponding the the utility realized in the original bundle is the ficticious point we use to calculate the income and substitution effects.
            The differnce between it and the original bundle is the substitution effect, and the income effect is the difference between the new utility maximizing bundle and it.
            Note: the income effect takes place along the new income expansion path and the substitution effect takes place along the indifference curve corresponding to the original utility.
            When only income changes, there is no substitution effect, only a income effect."),
          p("In the example in the plot above, the substitution effect was:", textOutput(outputId = "IncSubEffectsSub", inline = T),
            "the income effect was:", textOutput(outputId = "IncSubEffectsInc", inline = T), "and the total effect was:", textOutput(outputId = "IncSubEffectsTotal", inline = T)),
        ),
        wellPanel(
          tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                      src = "Market_Demand.pdf"),
        ),
        
      ),
      tabPanel("Practice",
               value = "DemandPractice",
               h1("Coming Soon")),
      tabPanel("Graphs",
               value = "DemandGraphs",
               h1("Coming Soon"))
    ),
    tabPanel("Graphs",
             value = "Graphs",
             h1("Coming Soon")),
    tabPanel("Code",
             value = "Code",
             h1("Coming Soon"))
  )
)
