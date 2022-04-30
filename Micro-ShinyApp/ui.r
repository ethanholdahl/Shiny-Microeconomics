
# Load and install the packages
library(shiny)
library(tidyverse)
#library(shinythemes)
library(plotly)
library(shinyjs)


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
    tabPanel("Home",
             value = "Home",
             h1("Coming Soon")),
    navbarMenu(
      "Consumer Behavior",
      tabPanel(
        "Study",
        value = "ConsumerStudy",
        h1("Hello!"),
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
              value = 6,
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
              value = 100,
              min = 1,
              width = '150px'
            )
          ),
          div(
            style = "display:inline-block",
            numericInput(
              inputId = "IndifferenceXMax",
              label = "Maximum x value of plot",
              value = 20,
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
              value = 20,
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
              value = 5,
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
              value = 5,
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
               h1("Hello!")),
      tabPanel("Graphs",
               value = "ConsumerGraphs",
               h1("Hello!"))
    ),
    navbarMenu(
      "Individual and Market Demand",
      tabPanel("Study",
               value = "DemandStudy",
               h1("Hello!")),
      tabPanel("Practice",
               value = "DemandPractice",
               h1("Hello!")),
      tabPanel("Graphs",
               value = "DemandGraphs",
               h1("Hello!"))
    ),
    tabPanel("Graphs",
             value = "Graphs",
             h1("Coming Soon")),
    tabPanel("Code",
             value = "Code",
             h1("Coming Soon"))
  )
)
