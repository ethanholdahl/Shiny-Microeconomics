

## Load and install the packages
library(tidyverse)
#library(shinythemes)
library(shiny)
library(plotly)


# Define UI for the application
fluidPage(
  #theme = shinytheme("sandstone"),
  tags$head(HTML(
    "<title>Intermediate Microeconomics by Ethan Holdahl </title>"
  )),
  navbarPage(
    id = "bannerTabs",
    title = HTML(
      "Intermediate Microeconomics by <a href='https://ethanholdahl.com' style='color:darkgreen'>Ethan Holdahl</a>"
    ),
    tabPanel("Home",
             value = "Home",
             h1("Hello!")),
    navbarMenu(
      "Consumer Behavior",
      tabPanel("Study",
               value = "ConsumerStudy",
               h1("Hello!")),
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
             h1("Beautiful Graphs!")),
    tabPanel("Code",
             value = "Code",
             h1("Beautiful Graphs!"))
  )
)
