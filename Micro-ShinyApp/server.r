
## Load and install the packages
library("tidyverse", "shiny", "stringr", "plotly")
theme_set(theme_minimal())


# Define server logic
function(input, output, session) {
  
  values <- reactiveValues(myurl = c(), parent_tab = "")
  
  observe({
    input$bannerTabs
    query <- parseQueryString(session$clientData$url_search)
    # output$a1 = renderPrint({query})
    url <- query$url
    if (is.null(url)) {
      url <- ""
    }
    
    # "depth" is how many levels the url in the query string is
    depth <- function(x)
      length(unlist(strsplit(x, "/")))
    
    # if we reached the end, done!
    if (length(values$myurl) == depth(url)) {
      return()
    }
    # base case - need to tell it what the first main nav name is
    else if (length(values$myurl) == 0) {
      values$parent_tab <- "bannerTabs"
    }
    # if we're waiting for a tab switch but the UI hasn't updated yet
    else if (is.null(input[[values$parent_tab]])) {
      return()
    }
    # same - waiting for a tab switch
    else if (tail(values$myurl, 1) != input[[values$parent_tab]]) {
      return()
    }
    # the UI is on the tab that we last switched to, and there are more
    # tabs to switch inside the current tab
    # make sure the tabs follow the naming scheme
    else {
      values$parent_tab <- paste0(tail(values$myurl, 1), "_tabs")
    }
    # figure out the id/value of the next tab
    new_tab <- unlist(strsplit(url, "/"))[length(values$myurl) + 1]
    # easy peasy.
    updateTabsetPanel(session, values$parent_tab, new_tab)
    values$myurl <- c(values$myurl, new_tab)
  })
  
}
