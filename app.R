library(shiny)
library(tidyverse)
library(reactable)
library(glue)
library(shinycssloaders)
library(igraph)
library(ggraph)

source("selection-algorithm.R")
source("circleFun.R")
source("net.R")

options(spinner.color = "#838B96")

ui <- tagList(
  tags$head(includeScript("window-size.js")),
  includeCSS("css.css"),
  fluidPage(
    titlePanel(h2("Ingredient Selection Assistant"), "Cocktail Ingredients App"),
    fluidRow(column(3,
                    class = "col",
                    wellPanel(
                      h4("Which cocktails would you like to make?"),
                      br(),
                      reactableOutput("cocktail_selection"),
                      actionButton("submit", "Go!")
                    )), # column
             column(3,
                    class = "col",
                    id = "col-selected",
                    wellPanel(
                      withSpinner(reactableOutput("optimal_reactable"), type = 8)
                    ),
             ), # column
             column(6,
                    uiOutput("ui_circle")
             ) # column
             
    ) # fluidRow
  )
)

server <- function(input, output, session) {
  cocktails_reactable <- group_by(cocktails, drink) %>% 
    summarise(ingredients = str_c(unique(ingredient), collapse = ", "),
              .groups = "drop") %>% 
    sample_n(size = nrow(.))
  
  output$cocktail_selection <- renderReactable({
    reactable(cocktails_reactable,
              selection = "multiple",
              onClick = "select",
              compact = TRUE,
              defaultPageSize = 6,
              theme = reactableTheme(
                style = list(backgroundColor = "#f5f5f5"),
                rowSelectedStyle = list(backgroundColor = "white", boxShadow = "inset 2px 0 0 0 #79D7F7")
              ),
              columns = list(
                drink = colDef(name = "Select All",
                               filterable = TRUE,
                               cell = function(value, index) {
                                 tagList(
                                   div(style = "font-weight: bold; font-size: 1.1em;", value),
                                   div(style = "color: darkslategray;font-size: smaller;", cocktails_reactable[index, "ingredients"][[1]])
                                   )
                               }),
                ingredients = colDef(show = FALSE)
              ) # columns
    ) # reactable
  }) # renderReactable
  
  drinks_selected <- eventReactive(input$submit, {
    state <- getReactableState("cocktail_selection", name = "selected")
    updateReactable("optimal_reactable", selected = NA)
    
    if (is.null(state)) return(NULL) else cocktails_reactable[state, ][[1]]
  })
  
  output$drinks_print <- renderUI({
    v <- drinks_selected()
    
    if (length(v) > 25) {
      o <- length(v)
      
      v <- v[1:25]
      v[26] <- glue("...and {o - 30} more")
    }
    
    HTML(paste(v, collapse = "<br/>"))
  }) # renderUI
  
  optimal <- reactive({
    req(drinks_selected())
    
    if (length(drinks_selected()) == nrow(cocktails_reactable)) {
      return(optimal_full)
    }
    
    df <- cocktails %>% 
      filter(drink %in% drinks_selected())
  
    tibble(ingredient = selection_path(df, ingredient, drink)) %>%
      mutate(performance = capability(ingredient, cocktails, drink, ingredient))
  })
  
  output$optimal_reactable <- renderReactable({
    reactable(optimal(),
              defaultPageSize = 20,
              selection = "single",
              onClick = "select",
              compact = TRUE,
              borderless = TRUE,
              sortable = FALSE,
              rownames = TRUE,
              highlight = TRUE,
              rowStyle = list(fontSize = "14px", height = "25px"),
              columns = list(
                .rownames = colDef(width = 30),
                ingredient = colDef(name = "Optimal Purchase Order"),
                performance = colDef(show = FALSE)
              ), # columns
              theme = reactableTheme(
                style = list(backgroundColor = "#fcfcfc"),
                rowHighlightStyle = list(backgroundColor = "#D1DDE6"),
                rowSelectedStyle = list(backgroundColor = "#D1DDE6", boxShadow = "inset 2px 0 0 0 #79D7F7")
              ) # reactableTheme
    ) # reactable
  }) # renderReactable
  
  ingredients_selected <- reactive({
    state <- getReactableState("optimal_reactable", name = "selected")
    
    if (is.null(state)) return(NULL) else optimal()[state,][[1]]
  })
  
  output$circle <- renderPlot({
    req(drinks_selected())
    
    generate_net(drinks_selected(), ingredients_selected())
  }) # renderPlot
  
  output$ui_circle <- renderUI({
    req(drinks_selected())
    
    withSpinner(
      plotOutput("circle", height = input$height * 0.85, width = input$height * 0.95),
      type = 8
    )
  })
}

shinyApp(ui, server)