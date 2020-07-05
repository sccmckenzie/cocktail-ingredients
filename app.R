library(shiny)
library(tidyverse)
library(rlang)
library(glue)
library(ggtext)
library(igraph)
library(ggraph)

source("selection-algorithm.R")
source("data.R")
source("circleFun.R")
source("net.R")

env_bind(
  global_env(),
  drinks = NULL,
  df1 = NULL, 
  colors = c(dormant = "#D1DDE6", partial = "#79D7F6", fulfilled = "#384967"),
  label_colors = c(dormant = "#838B96", partial = "#79D7F6", fulfilled = "#384967")
)

ui <- tagList(
  tags$head(includeScript("window-size.js")),
  includeCSS("css.css"),
  fluidPage(
    titlePanel(h2("Ingredient Selection Assistant"), "Cocktail Ingredients App"),
    div(
      actionButton("whiskey", "Whiskey-Based"),
      actionButton("martini", "Martini's"),
      actionButton("daiquiri", "Daiquiri's"),
      actionButton("random", "Random Selection"),
    ),
    br(),
    uiOutput("ui_selection_map", style = "display: inline-block; width: 50%"),
    uiOutput("ui_circle", style = "display: inline-block; width: 49%; vertical-align: top;")
  )
)

server <- function(input, output, session) {
  inv_drinks <- reactiveVal()
  inv_selection_map <- reactiveVal()
  
  choices <- exprs(whiskey, martini, daiquiri)
  
  walk(choices, ~ {
    eval(
      expr(
        observeEvent(`$`(input, !!..1), {
          env_poke(global_env(), "drinks", !!..1)
          
          inv_drinks(rnorm(1))
        })
      )
    )
  })
  
  observeEvent(input$random, {
    env_poke(global_env(),
             "drinks",
             cocktails %>% 
               filter(drink %in% sample(unique(drink), size = 20))
    )
    
    inv_drinks(rnorm(1))
  })
  
  observe({
    inv_drinks()
    req(drinks)
    
    df1 <- capability2(selection_path(drinks, ingredient, drink), drinks, drink, ingredient, collapse = TRUE)
    
    df2 <- drinks %>% 
      distinct(drink, ingredient) %>% 
      left_join(df1 %>% select(i, ingredient)) %>% 
      left_join(df1 %>% select(drink, performance))
    
    env_bind(global_env(), df1 = df1, df2 = df2, i_selected = 1)
    
    inv_selection_map(rnorm(1))
  })
  
  output$selection_map <- renderPlot({
    inv_selection_map()
    
    df2 <- df2 %>% 
      with_groups(drink, mutate, status = all(i %in% 1:i_selected)) %>% 
      mutate(status = case_when(
        status ~ "fulfilled",
        i %in% 1:i_selected ~ "partial",
        TRUE ~ "dormant"
      ),
      status = factor(status, levels = c("fulfilled", "partial", "dormant"), ordered = TRUE)) %>% 
      with_groups(drink, mutate, label_status = min(status)) %>% 
      mutate(drink = glue("<b style = 'color:{label_colors[as.character(label_status)]}'>{drink}</b>"),
             drink = fct_reorder(drink, performance),
             ingredient = if_else(i %in% 1:i_selected, glue("<b>{ingredient}</b>"), ingredient),
             ingredient = fct_reorder(ingredient, i))
    
    df_segment <- df2 %>% 
      filter(status == "fulfilled") %>% 
      with_groups(drink, slice_max, i)
    
    g <- df2 %>% 
      ggplot(aes(drink, fct_rev(ingredient))) +
      geom_point(aes(color = status), size = 7, shape = 15) +
      theme_minimal() +
      labs(x = "", y = "") +
      scale_x_discrete(position = "top") +
      scale_color_manual(values = colors, guide = FALSE) +
      theme(axis.text.x.top = element_markdown(angle = 90, hjust = 0),
            axis.text.y = element_markdown(),
            panel.grid = element_line(linetype = "dotted", color = "#D1DDE6", size = 0.4),
            text = element_text(size = 20))

    if (nrow(df_segment) == 0) {
      return(g)
    } else {
      g +
        geom_segment(data = df_segment,
                     aes(x = drink, xend = drink, y = head(levels(df2$ingredient), 1), yend = ingredient),
                     color = "#384967",
                     size = 1)
    }
  })
  
  observe({
    req(input$plot_click)
  
    env_bind(global_env(), i_selected = nrow(df1) - round(input$plot_click$y, 0) + 1)
    
    inv_selection_map(rnorm(1))
  })
  
  output$ui_selection_map <- renderUI({
    inv_drinks()
    req(drinks)
    
    div(
      id = "selection_map_container",
      style = glue("height: {input$height * 0.85}px; width: {input$width * 0.45 + 40}px; overflow-y: scroll;"),
      h4("Ingredient order optimized (starting from top) to substantiate recipes as quickly as possible."),
      plotOutput("selection_map",
                 click = "plot_click",
                 height = 364 * log(nrow(distinct(drinks, ingredient))),
                 width = input$width * 0.45)
    )
    
  })
  
  output$circle <- renderPlot({
    inv_selection_map()
    req(df1)
    generate_net(drinks, df1$ingredient[1:i_selected], drop_na(df1[1:i_selected,])$drink)
  })

  output$ui_circle <- renderUI({
    inv_selection_map()
    req(drinks)

    tagList(
      div(h4("Manipulate network by selecting squares in lefthand plot")),
      br(),
      plotOutput("circle", height = input$height * 0.8, width = input$width * 0.45),
      type = 8
    )
  })
}

shinyApp(ui, server)