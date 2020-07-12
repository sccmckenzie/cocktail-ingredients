library(readr)

cocktails <- read_rds("cocktails.rds") # output from data.R

optimal_selection <- capability2(selection_path(cocktails, ingredient, drink), cocktails, drink, ingredient)

optimal_selection %>% 
  write_rds("optimal-selection.rds")

optimal_selection %>% 
  write_csv("optimal-selection.csv")

library(ggplot2)
bind_rows(optimal_selection %>% 
            mutate(method = "algorithm"),
          capability2(count(cocktails, ingredient, sort = T)$ingredient,
                      cocktails,
                      drink,
                      ingredient) %>% 
            mutate(method = "frequency")) %>% 
  fill(performance, .direction = "down") %>% 
  ggplot(aes(i, performance)) +
  geom_line(aes(color = method)) +
  theme_light()
