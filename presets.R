cocktails_reactable <- group_by(cocktails, drink) %>% 
  summarise(ingredients = str_c(unique(ingredient), collapse = ", "),
            .groups = "drop") %>% 
  sample_n(size = n())

whiskey <- cocktails_reactable %>% 
  filter(str_detect(ingredients, "(bourbon|whiskey)")) %>% 
  pull(drink)

martini <- cocktails_reactable %>% 
  filter(str_detect(drink, "Martini")) %>% 
  pull(drink)

daiquiri <- cocktails_reactable %>% 
  filter(str_detect(drink, "Daiquiri")) %>% 
  pull(drink)
