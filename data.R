cocktails <- read_rds("cocktails.rds")

whiskey <- cocktails %>% 
  with_groups(drink, filter, any(str_detect(ingredient, "(bourbon|whiskey)")))

martini <- cocktails %>% 
  with_groups(drink, filter, str_detect(drink, "Martini"))

daiquiri <- cocktails %>% 
  with_groups(drink, filter, str_detect(drink, "Daiquiri"))



