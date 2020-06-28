circleFun <- function(r = NULL) {
  if (is.null(r)) r <- sample(c(rep(1, 3), rep(2, 5), rep(5, 3)))
  
  tibble(r = r) %>% 
    rowid_to_column() %>% 
    group_nest(r) %>% 
    mutate(data = map2(data, r, ~ {
      theta <- 2 * pi * seq(0, 1, length.out = nrow(..1) + 1)
      theta <- head(theta, -1) + runif(1, 0, pi / 4)
      
      ..1 %>% 
        mutate(x = ..2 * cos(theta),
               y = ..2 * sin(theta))
    })) %>% 
    unnest(cols = data) %>% 
    arrange(rowid) %>% 
    select(x, y) %>% 
    as.matrix()
}
