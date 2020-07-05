# Selection algorithm (generalized for any data set)
# Inspired by TidyTuesday cocktail dataset
# 2020-06-21

# this needs to be realized as a package

# end-user function: 
# selection_path()
# data - dataset that shares similar structure as cocktails
# var - individual element (ingredient) that comprises groups (recipe)
# group - column that identifies subset

# Needed improvements:
# Improve bottleneck in n4()
# Scale to multi-dimensional (algorithm has ability to take hypothetical ith+1 result into consideration)

library(dplyr)
library(tidyr)
library(rlang)

n4_0 <- function(data, var, group) {
  var <- enexpr(var)
  group <- enexpr(group)
  
  stopifnot(is.data.frame(data))
  data <- data
  
  new_function(
    exprs(inventory = , gamma = 5),
    expr({
      choices <- setdiff(unique(pull(data, {{var}})), inventory)
      a <- data %>% 
        filter(!{{var}} %in% inventory) %>% # remove elements chosen in previous iterations
        group_by({{group}}) %>% 
        summarise(y = list({{var}}), .groups = "drop") %>% # nest remaining elements as <chr> for each group (bottleneck)
        tidyr::crossing(c = choices) %>% 
        rowwise() %>% 
        mutate(y = length(y[!y %in% c]), # what is the length of remaining ingredients for each selection?
               y = exp(2 / (1 + gamma * y)) ^ 2) %>% # optimization fn
        ungroup() %>% 
        arrange(c, {{group}}) %>% 
        rename(grp = {{group}})
      
      # below code used to be accomplished with dplyr syntax - significant performance gain using matrices
      with(a, matrix(y, ncol = n_distinct(c), dimnames = list(unique(grp), unique(c)))) %>% 
        colSums() %>% 
        which.max() %>% 
        names()
    }),
    current_env()
  )
}

selection_path <- function(data, var, group, gamma = 5, cutoff = NULL) {
  var <- enexpr(var)
  group <- enexpr(group)
  
  n4 <- n4_0(data, {{var}}, {{group}})
  
  set <- unique(pull(data, {{var}}))
  if (is.null(cutoff)) cutoff <- length(set)
  out <- character(length = cutoff)
  
  for (i in seq_along(out)) {
    message(i)
    
    out[i] <- n4(out, gamma)
  }
  
  out
}

capability2 <- function(x, data, group, var, collapse = TRUE) {
  out <- list()
  
  for (i in seq_along(x)) {
    out[[i]] <- data %>% 
      with_groups({{group}}, filter, all({{var}} %in% x[1:i])) %>% 
      distinct({{group}}) %>% 
      pull()
  }
  
  df <- tibble(i = seq_along(x), x, out)
  
  df1 <- df %>% 
    unnest(cols = out, keep_empty = TRUE) %>% 
    with_groups(i, mutate, performance = n_distinct(out, na.rm = TRUE))
  
  if (!collapse) {
    output <- df1
  } else {
    df1 <- df1 %>%
      with_groups(out, slice_min, i)
    
    output <- df %>%
      filter(!x %in% df1$x) %>%
      select(i, x) %>% 
      bind_rows(df1)
  }
  
  output %>%
    rename({{var}} := x,
           {{group}} := out) %>%
    arrange(i)
}
