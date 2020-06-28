cocktails <- read_rds("cocktails.rds")
optimal_full <- read_rds("optimal-selection.rds")

# optimal_full <- read_rds("optimal-selection.rds")
# 
# cocktails <- read_rds("cocktails.rds") %>% 
#   with_groups(drink, filter, all(ingredient %in% optimal_full[, "ingredient"][[1]]))

# all nodes with highest amount of edges should be concentrated toward center
# common edges within rungs should be omitted

# edges1 <- cocktails %>% 
#   select(drink, ingredient) %>% 
#   inner_join(., ., by = "drink") %>% 
#   filter(ingredient.x != ingredient.y) %>% 
#   rowwise() %>%
#   mutate(ingredient = list(sort(c(ingredient.x, ingredient.y))),
#          .keep = "unused") %>%
#   ungroup() %>%
#   distinct() %>%
#   rowid_to_column(var = "edge") %>% 
#   unnest(cols = ingredient) %>% 
#   add_count(ingredient) %>% 
#   mutate(percentile = n / max(n))

# nodes <- edges1 %>% 
#   distinct(ingredient, percentile) %>% 
#   mutate(grp = cut_number(percentile, nrow(.) %/% 50 + 2) %>% as.integer(),
#          # grp = rev(sort(unique(grp)))[grp],
#          .keep = "unused")

# edges2 <- edges1 %>% 
#   left_join(nodes) %>% 
#   group_by(drink, edge) %>% 
#   filter(max(grp) - min(grp) == 1) %>% 
#   summarise(tibble(A = ingredient[1], B = ingredient[2]),
#             .groups = "drop") %>% 
#   relocate(A:B)

edges1 <- cocktails %>%
  select(ingredient, drink) %>%
  inner_join(., ., by = "ingredient") %>%
  filter(drink.x != drink.y) %>%
  rowwise() %>%
  mutate(drink = list(sort(c(drink.x, drink.y))),
         .keep = "unused") %>%
  ungroup() %>%
  distinct() %>%
  rowid_to_column(var = "edge") %>%
  unnest(cols = drink) %>%
  add_count(drink) %>%
  mutate(percentile = n / max(n))

generate_net <- function(drinks, ingredients) {
  nodes <- edges1 %>%
    distinct(drink, percentile) %>%
    mutate(grp = 1,
           selected = drink %in% drinks,
           show_label = if_else(selected & (sum(selected) < 20), TRUE, FALSE))
  
  set.seed(17)
  l <- circleFun(nodes$grp)
  
  edges2 <- edges1 %>% 
    left_join(nodes) %>% 
    group_by(ingredient, edge) %>% 
    filter(all(drink %in% drinks),
           ingredient %in% ingredients) %>%
    summarise(tibble(A = drink[1], B = drink[2]),
              .groups = "drop") %>% 
    relocate(A:B)

  net <- graph_from_data_frame(d = edges2, vertices = nodes)
  
  ggraph(net, layout = l, circular = TRUE) +
    geom_node_point(aes(color = selected, size = as.integer(selected), alpha = as.integer(selected))) +
    scale_size(range = c(0, 10)) +
    geom_edge_arc(n = 400, color = "#79D7F6") +
    geom_node_text(aes(filter = show_label, label = name), size = 5, nudge_y = 0.07) +
    scale_alpha(range = c(0.1, 1)) +
    scale_color_manual(values = c(`FALSE` = "#838B96", `TRUE` = "#79D7F6")) +
    theme_void() +
    coord_fixed() +
    theme(legend.position = "none")
}

# V(net)$size <- 8
# V(net)$frame.color <- "#838B96"
# V(net)$label <- ""
# E(net)$arrow.mode <- 0
# E(net)$curved <- 0.4
# 
# plot(net, layout = circleFun(V(net)$grp))
# 




