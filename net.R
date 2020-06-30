cocktails <- read_rds("cocktails.rds")
optimal_full <- read_rds("optimal-selection.rds")

o <- sample(seq_len(length(unique(cocktails$drink))))

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
    slice(o) %>% 
    mutate(grp = 1,
           selected = drink %in% drinks,
           show_label = if_else(selected & (sum(selected) <= 30), TRUE, FALSE))
  
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
    geom_node_text(aes(filter = show_label, label = name), repel = TRUE, size = 5, nudge_y = 0.025) +
    scale_alpha(range = c(0.1, 1)) +
    scale_color_manual(values = c(`FALSE` = "#838B96", `TRUE` = "#79D7F6")) +
    theme_void() +
    coord_fixed() +
    theme(legend.position = "none")
}




