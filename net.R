generate_net <- function(data, inventory, fulfilled) {
  edges1 <- data %>%
    select(ingredient, drink) %>%
    inner_join(., ., by = "ingredient") %>%
    filter(drink.x != drink.y) %>%
    rowwise() %>%
    mutate(drink = list(sort(c(drink.x, drink.y))),
           .keep = "unused") %>%
    ungroup() %>%
    distinct() %>%
    rowid_to_column(var = "edge") %>%
    unnest(cols = drink)
  
  partial <- edges1 %>% filter(ingredient %in% inventory) %>% distinct(drink) %>% pull()
  
  set.seed(17)
  
  nodes <- edges1 %>%
    distinct(drink) %>%
    sample_n(size = n()) %>% 
    mutate(grp = 1,
           status = case_when(
             drink %in% fulfilled ~ "fulfilled",
             drink %in% partial ~ "partial",
             TRUE ~ "dormant")
             
    )
  
  l <- circleFun(nodes$grp)
  l_label <- as_tibble(l) %>% 
    mutate(across(everything(), ~ ..1 * 1.2),
           label = nodes$drink)
    
  
  edges2 <- edges1 %>% 
    left_join(nodes) %>% 
    group_by(ingredient, edge) %>% 
    mutate(status = factor(status, levels = c("fulfilled", "partial", "dormant"), ordered = TRUE)) %>% 
    summarise(tibble(A = drink[1], B = drink[2], status = max(status)),
              .groups = "drop") %>% 
    arrange(desc(status)) %>% 
    modify_at(vars(status), as.character) %>% 
    relocate(A:B)
  
  net <- graph_from_data_frame(d = edges2, vertices = nodes)
  
  ggraph(net, layout = l, circular = TRUE) +
    geom_node_point(aes(color = status), size = 20) +
    geom_edge_arc(aes(color = status), n = 100, width = 1) +
    geom_node_label(data = l_label, aes(label = label), size = 5, repel = TRUE) +
    scale_color_manual(values = colors) +
    scale_edge_color_manual(values = colors) +
    theme_void() +
    coord_fixed(ylim = c(-1.5, 1.5), xlim = c(-1.5, 1.5)) +
    theme(legend.position = "none")
}
