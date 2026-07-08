# =========================
# Sequence analysis template
# =========================

# Packages
library(dplyr)
library(tidyr)
library(TraMineR)
library(cluster)
library(tidygraph)
library(ggraph)

# -------------------------
# 1) Choose your state space
# -------------------------
# Simple 4-state version based on your current state variable:
# none / price / reg / both
panel_seq <- panel %>%
  mutate(
    state4 = as.character(state)
  )

# Optional: if you want a richer state space using all 4 policy types,
# build a more detailed composite state instead.
# Comment this block out if you only want the 4-state version.
panel_seq <- panel_seq %>%
  mutate(
    state8 = case_when(
      price == 0 & reg == 0 & subsidy == 0 & standard == 0 ~ "none",
      price == 1 & reg == 0 & subsidy == 0 & standard == 0 ~ "price",
      price == 0 & reg == 1 & subsidy == 0 & standard == 0 ~ "reg",
      price == 0 & reg == 0 & subsidy == 1 & standard == 0 ~ "subsidy",
      price == 0 & reg == 0 & subsidy == 0 & standard == 1 ~ "standard",
      price == 1 & reg == 1 & subsidy == 0 & standard == 0 ~ "price_reg",
      price == 1 & reg == 0 & subsidy == 1 & standard == 0 ~ "price_subsidy",
      price == 1 & reg == 0 & subsidy == 0 & standard == 1 ~ "price_standard",
      price == 0 & reg == 1 & subsidy == 1 & standard == 0 ~ "reg_subsidy",
      price == 0 & reg == 1 & subsidy == 0 & standard == 1 ~ "reg_standard",
      price == 0 & reg == 0 & subsidy == 1 & standard == 1 ~ "subsidy_standard",
      price == 1 & reg == 1 & subsidy == 1 & standard == 0 ~ "price_reg_subsidy",
      price == 1 & reg == 1 & subsidy == 0 & standard == 1 ~ "price_reg_standard",
      price == 1 & reg == 0 & subsidy == 1 & standard == 1 ~ "price_subsidy_standard",
      price == 0 & reg == 1 & subsidy == 1 & standard == 1 ~ "reg_subsidy_standard",
      price == 1 & reg == 1 & subsidy == 1 & standard == 1 ~ "all_four",
      TRUE ~ "missing"
    )
  )

# ------------------------------------------------
# 2) Helper: convert long panel -> wide sequences
# ------------------------------------------------
prep_seq_df <- function(dat, state_col = "state4") {
  year_grid <- seq(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE), by = 1)
  
  dat %>%
    distinct(ISO, Module, year, .keep_all = TRUE) %>%
    select(ISO, Module, year, all_of(state_col)) %>%
    rename(state = all_of(state_col)) %>%
    group_by(ISO, Module) %>%
    complete(year = year_grid, fill = list(state = "none")) %>%
    ungroup() %>%
    arrange(ISO, Module, year)
}

make_seqobj <- function(dat_long) {
  wide <- dat_long %>%
    mutate(year = paste0("y", year)) %>%
    pivot_wider(names_from = year, values_from = state)
  
  #seq_mat <- wide %>%
    #select(starts_with("y"))
  
  #rownames(seq_mat) <- wide$ISO
  #rownames(seq_mat) <- paste(wide$ISO, wide$Module, sep = "_")
  
  #seqdef(seq_mat)
  
  seq_df <- wide %>%
    select(starts_with("y")) %>%
    as.data.frame()
  
  # Use ISO if unique, otherwise make it unique
  rownames(seq_df) <- make.unique(as.character(wide$ISO))
  head(rownames(seq_df))
  seqdef(seq_df)
}

# ------------------------------------------------
# 3) Run sequence analysis for each sector/module
# ------------------------------------------------
modules <- sort(unique(panel_seq$Module))

results <- vector("list", length(modules))
names(results) <- modules

for (m in modules) {
  message("Running sequence analysis for: ", m)
  
  dat_m <- panel_seq %>%
    filter(Module == m)
  
  # choose which state variable to analyze:
  # use state4 for the simple 4-state analysis
  # use state8 for the richer policy-mix analysis
  #dat_m_seq <- prep_seq_df(dat_m, state_col = "state4")
   dat_m_seq <- prep_seq_df(dat_m, state_col = "state8")
  
  seq_m <- make_seqobj(dat_m_seq)
  
  # Descriptive plots
  pdf(paste0("sequence_plots_", m, ".pdf"), width = 12, height = 8)
  par(mfrow = c(2, 2))
  seqdplot(seq_m, with.legend = "right", main = paste(m, "- state distribution"))
  seqIplot(seq_m, sortv = "from.start", main = paste(m, "- index plot"))
  seqfplot(seq_m, with.legend = "right", main = paste(m, "- sequence frequencies"))
  seqHtplot(seq_m, with.legend = "right", main = paste(m, "- state entropy"))
  seqmsplot(seq_m, with.legend = "right", main = paste(m, "- state entropy"))

  dev.off()
  
  # Most common sequences
  seq_strings <- apply(as.data.frame(seq_m), 1, paste, collapse = " -> ")
  top_sequences <- sort(table(seq_strings), decreasing = TRUE)
  
  # State transition matrix
  transition_matrix <- seqtrate(seq_m)
  
  # Distance matrix for clustering
  sm <- seqsubm(seq_m, method = "TRATE")
  diss <- seqdist(seq_m, method = "OM", sm = sm, indel = 1)
  
  # Hierarchical clustering
  hc <- hclust(as.dist(diss), method = "ward.D2")
  clusters <- cutree(hc, k = 4)
  
  # Save a dendrogram
  pdf(paste0("sequence_cluster_", m, ".pdf"), width = 12, height = 8)
  plot(hc, labels = FALSE, main = paste(m, "- sequence clustering"))
  rect.hclust(hc, k = 4, border = 2:5)
  dev.off()
  
  # Cluster sizes
  cluster_sizes <- table(clusters)
  
  # Put results in a list
  results[[m]] <- list(
    seqobj = seq_m,
    dat_long = dat_m_seq,
    top_sequences = top_sequences,
    transition_matrix = transition_matrix,
    dist = diss,
    hc = hc,
    clusters = clusters,
    cluster_sizes = cluster_sizes
  )
}

# ------------------------------------------------
# 4) Inspect one module, e.g. Industry
# ------------------------------------------------
results$Industry$cluster_sizes
head(results$Industry$top_sequences, 10)
round(results$Industry$transition_matrix, 3)

# Attach clusters back to the original sequence data for one module
Industry_seq <- results$Industry$seqobj
Industry_clusters <- results$Industry$clusters

# Plot sequences by cluster
pdf("Industry_cluster_iplot.pdf", width = 12, height = 8)

# Give each cluster its own page
for (k in sort(unique(Industry_clusters))) {
  sub_seq <- Industry_seq[Industry_clusters == k, ]
  
  if (nrow(sub_seq) == 0) next
  
  seqIplot(
    sub_seq,
    sortv = "from.start",
    ylab = "ISO",
    main = paste("Industry - Cluster", k)
  )
}

dev.off()

seqIplot(
  Industry_seq,
  group = Industry_clusters,
  sortv = "from.start",
  ylab = "ISO",
  main = "Industry by cluster"
)

# ------------------------------------------------
# 5) Export a cluster membership file for each module
# ------------------------------------------------
cluster_membership <- bind_rows(lapply(names(results), function(m) {
  cl <- results[[m]]$clusters
  ids <- rownames(results[[m]]$seqobj)
  
  tibble(
    id = ids,
    Module = m,
    cluster = as.integer(cl)
  )
}))

write.csv(cluster_membership, "sequence_clusters_all_modules.csv", row.names = FALSE)




plot_transition_network <- function(panel, module_name, state_var = "state8") {
  library(dplyr)
  library(tibble)
  library(tidygraph)
  library(ggraph)
  library(ggplot2)
  library(grid)
  
  panel_module <- panel %>%
    filter(Module == module_name)
  
  node_data <- panel_module %>%
    count(state = .data[[state_var]], name = "prevalence") %>%
    rename(name = state)
  
  transitions <- panel_module %>%
    arrange(ISO, year) %>%
    group_by(ISO) %>%
    mutate(next_state = lead(.data[[state_var]])) %>%
    ungroup() %>%
    filter(!is.na(next_state)) %>%
    filter(.data[[state_var]] != next_state) %>%
    count(
      from = .data[[state_var]],
      to = next_state,
      name = "n"
    ) %>%
    group_by(from) %>%
    mutate(prob = n / sum(n)) %>%
    ungroup()
  
  state_levels <- c(
    "none",
    "price",
    "reg",
    "standard",
    "subsidy",
    "price_reg",
    "price_standard",
    "price_subsidy",
    "reg_standard",
    "reg_subsidy",
    "subsidy_standard",
    "price_reg_standard",
    "price_reg_subsidy",
    "price_subsidy_standard",
    "reg_subsidy_standard",
    "all_four"
  )
  
  # Diamond-style manual layout:
  # top = none, bottom = all_four
  node_layout <- tibble(
    name = factor(state_levels, levels = state_levels),
    x = c(
      0,                 # none
      -3, -1, 1, 3,      # single-policy states
      -5, -3, -1, 1, 3, 5,# two-policy states
      -3, -1, 1, 3,      # three-policy states
      0                  # all_four
    ),
    y = c(
      5,                 # none
      4, 4, 4, 4,        # single-policy states
      3, 3, 3, 3, 3, 3,  # two-policy states
      2, 2, 2, 2,        # three-policy states
      0                  # all_four
    )
  )
  # Prevalence for scaling nodes
  node_layout <- node_layout %>%
    left_join(node_data, by = "name") %>%
    mutate(
      prevalence = replace_na(prevalence, 0)
    )
  
  #Price indicator for colouring
  node_layout <- node_layout %>%
    mutate(
      price_bundle = if_else(
        grepl("^price|_price|four", as.character(name)),
        "Includes price",
        "Regulatory only"
      )
    )
  
  edges <- transitions %>%
    mutate(
      from = factor(from, levels = state_levels),
      to   = factor(to, levels = state_levels)
    )
  
  graph <- tbl_graph(
    nodes = node_layout,
    edges = edges,
    directed = TRUE
  )
  
  p <- ggraph(graph, layout = "manual", x = x, y = y) +
    geom_edge_link(
      aes(width = prob, alpha = prob),
      colour = "grey55",
      lineend = "round",
      arrow = arrow(length = unit(2.8, "mm"), type = "closed"),
      end_cap = circle(4.5, "mm")
    ) +
    geom_node_point(
      aes(size = prevalence, fill = price_bundle),
      shape = 21,
      colour = "grey20",
      stroke = 0.8
    ) +
    scale_size_area(
      max_size = 12,
      name = "Country-years"
    ) +
    scale_fill_manual(
      values = c(
        "Includes price" = "#2C7BB6",
        "Regulatory only" = "#D95F02",
        "none" = "#000000"
      ),
      name = NULL) +
    geom_node_text(
      aes(label = gsub("_", " + ", as.character(name))),
      size = 3.4,
      fontface = "bold",
      colour = "black",
      vjust = -1.9
    ) +
    scale_edge_width(range = c(0.4, 4.5)) +
    scale_edge_alpha(range = c(0.15, 0.85)) +
    guides(
      fill = "none",
      edge_width = guide_legend(title = "Transition probability"),
      edge_alpha = "none"
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 8)),
      legend.position = "bottom"
    ) +
    labs(
      title = "Sector policy sequencing network",
      subtitle = "Diamond layout with fixed top-to-bottom hierarchy"
    )
  
  print(p)
}



plot_transition_network(panel_seq, "Electricity", state_var = "state8")
plot_transition_network(panel_seq, "Industry", state_var = "state8")
plot_transition_network(panel_seq, "Transport", state_var = "state8")
plot_transition_network(panel_seq, "Buildings", state_var = "state8")

