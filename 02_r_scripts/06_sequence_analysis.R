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
library(cowplot)

# -------------------------
# 1) Choose your state space
# -------------------------
# Simple 4-state version based on your current state variable:
# none / price / reg / both

df <- read_csv("01_tidy_data/policypanel_long.csv")

panel_seq <- df %>%
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




# 5. Graph Plotting -------------------------------------------------------


plot_transition_network <- function(panel, module_name, state_var = "state8") {
  library(dplyr)
  library(tibble)
  library(tidygraph)
  library(ggraph)
  library(ggplot2)
  library(grid)
  
  panel_module <- panel %>%
    filter(Module == module_name)
  
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
    "all_four",
    "end"
  )
  
  # Node prevalence for observed policy states
  node_data <- panel_module %>%
    count(state = .data[[state_var]], name = "prevalence") %>%
    rename(name = state) %>%
    mutate(name = as.character(name))
  
  # Year-to-year transitions excluding no-change self-transitions
  yearly_transitions <- panel_module %>%
    arrange(ISO, year) %>%
    group_by(ISO) %>%
    mutate(next_state = lead(.data[[state_var]])) %>%
    ungroup() %>%
    filter(!is.na(next_state)) %>%
    filter(.data[[state_var]] != next_state) %>%
    count(
      from = .data[[state_var]],
      to   = next_state,
      name = "n"
    ) %>%
    mutate(type = "transition")
  
  # Add absorbing end-of-sample edges from each country's last observed state
  terminal_edges <- panel_module %>%
    arrange(ISO, year) %>%
    group_by(ISO) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    count(from = .data[[state_var]], name = "n") %>%
    mutate(
      to = "end",
      type = "end"
    )
  
  edges <- bind_rows(yearly_transitions, terminal_edges) %>%
    mutate(
      from = factor(from, levels = state_levels),
      to   = factor(to, levels = state_levels)
    ) %>%
    group_by(from) %>%
    mutate(prob = n / sum(n)) %>%
    ungroup()
  
  # Manual layout with end node at bottom
  node_layout <- tibble(
    name = factor(state_levels, levels = state_levels),
    x = c(
      0,              # none
      
      1,1,1,1,        # one policy
      
      3,3,3,3,3,3,    # two policies
      
      5,5,5,5,        # three policies
      
      6.5,              # all four
      
      8              # end
    ),
    
    y = c(
      0,
      
      3,1,-1,-3,
      
      4.5,3,1,-1,-3,-4.5,
      
      3,1,-1,-3,
      
      0,
      
      0
    )
  ) %>%
    left_join(node_data, by = "name") %>%
    mutate(
      prevalence = replace_na(prevalence, 0),
      label = case_when(
        name == "end" ~ "End of sample",
        TRUE ~ gsub("_", " + ", as.character(name))
      ),
      bundle = case_when(
        name == "end" ~ "End",
        name == "none" ~ "None",
        grepl("^price|_price|four", as.character(name)) ~ "Includes price",
        TRUE ~ "Regulatory only"
      )
    )
  
  
  node_layout <- node_layout %>%
    mutate(
      prevalence = replace_na(prevalence, 0),
      observed = prevalence > 0 | name == "end" # Make end node not blurred
    )
  
  graph <- tbl_graph(
    nodes = node_layout,
    edges = edges,
    directed = TRUE
  )

  
  
  p <- ggraph(graph, layout = "manual", x = x, y = y) +
    geom_edge_link(
      aes(width = prob, linetype = type),
      colour = "grey55",
      alpha = 0.7,
      lineend = "round",
      arrow = arrow(length = unit(2.8, "mm"), type = "closed"),
      end_cap = circle(3, "mm")
    ) +
    geom_node_point(
      aes(
        size = pmax(prevalence, 1),
        fill = bundle,
        alpha = observed
      ),
      shape = 21,
      colour = "grey30",
      stroke = 0.8
    ) +
    scale_alpha_manual(
      values = c(`TRUE` = 1, `FALSE` = 0.15),
      guide = "none"
    ) +
    geom_node_text(
      aes(label = label, alpha = observed),
      size = 3.2,
      fontface = "bold",
      vjust = -2
    ) +
    scale_size_area(max_size = 10, name = "Country-years") +
    scale_fill_manual(
      values = c(
        "Includes price" = "#2C7BB6",
        "Regulatory only" = "#D95F02",
        "None" = "grey30",
        "End" = "grey75"
      ),
      name = "Node type"
    ) +
    scale_edge_width(range = c(0.3, 2.5), name = "Transition probability") +
    scale_edge_linetype_manual(
      values = c("transition" = "solid", "end" = "dashed"),
      labels = c("transition" = "Observed transition", "end" = "End-of-sample link"),
      name = "Edge type"
    ) +
    guides(
      fill = guide_legend(
        order = 1,
        override.aes = list(shape = 21, size = 5, alpha = 1, colour = "grey20")
      ),
      edge_width = guide_legend(
        order = 2,
        override.aes = list(alpha = 1, colour = "grey55", linetype = "solid")
      ),
      edge_linetype = guide_legend(
        order = 3,
        override.aes = list(alpha = 1, colour = "grey55", linewidth = 1.2)
      ),
      size = guide_legend(
        order = 4,
        override.aes = list(shape = 21, fill = "white", colour = "grey20", alpha = 1)
      )
    ) +
    annotate(
      "text",
      x = c(1, 3, 5),
      y = 4.5,
      label = c(
        "1 type",
        "2 types",
        "3 types"
      ),
      fontface = "bold",
      size = 4,
      colour = "grey20"
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 8)),
      legend.position = "right",
      legend.box = "vertical",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 9),
      legend.key.height = unit(4, "mm"),
      legend.key.width = unit(8, "mm"),
      legend.background = element_rect(
        fill = scales::alpha("white", 0.85),
        colour = "grey80",
        linewidth = 0.3
      ),
      legend.margin = margin(4, 4, 4, 4),
      legend.spacing.y = unit(2, "mm")
    )
  
  print(p)
}



plot_transition_network(panel_seq, "Electricity", state_var = "state8")
plot_transition_network(panel_seq, "Industry", state_var = "state8")
plot_transition_network(panel_seq, "Transport", state_var = "state8")
plot_transition_network(panel_seq, "Buildings", state_var = "state8")

# Pre-pricing carbon network graph

plot_pre_carbon_pricing_network <- function(panel, module_name, state_var = "state8") {
  library(dplyr)
  library(tibble)
  library(tidygraph)
  library(ggraph)
  library(ggplot2)
  library(grid)
  
  panel_module <- panel %>%
    filter(Module == module_name)
  
  # Keep only countries that eventually adopt carbon pricing
  pre_df <- panel_module %>%
    filter(
      !is.na(first_price_year),
      year < first_price_year
    )
  
  state_levels <- c(
    "none",
    "reg",
    "subsidy",
    "standard",
    "reg_subsidy",
    "reg_standard",
    "subsidy_standard",
    "reg_subsidy_standard",
    "carbon_pricing"
  )
  
  node_data <- pre_df %>%
    count(state = .data[[state_var]], name = "prevalence") %>%
    rename(name = state) %>%
    mutate(name = as.character(name))
  
  yearly_transitions <- pre_df %>%
    arrange(ISO, year) %>%
    group_by(ISO) %>%
    mutate(next_state = lead(.data[[state_var]])) %>%
    ungroup() %>%
    filter(!is.na(next_state)) %>%
    filter(.data[[state_var]] != next_state) %>%
    count(from = .data[[state_var]], to = next_state, name = "n") %>%
    mutate(type = "transition")
  
  terminal_edges <- pre_df %>%
    arrange(ISO, year) %>%
    group_by(ISO) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    count(from = .data[[state_var]], name = "n") %>%
    mutate(
      to = "carbon_pricing",
      type = "carbon_pricing"
    )
  
  edges <- bind_rows(yearly_transitions, terminal_edges) %>%
    mutate(
      from = factor(from, levels = state_levels),
      to   = factor(to, levels = state_levels)
    ) %>%
    group_by(from) %>%
    mutate(prob = n / sum(n)) %>%
    ungroup()
  
  node_layout <- tibble(
    name = factor(state_levels, levels = state_levels),
    x = c(
      0,   # none
      2, 2, 2,   # single-policy states
      4, 4, 4,   # two-policy states
      6,         # three-policy state
      8          # carbon pricing
    ),
    y = c(
      0,
      2, 0, -2,
      2, 0, -2,
      0,
      0
    )
  ) %>%
    left_join(node_data, by = "name") %>%
    mutate(
      prevalence = replace_na(prevalence, 0),
      observed = prevalence > 0 | name == "carbon_pricing",
      label = case_when(
        name == "carbon_pricing" ~ "Carbon pricing",
        TRUE ~ gsub("_", " + ", as.character(name))
      ),
      bundle = case_when(
        name == "carbon_pricing" ~ "Carbon pricing",
        name == "none" ~ "None",
        grepl("subsidy", as.character(name)) & grepl("reg", as.character(name)) ~ "Mixed",
        grepl("subsidy", as.character(name)) ~ "Subsidy only",
        TRUE ~ "Regulatory only"
      )
    )
  
  graph <- tbl_graph(
    nodes = node_layout,
    edges = edges,
    directed = TRUE
  )
  
  p <- ggraph(graph, layout = "manual", x = x, y = y) +
    geom_edge_link(
      aes(width = prob, linetype = type),
      colour = "grey55",
      alpha = 0.7,
      lineend = "round",
      arrow = arrow(length = unit(2.8, "mm"), type = "closed"),
      end_cap = circle(3, "mm")
    ) +
    geom_node_point(
      aes(size = pmax(prevalence, 1), fill = bundle, alpha = observed),
      shape = 21,
      colour = "grey30",
      stroke = 0.8
    ) +
    geom_node_text(
      aes(label = label, alpha = observed),
      size = 3.2,
      fontface = "bold",
      vjust = -2
    ) +
    scale_alpha_manual(
      values = c(`TRUE` = 1, `FALSE` = 0.15),
      guide = "none"
    ) +
    scale_size_area(max_size = 10, name = "Country-years") +
    scale_fill_manual(
      values = c(
        "Regulatory only" = "#D95F02",
        "Subsidy only" = "#2C7BB6",
        "Mixed" = "#7B3294",
        "None" = "grey30",
        "Carbon pricing" = "grey75"
      ),
      name = "Node type"
    ) +
    scale_edge_width(range = c(0.3, 2.5), name = "Transition probability") +
    scale_edge_linetype_manual(
      values = c("transition" = "solid", "carbon_pricing" = "dashed"),
      labels = c("transition" = "Observed transition", "carbon_pricing" = "Carbon pricing"),
      name = "Edge type"
    ) +
    guides(
      fill = guide_legend(
        order = 1,
        nrow = 2,
        byrow = TRUE,
        override.aes = list(shape = 21, size = 5, alpha = 1, colour = "grey20")
      ),
      edge_width = guide_legend(
        order = 2,
        override.aes = list(alpha = 1, colour = "grey55", linetype = "solid")
      ),
      edge_linetype = guide_legend(
        order = 3,
        override.aes = list(alpha = 1, colour = "grey55", linewidth = 1.2)
      ),
      size = guide_legend(
        order = 4,
        override.aes = list(shape = 21, fill = "white", colour = "grey20", alpha = 1)
      )
    ) +
    annotate(
      "text",
      x = c(0, 2, 4, 6, 8),
      y = -3.2,
      label = c("No policy", "1 policy", "2 policies", "3 policies", "Carbon pricing"),
      fontface = "bold",
      size = 4,
      colour = "grey30"
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 8)),
      legend.position = "right",
      legend.box = "vertical",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 9),
      legend.key.height = unit(4, "mm"),
      legend.key.width = unit(8, "mm"),
      legend.background = element_rect(
        fill = scales::alpha("white", 0.85),
        colour = "grey80",
        linewidth = 0.3
      ),
      legend.margin = margin(4, 4, 4, 4),
      legend.spacing.y = unit(2, "mm"),
      plot.margin = margin(8, 20, 20, 8)
    ) +
    labs(
      title = "Policy transitions before carbon pricing",
      subtitle = "Countries are followed only until the first year of carbon pricing"
    )
  
  print(p)
}

plot_pre_carbon_pricing_network(panel_seq, "Electricity", state_var = "state8")
plot_pre_carbon_pricing_network(panel_seq, "Industry", state_var = "state8")
plot_pre_carbon_pricing_network(panel_seq, "Transport", state_var = "state8")
plot_pre_carbon_pricing_network(panel_seq, "Buildings", state_var = "state8")


# Policy transition heatmap

plot_policy_transition_heatmap <- function(panel, module_name, policy_var = "Policy") {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  
  panel_module <- panel %>%
    filter(Module == module_name) %>%
    mutate(
      intro = as.integer(introduction == 1)
    ) %>%
    arrange(ISO, year)
  
  # Keep only actual policy introductions, and only before carbon pricing
  pre_df <- panel_module %>%
    filter(intro == 1, !is.na(first_price_year), year < first_price_year) %>%
    select(ISO, year, policy = all_of(policy_var)) %>%
    arrange(ISO, year, policy)
  
  # Transitions between successive policy introductions
  trans <- pre_df %>%
    group_by(ISO) %>%
    mutate(next_policy = lead(policy)) %>%
    ungroup() %>%
    filter(!is.na(next_policy), policy != next_policy) %>%
    count(from = policy, to = next_policy, name = "n") %>%
    group_by(from) %>%
    mutate(prob = n / sum(n)) %>%
    ungroup()
  
  # Order policies by total involvement in transitions
  policy_order <- trans %>%
    select(from, to, n) %>%
    pivot_longer(c(from, to), values_to = "policy") %>%
    count(policy, wt = n, sort = TRUE) %>%
    pull(policy)
  
  trans <- trans %>%
    mutate(
      from = factor(from, levels = rev(policy_order)),
      to   = factor(to, levels = policy_order)
    )
  
  ggplot(trans, aes(x = to, y = from, fill = prob)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(
      aes(label = ifelse(prob >= 0.05, sprintf("%.2f", prob), "")),
      size = 3
    ) +
    scale_fill_gradient(low = "grey95", high = "#2C7BB6", name = "Probability") +
    labs(
      title = paste0("Pre-pricing policy transitions: ", module_name),
      subtitle = "Rows show the previous policy introduction; columns show the next policy introduction",
      x = "Next policy introduced",
      y = "Previous policy introduced"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )
}

plot_policy_transition_heatmap(panel_seq, "Transport", policy_var = "Policy_new")
