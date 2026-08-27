library(dplyr)
library(tidyr)
library(TraMineR)
library(cluster)
library(tidygraph)
library(ggraph)
library(cowplot)
library(readr)
library(scales)

#=========================================================
# Project: Climate Policy Sequencing
# File: 06_sequence_analysis.r
# Description: This script categorises the policy into sequences, 
# and has a variety of plots and network graphs to reflect this 
# Inputs: policypanel_long.csv
# Outputs: No csvs. Lots of plots
#=========================================================


# 1. State spaces and Sequence formation -------------------------------------


# 1) Construct state spaces within sequence

df <- read_csv("01_tidy_data/policypanel_long.csv")

# Use simple state designated in 04_msm.R (none, price, reg, both)
panel_seq <- df %>%
  mutate(
    state4 = as.character(state)
  )

# Richer state that builds a more detailed composite state
panel_seq <- panel_seq %>%
  mutate(
    state16 = case_when(
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
    ), 
    state8 = case_when(
      price == 0 & subsidy == 0 & standard == 0 ~ "none",
      price == 1 & subsidy == 0 & standard == 0 ~ "price",
      price == 0 & subsidy == 1 & standard == 0 ~ "subsidy",
      price == 0 & subsidy == 0 & standard == 1 ~ "standard",
      price == 1 & subsidy == 1 & standard == 0 ~ "price_subsidy",
      price == 1 & subsidy == 0 & standard == 1 ~ "price_standard",
      price == 0 & subsidy == 1 & standard == 1 ~ "subsidy_standard",
      price == 1 & subsidy == 1 & standard == 1 ~ "all_three",
      TRUE ~ "missing"
    ),
    state8reg = case_when(
      price == 0 & subsidy == 0 & reg == 0 ~ "none",
      price == 1 & subsidy == 0 & reg == 0 ~ "price",
      price == 0 & subsidy == 1 & reg == 0 ~ "subsidy",
      price == 0 & subsidy == 0 & reg == 1 ~ "reg",
      price == 1 & subsidy == 1 & reg == 0 ~ "price_subsidy",
      price == 1 & subsidy == 0 & reg == 1 ~ "price_reg",
      price == 0 & subsidy == 1 & reg == 1 ~ "subsidy_reg",
      price == 1 & subsidy == 1 & reg == 1 ~ "all_three",
      TRUE ~ "missing"
    )
  )

# Orders for use when plotting
state_order <- c(
  "none",
  "price",
  "subsidy",
  "standard",
  "price_subsidy",
  "price_standard",
  "subsidy_standard",
  "all_three"
)

state_order_reg <- c(
  "none",
  "price",
  "subsidy",
  "reg",
  "price_subsidy",
  "price_reg",
  "subsidy_reg",
  "all_three"
)

state_colors_reg <- c(
  none            = "#F9F9F9",
  price           = "#3978B5",
  subsidy         = "#E6A04B",
  reg             = "#D95555",
  price_subsidy   = "#559B91",
  price_reg       = "#8D5C9E",
  subsidy_reg     = "#C87545",
  all_three       = "#625C68"
)

# Helper that summarises long policy panel into country-module-year-state panel
prep_seq_df <- function(dat, state_col = "state8reg") {
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

# Helper that pivots prep_seq_df's output to a wide format (years as columns) for TramineR to analyse
make_seqobj <- function(dat_long) {
  wide <- dat_long %>%
    mutate(year = paste0("y", year)) %>%
    pivot_wider(names_from = year, values_from = state)
  
  seq_df <- wide %>%
    select(starts_with("y")) %>%
    as.data.frame()
  
  # Use ISO if unique, otherwise make it unique
  rownames(seq_df) <- make.unique(as.character(wide$ISO))
  
  
  head(rownames(seq_df))
  seqdef(seq_df, alphabet = state_order_reg, cpal = unname(state_colors_reg))
}


# 2. Sectoral Sequence Analysis and Plotting ----------------------------------------------------

# Run sequence analysis for each sector (referred to as module in the data)
modules <- sort(unique(panel_seq$Module))

results <- vector("list", length(modules))
names(results) <- modules

seq_objects <- list()

for (m in modules) {
  message("Running sequence analysis for: ", m)
  
  dat_m <- panel_seq %>%
    filter(Module == m)
  
  # choose which state variable to analyze:
   dat_m_seq <- prep_seq_df(dat_m, state_col = "state8reg")
   seq_m <- make_seqobj(dat_m_seq)
   
   seq_objects[[m]] <- make_seqobj(dat_m_seq) # make data into a sequence object
   
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

# Figure 4.2: State Distributions
mods <- names(seq_objects)
layout(matrix(c(1, 2, 5,
                3, 4, 5), nrow = 2, byrow = TRUE),
       widths = c(1, 1, 0.4))

par(mar = c(3, 3, 3, 1))

for (i in seq_along(mods)) {
  m <- mods[i]
  seqobj <- seq_objects[[m]]
  
  xtlab_clean <- gsub("^y", "", colnames(as.data.frame(seqobj)))
  
  seqdplot(
    seqobj,
    family = "Times",
    with.legend = FALSE,
    main = m,
    border = NA,
    xlab = "",
    ylab = if (i %in% c(1, 3)) "Frequency" else "",
    xtlab = xtlab_clean,
    cex.main = 1.8,
    cex.lab  = 1.2,
    cex.axis = 1.1,
    xaxis = i %in% c(3, 4),
    yaxis = i %in% c(1, 3)
  )
}

legend_labels <- c(
  "None",
  "Price",
  "Subsidy",
  "Regulation",
  "Price + Subsidy",
  "Price + Regulation",
  "Subsidy + Regulation",
  "All Three"
)

# legend panel on the right
par(mar = c(0, 0, 0, 0))
par(family = "Times-Roman")
plot.new()
legend(
  "center",
  title = "Policy State",
  legend = legend_labels,
  fill = attr(seq_objects[[mods[1]]], "cpal"),
  bty = "n",
  cex = 1.7,
  xpd = NA
)

# legend panel below figure, horizontal
par(mfrow = c(1, 1))
par(mar = c(0, 0, 0, 0))
par(family = "Times-Roman")
plot.new()

legend(
  "center",
  legend = legend_labels,
  fill = attr(seq_objects[[mods[1]]], "cpal"),
  bty = "n",
  cex = 1.2,
  ncol = 4,
  xpd = NA,
  x.intersp = 0.8,
  text.width = 0.17,   # controls column width
  y.intersp = 1.2
)

par(mfrow = c(1, 1))
par(mar = c(0, 0, 0, 0))
par(family = "Times-Roman")

plot.new()

# Use the same width for all four columns
col_width <- max(
  strwidth(
    legend_labels,
    units = "user",
    cex = 1.1,
    family = "Times-Roman"
  )
)

legend(
  "center",
  title = "Policy State",
  legend = legend_labels,
  fill = attr(seq_objects[[mods[1]]], "cpal"),
  bty = "n",
  cex = 1.3,
  ncol = 4,
  xpd = NA,
  x.intersp = 0.8,
  text.width = col_width,
  y.intersp = 1.2
)

# Figure A2.4: Sequence Frequencies 
par(mar = c(4, 5, 3, 1))
for (i in seq_along(mods)) {
  m <- mods[i]
  seqobj <- seq_objects[[m]]
  
  xtlab_clean <- gsub("^y", "", colnames(as.data.frame(seqobj)))
  
  seqfplot(
    seqobj,
    idxs = 1:8,
    family = "Times",
    with.legend = FALSE,
    main = m,
    border = NA,
    xlab = "",
    xtlab = xtlab_clean,
    cex.main = 1.8,
    cex.lab  = 1.2,
    cex.axis = 1.1,
    xaxis = i %in% c(3, 4),
    yaxis = TRUE
  )
}

par(mar = c(0, 0, 0, 0))
par(family = "Times-Roman")
plot.new()
legend(
  "center",
  title = "Policy State",
  legend = alphabet(seq_objects[[mods[1]]]),
  fill = attr(seq_objects[[mods[1]]], "cpal"),
  bty = "n",
  cex = 1.7,
  xpd = NA
)
  
# Figure 4.3: Cluster graph for Electricity 
results$Electricity$cluster_sizes
head(results$Electricity$top_sequences, 10)
round(results$Electricity$transition_matrix, 3)

# Attach clusters back to the original sequence data for one module
Electricity_seq <- results$Electricity$seqobj

# Get country names back
country_labels <- results$Electricity$dat_long |>
  dplyr::distinct(ISO) |>
  dplyr::pull(ISO)

length(country_labels)
nrow(Electricity_seq)

rownames(Electricity_seq) <- country_labels
Electricity_clusters <- results$Electricity$clusters

par(mar = c(4, 4, 2, 0.2), xpd = NA)

cluster_names <- c(
  "1: Subsidy-Led Developing Countries",
  "2: Policy Leaders",
  "3: Mixed Pathways",
  "4: EU Followers"
)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (k in sort(unique(Electricity_clusters))) {
  sub_seq <- Electricity_seq[Electricity_clusters == k, ]

  seqIplot(
    sub_seq,
    sortv = "from.start",
    with.legend = FALSE,
    cex.main = 1.3,
    cex.axis = 0.8,
    las = 2,
    xtstep = 4,
    xtlab = xtlab_clean,
    border = NA,
    xaxis = TRUE,
    yaxis = TRUE,
    ylab = "",
    ytlab = "id",
    main = cluster_names[k]
  )
}

# Plots for other module clusters
plot_module_clusters <- function(module_name, cluster_names = NULL) {
  
  # Get sequence object and clusters
  seq_obj <- results[[module_name]]$seqobj
  clusters <- results[[module_name]]$clusters
  
  # Get country names
  country_labels <- results[[module_name]]$dat_long |>
    dplyr::distinct(ISO) |>
    dplyr::pull(ISO)
  
  # Attach country labels
  rownames(seq_obj) <- country_labels
  
  # Default cluster names
  if (is.null(cluster_names)) {
    cluster_names <- paste("Cluster", sort(unique(clusters)))
  }
  
  # Common x-axis labels
  xtlab_clean <- gsub("^y", "", colnames(as.data.frame(seq_obj)))
  
  # Set up 2 x 2 layout
  par(
    mfrow = c(2, 2),
    mar = c(4, 4, 3, 1),
    xpd = NA,
    family = "Times-Roman"
  )
  
  # Plot each cluster
  for (k in sort(unique(clusters))) {
    
    sub_seq <- seq_obj[clusters == k, ]
    
    seqIplot(
      sub_seq,
      sortv = "from.start",
      with.legend = FALSE,
      cex.main = 1.3,
      cex.axis = 0.8,
      las = 2,
      xtstep = 4,
      xtlab = xtlab_clean,
      border = NA,
      xaxis = TRUE,
      yaxis = TRUE,
      ylab = "",
      ytlab = "id",
      main = cluster_names[k]
    )
  }
}

plot_module_clusters("Buildings")

plot_module_clusters("Industry")

# Transport
plot_module_clusters("Transport")

cluster_names <- c(
  "Subsidy–Regulation-First\nDeveloping Economies",
  "Policy Leaders",
  "Mixed Sequences",
  "Price-Forward\nDeveloped Economies"
)

electricity_plot <- plot_sequence_cluster_workflow(
  results = results,
  module = "Transport",
  cluster_names = cluster_names,
  xtlab_clean = xtlab_clean,
  outdir = ".",
  k = 4
)

# Dendogram 
plot(hc, labels = FALSE, main = paste(m, "- sequence clustering"))
rect.hclust(hc, k = 4, border = 2:5)

# Simple sequence plot (Fig 4.1)

shape_map <- c(
  "Technology Standards" = 16,
  "Performance Standards" = 17,
  "Carbon Pricing" = 15,
  "Subsidy" = 18,
  "Information" = 20,
  "Other" = 8
)

# Color map for sector/module
module_map <- c(
  "Buildings" = "#7B3294",
  "Electricity" = "#E41A1C",
  "Industry" = "#00BFC4",
  "Transport" = "#D95F02"
)

plot_df <- df %>%
  mutate(
    Policytype_simple = case_when(
      Policytype_detail_new %in% c(
        "Other regulatory instruments",
        "Other MBI",
        "Other NMBI"
      ) ~ "Other",
      TRUE ~ Policytype_detail_new
    )
  ) %>%
  filter(introduction == 1) %>%
  filter(!is.na(year), !is.na(ISO), !is.na(Module), !is.na(Policytype_simple)) %>%
  group_by(ISO, Policytype_simple, Module) %>%
  filter(year == min(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    ISO = factor(ISO, levels = rev(sort(unique(ISO)))),
    Module = factor(Module, levels = names(module_map)),
    Policytype_simple = factor(Policytype_simple, levels = names(shape_map))
  ) %>%
  filter(ISO %in% c("CHN", "FRA", "SWE", "GBR", "JPN")) 
  

plot_df <- plot_df %>%
  mutate(
    ISO = factor(
      ISO,
      levels = c("CHN", "JPN", "FRA", "GBR", "SWE")
    )
  )

country_labels <- c(
  "CHN" = "China",
  "FRA" = "France",
  "GBR" = "United Kingdom",
  "JPN" = "Japan",
  "SWE" = "Sweden"
)

sequence_span <- plot_df %>%
  group_by(ISO) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_df, aes(x = year, y = ISO, color = Module, shape = Policytype_simple)) +
  # Sequence duration: first to last policy
  geom_segment(
    data = sequence_span,
    aes(
      x = first_year,
      xend = last_year,
      y = ISO,
      yend = ISO
    ),
    inherit.aes = FALSE,
    colour = "grey80",
    linewidth = 0.6
  ) +
  geom_point(size = 2.8, alpha = 0.9,
             position = position_jitter(width = 0.5, height = 0)) +
  scale_color_manual(values = module_map, drop = FALSE,  guide = guide_legend(
    title.theme = element_text(
      face = "bold",
      margin = margin(t = -20)
    )
  )) +
  scale_shape_manual(values = shape_map, drop = FALSE) +
  scale_y_discrete(
    labels = country_labels
  ) + 
  scale_x_continuous(breaks = seq(1995, 2025, 5), limits = c(1995, 2023)) +
  labs(x = NULL, y = NULL, color = "Sector", shape = "Instrument") +
  theme_classic(base_size = 13, base_family = "Times New Roman") +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.height = unit(0.8, "cm"),
    axis.title = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.2),
  )


# 5. Graph Plotting -------------------------------------------------------
developed <- c(
  "AUS", "AUT", "BEL", "CAN", "CHE", "CZE", "DEU", "DNK",
  "ESP", "EST", "EU27_2020", "FIN", "FRA", "GBR", "GRC",
  "HRV", "HUN", "IRL", "ISL", "ISR", "ITA", "JPN", "KOR",
  "LTU", "LUX", "LVA", "MLT", "NLD", "NOR", "NZL", "POL",
  "PRT", "SVK", "SVN", "SWE", "USA"
)

developing <- c(
  "ARG", "BGR", "BRA", "CHL", "CHN", "COL", "CRI", "IDN",
  "IND", "MEX", "PER", "ROU", "RUS", "SAU", "TUR", "ZAF"
)

panel_seq$dev_group <- dplyr::case_when(
  panel_seq$ISO %in% developed  ~ "Developed",
  panel_seq$ISO %in% developing ~ "Developing",
  TRUE ~ NA_character_
)

plot_transition_network <- function(panel, module_name, state_var = "state8", show_group_labels = FALSE) {
  library(dplyr)
  library(tibble)
  library(tidygraph)
  library(ggraph)
  library(ggplot2)
  library(grid)
  
  panel_module <- panel %>%
    filter(Module == module_name) %>%
    filter(dev_group == "Developing")
  
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
      
      5.5,              # all four
      
      7              # end
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
      aes(width = prob, linetype = type, colour = type),
      alpha = 0.5,
      lineend = "round",
      arrow = arrow(length = unit(2, "mm"), type = "open"),
      end_cap = circle(3, "mm")
    ) +
    scale_edge_colour_manual(
      values = c(
        "transition" = "grey55",
        "end" = "grey80"
      ),
      guide = "none"
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
      aes(label = ifelse(observed, label, NA_character_)),
      size = 3.2,
      fontface = "bold",
      vjust = -2,
      na.rm = TRUE
    )+
    scale_size_area(
      max_size = 10,
      breaks = c(1000, 2000, 3500, 5000),
      limits = c(0, 5000),
      name = "Country-years"
    ) +
    scale_fill_manual(
      values = c(
        "Includes price" = "#2C7BB6",
        "Regulatory only" = "#D95F02",
        "None" = "grey30",
        "End" = "grey75"
      ),
      name = "Node type"
    ) +
    scale_edge_width(
      range = c(0.3, 2.5),
      limits = c(0, 1),
      breaks = c(0.25, 0.50, 0.75, 1.00),
      name = "Transition probability"
    ) + 
    scale_edge_linetype_manual(
      values = c("transition" = "solid", "end" = "33"),
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
  
  if (show_group_labels) {
    p <- p +
      annotate(
        "text",
        x = c(1, 3, 5),
        y = -4.5,
        label = c("1 type", "2 types", "3 types"),
        fontface = "bold",
        size = 4,
        colour = "grey20"
      )
  }
  
  print(p)
}

library(patchwork)

p_elec <- plot_transition_network(panel_seq, "Electricity", "state8", show_group_labels = TRUE)
p_ind  <- plot_transition_network(panel_seq, "Industry", "state8")
p_tran <- plot_transition_network(panel_seq, "Transport", "state8")
p_bld  <- plot_transition_network(panel_seq, "Buildings", "state8")

(p_elec | p_ind) /
  (p_tran | p_bld) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")


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
      0,
      2.5, 2.5, 2.5,
      5, 5, 5,
      7.5,
      9
    ),
    y = c(
      0,
      2.5, 0, -2.5,
      2.5, 0, -2.5,
      0,
      -0.9
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
      aes(label = ifelse(observed, label, NA_character_)),
      size = 3.2,
      fontface = "bold",
      vjust = -2,
      na.rm = TRUE
    )+
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(20, 20, 20, 20)
    ) +
    scale_alpha_manual(
      values = c(`TRUE` = 1, `FALSE` = 0.15),
      guide = "none"
    ) + 
    scale_size_area(
      max_size = 10,
      breaks = c(1000, 2000, 3500, 5000),
      limits = c(0, 5000),
      name = "Country-years"
    ) +
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
    scale_edge_width(
      range = c(0.3, 2.5),
      limits = c(0, 1),
      breaks = c(0.25, 0.50, 0.75, 1.00),
      name = "Transition probability"
    ) + 
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
    ) 
  
  print(p)
}

p_pre_elec <- plot_pre_carbon_pricing_network(panel_seq, "Electricity", state_var = "state8")
p_pre_ind <- plot_pre_carbon_pricing_network(panel_seq, "Industry", state_var = "state8")
p_pre_tran <- plot_pre_carbon_pricing_network(panel_seq, "Transport", state_var = "state8")
p_pre_bld <- plot_pre_carbon_pricing_network(panel_seq, "Buildings", state_var = "state8")

(p_pre_elec | p_pre_ind) /
  (p_pre_tran | p_pre_bld) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

