library(dplyr)
library(waterfalls)
library(patchwork)

# ------------------------------------------------------------
# Calculate the waterfall components
# ------------------------------------------------------------

wf_data <- cf_countries %>%
  filter(
    Classification %in% c("LMICs", "UMICs", "HICs"),
    year %in% c(1996, 2022)
  ) %>%
  select(
    Classification,
    year,
    observed_gt,
    no_sequencing_gt
  ) %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = c(observed_gt, no_sequencing_gt)
  ) %>%
  mutate(
    emissions_increase =
      no_sequencing_gt_2022 - observed_gt_1996,
    
    sequencing_effect =
      observed_gt_2022 - no_sequencing_gt_2022
  )

wf_data

# ------------------------------------------------------------
# Function to make one waterfall
# ------------------------------------------------------------

make_waterfall <- function(classification) {
  
  x <- wf_data %>%
    filter(Classification == classification)
  
  values <- c(
    x$observed_gt_1996,
    x$emissions_increase,
    x$sequencing_effect
  )
  
  labels <- c(
    "Observed\n1996",
    "Emissions increase\nwithout sequencing",
    "Sequencing\neffect"
  )
  
  waterfall(
    values = values,
    labels = labels,
    
    calc_total = TRUE,
    total_axis_text = "Observed\n2022",
    total_rect_text = format(
      round(x$observed_gt_2022, 1),
      big.mark = ","
    ),
    
    rect_text_labels = format(
      round(values, 1),
      big.mark = ","
    ),
    
    fill_by_sign = FALSE,
    
    fill_colours = c(
      "grey30",
      "grey65",
      "#0072B2"
    ),
    
    rect_border = NA,
    draw_lines = TRUE,
    rect_text_size = 3,
    
    print_plot = FALSE
  ) +
    ggplot2::ggtitle(classification) +
    ggplot2::labs(
      x = NULL,
      y = "Emissions (MtCO2e)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold"
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}


# ------------------------------------------------------------
# Three panels
# ------------------------------------------------------------

p_lmic <- make_waterfall("LMICs")
p_umic <- make_waterfall("UMICs")
p_hic  <- make_waterfall("HICs")

p_lmic + p_umic + p_hic +
  plot_layout(ncol = 3)