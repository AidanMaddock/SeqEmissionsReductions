library(dplyr)
library(tidyr)
library(reshape2)
library(pheatmap)
library(fixest)
library(ggplot2)




# panel:
# country | year | policy | adopted
# adopted should be 0/1 (or anything >0 meaning present)
# One row per country-year-policy observation
df <- read.csv("00_raw_data/joined_data.csv")


build_relatedness <- function(df,
                              country_col = "ISO",
                              year_col = "year",
                              sector_col = "Module",
                              policy_col = "Policy_name_fig_1",
                              first_price_col = "first_price_year",
                              adopted_col = "adopted",
                              collapse_across_sectors = FALSE,
                              pre_price_only = TRUE) {
  
  ctry  <- rlang::sym(country_col)
  yr    <- rlang::sym(year_col)
  sect  <- rlang::sym(sector_col)
  pol   <- rlang::sym(policy_col)
  pyear <- rlang::sym(first_price_col)
  ad    <- rlang::sym(adopted_col)
  
  dat <- df %>%
    mutate(
      policy_key = if (collapse_across_sectors) {
        as.character(!!pol)
      } else {
        paste(as.character(!!sect), as.character(!!pol), sep = " :: ")
      }
    )
  
  analysis_data <-
    if (pre_price_only) {
      dat %>%
        filter(is.na(!!pyear) | !!yr < !!pyear)
    } else {
      dat
    }
  
  analysis_data <- analysis_data %>%
    mutate(policy_present = as.integer(coalesce(!!ad, 0) > 0))
  
  # Keep only pre-price history for each country
  pre_price <- analysis_data %>%
    group_by(!!ctry, policy_key) %>%
    summarise(
      adopted = as.integer(any(policy_present == 1, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Country x policy matrix
  wide <- pre_price %>%
    pivot_wider(
      names_from  = policy_key,
      values_from = adopted,
      values_fill = 0
    )
  
  X <- as.matrix(wide[, setdiff(names(wide), country_col), drop = FALSE])
  rownames(X) <- wide[[country_col]]
  
  # Drop policies never adopted in the pre-price sample
  keep <- colSums(X) > 0
  X <- X[, keep, drop = FALSE]
  
  # Co-occurrence-based relatedness
  C <- crossprod(X)
  n <- colSums(X)
  
  p_i_given_j <- sweep(C, 2, n, "/")
  p_j_given_i <- sweep(C, 1, n, "/")
  
  phi <- pmin(p_i_given_j, p_j_given_i)
  diag(phi) <- 0
  phi[!is.finite(phi)] <- 0
  
  dimnames(phi) <- list(colnames(X), colnames(X))
  
  list(
    relatedness = phi,
    country_policy_matrix = X,
    long_pre_price = pre_price
  )
}




phi_long <- melt(phi)

colnames(phi_long) <- c("Policy1", "Policy2", "Relatedness")

ggplot(phi_long, aes(Policy1, Policy2, fill = Relatedness)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6),
    panel.grid = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = expression(phi),
    title = "Climate Policy Relatedness Matrix"
  )


pheatmap(
  phi,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  color = colorRampPalette(c("white", "skyblue", "darkblue"))(100),
  border_color = NA,
  fontsize_row = 6,
  fontsize_col = 6,
  main = "Policy Relatedness Matrix"
)
compute_readiness <- function(df, phi,
                              country_col = "ISO",
                              policy_col = "Policy_name_fig_1",
                              target_rows = c("Carbon tax", "Emission trading scheme")) {
  
  ctry <- rlang::sym(country_col)
  pol  <- rlang::sym(policy_col)
  
  # country-level readiness from already pre-price data
  readiness <- df %>%
    group_by(!!ctry) %>%
    summarise(
      readiness = {
        ps <- intersect(unique(.data[[policy_col]]), rownames(phi))
        if (length(ps) == 0) {
          NA_real_
        } else {
          mean(phi[target_rows[target_rows %in% rownames(phi)], ps, drop = FALSE], na.rm = TRUE)
        }
      },
      n_preprice_policies = n_distinct(.data[[policy_col]]),
      .groups = "drop"
    )
  
  readiness
}

compute_readiness <- function(long_pre_price, phi,
                              country_col = "ISO",
                              policy_col = "Policy_name_fig_1",
                              adopted_col = "adopted",
                              target_row = "Carbon tax") {
  
  ctry <- rlang::sym(country_col)
  pol  <- rlang::sym(policy_col)
  ad   <- rlang::sym(adopted_col)
  
  if (!target_row %in% rownames(phi)) {
    stop("target_row not found in phi")
  }
  
  long_pre_price %>%
    filter(!!ad == 1) %>%
    group_by(!!ctry) %>%
    summarise(
      readiness = {
        ps <- intersect(unique(!!pol), rownames(phi))
        if (length(ps) == 0) NA_real_ else mean(phi[target_row, ps], na.rm = TRUE)
      },
      n_preprice_policies = n_distinct(!!pol),
      .groups = "drop"
    )
}

res <- build_relatedness(df, pre_price_only = FALSE, collapse_across_sectors = TRUE)
phi <- res$relatedness

readiness_df <- compute_readiness(df, phi)


# Regression


panel2 <- panel_data %>%
  left_join(readiness_df, by = c("ISO"))

m1 <- feols(
  lnEmissions_co2 ~ lag_numprice * readiness + lag_price_string + lag_reg_string  + pop + GDPpc2015 + annual_HDD + annual_CDD +
    GDPpc2015_cycle + tempvariation + regquality + ruleoflaw + importpcGDP + GDPgrowth + AVservicepcGDP | ISO^Module + year,
  cluster = ~ISO^Module,
  data = panel2
)

summary(m1)

