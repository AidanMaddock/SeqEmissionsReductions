library(tidyverse)
library(readr)
library(fixest)
library(stargazer)
library(ipw)
library(forcats)
library(nnet)
library(cobalt)
library(marginaleffects)
library(WeightIt)


devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)

#=========================================================
# Project: Climate Policy Sequencing
# File: 04_MSM.r
# Description: The first part of the script uses the policies dataset from 02_policies_cleaning.r
# to build both a policy- and sector-level panel. The second uses these panels to run and present
# a marginal structural model of policy sequencing and emissions reductions.
# Inputs: policies.csv, emissions_sector.csv, controls.csv
# Outputs: joined_data.csv
#=========================================================


# 1. Policy panel building

# Load in policy data
oecd_data <- read_csv("01_tidy_data/policies.csv")

# Inclusion Details for price and regulatory instruments 
price_categories <- c("Taxation", "Driving taxation")
subsidy_categories <- c("Green subsidy", "Renewable subsidy", "Financing mechanism")
standards_categories <- c("Air pollution standard", "Energy efficiency mandate", "Building code", "Minimum energy performance standard", "Renewable portfolio standard", "Label")
reg_categories <- c("Ban", "Speed Limit", "Planning")

# Filter out first few years and latest ones to align with the dataset
oecd_data <- oecd_data %>%
  filter(!year %in% c(1990,1991,1992,1993,1994, 1995, 2023)) 

# Build out panel for model
panel <- oecd_data %>%
  arrange(ISO, Module, year) %>%
  group_by(ISO, Module) %>%
  mutate(
    # Variables to assign policy introductions to categories
    
    is_price = as.integer(Cluster_categories == "Pricing"),
    is_subsidy = as.integer(Cluster_categories == "Subsidy"),
    is_standard = as.integer(Cluster_categories == "Information"),
    is_reg = as.integer(Cluster_categories == "Regulation"),
    
    # yearly introduction events based on introduction or intensification
    priceintro = as.integer(is_price  & introduction == 1 | is_price  & intensification == 1),
    regintro = as.integer(is_reg & (introduction == 1 | intensification == 1)),
    subsidyintro   = as.integer(is_subsidy & introduction == 1 | is_subsidy  & intensification == 1),
    standardintro   = as.integer(is_standard & introduction == 1 | is_standard  & intensification == 1),
    
    # cumulative in-force states
    price = cummax(priceintro),
    reg   = cummax(regintro),
    subsidy  = cummax(subsidyintro),
    standard  = cummax(standardintro),
    
    # time-varying regime state
    state = case_when(
      price == 0 & reg == 0 & subsidy == 0 & standard == 0 ~ "none",
      price == 1 & reg == 0 & subsidy == 0 & standard == 0 ~ "price",
      price == 0 & reg == 1 & subsidy == 0 & standard == 0  ~ "reg",
      price == 1 & reg == 1 ~ "both"
    ),
    state = factor(state, levels = c("none", "price", "reg", "both")),
    
    # first adoption years: fixed within group
    first_price_year = if (any(priceintro == 1)) min(year[priceintro == 1]) else NA_integer_,
    first_reg_year   = if (any(regintro == 1))   min(year[regintro == 1])   else NA_integer_,
    
    # fixed sequencing path at sector level
    path_group = case_when(
      !is.na(first_price_year) & !is.na(first_reg_year) & first_price_year < first_reg_year ~ "price_first",
      !is.na(first_price_year) & !is.na(first_reg_year) & first_reg_year < first_price_year ~ "reg_first",
      !is.na(first_price_year) & !is.na(first_reg_year) & first_price_year == first_reg_year ~ "simultaneous",
      !is.na(first_price_year) &  is.na(first_reg_year) ~ "price_only",
      is.na(first_price_year) & !is.na(first_reg_year) ~ "reg_only",
      TRUE ~ NA_character_
    ),
    
    # show path only once the relevant policy is active
    path = case_when(
      path_group == "price_first"   & year < first_reg_year   ~ "none",
      path_group == "reg_first"     & year < first_price_year ~ "none",
      path_group == "simultaneous"  & year == first_price_year ~ "none",
      path_group == "price_only"    & price == 0             ~ "none",
      path_group == "reg_only"      & reg == 0               ~ "none",
      TRUE ~ path_group
    ),
    path = factor(path, levels = c("none", "price_first", "reg_first", "simultaneous", "price_only", "reg_only"))
  ) %>%
  ungroup() 

write_csv(panel,"01_tidy_data/policypanel_long.csv")

# Collapse down into sectors (Module)
panel_sectors <- panel %>%
  group_by(ISO, Module, year) %>%
  summarise(
    
    numprice = sum(price == 1 & is_price, na.rm = TRUE),
    numreg   = sum(reg   == 1 & !is_price, na.rm = TRUE),
    numsubsidy = sum(subsidy == 1 & is_subsidy, na.rm = TRUE),
    numstandard = sum(standard == 1 & is_standard, na.rm = TRUE),
    
    price = as.integer(any(price == 1, na.rm = TRUE)),
    reg   = as.integer(any(reg   == 1, na.rm = TRUE)),
    subsidy = as.integer(any(subsidy == 1), na.rm = TRUE),
    standard = as.integer(any(standard == 1), na.rm = TRUE),
    
    path = first(path),
    state = first(state),
    pathgroup = first(path_group),
    
    price_stringency = if (any(is_price)) {
      mean(Value[is_price], na.rm = TRUE)
    } else NA_real_,
    
    reg_stringency = if (any(!is_price)) {
      mean(Value[!is_price], na.rm = TRUE)
    } else NA_real_,
    
    .groups = "drop"
  ) %>%
  arrange(ISO, Module, year) %>%
  group_by(ISO, Module) %>%
  mutate(
    # First adoption years
    first_price_year = if (any(price == 1)) min(year[price == 1]) else 0,
    first_sub_year   = if (any(subsidy == 1))   min(year[subsidy == 1])   else 0,
    first_standard_year   = if (any(standard == 1))   min(year[standard == 1])   else 0,
    first_reg_year   = if (any(reg == 1))   min(year[reg == 1])   else 0,
    
    priceintro = as.integer(year == first_price_year),
    regintro   = as.integer(year == first_reg_year),
    
    lag_price_string = lag(price_stringency, n = 1, default = 0),
    lag_reg_string   = lag(reg_stringency, n = 1,  default = 0),
    lag_numprice     = lag(numprice, n = 1,  default = 0),
    lag_numreg       = lag(numreg, n = 1, default = 0),
    lag_numsub = lag(numsubsidy, n = 1, default = 0),
    lag_numstandard = lag(numstandard, n = 1, default = 0),
  ) %>%
  ungroup()

# Combine with dependent and control variables for final model
emissions <- read.csv("01_tidy_data/emissions_sector.csv")
control_data <- read.csv("01_tidy_data/controls.csv")

panel_data <- panel_sectors %>%
  left_join(emissions,by = c("ISO", "year", "Module")) %>% # Join emissions data in
  left_join(control_data, by = c("ISO", "year")) %>%
  filter(!ISO %in% c("EU27_2020")) 

write_csv(panel,"00_raw_data/joined_data.csv") # Used in 05_matrix.R 


# Weighting (time-varying) -------------------------------------------------------------------------
lag_vars <- c(
  "GDPpc2015", "annual_HDD", "annual_CDD", "ruleoflaw",
  "importpcGDP", "tempvariation", "urbpop", "price", "subsidy", "standard"
)


bin_timing <- function(x) {
  cut(
    x,
    breaks = c(-Inf, -5, -1, Inf),
    labels = c("5plus_before", "1to4_before", "Concurrent_or_After"),
    right = TRUE
  )
}

timing_levels <- c("Concurrent_or_After", "5plus_before", "1to4_before")

# Get history state 
panel_msm <- panel_data %>%
  group_by(ISO, Module) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    across(all_of(lag_vars), ~ lag(.x), .names = "lag_{.col}"),
    
    sub_timing = if_else(
      is.na(first_price_year) | is.na(first_sub_year),
      NA_character_,
      as.character(bin_timing(first_sub_year - first_price_year))
    ),
    std_timing = if_else(
      is.na(first_price_year) | is.na(first_standard_year),
      NA_character_,
      as.character(bin_timing(first_standard_year - first_price_year))
    ),
    reg_timing = if_else(
      is.na(first_price_year) | is.na(first_reg_year),
      NA_character_,
      as.character(bin_timing(first_reg_year - first_price_year))
    ),
    
    sub_timing = factor(sub_timing, levels = timing_levels),
    std_timing = factor(std_timing, levels = timing_levels),
    reg_timing = factor(reg_timing, levels = timing_levels),
    
    history_state = interaction(sub_timing, std_timing, price, drop = TRUE, lex.order = TRUE)
  ) %>%
  ungroup()


#------------------------------------------------------------
# 2) Helper: stabilized IPTW component for one policy process
#    WeightIt gives inverse-probability weights; ratio of
#    numerator- to denominator-weights is the stabilized factor.
#------------------------------------------------------------
fit_sw_component <- function(dat, treat, num_rhs, den_rhs, prefix) {
  f_num <- as.formula(paste(treat, "~", paste(num_rhs, collapse = " + ")))
  f_den <- as.formula(paste(treat, "~", paste(den_rhs, collapse = " + ")))
  
  W_num <- weightit(
    f_num,
    data = dat,
    method = "glm",
    estimand = "ATE"
  )
  
  W_den <- weightit(
    f_den,
    data = dat,
    method = "glm",
    estimand = "ATE"
  )
  
  dat[[paste0(prefix, "_w_num")]] <- get.w(W_num)
  dat[[paste0(prefix, "_w_den")]] <- get.w(W_den)
  dat[[paste0(prefix, "_sw")]]    <- dat[[paste0(prefix, "_w_num")]] / dat[[paste0(prefix, "_w_den")]]
  
  dat
}

#------------------------------------------------------------
# 3) Treatment models
#    Denominator: full lagged confounder history + lagged other policies
#    Numerator: weakly stabilized model (year + own lag)
#    If you have true baseline covariates, add them to num_rhs.
#------------------------------------------------------------
den_covars <- c(
  "factor(year)",
  "lag_GDPpc2015", "lag_annual_HDD", "lag_annual_CDD",
  "lag_ruleoflaw", "lag_importpcGDP", "lag_tempvariation", "lag_urbpop",
  "lag_price", "lag_subsidy", "lag_standard"
)

# Weak stabilization fallback.
# Replace/augment with baseline covariates if you have them.
num_price_covars    <- c("factor(year)", "lag_price")
num_subsidy_covars  <- c("factor(year)", "lag_subsidy")
num_standard_covars <- c("factor(year)", "lag_standard")

panel_msm <- panel_msm %>%
  fit_sw_component(
    treat   = "price",
    num_rhs = num_price_covars,
    den_rhs = den_covars,
    prefix  = "cp"
  ) %>%
  fit_sw_component(
    treat   = "subsidy",
    num_rhs = num_subsidy_covars,
    den_rhs = den_covars,
    prefix  = "sub"
  ) %>%
  fit_sw_component(
    treat   = "standard",
    num_rhs = num_standard_covars,
    den_rhs = den_covars,
    prefix  = "std"
  ) 

W_cp_den <- weightit(as.formula(paste("price ~", paste(den_covars, collapse = " + "))),
                     data = panel_msm, method = "glm", estimand = "ATE")
W_sub_den <- weightit(as.formula(paste("subsidy ~", paste(den_covars, collapse = " + "))),
                      data = panel_msm, method = "glm", estimand = "ATE")
W_std_den <- weightit(as.formula(paste("standard ~", paste(den_covars, collapse = " + "))),
                      data = panel_msm, method = "glm", estimand = "ATE")

bal.tab(W_cp_den)
bal.tab(W_sub_den)
bal.tab(W_std_den)

love.plot(W_std_den)

panel_msm_weighted <- panel_msm %>%
  arrange(ISO, year) %>%
  group_by(ISO, Module) %>%
  mutate(
    sw_year = cp_sw * sub_sw * std_sw,
    sw_cum  = cumprod(sw_year)
  ) %>%
  ungroup()

# Truncate / winsorize to reduce variance inflation
q <- quantile(panel_msm_weighted$sw_cum, probs = c(0.01, 0.99), na.rm = TRUE)

panel_msm_weighted <- panel_msm_weighted %>%
  mutate(
    sw = pmin(pmax(sw_cum, q[[1]]), q[[2]])
  )

summary(panel_msm_weighted$sw)


#Look at excluding israel?



# Policy Sequencing Score Calculation -------------------------------------

policy_cols <- c("subsidy", "standard")


panel_sectors <- panel_sectors %>%
  mutate(unit_id = interaction(ISO, Module, drop = TRUE))

adopt_years <- panel_sectors %>%
  group_by(ISO, Module) %>%
  summarise(
    price = if(any(price==1)) min(year[price==1]) else NA,
    subsidy = if(any(subsidy==1)) min(year[subsidy==1]) else NA,
    standard = if(any(standard==1)) min(year[standard==1]) else NA,
    regulation = if(any(reg==1)) min(year[reg==1]) else NA,
    .groups="drop"
  )

policies <- c("price","subsidy","standard","regulation")

pair_freq <- expand.grid(
  first = policies,
  second = policies,
  stringsAsFactors = FALSE
) |>
  dplyr::filter(first != second) |>
  rowwise() |>
  mutate(
    
    numerator = sum(
      adopt_years[[first]] < adopt_years[[second]],
      na.rm = TRUE
    ),
    
    denominator = sum(
      !is.na(adopt_years[[second]])
    ),
    
    frequency = numerator / denominator
    
  ) |>
  ungroup()

pair_lookup <- pair_freq %>%
  mutate(key = paste(first, second, sep = "__")) %>%
  select(key, frequency) %>%
  deframe()

#------------------------------------------------------------
# 3) Function to compute sequencing score for one unit-year
#    - find policies adopted by year t
#    - determine their observed ordering
#    - look up frequencies
#    - sum them (paper method)
#------------------------------------------------------------
score_one_row <- function(year_t, ad_years, pair_lookup, policy_cols) {
  ad_years <- ad_years[policy_cols]
  names(ad_years) <- policy_cols
  
  adopted_now <- policy_cols[!is.na(ad_years) & ad_years <= year_t]
  
  # Need at least 2 adopted policies to form an ordering
  if (length(adopted_now) < 2) return(0)
  
  pairs <- combn(adopted_now, 2, simplify = FALSE)
  
  pair_scores <- vapply(pairs, function(pair) {
    a <- pair[1]
    b <- pair[2]
    
    ay <- ad_years[[a]]
    by <- ad_years[[b]]
    
    # If same-year adoption, treat as neutral
    if (isTRUE(ay == by)) return(0.5)
    
    # Use the observed ordering
    key <- if (ay < by) paste(a, b, sep = "__") else paste(b, a, sep = "__")
    
    val <- pair_lookup[[key]]
    if (is.null(val) || is.na(val)) 0 else val
  }, numeric(1))
  
  sum(pair_scores, na.rm = TRUE)
}

#------------------------------------------------------------
# 4) Attach the score to each ISO x Module x year
#------------------------------------------------------------
panel_scored <- panel_sectors %>%
  left_join(adopt_years, by = c("ISO", "Module"), suffix = c("", "_adopt")) %>%
  rowwise() %>%
  mutate(
    sequencing_score = score_one_row(
      year_t = year,
      ad_years = c(
        price    = priceintro,
        subsidy  = subsidy,
        standard = standard,
        reg      = regintro
      ),
      pair_lookup = pair_lookup,
      policy_cols = policy_cols
    )
  ) %>%
  ungroup()


# Models ------------------------------------------------------------------


# Model for co2 only
msm_model_co2 <- feols(
  lnEmissions_co2 ~ lag_price_string + sub_timing + std_timing + lag_price_string:sub_timing + lag_price_string:std_timing + lag_price_string + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_co2)

# Model for non-co2 emissions
msm_model_nonco2 <- feols(
  lnEmissions_nonco2 ~ lag_price_string + sub_timing + std_timing + lag_price_string:sub_timing + lag_price_string:std_timing + lag_price_string + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted,
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_nonco2)

msm_reg_model <- feols(
  lnEmissions_co2 ~ + lag_price_string + reg_timing + lag_price_string:reg_timing + lag_numreg + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_reg_model)

# 
pct_effects <- 100 * (exp(coef(msm_reg_model)[grep("^pathgroup", names(coef(msm_reg_model)))]) - 1)
pct_effects

# Tables 
var_labels <- c(
  lag_price_string = "Lagged presence of price",
  sub_timing1to4_before = "Subsidy timing: 1–4 years before",
  sub_timing5plus_before = "Subsidy timing: 5+ years before",
  std_timing1to4_before = "Standard timing: 1–4 years before",
  std_timing5plus_before = "Standard timing: 5+ years before",
  lag_numsub = "Lagged number of subsidies",
  lag_numstandard = "Lagged number of standards",
  pop = "Population",
  GDPpc2015 = "GDP per capita (2015)",
  annual_HDD = "Annual heating degree days",
  annual_CDD = "Annual cooling degree days",
  tempvariation = "Temperature variation",
  importpcGDP = "Imports / GDP",
  urbpop = "Urban population",
  ruleoflaw = "Rule of law",
  AVservicepcGDP = "Services / GDP",
  "lag_price_string:sub_timing1to4_before" = "Price × subsidy timing: 1–4 years before",
  "lag_price_string:sub_timing5plus_before" = "Price × subsidy timing: 5+ years before",
  "lag_price_string:std_timing1to4_before" = "Price × standard timing: 1–4 years before",
  "lag_price_string:std_timing5plus_before" = "Price × standard timing: 5+ years before"
)

etable(
  msm_model,
  msm_model_co2,
  headers = c("GHG emissions (CO₂e)", "CO₂ emissions"),
  dict = var_labels,
  style.tex = style.tex("aer"),
  depvar = FALSE,
  order = c(
    "lag_price_string",
    "lag_price_string:sub_timing",
    "lag_price_string:std_timing",
    "sub_timing",
    "std_timing",
    "lag_numsub",
    "lag_numstandard",
    "pop",
    "GDPpc2015",
    "annual_HDD",
    "annual_CDD",
    "tempvariation",
    "importpcGDP",
    "urbpop",
    "ruleoflaw",
    "AVservicepcGDP"
  ),
  se = "cluster",
  cluster = ~ ISO^Module,
  fitstat = ~ n + r2,
  tex = TRUE
)


etable(
  list(
    "CO2" = msm_model_co2,
    "Non-CO2" = msm_model_nonco2
  ),
  dict = var_labels,
  tex = TRUE,
  title = "Climate laws and their effect on emissions",
  style.tex = style.tex("aer"),
  depvar = FALSE,
  fitstat = ~ n + r2,
  digits = 3,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)



# Predicted effects (policy counterfactual)
actual_path <- panel_msm_weighted %>%
  mutate(emissions = exp(lnEmissions_co2e)) %>%
  group_by(year) %>%
  summarise(
    emissions = mean(emissions, na.rm = TRUE),
    scenario = "Observed",
    .groups = "drop"
  )

# 2) Counterfactual: no sequencing
#    Set both timing variables to the baseline category.
cf_data <- panel_msm_weighted %>%
  mutate(
    sub_timing = factor("Concurrent_or_After",
                        levels = c("Concurrent_or_After", "1to4_before", "5plus_before")),
    std_timing = factor("Concurrent_or_After",
                        levels = c("Concurrent_or_After", "1to4_before", "5plus_before"))
  )

cf_data$pred_cf_log <- as.numeric(predict(msm_model, newdata = cf_data))
cf_path <- cf_data %>%
  mutate(emissions = exp(pred_cf_log)) %>%
  group_by(year) %>%
  summarise(
    emissions = mean(emissions, na.rm = TRUE),
    scenario = "Counterfactual: no sequencing",
    .groups = "drop"
  )



std_eff <- avg_comparisons(
  msm_model,
  variables = "std_timing",
  by = "year",
  newdata = panel_msm_weighted
) %>%
  filter(term == "std_timing") %>%
  filter(grepl("5plus_before", contrast)) %>%
  mutate(
    pct_est  = 100 * (exp(estimate) - 1),
    pct_low  = 100 * (exp(conf.low) - 1),
    pct_high = 100 * (exp(conf.high) - 1)
  )

# Publication-style plot
p_std <- ggplot(std_eff, aes(x = year, y = pct_est)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_ribbon(aes(ymin = pct_low, ymax = pct_high), alpha = 0.15) +
  geom_line(linewidth = 1.1) +
  labs(
    x = "Year",
    y = "Effect of standards introduced 5+ years before pricing on emissions (%)"
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

p_std

# 3) Combine for plotting
plot_df <- bind_rows(actual_path, cf_path)


park_palette("SmokyMountains")
ggplot(plot_df, aes(x = year, y = emissions, color = scenario)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(
      values = park_palette("Badlands")
  ) +
  labs(
    x = "Year",
    y = "Emissions",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )



# Robustness Checks -------------------------------------------------------

# Do robustness checks here



# Figures -----------------------------------------------------------------
# Fig 4a
groupings <- read_csv("01_tidy_data/CountryGroupings.csv")

plot_panel <- panel_data %>%
  group_by(Classification, Module, ISO) %>%
  mutate(first_adopt = min(if_else(state != "none", year, NA_integer_), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    first_adopt = if_else(is.infinite(first_adopt), NA_real_, first_adopt),
    state = factor(state, levels = c("none", "reg", "price", "both"))
  )


ggplot(plot_panel, aes(x = year, y = ISO, fill = state)) +
  geom_tile() +
  facet_grid(Classification ~ Module, scales = "free_y", space = "free_y") +
  scale_fill_manual(
    values = c(
      "none" = "white",
      "reg" = "steelblue3",
      "price" = "darkorange2",
      "both" = "darkgreen"
    )
  ) +
  labs(
    x = "Year",
    y = "Country (ISO)",
    fill = "Policy state",
    title = "Policy adoption paths by country, sector, and development group"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(face = "bold")
  )


# Old code 



# Build policy-level panel
panel <- oecd_data %>%
  mutate(
    id = interaction(ISO, Module, Policy, drop = TRUE)
  ) %>%
  arrange(ISO, Module, id, year) %>%
  group_by(ISO, Module, id) %>%
  mutate(
    # Assign each row to one policy type
    policy_type = case_when(
      `Broad Category` %in% price_categories     ~ "price",
      `Broad Category` %in% subsidy_categories    ~ "subsidy",
      `Broad Category` %in% standards_categories  ~ "standard",
      TRUE                                        ~ "reg"
    ),
    
    is_price    = policy_type == "price",
    is_subsidy  = policy_type == "subsidy",
    is_standard = policy_type == "standard",
    is_reg      = policy_type == "reg",
    
    # Count both introductions and intensifications as new policy events
    price_event    = as.integer(is_price    & (introduction == 1 | intensification == 1)),
    reg_event      = as.integer(is_reg      & (introduction == 1 | intensification == 1)),
    subsidy_event  = as.integer(is_subsidy  & (introduction == 1 | intensification == 1)),
    standard_event = as.integer(is_standard & (introduction == 1 | intensification == 1)),
    
    # Policy becomes active from first event onward
    price_active    = cummax(price_event),
    reg_active      = cummax(reg_event),
    subsidy_active  = cummax(subsidy_event),
    standard_active = cummax(standard_event),
    adopted = as.integer(
      price_active == 1 |
        reg_active == 1 |
        subsidy_active == 1 |
        standard_active == 1
    ),
    
    # Time-varying regime state
    state = case_when(
      price_active == 0 & reg_active == 0 ~ "none",
      price_active == 1 & reg_active == 0 ~ "price",
      price_active == 0 & reg_active == 1 ~ "reg",
      price_active == 1 & reg_active == 1 ~ "both"
    ),
    state = factor(state, levels = c("none", "price", "reg", "both")),
    
    # First adoption years within each policy
    first_price_year = if (any(price_event == 1)) min(year[price_event == 1]) else NA_integer_,
    first_reg_year   = if (any(reg_event == 1))   min(year[reg_event == 1])   else NA_integer_,
    
    # Fixed sequencing path
    path_group = case_when(
      !is.na(first_price_year) & !is.na(first_reg_year) & first_price_year < first_reg_year ~ "price_first",
      !is.na(first_price_year) & !is.na(first_reg_year) & first_reg_year < first_price_year ~ "reg_first",
      !is.na(first_price_year) & !is.na(first_reg_year) & first_price_year == first_reg_year ~ "simultaneous",
      !is.na(first_price_year) &  is.na(first_reg_year) ~ "price_only",
      is.na(first_price_year) & !is.na(first_reg_year) ~ "reg_only",
      TRUE ~ NA_character_
    ),
    
    path = case_when(
      path_group == "price_first"  & year < first_reg_year   ~ "none",
      path_group == "reg_first"    & year < first_price_year ~ "none",
      path_group == "simultaneous" & year < first_price_year ~ "none",
      path_group == "price_only"   & price_active == 0       ~ "none",
      path_group == "reg_only"     & reg_active == 0         ~ "none",
      TRUE ~ path_group
    ),
    path = factor(path, levels = c("none", "price_first", "reg_first", "simultaneous", "price_only", "reg_only"))
  ) %>%
  ungroup()

write_csv(panel,"01_tidy_data/policy_panel.csv")

# Collapse to ISO x Module x year
panel_sectors <- panel %>%
  group_by(ISO, Module, year) %>%
  summarise(
    # New policy events this year
    numprice    = sum(price_event, na.rm = TRUE),
    numreg      = sum(reg_event, na.rm = TRUE),
    numsubsidy  = sum(subsidy_event, na.rm = TRUE),
    numstandard = sum(standard_event, na.rm = TRUE),
    
    # Policies currently active
    price    = as.integer(any(price_active == 1, na.rm = TRUE)),
    reg      = as.integer(any(reg_active == 1, na.rm = TRUE)),
    subsidy  = as.integer(any(subsidy_active == 1, na.rm = TRUE)),
    standard = as.integer(any(standard_active == 1, na.rm = TRUE)),
    
    
    # Sequencing / state
    path = first(path),
    state = first(state),
    pathgroup = first(path_group),
    
    # Average stringency by type
    price_stringency = if (any(is_price, na.rm = TRUE)) {
      mean(Value[is_price], na.rm = TRUE)
    } else NA_real_,
    
    reg_stringency = if (any(is_reg, na.rm = TRUE)) {
      mean(Value[is_reg], na.rm = TRUE)
    } else NA_real_,
    
    .groups = "drop"
  ) %>%
  arrange(ISO, Module, year) %>%
  group_by(ISO, Module) %>%
  mutate(
    # First active year at sector level
    first_price_year = if (any(price == 1)) min(year[price == 1]) else NA_integer_,
    first_reg_year   = if (any(reg == 1))   min(year[reg == 1])   else NA_integer_,
    
    # First year of a new event at sector level
    regintro = if_else(!is.na(first_reg_year) & year == first_reg_year, 1L, 0L),
    priceintro = if_else(!is.na(first_price_year) & year == first_price_year, 1L, 0L),
    
    lag_price_string = lag(price_stringency, n = 1, default = 0),
    lag_reg_string   = lag(reg_stringency, n = 1, default = 0),
    lag_numprice     = lag(numprice, n = 1, default = 0),
    lag_numreg       = lag(numreg, n = 1, default = 0),
    lag_numsub       = lag(numsubsidy, n = 1, default = 0),
    lag_numstandard  = lag(numstandard, n = 1, default = 0)
  ) %>%
  ungroup()



# Weighting (No country fixed effects) ---------------------------------------------------------------

# Weighting path 
baseline <- panel_data |>
  group_by(ISO, Module) |>
  filter(year == min(year[priceintro == 1 | regintro == 1])) |>
  filter(pathgroup != "simultaneous") %>%
  filter(pathgroup != "price_only") %>%
  ungroup() 

library(WeightIt)

W <- weightit(
  pathgroup ~ pop + GDPpc2015 +
    annual_HDD + annual_CDD + ruleoflaw +
    importpcGDP + tempvariation + urbpop,
  data = baseline,
  method = "glm",
  estimand = "ATE"
)

bal.tab(W)
summary(W)
love.plot(W)

# Model -------------------------------------------------------------------

#H1: Static path / group model
panel_data <- panel_data |>
  left_join(
    baseline |>
      mutate(ipw = get.w(W)) |>
      select(ISO, Module, ipw),
    by = c("ISO", "Module")
  )

# Specify controls 
controls <- ~ pop + GDPpc2015 + annual_HDD + annual_CDD  + ruleoflaw +tempvariation + urbpop + importpcGDP

# Model with interaction term, no effects
fml2 <- xpd(
  lnEmissions_co2 ~  pathgroup + lag_numprice + lag_numreg + lag_price_string + lag_reg_string + ..controls | Module + year,
  ..controls = controls
)


model <- feols(
  fml2,
  cluster = ~ISO,
  data = panel_data, 
  weights = ~ ipw
)

summary(model)

