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
library(scales)
library(pracma)

#=========================================================
# Project: Climate Policy Sequencing
# File: 04_MSM.r
# Description: The first part of the script uses the policies dataset from 02_policies_cleaning.r
# to build both a policy- and sector-level panel. The second uses these panels to run and present
# a marginal structural model of policy sequencing and emissions reductions.
# Inputs: policies.csv, emissions_sector.csv, controls.csv
# Outputs: joined_data.csv
#=========================================================

# 1: Policy Panel Building ------------------------------------------------

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
    is_tax = as.integer(`Policy_name_fig_1` == "Carbon tax"),
    is_ets = as.integer(`Policy_name_fig_1` == "Emission trading scheme"),
    
    # yearly introduction events based on introduction or intensification
    priceintro = as.integer(is_price  & introduction == 1 | is_price  & intensification == 1),
    etsintro = as.integer(is_ets  & introduction == 1 | is_ets  & intensification == 1),
    taxintro = as.integer(is_tax  & introduction == 1 | is_tax  & intensification == 1),
    regintro = as.integer(is_reg & (introduction == 1 | intensification == 1)),
    subsidyintro   = as.integer(is_subsidy & introduction == 1 | is_subsidy  & intensification == 1),
    standardintro   = as.integer(is_standard & introduction == 1 | is_standard  & intensification == 1),
    
    # cumulative in-force states
    price = cummax(priceintro),
    reg   = cummax(regintro),
    ets = cummax(etsintro),
    tax = cummax(taxintro),
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
    ets = as.integer(any(ets == 1, na.rm = TRUE)),
    tax = as.integer(any(tax == 1, na.rm = TRUE)),
    reg   = as.integer(any(reg   == 1, na.rm = TRUE)),
    subsidy = as.integer(any(subsidy == 1), na.rm = TRUE),
    standard = as.integer(any(standard == 1), na.rm = TRUE),
    
    path = first(path),
    state = first(state),
    pathgroup = first(path_group),
    
    price_stringency = if (any(is_price)) {
      mean(Value[is_price], na.rm = TRUE)
    } else NA_real_,
    
    tax_stringency = if (any(tax == 1)) {
      mean(Value[tax == 1], na.rm = TRUE)
    } else NA_real_,
    
    ets_stringency = if (any(is_ets)) {
      mean(Value[is_ets], na.rm = TRUE)
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
    lag_price_string2years = lag(price_stringency, n = 2, default = 0),
    lag_price_string3years = lag(price_stringency, n = 3, default = 0),
    lag_reg_string   = lag(reg_stringency, n = 1,  default = 0),
    lag_ets_string = lag(ets_stringency, n = 1,default = 0),
    lag_tax_string = lag(tax_stringency, n = 1,default = 0),
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


# 2: Inverse Probability of Treatment Weighting  -------------------------------------------------------------------------
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

bin_timing_4 <- function(x) {
  cut(
    x,
    breaks = c(-Inf, -4, -1, Inf),
    labels = c(
      "4plus_before",
      "1to3_before",
      "Concurrent_or_After"
    ),
    right = TRUE
  )
}

bin_timing_6 <- function(x) {
  cut(
    x,
    breaks = c(-Inf, -6, -1, Inf),
    labels = c(
      "6plus_before",
      "1to5_before",
      "Concurrent_or_After"
    ),
    right = TRUE
  )
}

# Factor levels

timing_levels <- c("Concurrent_or_After", "5plus_before", "1to4_before")

timing_levels_4 <- c(
  "Concurrent_or_After",
  "4plus_before",
  "1to3_before"
)

timing_levels_6 <- c(
  "Concurrent_or_After",
  "6plus_before",
  "1to5_before"
)

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
    
    # Robustness checks: 4 and 6 year windows 
    
    sub_timing_4 = if_else(
      is.na(first_price_year) | is.na(first_sub_year),
      NA_character_,
      as.character(bin_timing_4(first_sub_year - first_price_year))
    ),
    std_timing_4 = if_else(
      is.na(first_price_year) | is.na(first_standard_year),
      NA_character_,
      as.character(bin_timing_4(first_standard_year - first_price_year))
    ),
    
    sub_timing_4 = factor(sub_timing_4, levels = timing_levels_4),
    
    std_timing_4 = factor(std_timing_4, levels = timing_levels_4),
    
    sub_timing_6 = if_else(
      is.na(first_price_year) | is.na(first_sub_year),
      NA_character_,
      as.character(bin_timing_6(first_sub_year - first_price_year))
    ),
    std_timing_6 = if_else(
      is.na(first_price_year) | is.na(first_standard_year),
      NA_character_,
      as.character(bin_timing_6(first_standard_year - first_price_year))
    ),
    
    sub_timing_6 = factor(sub_timing_6, levels = timing_levels_6),
    
    std_timing_6 = factor(std_timing_6, levels = timing_levels_6),
    
    history_state = interaction(sub_timing, std_timing, price, drop = TRUE, lex.order = TRUE)
  ) %>%
  ungroup()

# 2) Helper: stabilized IPTW component for one policy process
#    WeightIt gives inverse-probability weights; ratio of
#    numerator- to denominator-weights is the stabilized factor.

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

# 3) Treatment models
#    Denominator: full lagged confounder history + lagged other policies
#    Numerator: weakly stabilized model (year + own lag)

den_covars <- c(
  "factor(year)",
  "lag_GDPpc2015", "lag_annual_HDD", "lag_annual_CDD",
  "lag_ruleoflaw", "lag_importpcGDP", "lag_tempvariation", "lag_urbpop",
  "lag_price", "lag_subsidy", "lag_standard"
)

# Weak stabilization fallback.
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

# Combine weights together (assuming conditional independence)
W_cp_den <- weightit(as.formula(paste("price ~", paste(den_covars, collapse = " + "))),
                     data = panel_msm, method = "glm", estimand = "ATE")
W_sub_den <- weightit(as.formula(paste("subsidy ~", paste(den_covars, collapse = " + "))),
                      data = panel_msm, method = "glm", estimand = "ATE")
W_std_den <- weightit(as.formula(paste("standard ~", paste(den_covars, collapse = " + "))),
                      data = panel_msm, method = "glm", estimand = "ATE")

bal.tab(W_cp_den)
bal.tab(W_sub_den)
bal.tab(W_std_den)

p <- love.plot(W_std_den, thresholds = 0.1)
p


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

# 3: Policy Sequencing Score Calculation -------------------------------------------------------------------------

# 
# First year of adoption for all policies
adopt_years <- panel %>%
  group_by(ISO, Module, Policy) %>%
  summarise(
    adopt_year = if (any(introduction == 1, na.rm = TRUE)) {
      min(year[introduction == 1], na.rm = TRUE)
    } else {
      NA_integer_
    },
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Policy,
    values_from = adopt_year
  )

# Get names of policy columns
policy_cols <- setdiff(names(adopt_years), c("ISO", "Module"))


# Pair lookup frequencies from observed adoption ordering
build_pair_lookup_df <- function(adopt_years, policy_cols) {
  pairs <- combn(policy_cols, 2, simplify = FALSE)
  
  bind_rows(lapply(pairs, function(pair) {
    a <- pair[1]
    b <- pair[2]
    
    tmp <- adopt_years %>%
      filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
    
    if (nrow(tmp) == 0) {
      return(tibble(
        key = c(paste(a, b, sep = "__"), paste(b, a, sep = "__")),
        freq = c(NA_real_, NA_real_)
      ))
    }
    
    a_before_b <- sum(tmp[[a]] < tmp[[b]], na.rm = TRUE)
    b_before_a <- sum(tmp[[b]] < tmp[[a]], na.rm = TRUE)
    denom <- a_before_b + b_before_a
    
    if (denom == 0) {
      freq_ab <- NA_real_
      freq_ba <- NA_real_
    } else {
      freq_ab <- a_before_b / denom
      freq_ba <- b_before_a / denom
    }
    
    tibble(
      key = c(paste(a, b, sep = "__"), paste(b, a, sep = "__")),
      freq = c(freq_ab, freq_ba)
    )
  }))
}


# 4: Models ------------------------------------------------------------------


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

# Model testing tax vs market based carbon pricing 
msm_model_taxets <- feols(
  lnEmissions_co2 ~ lag_ets_string + lag_tax_string + lag_ets_string:sub_timing + lag_ets_string:std_timing + lag_tax_string:sub_timing + lag_tax_string:std_timing + sub_timing + std_timing + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted,
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_taxets)

# Tax vs Market for Non-Co2 
msm_model_nonco2_taxets <- feols(
  lnEmissions_nonco2 ~ lag_ets_string + lag_tax_string + lag_ets_string:sub_timing + lag_ets_string:std_timing + lag_tax_string:sub_timing + lag_tax_string:std_timing + sub_timing + std_timing + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted,
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_nonco2_taxets)


# Robustness checks

# Robustness model 1: no weights
msm_model_co2_noweights <- feols(
  lnEmissions_co2 ~ lag_price_string + sub_timing + std_timing + lag_price_string:sub_timing + lag_price_string:std_timing + lag_price_string + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted, 
  cluster = ~ ISO^Module
)
summary(msm_model_co2_noweights)

# Robustness model 2: sector-specific effects
msm_model_co2_sector <- feols(
  lnEmissions_co2 ~ lag_price_string + sub_timing + std_timing + lag_price_string:sub_timing + lag_price_string:std_timing + lag_price_string + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + Module + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_co2_sector)

# Robustness model 3: 1-3 vs 4+ year sequencing bands
msm_model_co2_4band <- feols(
  lnEmissions_co2 ~ lag_price_string + sub_timing_4 + std_timing_4 + lag_price_string:sub_timing_4 + lag_price_string:std_timing_4 + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_co2_4band)

# Robustness model 4: 1-5 vs 6+ year sequencing bands
msm_model_co2_6band <- feols(
  lnEmissions_co2 ~ lag_price_string + sub_timing_6 + std_timing_6 + lag_price_string:sub_timing_6 + lag_price_string:std_timing_6 + lag_price_string + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_co2_6band)

# Robustness model 5: 2 year price stringency lag
msm_model_co2_2yearlag <- feols(
  lnEmissions_co2 ~ lag_price_string2years + sub_timing + std_timing + lag_price_string2years:sub_timing + lag_price_string2years:std_timing + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_co2_2yearlag)

# Robustness model 6: 3 year lag
msm_model_co2_3yearlag <- feols(
  lnEmissions_co2 ~ lag_price_string3years + sub_timing + std_timing + lag_price_string3years:sub_timing + lag_price_string3years:std_timing + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_co2_3yearlag)

# Robustness model 7: control-set sensitivity 
# 7.1 country + year FE only
msm_model_co2_naive <- feols(
  lnEmissions_co2 ~ lag_price_string + sub_timing + std_timing + lag_price_string:sub_timing + lag_price_string:std_timing + lag_numsub + lag_numstandard |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_co2_naive)

# 7.2 Economic controls + 7.1
msm_model_co2_econ <- feols(
  lnEmissions_co2 ~ lag_price_string + sub_timing + std_timing + lag_price_string:sub_timing + lag_price_string:std_timing + lag_numsub + lag_numstandard + pop + GDPpc2015 |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_co2_econ)

# 7.3 Weather controls + 7.2
msm_model_co2_weather <- feols(
  lnEmissions_co2 ~ lag_price_string + sub_timing + std_timing + lag_price_string:sub_timing + lag_price_string:std_timing + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_co2_weather)

# 7.4 Institutional controls + 7.3
msm_model_co2_inst <- feols(
  lnEmissions_co2 ~ lag_price_string + sub_timing + std_timing + lag_price_string:sub_timing + lag_price_string:std_timing + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model_co2_inst)


msm_reg_model <- feols(
  lnEmissions_co2 ~ lag_price_string + reg_timing + lag_price_string:reg_timing + lag_numreg + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_reg_model)


# 5: Sequencing Counterfactuals ----------------------------------------------
b <- coef(msm_model_co2)

cf_data <- panel_msm_weighted %>%
  mutate(seq_effect = 0)

# Subsidy timing effects
cf_data$seq_effect <-
  cf_data$seq_effect +
  ifelse(cf_data$sub_timing == "1to4_before",
         b["sub_timing1to4_before"] +
           b["lag_price_string:sub_timing1to4_before"] *
           cf_data$lag_price_string,
         0)

cf_data$seq_effect <-
  cf_data$seq_effect +
  ifelse(cf_data$sub_timing == "5plus_before",
         b["sub_timing5plus_before"] +
           b["lag_price_string:sub_timing5plus_before"] *
           cf_data$lag_price_string,
         0)

# Standard timing effects
cf_data$seq_effect <-
  cf_data$seq_effect +
  ifelse(cf_data$std_timing == "1to4_before",
         b["std_timing1to4_before"] +
           b["lag_price_string:std_timing1to4_before"] *
           cf_data$lag_price_string,
         0)

cf_data$seq_effect <-
  cf_data$seq_effect +
  ifelse(cf_data$std_timing == "5plus_before",
         b["std_timing5plus_before"] +
           b["lag_price_string:std_timing5plus_before"] *
           cf_data$lag_price_string,
         0)


cf_data <- cf_data %>%
  mutate(
    ln_cf = lnEmissions_co2 - seq_effect,
    emissions_cf = exp(ln_cf)
  )

cf_path <- cf_data %>%
  group_by(year) %>%
  summarise(
    emissions = sum(emissions_cf, na.rm = TRUE),
    emissions_gt = emissions / 1000000,
    scenario = "Counterfactual: no sequencing",
    .groups = "drop"
  )

actual_path <- panel_msm_weighted %>%
  mutate(emissions = exp(lnEmissions_co2)) %>%
  group_by(year) %>%
  summarise(
    emissions = sum(emissions, na.rm = TRUE),
    emissions_gt = emissions / 1000000,
    scenario = "Observed",
    .groups = "drop"
  )

plot_df <- bind_rows(actual_path, cf_path)
plot_df <- plot_df %>%
  mutate(emissions_gt = emissions / 1000000)


label_df <- plot_df %>%
  group_by(scenario) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(
    label = c("Counterfactual\n(no sequencing)",
              "Observed")
  )

ggplot(plot_df,
       aes(year, emissions_gt, colour = scenario)) +
  
  geom_line(linewidth = 1.3, lineend = "round") +
  
  geom_point(
    data = plot_df %>% filter(year %% 2 == 0),
    size = 1.8
  ) +
  
  geom_text(
    data = label_df,
    aes(label = label),
    hjust = 0,
    nudge_x = 0.4,
    lineheight = 0.95,
    fontface = "bold",
    size = 4.3,
    show.legend = FALSE
  ) +
  
  scale_colour_manual(
    values = c(
      "Counterfactual: no sequencing" = "#0072B2",
      "Observed" = "#D55E00"
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(1995, 2025, 5),
    limits = c(1996, 2024)
  ) +
  
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    x = NULL,
    y = expression("Emissions (Gt CO"[2]*")"),
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    legend.position = "none",
    
    plot.title = element_text(
      face = "bold",
      size = 17,
      margin = margin(b = 12)
    ),
    
    axis.title.y = element_text(face = "bold"),
    
    axis.text = element_text(colour = "grey20"),
    
    axis.line = element_line(linewidth = 0.4),
    
    axis.ticks = element_line(linewidth = 0.4),
    
    panel.grid.major.y = element_line(
      colour = "grey90",
      linewidth = 0.35
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(10, 70, 10, 10)
  )


gap_df <- actual_path %>%
  transmute(year, observed = emissions_gt) %>%
  full_join(
    cf_path %>% transmute(year, counterfactual = emissions_gt),
    by = "year"
  ) %>%
  arrange(year) %>%
  mutate(
    yearly_gap = counterfactual - observed,   # positive = emissions avoided by sequencing
    cum_gap = cumsum(yearly_gap)
  )

# Simple cumulative difference (annual sum)
total_cumulative_gap <- sum(gap_df$yearly_gap, na.rm = TRUE)

# Area under the gap curve (numerical integration; same as sum for evenly spaced annual data)
auc_gap <- pracma::trapz(gap_df$year, gap_df$yearly_gap)

total_cumulative_gap
auc_gap


# UK Single-country counterfactual map
uk_data <- panel_msm_weighted %>%
  filter(ISO == "GBR")

# Coefficients
b <- coef(msm_model_co2)

# Calculate sequencing effect
uk_cf <- uk_data %>%
  mutate(seq_effect = 0)

# Subsidy timing effects
uk_cf <- uk_cf %>%
  mutate(
    seq_effect = seq_effect +
      ifelse(
        sub_timing == "1to4_before",
        b["sub_timing1to4_before"] +
          b["lag_price_string:sub_timing1to4_before"] *
          lag_price_string,
        0
      ),
    
    seq_effect = seq_effect +
      ifelse(
        sub_timing == "5plus_before",
        b["sub_timing5plus_before"] +
          b["lag_price_string:sub_timing5plus_before"] *
          lag_price_string,
        0
      ),
    
    # Standard timing effects
    seq_effect = seq_effect +
      ifelse(
        std_timing == "1to4_before",
        b["std_timing1to4_before"] +
          b["lag_price_string:std_timing1to4_before"] *
          lag_price_string,
        0
      ),
    
    seq_effect = seq_effect +
      ifelse(
        std_timing == "5plus_before",
        b["std_timing5plus_before"] +
          b["lag_price_string:std_timing5plus_before"] *
          lag_price_string,
        0
      )
  )

# Counterfactual emissions
uk_cf <- uk_cf %>%
  mutate(
    ln_cf = lnEmissions_co2 - seq_effect,
    emissions_cf = exp(ln_cf)
  )

# UK counterfactual path
uk_cf_path <- uk_cf %>%
  group_by(year) %>%
  summarise(
    emissions = sum(emissions_cf, na.rm = TRUE),
    emissions_gt = emissions / 1000000,
    scenario = "Counterfactual: no sequencing",
    .groups = "drop"
  )

# UK observed path
uk_actual_path <- uk_data %>%
  mutate(emissions = exp(lnEmissions_co2)) %>%
  group_by(year) %>%
  summarise(
    emissions = sum(emissions, na.rm = TRUE),
    emissions_gt = emissions / 1000000,
    scenario = "Observed",
    .groups = "drop"
  )

# Combine
uk_plot_df <- bind_rows(
  uk_actual_path,
  uk_cf_path
)

# Labels at final year
uk_label_df <- uk_plot_df %>%
  group_by(scenario) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(
    label = ifelse(
      scenario == "Counterfactual: no sequencing",
      "Counterfactual\n(no sequencing)",
      "Observed"
    )
  )

# Plot
ggplot(
  uk_plot_df,
  aes(year, emissions_gt, colour = scenario)
) +
  
  geom_line(
    linewidth = 1.3,
    lineend = "round"
  ) +
  
  geom_point(
    data = uk_plot_df %>% filter(year %% 2 == 0),
    size = 1.8
  ) +
  
  geom_text(
    data = uk_label_df,
    aes(label = label),
    hjust = 0,
    nudge_x = 0.4,
    lineheight = 0.95,
    fontface = "bold",
    size = 4.3,
    show.legend = FALSE
  ) +
  
  scale_colour_manual(
    values = c(
      "Counterfactual: no sequencing" = "#0072B2",
      "Observed" = "#D55E00"
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(1995, 2025, 5),
    limits = c(1996, 2024)
  ) +
  
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    title = "UK CO₂ emissions: sequencing counterfactual",
    x = NULL,
    y = expression("Emissions (Gt CO"[2]*")")
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    legend.position = "none",
    
    plot.title = element_text(
      face = "bold",
      size = 17,
      margin = margin(b = 12)
    ),
    
    axis.title.y = element_text(face = "bold"),
    
    axis.text = element_text(colour = "grey20"),
    
    axis.line = element_line(linewidth = 0.4),
    
    axis.ticks = element_line(linewidth = 0.4),
    
    panel.grid.major.y = element_line(
      colour = "grey90",
      linewidth = 0.35
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(10, 70, 10, 10)
  )


# 6: Regression Tables -------------------------------------------------------
var_labels_price <- c(
  lag_price_string = "Lagged presence of price",
  "lag_price_string:sub_timing1to4_before" = "Price × subsidy timing: 1–4 years before",
  "lag_price_string:sub_timing5plus_before" = "Price × subsidy timing: 5+ years before",
  "lag_price_string:std_timing1to4_before" = "Price × standard timing: 1–4 years before",
  "lag_price_string:std_timing5plus_before" = "Price × standard timing: 5+ years before",
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
  sub_timing1to4_before = "Subsidy timing: 1–4 years before",
  sub_timing5plus_before = "Subsidy timing: 5+ years before",
  std_timing1to4_before = "Standard timing: 1–4 years before",
  std_timing5plus_before = "Standard timing: 5+ years before",
)

var_labels_taxets <- c(
  lag_ets_string = "Lagged ETS stringency",
  lag_tax_string = "Lagged carbon tax stringency",
  
  "lag_ets_string:sub_timing1to4_before" = "ETS × subsidy timing: 1–4 years before",
  "lag_ets_string:sub_timing5plus_before" = "ETS × subsidy timing: 5+ years before",
  "lag_ets_string:std_timing1to4_before" = "ETS × standard timing: 1–4 years before",
  "lag_ets_string:std_timing5plus_before" = "ETS × standard timing: 5+ years before",
  
  "lag_tax_string:sub_timing1to4_before" = "Carbon tax × subsidy timing: 1–4 years before",
  "lag_tax_string:sub_timing5plus_before" = "Carbon tax × subsidy timing: 5+ years before",
  "lag_tax_string:std_timing1to4_before" = "Carbon tax × standard timing: 1–4 years before",
  "lag_tax_string:std_timing5plus_before" = "Carbon tax × standard timing: 5+ years before",
  
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
  AVservicepcGDP = "Services / GDP"
)

# Etable for price-only model
etable(
  msm_model,
  msm_model_co2,
  headers = c("GHG emissions (CO₂e)", "CO₂ emissions"),
  dict = var_labels_price,
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


# Regression table distinguishing taxes from ETS 
etable(
  msm_model_taxets,
  headers = "CO₂ emissions",
  dict = var_labels_taxets,
  style.tex = style.tex("aer"),
  depvar = FALSE,
  order = c(
    "lag_ets_string",
    "lag_tax_string",
    
    "lag_ets_string:sub_timing",
    "lag_ets_string:std_timing",
    
    "lag_tax_string:sub_timing",
    "lag_tax_string:std_timing",
    
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

# Table for robustness 

models_co2_robust <- list(
  "Baseline"            = msm_model_co2,
  "No weights"          = msm_model_co2_noweights,
  "Sector FE"           = msm_model_co2_sector,
  "4-year bands"        = msm_model_co2_4band,
  "6-year bands"        = msm_model_co2_6band,
  "2-year lag"          = msm_model_co2_2yearlag,
  "3-year lag"          = msm_model_co2_3yearlag,
  "No controls"         = msm_model_co2_naive,
  "Economic controls"   = msm_model_co2_econ,
  "Weather controls"    = msm_model_co2_weather,
  "Full controls"       = msm_model_co2_inst
)


# 7: Old code of policy-level panels -----------------------------------------
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
    lag_price_string_2years = lag(price_stringency, n = 2, default = 0),
    lag_price_string_3years = lag(price_stringency, n = 2, default = 0),
    lag_reg_string   = lag(reg_stringency, n = 1, default = 0),
    lag_numprice     = lag(numprice, n = 1, default = 0),
    lag_numreg       = lag(numreg, n = 1, default = 0),
    lag_numsub       = lag(numsubsidy, n = 1, default = 0),
    lag_numstandard  = lag(numstandard, n = 1, default = 0)
  ) %>%
  ungroup()


