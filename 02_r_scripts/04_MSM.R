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

#=========================================================
# Project: Climate Policy Sequencing
# File: 04_MSM.r
# Description: 
# Inputs: 
# Outputs: 
#=========================================================

# Combine independent, dependent, and control variables -------------------
oecd_data <- read_csv("01_tidy_data/policies.csv")
emissions <- read.csv("01_tidy_data/emissions_sector.csv")
control_data <- read.csv("01_tidy_data/controls.csv")

# Inclusion Details for price and regulatory instruments 
price_categories <- c("Taxation", "Driving taxation")
subsidy_categories <- c("Green subsidy", "Renewable subsidy", "Financing mechanism")
standards_categories <- c("Air pollution standard", "Energy efficiency mandate", "Building code", "Minimum energy performance standard", "Renewable portfolio standard")

# Filter out first few years and latest ones 
oecd_data <- oecd_data %>%
  filter(!year %in% c(1990,1991,1992,1993,1994, 1995, 2023)) 

# Build out panel for model
panel <- oecd_data %>%
  arrange(ISO, Module, year) %>%
  group_by(ISO, Module) %>%
  mutate(
    # Variables to assign policy introductions to categories
    is_price = `Broad Category` %in% price_categories,
    is_subsidy = `Broad Category` %in% subsidy_categories,
    is_standard = `Broad Category` %in% standards_categories,
    is_reg = !is_price,
    
    # yearly introduction events based on introduction or intensification
    priceintro = as.integer(is_price  & introduction == 1 | is_price  & intensification == 1),
    regintro   = as.integer(!is_price & introduction == 1 | !is_price  & intensification == 1),
    subsidyintro   = as.integer(is_subsidy & introduction == 1 | is_subsidy  & intensification == 1),
    standardintro   = as.integer(is_standard & introduction == 1 | is_standard  & intensification == 1),
    
    # cumulative in-force states
    price = cummax(priceintro),
    reg   = cummax(regintro),
    subsidy  = cummax(subsidyintro),
    standard  = cummax(standardintro),
    
    # time-varying regime state
    state = case_when(
      price == 0 & reg == 0 ~ "none",
      price == 1 & reg == 0 ~ "price",
      price == 0 & reg == 1 ~ "reg",
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
      path_group == "simultaneous"  & year < first_price_year ~ "none",
      path_group == "price_only"    & price == 0             ~ "none",
      path_group == "reg_only"      & reg == 0               ~ "none",
      TRUE ~ path_group
    ),
    path = factor(path, levels = c("none", "price_first", "reg_first", "simultaneous", "price_only", "reg_only"))
  ) %>%
  ungroup() 

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
    regintro   = as.integer(year == first_sub_year),
    
    lag_price_string = lag(price_stringency, n = 1, default = 0),
    lag_reg_string   = lag(reg_stringency, n = 1,  default = 0),
    lag_numprice     = lag(numprice, n = 1,  default = 0),
    lag_numreg       = lag(numreg, n = 1, default = 0),
    lag_numsub = lag(numsubsidy, n = 1, default = 0),
    lag_numstandard = lag(numstandard, n = 1, default = 0),
  ) %>%
  ungroup()


# Join all three together for final model
panel_data <- panel_sectors %>%
  left_join(emissions,by = c("ISO", "year", "Module")) %>% # Join emissions data in
  left_join(control_data, by = c("ISO", "year")) %>%
  filter(!ISO %in% c("EU27_2020")) 


write_csv(panel,"00_raw_data/joined_data.csv")


# Weighting (time-varying)

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


msm_model <- feols(
  lnEmissions_co2e ~ lag_price + sub_timing + std_timing + lag_price_string:sub_timing + lag_price_string:std_timing + lag_price_string + lag_numsub + lag_numstandard + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_model)


msm_reg_model <- feols(
  lnEmissions_co2 ~ lag_price + reg_timing + lag_price:reg_timing + lag_price_string + lag_numreg + pop + GDPpc2015 + annual_HDD + annual_CDD + tempvariation + importpcGDP + urbpop + ruleoflaw + AVservicepcGDP |
    ISO + year,
  data    = panel_msm_weighted, 
  weights = ~sw,
  cluster = ~ ISO^Module
)
summary(msm_reg_model)

# Marginal effects
pct_effects <- 100 * (exp(coef(model)[grep("^pathgroup", names(coef(model)))]) - 1)
pct_effects


# Predicted effects





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

