library(tidyverse)
library(readr)
library(fixest)
library(stargazer)
library(ipw)
library(forcats)

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

oecd_data <- oecd_data %>%
  filter(!year %in% c(1990,1991,1992,1993,1994, 1995, 2023)) 



panel <- oecd_data %>%
  arrange(ISO, Module, year) %>%
  group_by(ISO, Module) %>%
  mutate(
    id = interaction(ISO, Module, drop = TRUE),
    t  = row_number() - 1,
    
    # yearly introduction events
    priceintro = as.integer(Policytype == "MBI"  & introduction == 1),
    regintro   = as.integer(Policytype == "NMBI" & introduction == 1),
    
    # cumulative in-force states
    price = cummax(priceintro),
    reg   = cummax(regintro),
    
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
  ungroup() %>%
  group_by(id) %>%
  mutate(
    lag_price = lag(price, default = 0),
    lag_reg   = lag(reg, default = 0),
    lag_state = lag(state, default = "none")
  ) %>%
  ungroup()

# Collapse down into sectors (Module)
panel_sectors <- panel %>%
  group_by(ISO, Module, year) %>%
  summarise(
    
    numprice = sum(price == 1 & Policytype == "MBI", na.rm = TRUE),
    numreg   = sum(reg   == 1 & Policytype == "NMBI", na.rm = TRUE),
    
    price = as.integer(any(price == 1, na.rm = TRUE)),
    reg   = as.integer(any(reg   == 1, na.rm = TRUE)),
    path = first(path),
    state = first(state),
    pathgroup = first(path_group),
    
    first_price_year = if (any(priceintro == 1, na.rm = TRUE)) {
      min(year[priceintro == 1], na.rm = TRUE)
    } else NA_integer_,
    
    first_reg_year = if (any(regintro == 1, na.rm = TRUE)) {
      min(year[regintro == 1], na.rm = TRUE)
    } else NA_integer_,
    
    price_stringency = if (any(Policytype == "MBI")) {
      sum(Value[Policytype == "MBI"], na.rm = TRUE)
    } else NA_real_,
    
    reg_stringency = if (any(Policytype == "NMBI")) {
      sum(Value[Policytype == "NMBI"], na.rm = TRUE)
    } else NA_real_,
    
    .groups = "drop"
  ) %>%
  arrange(ISO, Module, year) %>%
  group_by(ISO, Module) %>%
  mutate(
    lag_price_string = lag(price_stringency, n = 3, default = 0),
    lag_reg_string   = lag(reg_stringency, n = 3,  default = 0),
    lag_numprice     = lag(numprice, n = 3,  default = 0),
    lag_numreg       = lag(numreg, n = 3, default = 0)
  ) %>%
  ungroup()

# Join all three together for final model
panel_data <- panel_sectors %>%
  left_join(emissions,by = c("ISO", "year", "Module")) %>% # Join emissions data in
  left_join(control_data, by = c("ISO", "year")) %>%
  filter(Module == "Industry") %>%
  filter(ISO != "EU27_2020")

# Weighting ---------------------------------------------------------------


panel_weights <- panel_data %>%
  arrange(ISO, year) %>%
  group_by(ISO) %>%
  mutate(
    id = interaction(ISO, Module, drop = TRUE),   # or just ISO if no Module
    t  = row_number() - 1,
    price_start = as.integer(price == 1) 
  ) %>%
  ungroup()


w_price <- ipwtm(
  exposure   = price_start,
  family     = "binomial",
  link       = "logit",
  numerator  = ~ 1, 
  denominator = ~ lag_price_string + pop + GDPpc2015 + annual_HDD + annual_CDD +
    GDPpc2015_cycle + ruleoflaw + importpcGDP + tempvariation,
  id         = id,
  timevar    = t,
  type       = "all",
  data       = panel_weights,
  trunc      = 0.01
)

ipwplot(weights = temp$ipw.weights, timevar = haartdat$fuptime,
        binwidth = 100, ylim = c(-1.5, 1.5), main = "Stabilized inverse probability weights")

# Model -------------------------------------------------------------------


# Specify controls 
controls <- ~ pop + GDPpc2015 + annual_HDD + annual_CDD +
  GDPpc2015_cycle + tempvariation

fml <- xpd(
  Emissions_co2 ~ pathgroup + state + lag_price_string + lag_reg_string + ..controls | year,
  ..controls = controls
)

model <- feols(
  fml,
  cluster = ~ISO,
  data = panel_data
)

model
summary(model)




# Figures -----------------------------------------------------------------


# Fig 4a
groupings <- read_csv("01_tidy_data/CountryGroupings.csv")

plot_panel <- policy_panel %>%
  left_join(groupings, by = "ISO") %>%
  mutate(
    state = case_when(
      price == 0 & reg == 0 ~ "none",
      price == 0 & reg == 1 ~ "reg only",
      price == 1 & reg == 0 ~ "price only",
      price == 1 & reg == 1 ~ "both"
    )
  ) %>%
  group_by(Classification, Module, ISO) %>%
  mutate(first_adopt = min(if_else(state != "none", year, NA_integer_), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    first_adopt = if_else(is.infinite(first_adopt), NA_real_, first_adopt)
  ) %>%
  arrange(Classification, Module, first_adopt, ISO) %>%
  mutate(
    ISO = factor(ISO, levels = unique(ISO)),
    state = factor(state, levels = c("none", "reg only", "price only", "both"))
  )


ggplot(plot_panel, aes(x = year, y = ISO, fill = state)) +
  geom_tile() +
  facet_grid(Classification ~ Module, scales = "free_y", space = "free_y") +
  scale_fill_manual(
    values = c(
      "none" = "white",
      "reg only" = "steelblue3",
      "price only" = "darkorange2",
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



