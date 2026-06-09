library(tidyverse)
library(readr)
library(fixest)
library(stargazer)

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
controls <- read.csv("01_tidy_data/controls.csv")

oecd_data <- oecd_data %>%
  left_join(emissions,by = c("ISO", "year", "Module")) %>% # Join emissions data in
  left_join(controls, by = c("ISO", "year")) %>%
  filter(!year %in% c(1990,1991,1992,1993,1994, 1995, 2023))
  
policy_years <- oecd_data %>%
  group_by(ISO, year) %>%
  summarise(
    policies_introduced = sum(introduction, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ISO, year) %>%
  group_by(ISO) %>%
  mutate(
    policies_in_force = cumsum(policies_introduced)
  ) %>%
  ungroup()


# Model -------------------------------------------------------------------
oecd_data <- oecd_data %>%
  left_join(
    policy_years %>% select(ISO, year, policies_in_force),
    by = c("ISO", "year")
  )

controls <- ~ pop.x + GDPpc2015.x + annual_HDD.x + GDPpc2015_cycle.x + ruleoflaw.x + importpcGDP.x + AVservicepcGDP.x

fml <- xpd(
  policies_in_force ~ Emissions_co2 + ..controls | ISO + year,
  ..controls = controls
)

model <- feols(
  fml,
  cluster = ~ISO,
  data = oecd_data
)

model

