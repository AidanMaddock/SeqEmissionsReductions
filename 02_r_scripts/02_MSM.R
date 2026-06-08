library(tidyverse)
library(readr)
library(fixest)

#=========================================================
# Project: Climate Policy Sequencing
# File: 03_MSM.r
# Description: 
# Inputs: 
# Outputs: 
#=========================================================


oecd_data <- read_csv("01_tidy_data/policies.csv")
emissions <- read.csv("01_tidy_data/emissions_sector.csv")

oecd_data <- oecd_data %>%
  left_join(
    emissions,
    by = c("ISO", "year", "Module")
  ) 

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

oecd_data <- oecd_data %>%
  left_join(
    policy_years %>% select(ISO, year, policies_in_force),
    by = c("ISO", "year")
  )

model <- feols(
  policies_in_force ~ Emissions_co2 | ISO + year,
  cluster = ~ISO,
  data = oecd_data
)

model
