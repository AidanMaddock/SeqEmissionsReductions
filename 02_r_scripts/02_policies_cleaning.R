library(tidyverse)
library(readr)

#=========================================================
# Project: Climate Policy Sequencing
# File: 01_policies_cleaning.R
# Description: This file cleans the climate policy dataset
# Inputs: OECD_CAMPF_Aggregated.csv
# Outputs: 
#=========================================================



# Variable Mapping --------------------------------------------------------


# Load in raw OECD data
oecd_data <- read_csv("00_raw_data/policies/OECD_CAMPF_Aggregated.csv")

# Map policies from raw OECD to the names adopted by Stechemesser et al. (2024)
policy_map <- tibble(
  `Climate actions and policies` = c(
    "Air emission standards",
    "Ban and phase out of fossil fuel heating systems",
    "Ban and phase out of passengers cars with ICE",
    "Ban and phase out on the construction of coal-fired power plants",
    "Building energy codes",
    "Carbon tax - Buildings",
    "Carbon Tax - Electricity",
    "Carbon Tax - Industry",
    "Carbon tax - Transport",
    "Congestion charges",
    "ETS - Buildings",
    "ETS - Electricity",
    "ETS - Industry",
    "ETS - Transport",
    "Energy efficiency mandates",
    "Feed-In-Tariffs",
    "Financing mechanisms available - Buildings",
    "Financing mechanisms available - Industry",
    "Fossil fuels excise taxes - Buildings",
    "Fossil fuels excise taxes - Electricity",
    "Fossil fuels excise taxes - Industry",
    "Fossil fuels excise taxes - Transport",
    "Fossil fuels subsidies - Buildings",
    "Fossil Fuel Subsidies - Industry",
    "Fossil fuels subsidies - Transport",
    "Fossil Fuel Subsidies -  Electricity",
    "Labels for vehicles",
    "Mandatory energy labels for appliances",
    "MEPS for electric motors",
    "MEPS Transport",
    "MEPS of appliances",
    "Planning for renewables expansion",
    "Renewable energy auctions",
    "Renewable energy certificates",
    "Share of rail on total surface transport public expenditure",
    "Speed limits on motorways"
  ),
  `Policy Name (OECD)` = c(
    "Air pollution standards for coal power plants",
    "Ban and phase out of fossil-fuel heating systems",
    "Ban and phase out of passengers cars with ICE",
    "Ban on the construction of new and phase out of existing unabated coal plants",
    "Building energy codes",
    "Carbon tax - Buildings",
    "Carbon tax - Electricity",
    "Carbon tax - Industry",
    "Carbon tax - Transport",
    "Congestion Charges",
    "Emissions trading schemes - Buildings",
    "Emissions trading schemes - Electricity",
    "Emissions trading schemes - Industry",
    "Emissions trading scheme - Transport",
    "Energy efficiency mandates",
    "Feed in tariffs for solar PV and wind",
    "Financing mechanisms for energy efficiency - Buildings",
    "Financing mechanism for energy efficiency - Industry",
    "Fossil fuels excise taxes - Buildings",
    "Fossil fuels excise taxes - Electricity",
    "Fossil fuels excise taxes - Industry",
    "Fossil fuels excise taxes - Transport",
    "Fossil fuels subsidies reform - Buildings",
    "Fossil fuels subsidies reform - Industry",
    "Fossil fuels subsidies reform - Transport",
    "Fossil fuels subsidies refrom - Electricity",
    "Mandatory fuel economy labels for light duty vehicles/ Energy Labels - Passenger Cars",
    "Mandatory labels for appliances",
    "Minimum energy performance standards - Electric motors",
    "Minimum energy performance standards - Transport / Fuel economy standards",
    "Minimum energy performance standards of appliances",
    "Planning for renewables expansion",
    "Auctions for solar PV and wind",
    "RPS with tradeable renewable energy certificates",
    "Share of rail on total surface transport public expenditure",
    "Speed limits on motorways"
  )
)

# Join in USA data (from IPAC)
policy_data_usa = read_csv("00_raw_data/policies/IPAC_policy_usa.csv")

policy_data_usa <- policy_data_usa %>%
  mutate(
    `Policy Name (OECD)` = recode(
      Policy,
      "AirEmissionStandards" = "Air pollution standards for coal power plants",
      "Auction" = "Auctions for solar PV and wind",
      "BanPhaseOutCoal" = "Ban on the construction of new and phase out of existing unabated coal plants",
      "Bans and Phaseouts Fossil Fuels Heating" = "Ban and phase out of fossil-fuel heating systems",
      "Bans and Phaseouts passenger cars ICE" = "Ban and phase out of passengers cars with ICE",
      "BuildingEnergyCodes" = "Building energy codes",
      "Carbon Tax Buildings" = "Carbon tax - Buildings",
      "Carbon Tax Electricity" = "Carbon tax - Electricity",
      "Carbon Tax Industry" = "Carbon tax - Industry",
      "Carbon Tax Transport" = "Carbon tax - Transport",
      "Congestion Charges" = "Congestion Charges",
      "EnergyEfficiencyMandates" = "Energy efficiency mandates",
      "ETS Buildings" = "Emissions trading schemes - Buildings",
      "ETS Electricity" = "Emissions trading schemes - Electricity",
      "ETS Industry" = "Emissions trading schemes - Industry",
      "ETS Transport" = "Emissions trading scheme - Transport",
      "ETS_Buildings" = "Emissions trading schemes - Buildings",
      "Excise Tax Transport" = "Fossil fuels excise taxes - Transport",
      "Excise Taxes Buildings" = "Fossil fuels excise taxes - Buildings",
      "Excise Taxes Electricity" = "Fossil fuels excise taxes - Electricity",
      "Excise Taxes Industry" = "Fossil fuels excise taxes - Industry",
      "Feed-In-Tariffs" = "Feed in tariffs for solar PV and wind",
      "Financing Mechanism Buildings" = "Financing mechanisms for energy efficiency - Buildings",
      "Financing Mechanism Industry" = "Financing mechanism for energy efficiency - Industry",
      "Fossil Fuel Subsides Buildings" = "Fossil fuels subsidies reform - Buildings",
      "Fossil Fuel Subsidies Electricity" = "Fossil fuels subsidies refrom - Electricity",
      "Fossil Fuel Subsidies Industry" = "Fossil fuels subsidies reform - Industry",
      "Fossil Fuel Subsidies Transport" = "Fossil fuels subsidies reform - Transport",
      "Labels Appliances" = "Mandatory labels for appliances",
      "Labels Passenger Cars" = "Mandatory fuel economy labels for light duty vehicles/ Energy Labels - Passenger Cars",
      "MEPS electric motors" = "Minimum energy performance standards - Electric motors",
      "MEPS of appliances" = "Minimum energy performance standards of appliances",
      "MEPS Transport" = "Minimum energy performance standards - Transport / Fuel economy standards",
      "PlanningRenewablesExpansion" = "Planning for renewables expansion",
      "Renewable energy certificates" = "RPS with tradeable renewable energy certificates",
      "Investment in Rail Infrastructure" = "Share of rail on total surface transport public expenditure",
      "Speed Limits" = "Speed limits on motorways",
      .default = NA_character_
    )
  )

# Load in OECD names from Stechemesser to map towards sectors and market/nonmarket instruments
policynames <- read_csv("00_raw_data/policies/policy_names_OECD.csv")

# First clean of OECD data to add in US data 
oecd_data <- oecd_data %>%
  left_join(policy_map, by = "Climate actions and policies") %>%
  rename(
    Value = OBS_VALUE,
    year = TIME_PERIOD,
    ISO = REF_AREA,
    Country = `Reference area`
  ) %>%
  select(
    ISO,
    year,
    Value,
    `Policy Name (OECD)`
  ) %>%
  filter(ISO != "USA") #USA data supressed in public version of database

policy_data_usa <- policy_data_usa %>%
  rename(Value = valueCAP_Comp) %>%
  select(
    ISO,
    year,
    Value,
    `Policy Name (OECD)`
  )

combined_data <- bind_rows(oecd_data, policy_data_usa)
oecd_data <- combined_data

oecd_data <- oecd_data %>%
  left_join(policynames, by = "Policy Name (OECD)") %>%
  filter(!is.na(`Policy Name (OECD)`)) %>%
  mutate(Policy = `Policy Name (OECD)`) 


# Remove non-necessary columns
oecd_data <- oecd_data %>%
  select(-`Policy Name (OECD)`) %>%
  select(- `Note`)


# Reorder for viewing 
oecd_data <- oecd_data %>%
  select(ISO, year, Value, Module, Policy, 'Broad Category', Market_non_market, everything())



# 2. Policy Introductions ----------------------------------------------------

# Code policies as introduced the first non-zero year, excluding NA values (no data on whether policy exists)
oecd_grouped <- oecd_data %>%
  group_by(ISO, Buildingblock, Module, Policy) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    prev_value = lag(Value),
    
    # first entry into policy
    introduction = ifelse(
      !is.na(Value) & Value > 0 &
        !is.na(prev_value) & prev_value == 0,
      1, 0
    ),
    
    jump = Value - prev_value,
    
    intensification = as.integer(
      !is.na(jump) &
        jump > 0 &
        prev_value > 0 &
        introduction == 0
    ),
    
    intensification_jump = if_else(intensification == 1, jump, NA_real_)
  ) %>%
  ungroup()


# 3. Stringency Increases -------------------------------------------------

stringency_per_year <- oecd_grouped %>%
  filter(Value > 0, .preserve = FALSE) %>%
  group_by(year) %>%
  summarise(Value = mean(Value, na.rm = TRUE))


# Save outputs for regression
write.csv(oecd_grouped, "01_tidy_data/policies.csv")




# Graphing ----------------------------------------------------------------

intro_plot_data <- oecd_grouped %>%
  filter(introduction == 1) %>%
  mutate(
    year = as.integer(year),
    ISO = factor(ISO)
  ) %>%
  mutate(ISO = factor(ISO, levels = rev(sort(unique(ISO)))))

ggplot(intro_plot_data, aes(x = year, y = ISO, fill = Policytype_detail_new)) +
  geom_tile(height = 0.8, width = 0.9, colour = "white") +
  facet_wrap(~ Module, ncol = 2) +
  labs(
    x = "Year",
    y = "Country",
    fill = "Policy Type",
    title = "Policy Introductions by Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8)
  )


policy_per_year <- oecd_grouped %>%
  group_by(year) %>%
  summarise(
    introductions = sum(introduction, na.rm = TRUE),
    intensifications = sum(intensification, na.rm = TRUE)
  )

policy_long <- policy_per_year %>%
  pivot_longer(
    cols = c(introductions, intensifications),
    names_to = "policy_type",
    values_to = "count"
  )

# Bar graph
ggplot(introductions_per_year,
       aes(x = factor(year), y = introductions)) +
  geom_col() +
  labs(
    title = "Policy Introductions per Year",
    x = "Year",
    y = "Number of Introductions"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(policy_long,
       aes(x = factor(year),
           y = count,
           fill = policy_type)) +
  geom_col() +
  labs(
    title = "Policy Changes per Year",
    x = "Year",
    y = "Count",
    fill = "Policy Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Line graph stringency
ggplot(stringency_per_year,
       aes(x = year, y = Value)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 10)) +
  labs(
    title = "Stringency Evolution",
    x = "Year",
    y = "Average Stringency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

