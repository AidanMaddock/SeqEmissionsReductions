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

# Load in OECD names from Stechemesser to map towards sectors and market/nonmarket instruments
policynames <- read_csv("00_raw_data/policies/policy_names_OECD.csv")

# Map OECD data to new names and sectors
oecd_data <- oecd_data %>%
  left_join(policy_map, by = "Climate actions and policies") %>%
  left_join(policynames, by = "Policy Name (OECD)") %>%
  filter(!is.na(`Policy Name (OECD)`))

# Remove non-necessary columns
oecd_data <- oecd_data[-c(1,2,3,4,7,8,9,10,13,14,16,18,19,21,22,23,24)]

# Add in US policy data
policy_data_usa = read_csv("00_raw_data/policies/IPAC_policy_usa.csv")
