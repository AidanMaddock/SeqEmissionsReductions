library(tidyverse)
library(readxl)

#=========================================================
# Project: Climate Policy Sequencing
# File: 00_emissions_cleaning.R
# Description: This file attaches IPCC industry codes to
# the EDGAR datasets (co2 and co2e) and aggregates by sector and country
# Inputs: IEA_EDGAR_CO2_1970_2024.xlsx, EDGAR_AR5_GHG_1970_2024.xlsx
# Outputs: co2_sector.csv, co2e_sector.csv
#=========================================================




# Load sector-country CO2 data
excel_sheets("00_raw_data/emissions/IEA_EDGAR_CO2_1970_2024.xlsx")

co2 <- read_excel(
  "00_raw_data/emissions/IEA_EDGAR_CO2_1970_2024.xlsx", sheet = 2, skip = 9 #skip header
)

sort(unique(co2$ipcc_code_2006_for_standard_report))

# Define map of IPCC industry codes (2006) to sectors
sector_map <- tribble(
  ~ipcc_code_2006_for_standard_report, ~Sector,
  "1.A.1.a", "Electricity & heat",
  
  "1.A.2", "Industry",
  "2.A.1", "Industry",
  "2.A.2", "Industry",
  "2.A.3", "Industry",
  "2.A.4", "Industry",
  "2.B",   "Industry",
  "2.C",   "Industry",
  "2.D",   "Industry",
  
  "1.A.3.b_noRES", "Transport",
  "1.A.3.c",       "Transport",
  
  "1.A.4", "Buildings"
)


co2_sector_mapped <- co2 %>%
  left_join(sector_map,
            by = "ipcc_code_2006_for_standard_report")

# Aggregate all mapped sectors (some, e.g. aviation excluded) by name and four sectors
# Aggregate at the yearly level, given Y_... 
co2_sector <- co2_sector_mapped %>%
  filter(!is.na(Sector)) %>%
  group_by(Name, Sector) %>%
  summarise(
    across(
      starts_with("Y_"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Load country_level CO2e data
co2e <- read_excel(
  "00_raw_data/emissions/EDGAR_AR5_GHG_1970_2024.xlsx", sheet = 2, skip = 9 #skip header
)

co2e_sector_mapped <- co2e %>%
  left_join(sector_map,
            by = "ipcc_code_2006_for_standard_report")

# Likewise aggregate for carbon equivalent emissions (including N2O, CH4...)
co2e_sector <- co2e_sector_mapped %>%
  filter(!is.na(Sector)) %>%
  group_by(Name, Sector) %>%
  summarise(
    across(
      starts_with("Y_"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Remove placeholder dataframes
rm(co2_sector_mapped, co2, co2e_sector_mapped, co2e)

# Save outputs 
write.csv(co2_sector, "01_tidy_data/co2_sector.csv")
write.csv(co2e_sector, "01_tidy_data/co2e_sector.csv")
