library(tidyverse)
library(readr)
library(hpfilter)
library(readxl)
source("02_r_scripts/00_project_functions.R")

#=========================================================
# Project: Climate Policy Sequencing
# File: 03_controls_cleaning
# Description: This file cleans the control variables and outputs
# a single combined file
# Inputs: HDD and CDD data (IEA), Temperature data (WB), WB WDI and Governance Data
# Outputs: controls.csv 
#=========================================================

# Load in 16 degree baseline HDD and 18 degree CDD
hdd <- read_csv("00_raw_data/controls/IEA_CMCC_HDD16monthlyworldbypopallmonths.csv", skip = 9) 
cdd <- read_csv("00_raw_data/controls/IEA_CMCC_CDD18monthlyworldbypopallmonths.csv", skip = 9)

# Join temp data together
temp_data <- hdd %>%
  left_join(cdd, by = c("ISO3", "Date")) %>%
  select(ISO3, Date, HDD16, CDD18) %>%
  rename(ISO = ISO3)

# Summarise HDD and CDD into yearly format
annual_temp <- temp_data %>%
  mutate(year = format(Date, "%Y"), year = as.integer(year)) %>%
  group_by(ISO, year) %>%
  summarise(
    annual_HDD = sum(HDD16, na.rm = TRUE),
    annual_CDD = sum(CDD18, na.rm = TRUE),
    .groups = "drop"
  )


# Temperature variation data 
tempvar <- read_excel("/Users/aidanmaddock/Desktop/Dissertation/SeqEmissionsReductions/00_raw_data/controls/CRU0.5_tempdata.xlsx")

# Get rid of monthly variable in tempvar
names(tempvar)[names(tempvar) %in% year_column_temp] <- 
  substr(year_column_temp, 1, nchar(year_column_temp) - 3)

year_column_temp <- grep("^(19|20)", names(tempvar), value = TRUE)
  
temp_long <- tempvar %>%
  pivot_longer(
    cols = all_of(year_column_temp),
    names_to = "year",
    values_to = "meantemp"
  ) %>%
  rename(ISO = code) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year >= 1990, year <= 2024)

# Calculate long-run average and variation from it
temp_long <- temp_long %>%
  group_by(ISO) %>%
  mutate(
    longrun_avg = mean(meantemp) # Long run temperature average from 1990 to 2024
  ) %>%
  ungroup() %>%
  mutate(
    tempvariation = meantemp - longrun_avg
  ) %>%
  select(c(ISO,year,tempvariation))


# WB Data -----------------------------------------------------------------

#Data for economic control variables 

wbdata <- read_csv("/Users/aidanmaddock/Desktop/Dissertation/SeqEmissionsReductions/00_raw_data/controls/WB_WDIData.csv")
wbdata <- wbdata[1:(nrow(wbdata) - 5), ] # Footer data is being erroneously loaded in

year_cols <- grep("^(19|20)", names(wbdata), value = TRUE) # Make list of year columns for pivoting

# Convert long year-country data repeated across sectors to a long year-country with sector data
wb_out <- wbdata %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(str_extract(year, "\\d{4}")), #Get rid of [YR_1970] and just use the year
    value = na_if(value, ".."),
    value = parse_number(value) # make sure dbl
  ) %>%
  select(`Country Code`, `Series Code`, year, value) %>%
  pivot_wider(
    names_from = `Series Code`,
    values_from = value
  ) 

wb_out <- wb_out %>%
  rename(
    pop = SP.POP.TOTL,
    importpcGDP = NE.IMP.GNFS.ZS,
    AVservicepcGDP = NV.SRV.TOTL.ZS,
    GDP2015 = NY.GDP.MKTP.KD,
    GDPgrowth = NY.GDP.MKTP.KD.ZG,
    GDPpc2015 = NY.GDP.PCAP.KD,
    GDPpc2015ppp = NY.GDP.PCAP.PP.KD,
    urbpop = SP.URB.TOTL
  )


# Do similarily with WBgov data (rule of law)
wbgov <- read_csv("/Users/aidanmaddock/Desktop/Dissertation/SeqEmissionsReductions/00_raw_data/controls/WB_govData.csv")
wbgov <- wbgov[1:(nrow(wbgov) - 5), ] # Footer data is being erroneously loaded in

year_cols_gov <- grep("^(19|20)", names(wbgov), value = TRUE) # Make seperate list of year columns for pivoting
# Convert long year-country data repeated across sectors to a long year-country with sector data
wbgov_out <- wbgov %>%
  pivot_longer(
    cols = all_of(year_cols_gov),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(str_extract(year, "\\d{4}")), #Get rid of [YR_1970] and just use the year
    value = na_if(value, ".."),
    value = parse_number(value) # make sure dbl
  ) %>%
  select(`Country Code`, `Series Code`, year, value) %>%
  pivot_wider(
    names_from = `Series Code`,
    values_from = value
  ) 


# No data for 1997, 1999, and 2001. Create columns that have the average value of the year before and after for these years (linear interpolation)
wbgov_interpolated <- wbgov_out %>%
  group_by(`Country Code`) %>%
  complete(year = 1996:2024) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    across(
      -year,
      ~ zoo::na.approx(.x, x = year, na.rm = FALSE)
    )
  ) %>%
  ungroup()

# Rename rule of law indicators
wbgov_interpolated <- wbgov_interpolated %>%
  rename(
    govteffectiveness = GOV_WGI_GE.SC,
    regquality = GOV_WGI_RQ.SC,
    ruleoflaw = GOV_WGI_RL.SC
  )


# Filter to study datasets
wb_econ <- filter_countries_and_years(
  data = wb_out,
  iso_col = "Country Code",
  year_col = "year",
  min_year = 1994, # Note that initially this is filtered to earlier to calculate the HP filter as it needs a trend
  max_year = 2024
)

# Calculation of HP filter for cyclical GDP components
wb_econ_hp <- wb_econ %>%
  arrange(ISO, year) %>%
  group_by(ISO) %>%
  group_modify(~ {
    x <- data.frame(GDPpc2015 = .x$GDPpc2015)
    
    tr <- hp2(x, 6.25) # Two-sided HP filter with a smoothing lambda of 6.25, from Ravn and Uhlig (2002)
    
    # hp2 returns a data frame; for one variable, take the first column
    trend <- as.numeric(tr[[1]])
    
    .x %>%
      mutate(
        GDPpc2015_trend = trend,
        GDPpc2015_cycle = GDPpc2015 - GDPpc2015_trend
      )
  }) %>%
  ungroup() %>%
  filter(!year %in% c(1994, 1995, 2023, 2024))

wb_gov <- filter_countries_and_years(
  data = wbgov_interpolated,
  iso_col = "Country Code",
  year_col = "year",
  min_year = 1996,
  max_year = 2022
)

temp <- filter_countries_and_years(
  data = annual_temp,
  iso_col = "ISO",
  year_col = "year",
  min_year = 1996,
  max_year = 2022
)

temp_var <- filter_countries_and_years(
  data = temp_long,
  iso_col = "ISO",
  year_col = "year",
  min_year = 1996,
  max_year = 2022
)


# Joining -----------------------------------------------------------------

# Join all three into one control dataset
controls <- wb_econ_hp %>%
  left_join(wb_gov, by = c("ISO", "year")) %>%
  left_join(temp, by = c("ISO", "year")) %>%
  left_join(temp_var, by = c("ISO","year"))

write_csv(controls, "01_tidy_data/controls.csv")

# Old code
wbgdp <- wb_econ %>% 
  select(c(ISO,year,GDPpc2015)) %>% 
  filter(ISO == "ARG") %>% 
  select(-ISO, -year)

ytrend <- hp2(wbgdp, 6.25)
ycycle <- wbgdp - ytrend



ggplot(temp_var, aes(x = year, y = tempvariation, colour = ISO, group = ISO)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Temperature variation from 1990–2024 average",
    colour = "ISO"
  ) +
  theme_minimal()



# Plot GDP cyclicality as a proof of concept 
plot(wbgdp$GDPpc2015, type="l", col="black", lty=1)
lines(ytrend$GDPpc2015, col="#066462")
polygon(c(1, seq(ycycle$GDPpc2015), length(ycycle$GDPpc2015)),
        c(0, ycycle$GDPpc2015, 0), col = "#E0F2F1")
legend("bottom", horiz=TRUE, cex=0.75, c("y", "ytrend",
                                         "ycycle"), lty = 1, col = c("black", "#066462", "#75bfbd"))
