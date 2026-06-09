library(dplyr)

#=========================================================
# Project: Climate Policy Sequencing
# File: 00_project_functions.R
# Description: This file contains functions called throughout the project
# Inputs: CountryGroupings.csv
# Outputs: 
#=========================================================


# Function for filtering countries and years 
filter_countries_and_years <- function(
    data,
    country_group = read_csv("/Users/aidanmaddock/Desktop/Dissertation/SeqEmissionsReductions/01_tidy_data/CountryGroupings.csv"),
    iso_col = "ISO",
    year_col = "year",
    min_year = NULL,
    max_year = NULL,
    classification = NULL
) {
  
  # Keep only countries present in country_group
  out <- data %>%
    inner_join(
      country_group,
      by = setNames("ISO", iso_col)
    )
  
  # Filter years
  if (!is.null(min_year)) {
    out <- out %>%
      filter(.data[[year_col]] >= min_year)
  }
  
  if (!is.null(max_year)) {
    out <- out %>%
      filter(.data[[year_col]] <= max_year)
  }
  
  # Optional classification filter
  if (!is.null(classification)) {
    out <- out %>%
      filter(Classification %in% classification)
  }
  out <- out %>%
    rename(ISO = all_of(iso_col)) %>%
    select(-c(Name, Classification))
   
  
  out
}