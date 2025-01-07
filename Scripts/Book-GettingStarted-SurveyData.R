##
## PROJECT: r-survey
## PURPOSE: Training - Exploring Complex Survey Data Analysis with R
## AUTHOR: Baboyma Kagniniwa 
## DATE: 2024-12-24
## UPDATED: 2025-01-06
## REFERENCE: https://tidy-survey-r.github.io/tidy-survey-book/
##

# LIBRARIES ----

  library(tidyverse)  # Data Munging and Viz
  library(survey)     # Survey Data Processing & Analysis
  library(srvyr)
  library(srvyrexploR)# Contains sample survey data - pak::pak("tidy-survey-r/srvyrexploR")
  library(broom)      # Data Conversion: Stats to Tidy Tibbles
  library(gt)         # Great Tables
  library(gtsummary)
  library(censusapi)  # Census API for pop data extraction. Set API Key with: Sys.setenv(CENSUS_KEY = "YOUR_API_KEY_HERE")

# PARAMS ----

  ## Directories

  dir_temp <- "./Data/Temp"
  dir_data <- "./Data/Raw"
  dir_dataout <- "./Data/Processed"

  ## Files

  ## Other 

  #glamr::set_key("census", "census_key")
  #census_key <- glamr::get_key("census", "census_key")
  api_key <- Sys.getenv("CENSUS_KEY")

# DATA IMPORT ----

  ## Explore existing sample data
  help(package = "srvyrexploR")

  ## The American National Election Studies (ANES)

  data("anes_2020", package = "srvyrexploR")

  anes <- anes_2020

  anes |> glimpse()

  anes |> 
    select(-matches("^V\\d")) |> 
    glimpse()

  ## The Residential Energy Consumption Survey (RECS)

  data("recs_2020", package = "srvyrexploR")

  recs <- recs_2020

  recs |> glimpse()

  recs |> 
    select(-matches("^NWEIGHT")) |> 
    glimpse()

# DATA PROCESSING ----

  ## CPS - Current Population Survey - Data from Census
  cps_state_in <- getCensus(
    name = "cps/basic/mar",
    vintage = 2020,
    region = "state",
    vars = c(
      "HRMONTH", "HRYEAR4",
      "PRTAGE", "PRCITSHP", "PWSSWGT"
    ),
    key = api_key
  )
  
  cps_state_in %>% glimpse()
  
  cps_state <- cps_state_in %>%
    as_tibble() %>%
    mutate(across(
      .cols = everything(),
      .fns = as.numeric
    ))

  cps_state %>% distinct(HRMONTH, HRYEAR4)
  cps_state %>% distinct(PRCITSHP) # Citizenship

  # Sub-population: 18+ yo & residents
  cps_narrow_resp <- cps_state %>%
    filter(
      PRTAGE >= 18,
      PRCITSHP %in% c(1:4)
    )
  
  targetpop <- cps_narrow_resp %>%
    pull(PWSSWGT) %>%
    sum()
  
  scales::comma(targetpop)

  # Adjust pop weights
  anes_adjwgt <- anes %>%
    mutate(Weight = V200010b / sum(V200010b) * targetpop)

  # ANES - Survey design
  anes_des <- anes_adjwgt %>%
    as_survey_design(
      weights = Weight,
      strata = V200010d,
      ids = V200010c,
      nest = TRUE
    )

  anes_des
  
  # RECS - Survey Design
  recs_des <- recs %>% 
    as_survey_design(
      weights = NWEIGHT, 
      repweights = NWEIGHT1:NWEIGHT60,
      type = "JK1",
      scale = 59 / 60,
      mse = TRUE,
      variables = c(ACUsed, Region)
    )
  
  recs_des
  
  recs_des %>% 
    summarize(
      p = survey_mean(ACUsed, vartype = "ci", proportion = TRUE, prop_method = "logit"),
      .by = Region
    )

## DATA ANALYSIS

## DATA VIZ

## OUTPUTS