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
  
  