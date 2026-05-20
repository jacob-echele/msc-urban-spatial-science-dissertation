library(sf)
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(RColorBrewer)
library(tidyverse)
library(car)

###########################
#   REGRESSION ANALYSIS
###########################

#set working directory
setwd("C:/Users/User/OneDrive/Desktop/School/7- Graduate School/Term 2/Dissertation/Data/")

#read in font for outputs
windowsFonts(Times = windowsFont("Times New Roman"))

#read in regression-ready datasets
stl_regression_data <- readRDS("Processed Data/Regression Outputs/stl_regression_data.rds")
minneapolis_regression_data <- readRDS("Processed Data/Regression Outputs/minneapolis_regression_data.rds")
nyc_regression_data <- readRDS("Processed Data/Regression Outputs/nyc_regression_data.rds")
combined_regression_data <- readRDS("Processed Data/Regression Outputs/combined_regression_data.rds")

#read in original combined analysis datasets if needed later
stl_hex_analysis <- readRDS("Processed Data/Regression Outputs/stl_hex_analysis.rds")
hennepin_county_hex_analysis <- readRDS("Processed Data/Regression Outputs/hennepin_county_hex_analysis.rds")
nyc_hex_analysis <- readRDS("Processed Data/Regression Outputs/nyc_hex_analysis.rds")
combined_correlation_data <- readRDS("Processed Data/Regression Outputs/combined_correlation_data.rds")

#quick checks to make sure everything loaded correctly
glimpse(combined_regression_data)
table(combined_regression_data$city)

##########################
#   COMBINED REGRESSION
##########################

#pooled regression model
combined_regression_model <- lm(
  log_fiscal_productivity~
    log_pop_density_acre+
    log_perceived_job_density+
    log_transit_access_jobs_45,
  data = combined_regression_data
)

summary(combined_regression_model)

#################################
#   INDIVIDUAL CITY REGRESSION
#################################

#St. Louis regression model
stl_regression_model <- lm(
  log_fiscal_productivity~
    log_pop_density_acre+
    log_perceived_job_density+
    log_transit_access_jobs_45,
  data = stl_regression_data
)

summary(stl_regression_model)

#Minneapolis regression model
minneapolis_regression_model <- lm(
  log_fiscal_productivity~
    log_pop_density_acre+
    log_perceived_job_density+
    log_transit_access_jobs_45,
  data = minneapolis_regression_data
)

summary(minneapolis_regression_model)

#New York regression model
nyc_regression_model <- lm(
  log_fiscal_productivity~
    log_pop_density_acre+
    log_perceived_job_density+
    log_transit_access_jobs_45,
  data = nyc_regression_data
)

summary(nyc_regression_model)
