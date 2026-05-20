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
library(spdep)

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

# -----------------------
#    residual graphing
# -----------------------

plot(combined_regression_model)
plot(stl_regression_model)
plot(minneapolis_regression_model)
plot(nyc_regression_model)

########################
#   MORAN'S I TESTING
########################

# ---------------
#    St. Louis
# ---------------

#add model residuals to regression data
stl_residuals <- stl_regression_data%>%
  mutate(
    residuals = residuals(stl_regression_model)
  )%>%
  select(hex_id, residuals)

#join residuals back to spatial hex dataset
stl_hex_analysis_residuals <- stl_hex_fp%>%
  left_join(
    stl_residuals,
    by = "hex_id"
  )%>%
  filter(!is.na(residuals))

#check rows match
nrow(stl_hex_analysis_residuals)
length(residuals(stl_regression_model))

#create neighbor structure
stl_neighbors <- stl_hex_analysis_residuals%>%
  st_geometry()%>%
  poly2nb()

#create weights matrix
stl_weights <- nb2listw(
  stl_neighbors,
  style = "W",
  zero.policy = TRUE
)

#run morans i
stl_residual_moran <- moran.test(
  stl_hex_analysis_residuals$residuals,
  stl_weights,
  zero.policy = TRUE
)

stl_residual_moran

#quick map
qtm(stl_hex_analysis_residuals, fill = "residuals")


# -----------------
#    Minneapolis
# -----------------

#add model residuals to regression data
minneapolis_residuals <- minneapolis_regression_data%>%
  mutate(
    residuals = residuals(minneapolis_regression_model)
  )%>%
  select(hex_id, residuals)

#join residuals back to spatial hex dataset
minneapolis_hex_analysis_residuals <- hennepin_county_hex_fp%>%
  left_join(
    minneapolis_residuals,
    by = "hex_id"
  )%>%
  filter(!is.na(residuals))

#check rows match
nrow(minneapolis_hex_analysis_residuals)
length(residuals(minneapolis_regression_model))

#create neighbor structure
minneapolis_neighbors <- minneapolis_hex_analysis_residuals%>%
  st_geometry()%>%
  poly2nb()

#create weights matrix
minneapolis_weights <- nb2listw(
  minneapolis_neighbors,
  style = "W",
  zero.policy = TRUE
)

#run morans i
minneapolis_residual_moran <- moran.test(
  minneapolis_hex_analysis_residuals$residuals,
  minneapolis_weights,
  zero.policy = TRUE
)

minneapolis_residual_moran

#quick map
qtm(minneapolis_hex_analysis_residuals, fill = "residuals")

# --------------
#    New York
# --------------

#add model residuals to regression data
nyc_residuals <- nyc_regression_data%>%
  mutate(
    residuals = residuals(nyc_regression_model)
  )%>%
  select(hex_id, residuals)

#join residuals back to spatial hex dataset
nyc_hex_analysis_residuals <- nyc_hex_fp%>%
  left_join(
    nyc_residuals,
    by = "hex_id"
  )%>%
  filter(!is.na(residuals))

#check rows match
nrow(nyc_hex_analysis_residuals)
length(residuals(nyc_regression_model))

#create neighbor structure
nyc_neighbors <- nyc_hex_analysis_residuals%>%
  st_geometry()%>%
  poly2nb()

#create weights matrix
nyc_weights <- nb2listw(
  nyc_neighbors,
  style = "W",
  zero.policy = TRUE
)

#run morans i
nyc_residual_moran <- moran.test(
  nyc_hex_analysis_residuals$residuals,
  nyc_weights,
  zero.policy = TRUE
)

nyc_residual_moran

#quick map
qtm(nyc_hex_analysis_residuals, fill = "residuals")
