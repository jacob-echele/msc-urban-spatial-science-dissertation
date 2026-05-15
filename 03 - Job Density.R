library(here)
library(sf)
library(tmap)
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)

###############################
#   JOB DENSITY CALCULATIONS   
###############################

#set working directory
setwd("C:/Users/User/OneDrive/Desktop/School/7- Graduate School/Term 2/Dissertation/Data/")

################
#   ST. LOUIS
################

#read in spatial data; pre-made hex map of combined study area
stl_hex_1km <- readRDS("Processed Data/stl_hex_1km.rds")

#read in LODES data; select geocode and c000 which is total jobs per census tract
missouri_jobs <- read_csv("LODES WAC/mo_wac_S000_JT00_2023.csv/mo_wac_S000_JT00_2023.csv")%>%
  clean_names()%>%
  select(w_geocode, total_jobs = c000)%>%
  mutate(w_geocode = as.character(w_geocode))

#read in spatial data for census tracts
missouri_census_tracts <- st_read("Shapefiles/St. Louis/missouri-census-tract-shapefiles/tl_2025_29_tract.shp")%>%
  clean_names()

#make sure census tracts use same CRS as hex grid
missouri_census_tracts <- st_transform(missouri_census_tracts, st_crs(stl_hex_1km))

#read in census blocks spatial data b/c LODES data is at block level NOT tract level
missouri_census_blocks <- st_read("Shapefiles/St. Louis/missouri-census-blocks-shapefile/tl_2022_29_tabblock20.shp")%>%
  clean_names()%>%
  st_transform(st_crs(stl_hex_1km))%>%
  mutate(geoid = as.character(geoid20)) #rename geoid20 to match geoid

#join LODES data to census block geometries
missouri_cenus_blocks_jobs <- missouri_census_blocks%>%
  left_join(missouri_jobs, by = c("geoid" = "w_geocode"))
