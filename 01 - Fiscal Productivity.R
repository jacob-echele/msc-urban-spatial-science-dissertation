library(here)
library(sf)
library(tmap)
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)
library(readr)

#######################################
#   FISCAL PRODCUTIVITY CALCULATIONS
#######################################

#set working directory
setwd("C:/Users/User/OneDrive/Desktop/School/7- Graduate School/Term 2/Dissertation/Data/")

# ---------------
#    ST. LOUIS
# ---------------

#read in spatial data; CRS is NAD1983 State Plane Missouri East, used for St. Louis City and St. Louis County; units in feete
stl_city_boundaries <- st_read("Shapefiles/St. Louis/st-louis-boundary-shapefile/stl_boundary.shp")%>%
  st_simplify()%>%
  clean_names()

stl_county_boundaries <- st_read("Shapefiles/St. Louis/stl-county-boundary.geojson")%>%
  st_simplify()%>%
  clean_names()%>%
  st_transform(102696) #set CRS to match city; CRS is NAD1983 State Plane Missouri East; units in feet

#make combined study area
stl_study_area <- bind_rows(
  stl_city_boundaries,
  stl_county_boundaries
)

#combine into one spatial object
stl_study_area <- st_union(stl_study_area)

#read in parcel data; parcel shapefile; not simplifying to get correct area calculations later on
stl_city_parcel_shapefile <- st_read("Shapefiles/St. Louis/st-louis-parcel-shapefile/PARCELS/PARCELS.shp")%>%
  clean_names()%>%
  st_transform(102696)

stl_county_parcel_shapefile <- st_read("Shapefiles/St. Louis/stl-county-parcels-shapefile/Parcels.shp")%>%
  clean_names()%>%
  st_transform(102696)

#select only needed columns and remove individual information
stl_city_parcel_shapefile <- stl_city_parcel_shapefile%>%
  select(parcel_id, asd_total, landarea, asr_land_use, zoning, vacant_lot, nbr_of_bldgs, sqft, siteaddr, ward, cens_tract1, geometry)

stl_county_parcel_shpefile <- stl_county_parcel_shapefile%>%
  select()

# ---------------------------------------------
#    exploratory map of total assessed value
# ---------------------------------------------

tm_shape(stl_city_parcel_shapefile) +
  tm_polygons("asd_total")

#log mutation to better represent data with regards to outliers
stl_city_parcel_shapefile <- stl_city_parcel_shapefile%>%
  mutate(log_asd_total = log1p(asd_total))

#exploratory map of log total assessed value
tm_shape(stl_city_parcel_shapefile) +
  tm_polygons("log_asd_total")

# -------------------------------
#    calculating value metrics
# -------------------------------

stl_city_parcel_shapefile <- stl_city_parcel_shapefile %>%
  mutate(
    parcel_area_sqft = as.numeric(st_area(geometry)),
    parcel_area_acres = parcel_area_sqft / 43560, #43,560 sqft per acre
    value_per_acre = asd_total / parcel_area_acres,
    log_value_per_acre = log1p(value_per_acre)
  )

# --------------------
#    making hex map
# --------------------

stl_hex_1km <- st_make_grid(
  stl_study_area,
  cellsize = 3280, #3280 ft in one kilometer
  square = FALSE
)%>%
  st_sf()%>%
  mutate(hex_id = row_number())

#link hex map to study area
stl_hex_1km <- st_intersection(stl_hex_1km, stl_study_area)%>%
  select(hex_id, geometry)

#assign centroids to parcels
stl_city_parcel_centroids <- st_centroid(stl_city_parcel_shapefile)
