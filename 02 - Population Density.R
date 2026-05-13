library(here)
library(sf)
library(tmap)
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)

######################################
#   POPULATION DENSITY CALCULATIONS   
######################################

#set working directory
setwd("C:/Users/User/OneDrive/Desktop/School/7- Graduate School/Term 2/Dissertation/Data/")

###############
#  ST. LOUIS   
###############

#read in spatial data; pre-made hex map of combined study area
stl_hex_1km <- readRDS("Processed Data/stl_hex_1km.rds")

#read in spatial data for census tracts
missouri_census_tracts <- st_read("Shapefiles/St. Louis/missouri-census-tract-shapefiles/tl_2025_29_tract.shp")%>%
  clean_names()

#make sure census tracts use same CRS as hex grid
missouri_census_tracts <- st_transform(missouri_census_tracts, st_crs(stl_hex_1km))

#read in quantitative data for census tracts
missouri_pop_by_ct <- read_csv("Census Population/st-louis-city-and-county-population.csv")%>%
  clean_names()

#keep only total population row and reshape
missouri_pop_total <- missouri_pop_by_ct%>%
  filter(label_grouping == "Total:")%>%
  pivot_longer(
    cols = -label_grouping,
    names_to = "tract_name",
    values_to = "total_population"
  )%>%
  mutate(
    tract_number = tract_name%>%
      str_extract("\\d+_\\d+|\\d+")%>%
      str_replace("_", ".")
  )%>%
  select(tract_number, total_population)

# ----------------------
#    prepare tracts
# ----------------------

#keep only census tracts that intersect the hex grid
stl_census_tracts <- st_filter(
  missouri_census_tracts,
  st_union(stl_hex_1km),
  .predicate = st_intersects
)

#make tract number field match population table
stl_census_tracts <- stl_census_tracts%>%
  mutate(
    tract_number = as.character(name),
    tract_area = as.numeric(st_area(.))
  )

#join total population to census tract polygons
stl_census_tracts <- stl_census_tracts%>%
  left_join(
    missouri_pop_total,
    by = "tract_number"
  )

#check join
sum(is.na(stl_census_tracts$total_population))

# -----------------------------------------
#    area-weighted population allocation
# -----------------------------------------

#intersect census tracts with hex cells
stl_tract_hex_intersections <- st_intersection(
  stl_census_tracts,
  stl_hex_1km
)

#calculate each tract fragment's share of its original census tract
stl_tract_hex_intersections <- stl_tract_hex_intersections%>%
  mutate(
    intersect_area = as.numeric(st_area(.)),
    area_proportion = intersect_area / tract_area,
    allocated_population = total_population * area_proportion
  )

# ----------------------------------
#    calculate population metrics
# ----------------------------------

#sum allocated population by hex cell
stl_population_by_hex <- stl_tract_hex_intersections%>%
  st_drop_geometry() %>%
  group_by(hex_id) %>%
  summarise(
    total_population = sum(allocated_population, na.rm = TRUE)
  )

#join population totals back to hex grid and calculate density
stl_hex_population_density <- stl_hex_1km%>%
  left_join(stl_population_by_hex, by = "hex_id")%>%
  mutate(
    total_population = replace_na(total_population, 0),
    area_acres = as.numeric(st_area(.)) / 43560, #43,560 sqft per acre
    pop_density_acre = total_population / area_acres
  )

#quick map
qtm(stl_hex_population_density, fill = "pop_density_acre")

# --------------------
#    actual mapping
# --------------------

### MULTIPLE MAP TYPES FOR COMPARISON, NOT SURE WHAT WILL BE FINAL AS OF 12/5/2026 ###

#raw fiscal productivity; QUANTILE, n=6
stl_hex_population_density_quantile <- tm_shape(stl_hex_population_density) +
  tm_polygons(
    "pop_density_acre",
    style = "quantile",
    n = 7,
    title = "Population Density (per acre)"
  ) +
  tm_layout(frame = FALSE)

stl_hex_population_density_quantile

# -------------------------------------------------------
#    ADDING ROADS TO MAP FOR CONTEXT/PERSONAL INTEREST
# -------------------------------------------------------

#loading in road spatial data
stl_city_and_county_roads <- st_read("Shapefiles/St. Louis/st-louis-city-and-county-streets-shapefile/Streets_1.2K.shp")%>%
  st_simplify()%>%
  clean_names()

#THIS BIT IS FROM CASE STUDY VISUALIZATIONS FILE
#trying to decipher the road hierarchy in the dataset
major_roads <- stl_city_and_county_roads %>%
  filter(roads_net == "Yes")

#no metadata found so best guesses of trying to assign different road types to own variables for hierarchical mapping
mo_interstates <- major_roads%>%
  filter(symbol == 6)
mo_highways <- major_roads%>%
  filter(symbol == 4)
mo_highway_67 <- major_roads%>% #found comparing with google maps
  filter(symbol == 5)
mo_arterials <- major_roads%>%
  filter(symbol == 3)
mo_filters <- major_roads%>%
  filter(symbol == 2)

#using local knowledge to include key arterials that are missing to make map more complete, especially for within city limits
mo_missing <- mo_filters %>%
  filter(strname %in% c("BIG BEND", "NEW BALLWIN", "HANLEY", "HAMPTON", "KINGSHIGHWAY", "GRAND", "CHIPPEWA", "JEFFERSON", "DELMAR", "UNION", "GOODFELLOW", "MCCAUSLAND", "LACLEDE STATION", "MCKNIGHT", "LOCKWOOD", "BERRY", "JAMIESON", "GERMANIA", "WEBER", "BAYLESS", "MORGANFORD", "MACKENZIE", "TUCKER", "KIEFER CREEK"))

#clip roads to study area so roads don't extent past borders
mo_highways_clip <- st_intersection(mo_highways, stl_hex_1km)
mo_interstates_clip <- st_intersection(mo_interstates, stl_hex_1km)
mo_highway_67_clip <- st_intersection(mo_highway_67, stl_hex_1km)
mo_arterials_clip <- st_intersection(mo_arterials, stl_hex_1km)
mo_missing_clip <- st_intersection(mo_missing, stl_hex_1km)

#actual mapping
stl_hex_population_density_with_roads <- stl_hex_population_density_quantile +
  tm_shape(mo_highways_clip) +
  tm_lines(col = "white", lwd = 1) +
  tm_shape(mo_interstates_clip) +
  tm_lines(col = "white", lwd = 2.75) +
  tm_shape(mo_highway_67_clip) +
  tm_lines(col = "white", lwd = 1.75) +
  tm_shape(mo_arterials_clip) +
  tm_lines(col = "white", lwd = 1.5) +
  tm_shape(mo_missing_clip) +
  tm_lines(col = "white", lwd = 1)

#layout/legend
stl_hex_population_density_with_roads +
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scalebar(breaks = c(0,2,4), text.size = 1, position = c("left", "top"))

stl_hex_population_density_with_roads

# -----------------------------------------------------
#    saving outputs for future use/easier processing
# -----------------------------------------------------

#save processed data
saveRDS(stl_hex_population_density, "Processed Data/stl_hex_population_density.rds")

##################
#   MINNEAPOLIS
##################


