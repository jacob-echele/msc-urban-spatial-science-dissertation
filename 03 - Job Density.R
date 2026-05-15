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
missouri_jobs <- read_csv("LODES WAC/mo_wac_S000_JT00_2023.csv/mo_wac_S000_JT00_2023.csv",
  col_types = cols(
    w_geocode = col_character()
  )
)%>%
  clean_names()%>%
  select(w_geocode, total_jobs = c000)

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
missouri_census_blocks_jobs <- missouri_census_blocks%>%
  left_join(missouri_jobs, by = c("geoid" = "w_geocode"))

#replace missing job values with zero
missouri_census_blocks_jobs <- missouri_census_blocks_jobs %>%
  mutate(total_jobs = replace_na(total_jobs, 0))

# --------------------
#    prepare tracts
# --------------------

#keep only census blocks that intersect the hex grid
stl_census_blocks_jobs <- st_filter(
  missouri_census_blocks_jobs,
  stl_hex_1km
)

#calculate census block area
stl_census_blocks_jobs <- stl_census_blocks_jobs%>%
  mutate(
    block_area = as.numeric(st_area(.))
  )

# ----------------------------------
#    area-weighted job allocation
# ----------------------------------

#intersect census blocks with hex cells
stl_block_hex_intersections <- st_intersection(
  stl_census_blocks_jobs,
  stl_hex_1km
)

#calculate what proportion of block polygon and it's job count falls within hex boundaries 
stl_block_hex_intersections <- stl_block_hex_intersections%>%
  mutate(
    intersect_area = as.numeric(st_area(.)),
    area_proportion = intersect_area / block_area,
    allocated_jobs = total_jobs * area_proportion
  )

# ---------------------------
#    calculate job metrics
# ---------------------------

#sum allocated jobs by hex cell
stl_jobs_by_hex <- stl_block_hex_intersections%>%
  st_drop_geometry()%>%
  group_by(hex_id)%>%
  summarise(
    total_jobs = sum(allocated_jobs, na.rm = TRUE)
  )

#join job totals back to hex grid and calculate job density
stl_hex_job_density <- stl_hex_1km%>%
  left_join(stl_jobs_by_hex, by = "hex_id")%>%
  mutate(
    total_jobs = replace_na(total_jobs, 0),
    area_acres = as.numeric(st_area(.)) / 43560, #43,560 sqft in an acre
    job_density_acre = total_jobs / area_acres
  )

#quick map
qtm(stl_hex_job_density, fill = "job_density_acre")

# --------------------
#    actual mapping
# --------------------

#job density; QUANTILE, n=7
stl_hex_job_density_quantile <- tm_shape(stl_hex_job_density) +
  tm_polygons(
    "job_density_acre",
    style = "quantile",
    n = 7,
    title = "Jobs per acre"
  ) +
  tm_layout(frame = FALSE)

stl_hex_job_density_quantile

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
stl_hex_job_density_with_roads <- stl_hex_job_density_quantile +
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
stl_hex_job_density_with_roads +
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scalebar(breaks = c(0,2,4), text.size = 1, position = c("left", "top"))

stl_hex_job_density_with_roads

# -----------------------------------------------------
#    saving outputs for future use/easier processing
# -----------------------------------------------------

#save processed data
saveRDS(stl_hex_job_density, "Processed Data/stl_hex_job_density.rds")

##################
#   MINNEAPOLIS
##################

#read in spatial data; pre-made hex map of study area
hennepin_county_hex_1km <- readRDS("Processed Data/hennepin_county_hex_1km.rds")

#read in LODES data; select geocode and c000 which is total jobs per census tract
minnesota_jobs <- read_csv("LODES WAC/mn_wac_S000_JT00_2023.csv/mn_wac_S000_JT00_2023.csv",
  col_types = cols(
    w_geocode = col_character()
  )
)%>%
  clean_names()%>%
  select(w_geocode, total_jobs = c000)

#read in spatial data for census tracts
minnesota_census_tracts <- st_read("Shapefiles/Minneapolis/minnesota-census-tracts/tl_2025_27_tract.shp")%>%
  clean_names()

#make sure census tracts use same CRS as hex grid
minnesota_census_tracts <- st_transform(minnesota_census_tracts, st_crs(hennepin_county_hex_1km))

#read in census blocks spatial data b/c LODES data is at block level NOT tract level
minnesota_census_blocks <- st_read("Shapefiles/Minneapolis/minnesota-census-blocks/tl_2022_27_tabblock20.shp")%>%
  clean_names()%>%
  st_transform(st_crs(hennepin_county_hex_1km))%>%
  mutate(geoid = as.character(geoid20)) #rename geoid20 to match geoid

#join LODES data to census block geometries
minnesota_census_blocks_jobs <- minnesota_census_blocks%>%
  left_join(minnesota_jobs, by = c("geoid" = "w_geocode"))

#replace missing job values with zero
minnesota_census_blocks_jobs <- minnesota_census_blocks_jobs %>%
  mutate(total_jobs = replace_na(total_jobs, 0))

# --------------------
#    prepare tracts
# --------------------

#keep only census blocks that intersect the hex grid
hennepin_county_census_blocks_jobs <- st_filter(
  minnesota_census_blocks_jobs,
  hennepin_county_hex_1km
)

#calculate census block area
hennepin_county_census_blocks_jobs <- hennepin_county_census_blocks_jobs%>%
  mutate(
    block_area = as.numeric(st_area(.))
  )

# ----------------------------------
#    area-weighted job allocation
# ----------------------------------

#intersect census blocks with hex cells
hennepin_county_block_hex_intersections <- st_intersection(
  hennepin_county_census_blocks_jobs,
  hennepin_county_hex_1km
)

#calculate what proportion of block polygon and it's job count falls within hex boundaries 
hennepin_county_block_hex_intersections <- hennepin_county_block_hex_intersections%>%
  mutate(
    intersect_area = as.numeric(st_area(.)),
    area_proportion = intersect_area / block_area,
    allocated_jobs = total_jobs * area_proportion
  )

# ---------------------------
#    calculate job metrics
# ---------------------------

#sum allocated jobs by hex cell
hennepin_county_jobs_by_hex <- hennepin_county_block_hex_intersections%>%
  st_drop_geometry()%>%
  group_by(hex_id)%>%
  summarise(
    total_jobs = sum(allocated_jobs, na.rm = TRUE)
  )

#join job totals back to hex grid and calculate job density
hennepin_county_hex_job_density <- hennepin_county_hex_1km%>%
  left_join(hennepin_county_jobs_by_hex, by = "hex_id")%>%
  mutate(
    total_jobs = replace_na(total_jobs, 0),
    area_acres = as.numeric(st_area(.)) / 43560, #43,560 sqft in an acre
    job_density_acre = total_jobs / area_acres
  )

#quick map
qtm(hennepin_county_hex_job_density, fill = "job_density_acre")

# --------------------
#    actual mapping
# --------------------

#job density; QUANTILE, n=7
hennepin_county_hex_job_density_quantile <- tm_shape(hennepin_county_hex_job_density) +
  tm_polygons(
    "job_density_acre",
    style = "quantile",
    n = 7,
    title = "Jobs per acre"
  ) +
  tm_layout(frame = FALSE)

hennepin_county_hex_job_density_quantile

# -------------------------------------------------------
#    ADDING ROADS TO MAP FOR CONTEXT/PERSONAL INTEREST
# -------------------------------------------------------

#read in road spatial data
hennepin_county_roads <- st_read("Shapefiles/Minneapolis/hennepin-county-street-centerlines/Hennepin_County_Street_Centerlines.shp")%>%
  st_simplify()%>%
  clean_names()%>%
  filter(route_sys %in% c("01", "02", "03", "04")) #reduce visual clutter; 01 - interstate, 02 - US highway, 03 - MN highway, 04 - county road

#assign different road types to own variables for hierarchical mapping
mn_interstates <- hennepin_county_roads%>%
  filter(route_sys == "01")
mn_us_highways <- hennepin_county_roads%>%
  filter(route_sys == "02")
mn_highways <- hennepin_county_roads%>%
  filter(route_sys == "03")
mn_county_highways <- hennepin_county_roads%>%
  filter(route_sys == "04")

#actual mapping
hennepin_county_hex_job_density_with_roads <- hennepin_county_hex_job_density_quantile +
  tm_shape(mn_interstates) +
  tm_lines(col = "white", lwd = 2.75) +
  tm_shape(mn_us_highways) +
  tm_lines(col = "white", lwd = 2) +
  tm_shape(mn_highways) +
  tm_lines(col = "white", lwd = 1.5) +
  tm_shape(mn_county_highways) +
  tm_lines(col = "white", lwd = 1)

#layout/legend
hennepin_county_hex_job_density_with_roads <- hennepin_county_hex_job_density_with_roads +
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scalebar(breaks = c(0,2,4), text.size = 1, position = c("right", "top"))

hennepin_county_hex_job_density_with_roads

# -----------------------------------------------------
#    saving outputs for future use/easier processing
# -----------------------------------------------------

#save processed data
saveRDS(hennepin_county_hex_job_density, "Processed Data/hennepin_county_hex_job_density.rds")

####################
#   NEW YORK CITY
####################

