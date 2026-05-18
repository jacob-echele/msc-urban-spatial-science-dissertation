#increase memory available in Java before loading in r5r; following documentation available at: https://cran.r-project.org/web/packages/r5r/vignettes/r5r.html
library(rJava)
library(rlang)

#load in JDK before loading r5r
library(rJavaEnv)

#install Java
#rJavaEnv::java_quick_install(version = 21)

#check version of Java currently installed
options(java.parameters = "-Xmx6G")

#load in rest of packags
library(here)
library(sf)
library(tmap)
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(r5r)

###################################
#   TRANSPORTATION ACCESSIBILITY
###################################

#set working directory
setwd("C:/Users/User/OneDrive/Desktop/School/7- Graduate School/Term 2/Dissertation/Data/")

################
#   ST. LOUIS
################

#read in spatial data; pre-made hex map of combined study area
stl_hex_1km <- readRDS("Processed Data/stl_hex_1km.rds")

#read in job density data; pre-made hex map of combined study area
stl_hex_job_density <- readRDS("Processed Data/stl_hex_job_density.rds")

# ------------------------------------------
#    assign origin and destination points
# ------------------------------------------

#make origin and destination from hex centroids
stl_heX_points <- st_centroid(stl_hex_job_density)

#change CRS to WGS84 as per r5r requirements
stl_hex_points <- st_transform(stl_heX_points, crs = 4326)

#actually creation of origin and destination in new df
stl_points <- stl_hex_points%>%
  mutate(lon = st_coordinates(.)[,1], #st_coordinates turns hex centroid coordinates into a matrix; st_coords(.) is the same as st_coords(stl_hex_points)
         lat = st_coordinates(.)[,2], #[,1] and [,2] takes all rows and first column [,1], and second column [,2]
         id = as.character (hex_id)
         )%>%
  st_drop_geometry()%>%
  select(id, lon, lat, total_jobs)

# -------------------------------------------
#    build network using GTFS and OSM data
# -------------------------------------------

#set path to St. Louis GTFS and OSM data
stl_network_path <- "Transport Accessibility/St. Louis"

#actual building of network
stl_r5r_network <- build_network(data_path = stl_network_path)

# ----------------------------------
#    set accessibility parameters
# ----------------------------------

#departure date and time
departure_datetime <- as.POSIXct("2026-05-20 08:00:00", format = "%Y-%m-%d %H:%M:%S") #Wednesday morning peak; simulate normal commuting conditions

#set modes; walk and transit
mode <- c("WALK", "TRANSIT")

#accessibility cutoffs (minutes; represent commute times)
cutoffs <- c(30,45,60)

# -----------------------------------
#    calculating job accessibility
# -----------------------------------

#combining jobs dataset and preset values
stl_transit_accessibility <- accessibility(
  stl_r5r_network,
  origins = stl_points,
  destinations = stl_points,
  opportunities_colnames = "total_jobs",
  mode = mode,
  departure_datetime = departure_datetime,
  decay_function = "step",
  cutoffs = cutoffs
)

#reshape accessibility results from long to wide format; create one row per hex cell
stl_transit_accessibility_wide <- stl_transit_accessibility%>%
  select(id, cutoff, accessibility)%>%
  pivot_wider(
    names_from = cutoff,
    values_from = accessibility,
    names_prefix = "transit_access_jobs_"
  )

# -----------------------------------------------
#    re-join transit accessibility to hex grid
# -----------------------------------------------

stl_hex_transit_accessibility <- stl_hex_job_density%>%
  mutate(hex_id = as.character(hex_id))%>%
  left_join(
    stl_transit_accessibility_wide,
    by = c("hex_id" = "id")
  )

# --------------------
#    actual mapping
# --------------------

#transit access to jobs; QUANTILE, n = 7
stl_hex_transit_accessibility_quantile <- tm_shape(stl_hex_transit_accessibility) +
  tm_polygons(
    "transit_access_jobs_45",
    style = "quantile",
    n = 7,
    title = "Jobs accessible by transit within 45 minutes"
  ) +
  tm_layout(frame = FALSE)

stl_hex_transit_accessibility_quantile

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
stl_hex_transit_accessibility_with_roads <- stl_hex_transit_accessibility_quantile +
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
stl_hex_transit_accessibility_with_roads +
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scalebar(breaks = c(0,2,4), text.size = 1, position = c("left", "top"))

stl_hex_transit_accessibility_with_roads

# -----------------------------------------------------
#    saving outputs for future use/easier processing
# -----------------------------------------------------

#save processed data
saveRDS(stl_hex_transit_accessibility, "Processed Data/stl_hex_transit_accessibility.rds")

##################
#   MINNEAPOLIS
##################

