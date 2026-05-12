library(here)
library(sf)
library(tmap)
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)
library(readr)

###########################################
#   3.3 - CASE STUDY AREA VISUALIZATIONs
###########################################

#set working directory
setwd("C:/Users/User/OneDrive/Desktop/School/7- Graduate School/Term 2/Dissertation/Data/")

# --------------
#    ST LOUIS
# --------------

#read in data
stl_city_boundaries <- st_read("Shapefiles/St. Louis/st-louis-boundary-shapefile/stl_boundary.shp")%>%
  st_simplify()%>%
  clean_names()

stl_county_boundaries <- st_read("Shapefiles/St. Louis/stl-county-boundary.geojson")%>%
  st_simplify()%>%
  clean_names()
#match CRS with city
stl_county_boundaries <- st_transform(stl_county_boundaries, st_crs(stl_city_boundaries))

stl_city_parks <- st_read("Shapefiles/St. Louis/st-louis-parks-shapefile/parks.shp")%>%
  st_simplify()%>%
  clean_names()

stl_county_parks <- st_read("Shapefiles/St. Louis/stl-county-parks-shapefiles/Park_Boundaries.shp")%>%
  st_simplify()%>%
  clean_names()

#match CRS with city
stl_county_parks <- st_transform(stl_county_parks, st_crs(stl_city_boundaries))

stl_city_water <- st_read("Shapefiles/St. Louis/st-louis-waterway-shapefiles/tl_2022_29510_areawater.shp")%>%
  st_simplify()%>%
  clean_names()

stl_county_water <- st_read("Shapefiles/St. Louis/stl-county-waterway-shapefiles/tl_2024_29189_linearwater.shp")%>%
  st_simplify()%>%
  clean_names()

#match CRS with city
stl_county_water <- st_transform(stl_county_water, st_crs(stl_city_boundaries))

stl_city_and_county_roads <- st_read("Shapefiles/St. Louis/st-louis-city-and-county-streets-shapefile/Streets_1.2K.shp")%>%
  st_simplify()%>%
  clean_names()

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

#match CRS for road variables to city CRS
mo_highways <- st_transform(mo_highways, st_crs(stl_city_boundaries))
mo_interstates <- st_transform(mo_interstates, st_crs(stl_city_boundaries))
mo_highway_67 <- st_transform(mo_highway_67, st_crs(stl_city_boundaries))
mo_arterials <- st_transform(mo_arterials, st_crs(stl_city_boundaries))
mo_missing <- st_transform(mo_missing, st_crs(stl_city_boundaries))

#make combined STL city and STL county boundary for mapping
stl_study_area <- bind_rows(
  stl_city_boundaries,
  stl_county_boundaries
)

#clip roads to study area so roads don't extent past borders
stl_study_area_union <- st_union(stl_study_area)

mo_highways_clip <- st_intersection(mo_highways, stl_study_area_union)
mo_interstates_clip <- st_intersection(mo_interstates, stl_study_area_union)
mo_highway_67_clip <- st_intersection(mo_highway_67, stl_study_area_union)
mo_arterials_clip <- st_intersection(mo_arterials, stl_study_area_union)
mo_missing_clip <- st_intersection(mo_missing, stl_study_area_union)

#St. Louis mapping
st_louis_map <- tm_shape(stl_study_area, unit = "mi") +
  tm_fill(fill = "#feffd8") +
  tm_borders() +
tm_shape(stl_city_parks) +
  tm_polygons(fill = "#8AEB7F") +
tm_shape(stl_county_parks) +
  tm_polygons(fill = "#8AEB7F") +
tm_shape(stl_city_water) +
  tm_polygons(fill = "#8CCCF5") +
tm_shape(stl_county_water) +
  tm_lines(col = "#8CCCF5") +
tm_shape(mo_highways_clip) +
  tm_lines(col = "black", lwd = 1) +
tm_shape(mo_interstates_clip) +
  tm_lines(col = "black", lwd = 2.75) +
tm_shape(mo_highway_67_clip) +
  tm_lines(col = "black", lwd = 1.75) +
tm_shape(mo_arterials_clip) +
  tm_lines(col = "black", lwd = 1.5) +
tm_shape(mo_missing_clip) +
  tm_lines(col = "black", lwd = 1)

#St. Louis layout/legend
st_louis_map <- st_louis_map + 
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scalebar(breaks = c(0,2,4), text.size = 1, position = c("left", "top"))

st_louis_map

# -----------------
#    MINNEAPOLIS
# -----------------

#read in data
hennepin_county_borders <- st_read("Shapefiles/Minneapolis/hennepin-county-boundaries/Hennepin_County_Boundary.shp")%>%
  st_simplify()%>%
  clean_names()
qtm(hennepin_county_borders)

hennepin_county_water <- st_read("Shapefiles/Minneapolis/hennepin-county-water/tl_2022_27053_areawater.shp")%>%
  st_simplify()%>%
  clean_names()

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

hennepin_county_parks <- st_read("Shapefiles/Minneapolis/hennepin-county-parks/Hennepin_County_Parks.shp")%>%
  st_simplify()%>%
  clean_names()

#Minneapolis mapping
minneapolis_map <- tm_shape(hennepin_county_borders, unit = "mi") +
  tm_fill(fill = "#feffd8") +
  tm_borders() +
tm_shape(hennepin_county_parks) +
  tm_polygons(fill = "#8AEB7F") +
tm_shape(hennepin_county_water) +
  tm_polygons(fill = '#8CCCF5') +
tm_shape(mn_interstates) +
  tm_lines(col = "black", lwd = 2.75) +
tm_shape(mn_us_highways) +
  tm_lines(col = "black", lwd = 2) +
tm_shape(mn_highways) +
  tm_lines(col = "black", lwd = 1.5) +
tm_shape(mn_county_highways) +
  tm_lines(col = "black", lwd = 1)

#Minneapolis layout/legend
minneapolis_map <- minneapolis_map + 
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scalebar(breaks = c(0,2,4), text.size = 1, position = c("right", "top"))

minneapolis_map

# -------------- 
#    NEW YORK
# --------------

#read in data
nyc_boundaries <- st_read("Shapefiles/New York City/nyc-boundaries/nybb_26a/nybb.shp")%>%
  st_simplify()%>%
  clean_names()
qtm(nyc_boundaries)

manhattan_water <- st_read("Shapefiles/New York City/manhattan-water/tl_2025_36061_areawater.shp")%>%
  st_simplify()%>%
  clean_names()

brooklyn_water <- st_read("Shapefiles/New York City/brooklyn-water/tl_2025_36047_areawater.shp")%>%
  st_simplify()%>%
  clean_names()

queens_water <- st_read("Shapefiles/New York City/queens-water/tl_2025_36081_areawater.shp")%>%
  st_simplify()%>%
  clean_names()

bronx_water <- st_read("Shapefiles/New York City/bronx-water/tl_2025_36005_areawater.shp")%>%
  st_simplify()%>%
  clean_names()

staten_island_water <- st_read("Shapefiles/New York City/staten-island-water/tl_2025_36085_areawater.shp")%>%
  st_simplify()%>%
  clean_names()

#Street centerlines were a bit more complicated
nyc_street_centerlines_raw <- read_csv("Shapefiles/NEW York City/nyc-stree-centerlines.csv",
  show_col_types = FALSE
) %>%
  clean_names()
nyc_street_centerlines <- nyc_street_centerlines_raw %>%
  st_as_sf(
    wkt = "the_geom",
    crs = 4326
  )

#parks were also a bit more complicated
nyc_parks_raw <- read_csv("Shapefiles/New York City/nyc-parks.csv",
  show_col_types = FALSE
) %>%
  clean_names()

nyc_parks <- nyc_parks_raw %>%
  st_as_sf(
    wkt = "multipolygon",
    crs = 4326
  )

#assign different road types to own variables for hierarchical mapping
nyc_highways <- nyc_street_centerlines%>%
  filter(rw_type == 2)
nyc_bridges <- nyc_street_centerlines%>%
  filter(rw_type == 3)
nyc_tunnels <- nyc_street_centerlines%>%
  filter(rw_type == 4)

#nyc mapping
nyc_map <- tm_shape(nyc_boundaries, unit = "mi") +
  tm_fill(fill = "#feffd8") +
  tm_borders() +
tm_shape(nyc_parks) +
  tm_polygons(fill = "#8AEB7F") +
tm_shape(manhattan_water) +
  tm_polygons(fill = "#8CCCF5") +
tm_shape(brooklyn_water) +
  tm_polygons(fill = "#8CCCF5") +
tm_shape(queens_water) +
  tm_polygons(fill = "#8CCCF5") +
tm_shape(bronx_water) +
  tm_polygons(fill = "#8CCCF5") +
tm_shape(staten_island_water) +
  tm_polygons(fill = "#8CCCF5") +
tm_shape(nyc_highways) +
  tm_lines(col = "black", lwd = 2) +
tm_shape(nyc_bridges) +
  tm_lines(col = "black", lwd = 2) +
tm_shape(nyc_tunnels) +
  tm_lines(col = "black", lwd = 2)

#nyc layout/legend
nyc_map <- nyc_map +
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scalebar(breaks = c(0,2,4), text.size = 1, position = c("left", "top"))

nyc_map
