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

# --------------
#    ST LOUIS
# --------------

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
