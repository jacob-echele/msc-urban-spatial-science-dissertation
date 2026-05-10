library(here)
library(sf)
library(tmap)
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)

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
nyc_boundaries <- st_read("Shapefiles/New York City/nyc-borough-boundaries-water-included/nybbwi_26a/nybbwi.shp")
qtm(nyc_boundaries)
