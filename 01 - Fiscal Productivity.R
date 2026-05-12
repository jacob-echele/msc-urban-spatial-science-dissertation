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

###############
#  ST. LOUIS   
###############

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

stl_county_parcel_shapefile <- stl_county_parcel_shapefile%>%
  select(objectid, totassmt, landuse2, zoning, propclass, resqft, munycode, prop_add, nbhd, geometry)

#rename county parcel columns to match city parcel column names
stl_county_parcel_shapefile <- stl_county_parcel_shapefile%>%
  rename(
    parcel_id = objectid,
    asd_total = totassmt,
    asr_land_use = landuse2,
    sqft = resqft
  )

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

#city
stl_city_parcel_shapefile <- stl_city_parcel_shapefile%>%
  mutate(
    parcel_area_sqft = as.numeric(st_area(geometry)),
    parcel_area_acres = parcel_area_sqft / 43560, #43,560 sqft per acre
    value_per_acre = asd_total / parcel_area_acres,
    log_value_per_acre = log1p(value_per_acre)
  )

#county
stl_county_parcel_shapefile <- stl_county_parcel_shapefile%>%
  mutate(
    parcel_area_sqft = as.numeric(st_area(geometry)),
    parcel_area_acres = parcel_area_sqft / 43560, #43,560 sqft per acre
    value_per_acre = asd_total / parcel_area_acres,
    log_value_per_acre = log1p(value_per_acre)
  )

# ----------------------------------------------------------------------------------
#    adding city/county indicator for future debugging/use in combined data frame
# ----------------------------------------------------------------------------------

#city
stl_city_parcel_shapefile <- stl_city_parcel_shapefile%>%
  mutate(source = "city")

#county
stl_county_parcel_shapefile <- stl_county_parcel_shapefile%>%
  mutate(source = "county")

# -------------------------
#    combining data sets
# -------------------------

#ensuring essential columns are same data type for combining data
#parcel_id
stl_city_parcel_shapefile <- stl_city_parcel_shapefile%>%
  mutate(parcel_id = as.character(parcel_id))
stl_county_parcel_shapefile <- stl_county_parcel_shapefile%>%
  mutate(parcel_id = as.character(parcel_id))

#asr_land_use
stl_city_parcel_shapefile <- stl_city_parcel_shapefile%>%
  mutate(asr_land_use = as.character(asr_land_use))
stl_county_parcel_shapefile <- stl_county_parcel_shapefile%>%
  mutate(asr_land_use = as.character(asr_land_use))

#actual combination
stl_parcels_combined <- bind_rows(
  stl_city_parcel_shapefile,
  stl_county_parcel_shapefile
)

# ----------------------------------------
#    calculating combined value metrics
# ----------------------------------------

stl_parcels_combined <- stl_parcels_combined%>%
  mutate(
    parcel_area_sqft = as.numeric(st_area(geometry)),
    parcel_area_acres = parcel_area_sqft / 43560,
    value_per_acre = asd_total / parcel_area_acres,
    log_value_per_acre = log1p(value_per_acre)
  )%>%
  filter( #filter out parcels with 0 sqft and NA values
    parcel_area_sqft > 0,
    !is.na(asd_total),
    !is.na(value_per_acre),
    is.finite(value_per_acre)
  )

#more filtering out unrealistic data
stl_parcels_combined <- stl_parcels_combined%>%
  filter(
    parcel_area_acres > 0,
    is.finite(value_per_acre),
    value_per_acre >= 0
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
stl_parcel_centroids <- st_centroid(stl_parcels_combined)

#join centroids to hexes; any parcel centroid that falls within hex is counted
stl_parcels_hex <- st_join(
  stl_parcel_centroids,
  stl_hex_1km,
  join = st_within
)

# ----------------------------------------------
#    calculating hex aggregated value metrics
# ----------------------------------------------

stl_hex_fp <- stl_parcels_hex%>% #every parcel belongs inside a hex
  st_drop_geometry()%>%
  filter(!is.na(hex_id))%>% #get rid of parcels with centroids on top of hex boundaries
  group_by(hex_id)%>% #calculate for each hex, not each parcel
  summarise(
    parcel_count = n(), #number of parcels in each hex
    total_assessed_value = sum(asd_total, na.rm = TRUE), #assessed value of all parcels within hex
    total_parcel_area_acres = sum(parcel_area_acres, na.rm = TRUE), #total area of all parcels within hex
    fiscal_productivity = total_assessed_value / total_parcel_area_acres, #fiscal productivity of all parcels within hex
    median_value_per_acre = median(value_per_acre, na.rm = TRUE), #median parcel prodcutivity within hex
    .groups = "drop"
  )%>%
  right_join(stl_hex_1km, by = "hex_id")%>% #join back geometry
  st_as_sf() #convert back to spatial object for mapping

#adding log values
stl_hex_fp <- stl_hex_fp%>%
  mutate(
    log_fiscal_productivity = log1p(fiscal_productivity),
    log_median_value_per_acre = log1p(median_value_per_acre)
  )

# --------------------
#    actual mapping
# --------------------

### MULTIPLE MAP TYPES FOR COMPARISON, NOT SURE WHAT WILL BE FINAL AS OF 12/5/2026 ###

#raw fiscal productivity; QUANTILE, n=6
stl_raw_fp <- tm_shape(stl_hex_fp) +
  tm_polygons(
    "log_fiscal_productivity",
    style = "quantile",
    n = 6,
    title = "Log assessed value per acre"
  ) +
  tm_layout(frame = FALSE)

stl_raw_fp
#raw fiscale productivity; QUANTILE, n=7
stl_raw_fp_quantile <- tm_shape(stl_hex_fp) +
  tm_polygons(
    "fiscal_productivity",
    style = "quantile",
    n = 7,
    title = "Assessed value per acre"
  ) +
  tm_layout(frame = FALSE)

stl_raw_fp_quantile

#log fiscal productivity; QUANTILE, n=6
stl_log_fp <- tm_shape(stl_hex_fp) +
  tm_polygons(
    "log_fiscal_productivity",
    style = "quantile",
    n = 6,
    title = "Log assessed value per acre"
  ) +
  tm_layout(frame = FALSE)

stl_log_fp

#log fiscal productivity; QUANTILE, n=8
stl_log_fp_8 <- tm_shape(stl_hex_fp) +
  tm_polygons(
    "log_fiscal_productivity",
    style = "quantile",
    n = 8,
    title = "Log assessed value per acre"
  ) +
  tm_layout(frame = FALSE)

stl_log_fp_8

#log fiscal prodcutivity; JENKS, n=7
stl_log_fp_jenks <- tm_shape(stl_hex_fp) +
  tm_polygons(
    "log_fiscal_productivity",
    style = "jenks",
    n = 7,
    title = "Log assessed value per acre"
  ) +
  tm_layout(frame = FALSE)

stl_log_fp_jenks

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
mo_highways_clip <- st_intersection(mo_highways, stl_study_area)
mo_interstates_clip <- st_intersection(mo_interstates, stl_study_area)
mo_highway_67_clip <- st_intersection(mo_highway_67, stl_study_area)
mo_arterials_clip <- st_intersection(mo_arterials, stl_study_area)
mo_missing_clip <- st_intersection(mo_missing, stl_study_area)

#actual mapping
stl_raw_fp_quantile_with_roads <- stl_raw_fp_quantile +
  tm_shape(mo_highways_clip) +
  tm_lines() +
  tm_shape(mo_interstates_clip) +
  tm_lines(col = "white", lwd = 2.75) +
  tm_shape(mo_highway_67_clip) +
  tm_lines(col = "white", lwd = 1.75) +
  tm_shape(mo_arterials_clip) +
  tm_lines(col = "white", lwd = 1.5) +
  tm_shape(mo_missing_clip) +
  tm_lines(col = "white", lwd = 1)

#layout/legend
stl_raw_fp_quantile_with_roads +
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scalebar(breaks = c(0,2,4), text.size = 1, position = c("left", "top"))

stl_raw_fp_quantile_with_roads

# ------------------------
#    summary statistics
# ------------------------

#min, 1Q, Median, Mean, 3Q, max, NAs
summary(stl_hex_fp$fiscal_productivity)

#percentiles
quantile(
  stl_hex_fp$fiscal_productivity,
  probs = c(0, .1, .25, .5, .75, .9, .95, .99, 1),
  na.rm = TRUE
)

# -----------------------------------------------------
#    saving outputs for future use/easier processing
# -----------------------------------------------------

#main stl hex map with aggregated values and spatial data
st_write(
  stl_hex_fp,
  "Processed Data/stl_hex_fiscal_productivity.geojson",
  delete_dsn = TRUE
)

#R-native version
saveRDS(
  stl_hex_fp,
  "Processed Data/stl_hex_fiscal_productivity.rds"
)

#combined parcels; R-native version
saveRDS(
  stl_parcels_combined,
  "Processed Data/stl_parcels_combined.rds"
)

#initial hex grid; R-native version
saveRDS(
  stl_hex_1km, 
  "Processed Data/stl_hex_1km.rds"
)

##################
#   MINNEAPOLIS
##################

#read in spatial data
hennepin_county_borders <- st_read("Shapefiles/Minneapolis/hennepin-county-boundaries/Hennepin_County_Boundary.shp")%>%
  st_simplify()%>%
  clean_names()

#not simplifying geometries to calculate area later on
hennepin_county_parcels <- st_read("Shapefiles/Minneapolis/hennepin-county-parcels/County_Parcels.shp")%>%
  clean_names()

#select only needed columns and remove individual information
hennepin_county_parcels <- hennepin_county_parcels%>%
  select(objectid,pid,
    parcel_are, total_mv1, land_mv1, bldg_mv1, pr_typ_nm1, build_yr, sale_price, sale_date, geometry)%>%
  rename( #rename key columns to stay consistent with St. Louis names
    parcel_id = pid,
    asd_total = total_mv1,
    asr_land_use = pr_typ_nm1
  )

# -------------------------------
#    calculating value metrics
# -------------------------------

hennepin_county_parcels <- hennepin_county_parcels%>%
  mutate(
    parcel_area_sqft = as.numeric(st_area(geometry)),
    parcel_area_acres = parcel_area_sqft / 43560, #43,560 sqft per acre
    value_per_acre = asd_total / parcel_area_acres,
    log_value_per_acre = log1p(value_per_acre)
  )%>%
  filter( #filter out parcels with 0 sqft and NA values
  parcel_area_sqft > 0,
  !is.na(asd_total),
  !is.na(value_per_acre),
  is.finite(value_per_acre)
)

#more filtering out unrealistic data
hennepin_county_parcels <- hennepin_county_parcels%>%
  filter(
    parcel_area_acres > 0,
    is.finite(value_per_acre),
    value_per_acre >= 0
  )

# --------------------
#    making hex map
# --------------------

hennepin_county_hex_1km <- st_make_grid(
  hennepin_county_borders,
  cellsize = 3280, #3280 ft in one kilometer
  square = FALSE
)%>%
  st_sf()%>%
  mutate(hex_id = row_number())

#link hex map to study area
hennepin_county_hex_1km <- st_intersection(hennepin_county_hex_1km, hennepin_county_borders)%>%
  select(hex_id, geometry)

#assign centroids to parcels
hennepin_county_parcel_centroids <- st_centroid(hennepin_county_parcels)

#join centroids to hexes; any parcel centroid that falls within hex is counted
hennepin_county_parcels_hex <- st_join(
  hennepin_county_parcel_centroids,
  hennepin_county_hex_1km,
  join = st_within
)

# ----------------------------------------------
#    calculating hex aggregated value metrics
# ----------------------------------------------

hennepin_county_hex_fp <- hennepin_county_parcels_hex%>% #every parcel belongs inside a hex
  st_drop_geometry()%>%
  filter(!is.na(hex_id))%>% #get rid of parcels with centroids on top of hex boundaries
  group_by(hex_id)%>% #calculate for each hex, not each parcel
  summarise(
    parcel_count = n(), #number of parcels in each hex
    total_assessed_value = sum(asd_total, na.rm = TRUE), #assessed value of all parcels within hex
    total_parcel_area_acres = sum(parcel_area_acres, na.rm = TRUE), #total area of all parcels within hex
    fiscal_productivity = total_assessed_value / total_parcel_area_acres, #fiscal productivity of all parcels within hex
    median_value_per_acre = median(value_per_acre, na.rm = TRUE), #median parcel prodcutivity within hex
    .groups = "drop"
  )%>%
  right_join(hennepin_county_hex_1km, by = "hex_id")%>% #join back geometry
  st_as_sf() #convert back to spatial object for mapping

#adding log values
hennepin_county_hex_fp <- hennepin_county_hex_fp%>%
  mutate(
    log_fiscal_productivity = log1p(fiscal_productivity),
    log_median_value_per_acre = log1p(median_value_per_acre)
  )

# --------------------
#    actual mapping
# --------------------

### MULTIPLE MAP TYPES FOR COMPARISON, NOT SURE WHAT WILL BE FINAL AS OF 12/5/2026 ###

#raw fiscal productivity; QUANTILE, n=6
hennepin_county_raw_fp <- tm_shape(hennepin_county_hex_fp) +
  tm_polygons(
    "log_fiscal_productivity",
    style = "quantile",
    n = 6,
    title = "Log assessed value per acre"
  ) +
  tm_layout(frame = FALSE)

hennepin_county_raw_fp

#raw fiscale productivity; QUANTILE, n=7
hennepin_county_raw_fp_quantile <- tm_shape(hennepin_county_hex_fp) +
  tm_polygons(
    "fiscal_productivity",
    style = "quantile",
    n = 7,
    title = "Assessed value per acre"
  ) +
  tm_layout(frame = FALSE)

hennepin_county_raw_fp_quantile

#log fiscal productivity; QUANTILE, n=6
hennepin_county_log_fp <- tm_shape(hennepin_county_hex_fp) +
  tm_polygons(
    "log_fiscal_productivity",
    style = "quantile",
    n = 6,
    title = "Log assessed value per acre"
  ) +
  tm_layout(frame = FALSE)

hennepin_county_log_fp

#log fiscal productivity; QUANTILE, n=8
hennepin_county_log_fp_8 <- tm_shape(hennepin_county_hex_fp) +
  tm_polygons(
    "log_fiscal_productivity",
    style = "quantile",
    n = 8,
    title = "Log assessed value per acre"
  ) +
  tm_layout(frame = FALSE)

hennepin_county_log_fp_8

#log fiscal prodcutivity; JENKS, n=7
hennepin_county_log_fp_jenks <- tm_shape(hennepin_county_hex_fp) +
  tm_polygons(
    "log_fiscal_productivity",
    style = "jenks",
    n = 7,
    title = "Log assessed value per acre"
  ) +
  tm_layout(frame = FALSE)

hennepin_county_log_fp_jenks

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
hennepin_county_raw_fp_quantile_with_roads <- hennepin_county_raw_fp_quantile +
  tm_shape(mn_interstates) +
  tm_lines(col = "white", lwd = 2.75) +
  tm_shape(mn_us_highways) +
  tm_lines(col = "white", lwd = 2) +
  tm_shape(mn_highways) +
  tm_lines(col = "white", lwd = 1.5) +
  tm_shape(mn_county_highways) +
  tm_lines(col = "white", lwd = 1)

#layout/legend
hennepin_county_raw_fp_quantile_with_roads <- hennepin_county_raw_fp_quantile_with_roads +
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scalebar(breaks = c(0,2,4), text.size = 1, position = c("right", "top"))

hennepin_county_raw_fp_quantile_with_roads

# ------------------------
#    summary statistics
# ------------------------

#min, 1Q, Median, Mean, 3Q, max, NAs
summary(hennepin_county_hex_fp$fiscal_productivity)

#percentiles
quantile(
  hennepin_county_hex_fp$fiscal_productivity,
  probs = c(0, .1, .25, .5, .75, .9, .95, .99, 1),
  na.rm = TRUE
)

# -----------------------------------------------------
#    saving outputs for future use/easier processing
# -----------------------------------------------------

#main stl hex map with aggregated values and spatial data
st_write(
  hennepin_county_hex_fp,
  "Processed Data/hennepin_county_hex_fiscal_productivity.geojson",
  delete_dsn = TRUE
)

#R-native version
saveRDS(
  hennepin_county_hex_fp,
  "Processed Data/hennepin_county_hex_fiscal_productivity.rds"
)

#combined parcels; R-native version
saveRDS(
  hennepin_county_parcels,
  "Processed Data/hennepin_county_parcels.rds"
)

#initial hex grid; R-native version
saveRDS(
  hennepin_county_hex_1km, 
  "Processed Data/hennepin_county_hex_1km.rds"
)

###############
#   NEW YORK
###############

#read in spatial data


