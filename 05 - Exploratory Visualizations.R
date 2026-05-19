library(here)
library(sf)
library(tmap)
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(RColorBrewer)

#################################
#   EXPLORATORY VISUALIZATIONS
#################################

#set working directory
setwd("C:/Users/User/OneDrive/Desktop/School/7- Graduate School/Term 2/Dissertation/Data/")

#read in font for outputs
windowsFonts(Times = windowsFont("Times New Roman"))

##########################
#   FISCAL PRODUCTIVITY
##########################

#read in processed fiscal productivity outputs
stl_hex_fp <- readRDS("Processed Data/stl_hex_fiscal_productivity.rds")
hennepin_county_hex_fp <- readRDS("Processed Data/hennepin_county_hex_fiscal_productivity.rds")
nyc_hex_fp <- readRDS("Processed Data/nyc_hex_fiscal_productivity.rds")

#St. Louis fiscal productivity map
stl_fp_map <- tm_shape(stl_hex_fp) +
    tm_polygons(
    fill = "fiscal_productivity",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 7,
      values = "Greens"
    ),
    fill.legend = tm_legend(
      title = "Assessed value per acre"
    )
  ) +
  tm_borders(col = "black", lwd = .15) +
  tm_title("St. Louis", size = 1.2, fontfamily = "Times") +
  tm_layout(frame = FALSE, legend.outside = FALSE, fontfamily = "Times"  )

stl_fp_map

#Minneapolis fiscal productivity map
hennepin_county_fp_map <- tm_shape(hennepin_county_hex_fp) +
  tm_polygons(
    fill = "fiscal_productivity",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 7,
      values = "Greens"
    ),
    fill.legend = tm_legend(
      title = "Assessed value per acre"
    )
  ) +
  tm_borders(col = "black", lwd = .15) +
  tm_title("Minneapolis", fontfamily = "Times") +
  tm_layout(frame = FALSE, legend.outside = FALSE, fontfamily = "Times"  )

hennepin_county_fp_map

#New York fiscal productivity map
nyc_fp_map <- tm_shape(nyc_hex_fp) +
  tm_polygons(
    fill = "fiscal_productivity",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 7,
      values = "Greens"
    ),
    fill.legend = tm_legend(
      title = "Assessed value per acre"
    )
  ) +
  tm_borders(col = "black", lwd = .15) +
  tm_title("New York City", fontfamily = "Times") +
  tm_layout(frame = FALSE, legend.outside = FALSE, fontfamily = "Times"  )

nyc_fp_map

#combine into one three-panel figure
fp_three_panel <- tmap_arrange(
  stl_fp_map,
  hennepin_county_fp_map,
  nyc_fp_map,
  ncol = 1 #stack vertically for portrait format in Word Doc
)

fp_three_panel

# -------------------
#    saving output
# -------------------

#save output for inclusion in Word Doc
tmap_save(
  fp_three_panel,
  "Outputs/fp_three_panel.png",
  width = 8,
  height = 10,
  dpi = 300
)

#################################
#   POPULATION AND JOB DENSITY
#################################

#read in processed density outputs
#population density
stl_hex_population_density <- readRDS("Processed Data/stl_hex_population_density.rds")
hennepin_county_hex_population_density <- readRDS("Processed Data/hennepin_county_hex_population_density.rds")
nyc_hex_population_density <- readRDS("Processed Data/nyc_hex_population_density.rds")

#job density
stl_hex_job_density <- readRDS("Processed Data/stl_hex_job_density.rds")
hennepin_county_hex_job_density <- readRDS("Processed Data/hennepin_county_hex_job_density.rds")
nyc_hex_job_density <- readRDS("Processed Data/nyc_hex_job_density.rds")

# ------------------------
#    population density
# ------------------------

#St. Louis population density map
stl_pop_density_map <- tm_shape(stl_hex_population_density, unit = "mi") +
  tm_polygons(
    fill = "pop_density_acre",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 7,
      values = "Blues"
    ),
    fill.legend = tm_legend(
      title = "People per acre"
    )
  ) +
  tm_borders(col = "black", lwd = .15) +
  tm_title("St. Louis", size = 1.2, fontfamily = "Times") +
  tm_layout(frame = FALSE, legend.outside = FALSE, fontfamily = "Times") +
  tm_compass(type = "arrow", position = c("left", "top"), size = 1.5) +
  tm_scalebar(breaks = c(0, 2, 4), text.size = 1, position = c(0.5, 0.06))

stl_pop_density_map

#Minneapolis population density map
hennepin_county_pop_density_map <- tm_shape(hennepin_county_hex_population_density, unit = "mi") +
  tm_polygons(
    fill = "pop_density_acre",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 7,
      values = "Blues"
    ),
    fill.legend = tm_legend(
      title = "People per acre"
    )
  ) +
  tm_borders(col = "black", lwd = .15) +
  tm_title("Minneapolis", size = 1.2, fontfamily = "Times") +
  tm_layout(frame = FALSE, legend.outside = FALSE, fontfamily = "Times") +
  tm_compass(type = "arrow", position = c("left", "top"), size = 1.5) +
  tm_scalebar(breaks = c(0, 2, 4), text.size = 1, position = c(0.5, 0.06))

hennepin_county_pop_density_map

#New York population density map
nyc_pop_density_map <- tm_shape(nyc_hex_population_density, unit = "mi") +
  tm_polygons(
    fill = "pop_density_acre",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 7,
      values = "Blues"
    ),
    fill.legend = tm_legend(
      title = "People per acre"
    )
  ) +
  tm_borders(col = "black", lwd = .15) +
  tm_title("New York City", size = 1.2, fontfamily = "Times") +
  tm_layout(frame = FALSE, legend.outside = FALSE, fontfamily = "Times") +
  tm_compass(type = "arrow", position = c("left", "top"), size = 1.5) +
  tm_scalebar(breaks = c(0, 2, 4), text.size = 1, position = c(0.5, 0.06))

nyc_pop_density_map

# ---------------------------
#    perceived job density
# ---------------------------

#St. Louis perceived job density map
stl_job_density_map <- tm_shape(stl_hex_job_density, unit = "mi") +
  tm_polygons(
    fill = "perceived_job_density",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 7,
      values = "Purples"
    ),
    fill.legend = tm_legend(
      title = "Perceived jobs per acre"
    )
  ) +
  tm_borders(col = "black", lwd = .15) +
  tm_title("St. Louis", size = 1.2, fontfamily = "Times") +
  tm_layout(frame = FALSE, legend.outside = FALSE, fontfamily = "Times") +
  tm_compass(type = "arrow", position = c("left", "top"), size = 1.5) +
  tm_scalebar(breaks = c(0, 2, 4), text.size = 1, position = c(0.5, 0.06))

stl_job_density_map

#Minneapolis job density map
hennepin_county_job_density_map <- tm_shape(hennepin_county_hex_job_density, unit = "mi") +
  tm_polygons(
    fill = "perceived_job_density",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 7,
      values = "Purples"
    ),
    fill.legend = tm_legend(
      title = "Perceived jobs per acre"
    )
  ) +
  tm_borders(col = "black", lwd = .15) +
  tm_title("Minneapolis", size = 1.2, fontfamily = "Times") +
  tm_layout(frame = FALSE, legend.outside = FALSE, fontfamily = "Times") +
  tm_compass(type = "arrow", position = c("left", "top"), size = 1.5) +
  tm_scalebar(breaks = c(0, 2, 4), text.size = 1, position = c(0.5, 0.06))

hennepin_county_job_density_map

#New York job density map
nyc_job_density_map <- tm_shape(nyc_hex_job_density, unit = "mi") +
  tm_polygons(
    fill = "perceived_job_density",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 7,
      values = "Purples"
    ),
    fill.legend = tm_legend(
      title = "Perceived jobs per acre"
    )
  ) +
  tm_borders(col = "black", lwd = .15) +
  tm_title("New York City", size = 1.2, fontfamily = "Times") +
  tm_layout(frame = FALSE, legend.outside = FALSE, fontfamily = "Times") +
  tm_compass(type = "arrow", position = c("left", "top"), size = 1.5) +
  tm_scalebar(breaks = c(0, 2, 4), text.size = 1, position = c(0.5, 0.06))

nyc_job_density_map

#combine population and job density maps
density_maps <- tmap_arrange(
  stl_pop_density_map, stl_job_density_map,
  hennepin_county_pop_density_map, hennepin_county_job_density_map,
  nyc_pop_density_map, nyc_job_density_map,
  ncol = 2
)

density_maps

# --------------------
#    saving outputs
# --------------------

#save density map figure
tmap_save(
  density_maps,
  "Outputs/density_maps.png",
  width = 8,
  height = 10,
  dpi = 300
)

###################################
#   TRANSPORTATION ACCESSIBILITY
###################################

#read in processed transit accessibility outputs
stl_hex_transit_accessibility <- readRDS("Processed Data/stl_hex_transit_accessibility.rds")
hennepin_county_hex_transit_accessibility <- readRDS("Processed Data/hennepin_county_hex_transit_accessibility.rds")
nyc_hex_transit_accessibility <- readRDS("Processed Data/nyc_hex_transit_accessibility.rds")

#St. Louis transit accessibility map
stl_transit_accessibility_map <- tm_shape(stl_hex_transit_accessibility, unit = "mi") +
  tm_polygons(
  fill = "transit_access_jobs_45",
  fill.scale = tm_scale_intervals(
    style = "quantile",
    n = 7,
    values = "Oranges"
  ),
  fill.legend = tm_legend(
    title = "Jobs accessible within 45 minutes"
  )
) +
  tm_borders(col = "black", lwd = .15) +
  tm_title("St. Louis", size = 1.2, fontfamily = "Times") +
  tm_layout(frame = FALSE, legend.outside = FALSE, fontfamily = "Times") +
  tm_compass(type = "arrow", position = c("left", "top"), size = 1.5) +
  tm_scalebar(breaks = c(0, 2, 4), text.size = 1, position = c(0.5, 0.06))

stl_transit_accessibility_map

#Minneapolis transit accessibility map
hennepin_county_transit_accessibility_map <- tm_shape(hennepin_county_hex_transit_accessibility, unit = "mi") +
  tm_polygons(
    fill = "transit_access_jobs_45",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 7,
      values = "Oranges"
    ),
    fill.legend = tm_legend(
      title = "Jobs accessible within 45 minutes"
    )
  ) +
  tm_borders(col = "black", lwd = .15) +
  tm_title("Minneapolis", size = 1.2, fontfamily = "Times") +
  tm_layout(frame = FALSE, legend.outside = FALSE, fontfamily = "Times") +
  tm_compass(type = "arrow", position = c("left", "top"), size = 1.5) +
  tm_scalebar(breaks = c(0, 2, 4), text.size = 1, position = c(0.5, 0.06))

hennepin_county_transit_accessibility_map

#New York transit accessibility map
nyc_transit_accessibility_map <- tm_shape(nyc_hex_transit_accessibility, unit = "mi") +
  tm_polygons(
    fill = "transit_access_jobs_45",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 7,
      values = "Oranges"
    ),
    fill.legend = tm_legend(
      title = "Jobs accessible within 45 minutes"
    )
  ) +
  tm_borders(col = "black", lwd = .15) +
  tm_title("New York City", size = 1.2, fontfamily = "Times") +
  tm_layout(frame = FALSE, legend.outside = FALSE, fontfamily = "Times") +
  tm_compass(type = "arrow", position = c("left", "top"), size = 1.5) +
  tm_scalebar(breaks = c(0, 2, 4), text.size = 1, position = c(0.5, 0.06))

nyc_transit_accessibility_map

#combine into one three-panel figure
transit_accessibility_maps <- tmap_arrange(
  stl_transit_accessibility_map,
  hennepin_county_transit_accessibility_map,
  nyc_transit_accessibility_map,
  ncol = 1
)

transit_accessibility_maps

# -----------------
#    save output
# -----------------

tmap_save(
  transit_accessibility_maps,
  "Outputs/transit_accessibility_maps.png",
  width = 8,
  height = 10,
  dpi = 300
)

