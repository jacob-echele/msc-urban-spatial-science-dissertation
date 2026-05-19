library(sf)
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(RColorBrewer)
library(tidyverse)

###########################
#   EXPLORATORY ANALYSIS
###########################

#set working directory
setwd("C:/Users/User/OneDrive/Desktop/School/7- Graduate School/Term 2/Dissertation/Data/")

#read in font for outputs
windowsFonts(Times = windowsFont("Times New Roman"))

##########################################
#   FISCAL PRODUCTIVITY VIOLIN BOX PLOT
##########################################

#read processed outputs
stl_hex_fp <- readRDS("Processed Data/stl_hex_fiscal_productivity.rds")
hennepin_county_hex_fp <- readRDS("Processed Data/hennepin_county_hex_fiscal_productivity.rds")
nyc_hex_fp <- readRDS("Processed Data/nyc_hex_fiscal_productivity.rds")

#remove geometry and combine into one dF
fp_distribution <- bind_rows(
  stl_hex_fp%>%
    st_drop_geometry()%>%
    mutate(city = "St. Louis"),
  hennepin_county_hex_fp%>%
    st_drop_geometry()%>%
    mutate(city = "Minneapolis"),
  nyc_hex_fp%>%
    st_drop_geometry()%>%
    mutate(city = "New York"))%>%
  select(city, fiscal_productivity)%>%
  filter(fiscal_productivity > 0)%>% #remove zeros for log scale
  mutate(
    city = factor(city, levels = c( #keep city order consistent with dissertation
        "St. Louis",
        "Minneapolis",
        "New York")))

# ---------------------
#    actual plotting
# ---------------------

#fiscal productivity distribution figure
fiscal_distribution_plot <- ggplot(
  fp_distribution,
  aes(
    x = city,
    y = fiscal_productivity,
    fill = city
  )
) +
  geom_violin(alpha = .8, trim = FALSE) +
  geom_boxplot(width = .12, fill = "white", outlier.shape = NA) +
  scale_fill_brewer(palette = "Greens") +
  scale_y_log10(labels = scales::label_number()) +   #log transform display only
  labs(
    title = "Distribution of Fiscal Productivity",
    x = NULL,
    y = "Assessed value per acre (log scale)"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Times"),
    plot.title = element_text(size = 12, hjust = .5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10), legend.position = "none", panel.grid.minor = element_blank())

fiscal_distribution_plot

# --------------------
#    saving outputs
# --------------------

ggsave(
  "Outputs/fiscal_productivity_distribution.png",
  fiscal_distribution_plot,
  width = 8,
  height = 5,
  dpi = 300
)
