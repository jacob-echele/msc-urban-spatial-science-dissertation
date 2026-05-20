library(sf)
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(RColorBrewer)
library(tidyverse)
library(corrplot) #for correlation matrix
library(Hmisc) #for correlation matrix

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
    axis.text = element_text(size = 10), legend.position = "none", panel.grid.minor = element_blank()) #center title, remove small gridlines

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

##########################################
#   POPULATION DENSITY VIOLIN BOX PLOT
##########################################

#read processed outputs
stl_hex_population_density <- readRDS("Processed Data/stl_hex_population_density.rds")
hennepin_county_hex_population_density <- readRDS("Processed Data/hennepin_county_hex_population_density.rds")
nyc_hex_population_density <- readRDS("Processed Data/nyc_hex_population_density.rds")

#remove geometry and combine into one dF
population_density_distribution <- bind_rows(
  stl_hex_population_density%>%
    st_drop_geometry()%>%
    mutate(city = "St. Louis"),
  hennepin_county_hex_population_density%>%
    st_drop_geometry()%>%
    mutate(city = "Minneapolis"),
  nyc_hex_population_density%>%
    st_drop_geometry()%>%
    mutate(city = "New York"))%>%
  select(city, pop_density_acre)%>%
  filter(pop_density_acre >= .1)%>% #remove zeros and effectively empty hexes for log scale and visualization
  mutate(
    city = factor(city,levels = c( #keep city order consistent with dissertation
        "St. Louis",
        "Minneapolis",
        "New York")))

# ---------------------
#    actual plotting
# ---------------------

#population density distribution figure
population_density_distribution_plot <- ggplot(
  population_density_distribution,
  aes(
    x = city,
    y = pop_density_acre,
    fill = city
  )
) +
  geom_violin(alpha = .8, trim = FALSE) +
  geom_boxplot(width = .12, fill = "white", outlier.shape = NA) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_log10(labels = scales::label_number()) + #log transform display only
  labs(
    title = "Distribution of Population Density",
    x = NULL,
    y = "Population Density (people per acre, log scale)"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Times"),
    plot.title = element_text(size = 12, hjust = .5),
    axis.title = element_text(size = 11), axis.text = element_text(size = 10),legend.position = "none", panel.grid.minor = element_blank()) #center title, remove small gridlines

population_density_distribution_plot

# --------------------
#    saving outputs
# --------------------

ggsave(
  "Outputs/population_density_distribution.png",
  population_density_distribution_plot,
  width = 8,
  height = 5,
  dpi = 300
)

############################################
#   PERCEIVED JOB DENSITY VIOLIN BOX PLOT
############################################

#read processed outputs
stl_hex_job_density <- readRDS("Processed Data/stl_hex_job_density.rds")
hennepin_county_hex_job_density <- readRDS("Processed Data/hennepin_county_hex_job_density.rds")
nyc_hex_job_density <- readRDS("Processed Data/nyc_hex_job_density.rds")

#remove geometry and combine into one dF
job_density_distribution <- bind_rows(
  stl_hex_job_density%>%
    st_drop_geometry()%>%
    mutate(city = "St. Louis"),
  hennepin_county_hex_job_density%>%
    st_drop_geometry()%>%
    mutate(city = "Minneapolis"),
  nyc_hex_job_density%>%
    st_drop_geometry()%>%
    mutate(city = "New York"))%>%
  select(city, perceived_job_density)%>%
  filter(perceived_job_density > 0)%>% #remove zeros and effectively empty hexes for log scale and visualization
  mutate(
    city = factor(city,levels = c( #keep city order consistent with dissertation
      "St. Louis",
      "Minneapolis",
      "New York")))

# ---------------------
#    actual plotting
# ---------------------

#population density distribution figure
job_density_distribution_plot <- ggplot(
  job_density_distribution,
  aes(
    x = city,
    y = perceived_job_density,
    fill = city
  )
) +
  geom_violin(alpha = .8, trim = FALSE) +
  geom_boxplot(width = .12, fill = "white", outlier.shape = NA) +
  scale_fill_brewer(palette = "Purples") +
  scale_y_log10(labels = scales::label_number()) + #log transform display only
  labs(
    title = "Distribution of Perceived Job Density",
    x = NULL,
    y = "Perceived Jobs per acre (log scale)"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(size = 12, hjust = .5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 10),legend.position = "none", panel.grid.minor = element_blank()) #center title, remove small gridlines

job_density_distribution_plot

# --------------------
#    saving outputs
# --------------------

ggsave(
  "Outputs/job_density_distribution.png",
  job_density_distribution_plot,
  width = 8,
  height = 5,
  dpi = 300
)

############################################
#   TRANSIT ACCESSIBILITY VIOLIN BOX PLOT
############################################

stl_hex_transit_accessibility <- readRDS("Processed Data/stl_hex_transit_accessibility.rds")
hennepin_county_hex_transit_accessibility <- readRDS("Processed Data/hennepin_county_hex_transit_accessibility.rds")
nyc_hex_transit_accessibility <- readRDS("Processed Data/nyc_hex_transit_accessibility.rds")

#remove geometry and combine into one dF
transit_accessibility_distribution <- bind_rows(
  stl_hex_transit_accessibility%>%
    st_drop_geometry()%>%
    mutate(city = "St. Louis"),
  hennepin_county_hex_transit_accessibility%>%
    st_drop_geometry()%>%
    mutate(city = "Minneapolis"),
  nyc_hex_transit_accessibility%>%
    st_drop_geometry()%>%
    mutate(city = "New York"))%>%
  select(city, transit_access_jobs_45)%>%
  filter(!is.na(transit_access_jobs_45), transit_access_jobs_45 > 0)%>% #remove zeros and effectively empty hexes for log scale and visualization
  mutate(
    city = factor(city,levels = c( #keep city order consistent with dissertation
      "St. Louis",
      "Minneapolis",
      "New York")))

# ---------------------
#    actual plotting
# ---------------------

#population density distribution figure
transit_accessibility_distribution_plot <- ggplot(
  transit_accessibility_distribution,
  aes(
    x = city,
    y = transit_access_jobs_45,
    fill = city
  )
) +
  geom_violin(alpha = .8, trim = FALSE) +
  geom_boxplot(width = .12, fill = "white", outlier.shape = NA) +
  scale_fill_brewer(palette = "Oranges") +
  scale_y_log10(labels = scales::label_number()) + #log transform display only
  labs(
    title = "Distribution of Transportation Accessibility",
    x = NULL,
    y = "Jobs Accessible within 45 minutes (log scale)"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Times"),
        plot.title = element_text(size = 12, hjust = .5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 10),legend.position = "none", panel.grid.minor = element_blank()) #center title, remove small gridlines

transit_accessibility_distribution_plot

# --------------------
#    saving outputs
# --------------------

ggsave(
  "Outputs/transit_accessibility_distribution.png",
  transit_accessibility_distribution_plot,
  width = 8,
  height = 5,
  dpi = 300
)

##################################
#   COMBINED CORRELATION MATRIX
##################################

#ensure hex_id data type matches
stl_hex_fp$hex_id <- as.character(stl_hex_fp$hex_id)
stl_hex_population_density$hex_id <- as.character(stl_hex_population_density$hex_id)
stl_hex_job_density$hex_id <- as.character(stl_hex_job_density$hex_id)
stl_hex_transit_accessibility$hex_id <- as.character(stl_hex_transit_accessibility$hex_id)

#join prepared St. Louis variables into one df
stl_hex_analysis <- stl_hex_fp%>%
  st_drop_geometry()%>%
  select(hex_id, fiscal_productivity)%>%
  left_join(
    stl_hex_population_density%>%
      st_drop_geometry()%>%
      select(hex_id, pop_density_acre),
    by = "hex_id"
  )%>%
  left_join(
    stl_hex_job_density%>%
      st_drop_geometry()%>%
      select(hex_id, job_density_acre, perceived_job_density),
    by = "hex_id"
  )%>%
  left_join(
    stl_hex_transit_accessibility%>%
      st_drop_geometry()%>%
      select(hex_id, transit_access_jobs_30, transit_access_jobs_45, transit_access_jobs_60),
    by = "hex_id"
  )%>%
  mutate(city = "St. Louis")

#ensure hex_id data type matches
hennepin_county_hex_fp$hex_id <- as.character(hennepin_county_hex_fp$hex_id)
hennepin_county_hex_population_density$hex_id <- as.character(hennepin_county_hex_population_density$hex_id)
hennepin_county_hex_job_density$hex_id <- as.character(hennepin_county_hex_job_density$hex_id)
hennepin_county_hex_transit_accessibility$hex_id <- as.character(hennepin_county_hex_transit_accessibility$hex_id)

#join prepared Minneapolis variables into one df
hennepin_county_hex_analysis <- hennepin_county_hex_fp%>%
  st_drop_geometry()%>%
  select(hex_id, fiscal_productivity)%>%
  left_join(
    hennepin_county_hex_population_density%>%
      st_drop_geometry()%>%
      select(hex_id, pop_density_acre),
    by = "hex_id"
  )%>%
  left_join(
    hennepin_county_hex_job_density%>%
      st_drop_geometry()%>%
      select(hex_id, perceived_job_density),
    by = "hex_id"
  )%>%
  left_join(
    hennepin_county_hex_transit_accessibility%>%
      st_drop_geometry()%>%
      select(hex_id, transit_access_jobs_45),
    by = "hex_id"
  )%>%
  mutate(city = "Minneapolis")

#ensure hex_id data type matches
nyc_hex_fp$hex_id <- as.character(nyc_hex_fp$hex_id)
nyc_hex_population_density$hex_id <- as.character(nyc_hex_population_density$hex_id)
nyc_hex_job_density$hex_id <- as.character(nyc_hex_job_density$hex_id)
nyc_hex_transit_accessibility$hex_id <- as.character(nyc_hex_transit_accessibility$hex_id)

#join new york variables into one dataset
nyc_hex_analysis <- nyc_hex_fp%>%
  st_drop_geometry()%>%
  select(hex_id, fiscal_productivity)%>%
  left_join(
    nyc_hex_population_density%>%
      st_drop_geometry()%>%
      select(hex_id, pop_density_acre),
    by = "hex_id"
  )%>%
  left_join(
    nyc_hex_job_density%>%
      st_drop_geometry()%>%
      select(hex_id, perceived_job_density),
    by = "hex_id"
  )%>%
  left_join(
    nyc_hex_transit_accessibility%>%
      st_drop_geometry()%>%
      select(hex_id, transit_access_jobs_45),
    by = "hex_id"
  )%>%
  mutate(city = "New York")

# -----------------------
#    combine variables
# -----------------------

correlation_data <- bind_rows(
    stl_hex_analysis%>%
      st_drop_geometry()%>%
      mutate(city="St. Louis"),
    hennepin_county_hex_analysis%>%
      st_drop_geometry()%>%
      mutate(city="Minneapolis"),
    nyc_hex_analysis%>%
      st_drop_geometry()%>%
      mutate(city="New York")
    )

#keep only variables needed for correlation matrix
correlation_variables <- correlation_data%>%
  select(fiscal_productivity, pop_density_acre, perceived_job_density, transit_access_jobs_45)%>%
  filter(
    fiscal_productivity > 0,
    pop_density_acre > 0,
    perceived_job_density > 0,
    transit_access_jobs_45 > 0
  )

#log transform variables because values are highly skewed
correlation_variables_log <- correlation_variables%>%
  mutate(
    log_fiscal_productivity = log10(fiscal_productivity),
    log_pop_density_acre = log10(pop_density_acre),
    log_perceived_job_density = log10(perceived_job_density),
    log_transit_access_jobs_45 = log10(transit_access_jobs_45)
  )%>%
  select(log_fiscal_productivity, log_pop_density_acre, log_perceived_job_density, log_transit_access_jobs_45)

#calculate correlation matrix
correlation_matrix <- cor(
  correlation_variables_log,
  use = "pairwise.complete.obs",
  method = "pearson"
)

#rename rows and columns for cleaner figure
colnames(correlation_matrix) <- c("Fiscal Productivity", "Population Density", "Perceived Job Density", "Transit Accessibility")

rownames(correlation_matrix) <- c("Fiscal Productivity", "Population Density", "Perceived Job Density", "Transit Accessibility")

correlation_matrix

# ---------------------
#    actual plotting
# ---------------------

#combined correlation matrix plot
corrplot(
  correlation_matrix,
  method = "circle", #size of circle depends on strength of correlation
  type = "lower", #only include lower triangle of results for clarity
  addCoef.col = "black",
  number.cex = .8,
  tl.col = "black",
  tl.srt = 45, #rotate labels 45 degrees for legibility
  diag = FALSE,
  col = colorRampPalette(
    c(
      "#ca0020",
      "white",
      "#0571b0"
    )
  )(200) #sets number of colors in between max (blue) and min (red) values
)

#########################################
#   CITY-SPECIFIC CORRELATION MATRICES
#########################################

#St. Louis
