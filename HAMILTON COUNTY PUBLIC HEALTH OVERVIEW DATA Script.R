############################################################
# HAMILTON COUNTY, INDIANA
# U.S. CENSUS, Tracts and PLACES data
# Author: Anita Kumari
# Date: Jul-09-2026
############################################################
# Install all libraries required for pulling, cleaning, transforming,
# merging, and mapping public health datasets in R.
#
# tidycensus: Needed to download and process American Community Survey (ACS)
#             data such as poverty, uninsured rate, disability, age distribution.
#
# tidyverse: Required for data cleaning, filtering, reshaping, joining tables,
#            importing CSV files, and performing most data manipulation tasks.
#
# tigris: Provides Census geographic boundary files (tracts, ZIPs/ZCTAs, counties)
#         which are necessary to map ACS + PLACES data spatially.
#
# sf: Enables handling spatial data objects, performing geographic joins,
#     creating centroids, and preparing shapes for heat maps.
#
# ggplot2: Creates all visualizations including heat maps, bar charts,
#          scatterplots, and ZIP???labeled tract maps.
#
# viridis: Provides colorblind???friendly, scientific color palettes
#          recommended for public???facing health maps and reports.
#
# readr: Allows fast, clean import of CDC PLACES data from CSV or API
#        and avoids formatting errors common with base R's read.csv.

############################################################

# Install packages (only once needed)
install.packages(c("tidycensus", "tidyverse", "tigris",
                   "sf", "ggplot2", "viridis", "readr"))

############################################################


############################################################
## Step 1: Load Libraries & Census API Key
############################################################
#Load all required R packages to pull, clean, join, and map public health data.
# tidycensus = download ACS demographic indicators.
# tidyverse = clean and reshape PLACES + ACS.
# tigris = download geographic shapefiles (tracts, ZIPs).
# sf = manage spatial objects needed for heat maps.
# ggplot2 = draw all maps and plots.
# viridis = colorblind friendly color scales for maps.
# readr = load CDC PLACES CSV/API data.
# readRenviron() = loads your Census API key so ACS pulls work

library(readr)
library(tidycensus)
library(tidyverse)
library(tigris)
library(ggplot2)
library(sf)
library(viridis)


url <- "https://data.cdc.gov/api/views/cwsq-ngmh/rows.csv?accessType=DOWNLOAD"
places <- read_csv(url)

options(tigris_use_cache = TRUE)
readRenviron("~/.Renviron")


############################################################
############################################################
## Step 2: Load ACS Data for Hamilton County
############################################################
# ACS gives population characteristics for each census tract.
# Includes uninsured, poverty, disability, age 65+, median income.
# Data arrives long (multiple rows per tract), so pivot_wider makes:
# one row per tract + multiple condition columns.
# Provides socioeconomic context for interpreting chronic disease patterns.

variables <- c(
  uninsured = "S2701_C05_001E",
  poverty   = "S1701_C02_001E",
  disability = "S1810_C02_001E",
  age65plus = "S0101_C02_030E",
  income    = "B19013_001E"
)

acs <- get_acs(
  geography = "tract",
  variables = variables,
  state     = "IN",
  county    = "Hamilton County",
  year      = 2022
)

data_clean <- acs %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)


############################################################
## Step 3: Load Census Tract Shapefiles
############################################################

# Tract shapefiles contain geographic boundaries for each Census tract.
# Needed because PLACES and ACS data are both tract???level.
# Enables spatial joins and creation of geographic heat maps.

tract_shapes <- tracts(
  state  = "IN",
  county = "Hamilton",
  cb     = TRUE
)

############################################################
## Step 4: Download CDC PLACES Data & Filter Hamilton County
############################################################

# PLACES provides modeled chronic disease estimates per tract.
# Includes depression, heart disease, obesity, housing insecurity, cholesterol.
# Filter to Hamilton County to get only local tracts.
# This supplies the chronic illness indicators ACS does not include.

url <- "https://data.cdc.gov/api/views/cwsq-ngmh/rows.csv?accessType=DOWNLOAD"
places <- read_csv(url)
class(places)
hamilton_places <- places %>%
  filter(StateAbbr == "IN", CountyName == "Hamilton")

############################################################
## Step 5: Select PLACES Measures (5 Conditions Total)
############################################################

# PLACES has hundreds of measures. You choose only the required ones:
# - Depression among adults
# - Coronary heart disease among adults
# - Housing insecurity in the past 12 months among adults
# - Obesity among adults
# - High cholesterol among adults who have ever been screened
# Filtering reduces noise and focuses your chronic disease analysis.

selected_measures <- c(
  "Depression among adults",
  "Coronary heart disease among adults",
  "Food insecurity in the past 12 months among adults",
  "Obesity among adults",
  "High cholesterol among adults who have ever been screened"
)

hamilton_selected <- hamilton_places %>%
  filter(Measure %in% selected_measures)


unique(hamilton_places$Measure)
############################################################
## Step 6: Pivot PLACES Wider (Creates Columns per Condition)
############################################################

# PLACES arrives long: one row per tract per condition.
# pivot_wider transforms long ??? wide:
# Each tract gets its own row.
# Each selected measure becomes a column.
# Required so ggplot + joins + scatterplots work correctly.

places_wide_conditions <- hamilton_selected %>%
  select(GEOID = LocationID, Measure, Data_Value) %>%
  pivot_wider(
    names_from  = Measure,
    values_from = Data_Value
  )

############################################################
## Step 7: Join ACS Tracts with PLACES Conditions
############################################################

# Merge tract shapes + ACS demographics + PLACES chronic disease values.
# Creates a single unified dataset ready for mapping.
# This combined object is used in all heat maps.

conditions_map <- tract_shapes %>%
  left_join(places_wide_conditions, by = "GEOID")


############################################################
## Step 8: Load ZIP Code (ZCTA) Shapes Using GEOID20
############################################################

# ZIPs (ZCTAs) make maps easier to interpret for the public and leadership.
# zctas() loads ZIP boundaries for the entire US.
# st_join intersects ZIPs with Hamilton County boundary.
# select(ZIP = GEOID20) = uses your working ZIP field.
# st_centroid() places ZIP labels inside each ZIP polygon.
# Adds ZIP overlays for location clarity on all maps.

zctas <- zctas(year = 2020, cb = TRUE)

hamilton_county <- counties(state = "IN", cb = TRUE) %>%
  filter(NAME == "Hamilton")

hamilton_zips <- st_join(zctas, hamilton_county, join = st_intersects) %>%
  filter(!is.na(NAME)) %>%
  select(ZIP = GEOID20, geometry)

hamilton_zip_centroids <- st_centroid(hamilton_zips)


############################################################
## Step 9: Heat Map|Depression (ZIP labeled)
############################################################

# Shows geographic clustering of depression across tracts.
# ZIP labels help orient users to familiar locations.
# Supports mental health program targeting and community outreach planning.

ggplot() +
  geom_sf(data = conditions_map,
          aes(fill = `Depression among adults`),
          color = NA) +
  geom_sf(data = hamilton_zips, fill = NA, color = "black", size = 0.2) +
  geom_sf_text(data = hamilton_zip_centroids,
               aes(label = ZIP),
               size = 3,
               color = "black") +
  scale_fill_viridis(option = "plasma") +
  labs(title = "Depression Among Adults ??? Hamilton County (ZIP labeled)",
       fill  = "% Depression") +
  theme_minimal()


############################################################
## Step 10: Heat Map|Coronary Heart Disease (ZIP labeled)
############################################################

# Displays CHD burden spatially across Hamilton County.
# Identifies areas needing cardiovascular health interventions.

ggplot() +
  geom_sf(data = conditions_map,
          aes(fill = `Coronary heart disease among adults`),
          color = NA) +
  geom_sf(data = hamilton_zips, fill = NA, color = "black", size = 0.2) +
  geom_sf_text(data = hamilton_zip_centroids, aes(label = ZIP),
               size = 3, color = "black") +
  scale_fill_viridis(option = "magma") +
  labs(title = "Coronary Heart Disease ??? Hamilton County (ZIP labeled)",
       fill  = "% CHD") +
  theme_minimal()

############################################################
## Step 11: Heat Map ??? Housing Insecurity (ZIP labeled)
############################################################

# Maps housing insecurity as a social determinant.
# Highlights vulnerable communities and inequities.
# Useful for resource planning and equity initiatives.

ggplot() +
  geom_sf(data = conditions_map,
          aes(fill = `Food insecurity in the past 12 months among adults`),
          color = NA) +
  geom_sf(data = hamilton_zips, fill = NA, color = "black", size = 0.2) +
  geom_sf_text(data = hamilton_zip_centroids, aes(label = ZIP),
               size = 3, color = "black") +
  scale_fill_viridis(option = "inferno") +
  labs(title = "Food insecurity in the past 12 months among adults",
       fill  = "% Insecurity") +
  theme_minimal()


############################################################
## Step 12: Heat Map ??? Obesity Among Adults (ZIP labeled)
############################################################

# Shows obesity prevalence geographically.
# Supports wellness, nutrition, and prevention program design.

ggplot() +
  geom_sf(data = conditions_map,
          aes(fill = `Obesity among adults`),
          color = NA) +
  geom_sf(data = hamilton_zips, fill = NA, color = "black", size = 0.2) +
  geom_sf_text(data = hamilton_zip_centroids, aes(label = ZIP),
               size = 3, color = "black") +
  scale_fill_viridis(option = "cividis") +
  labs(title = "Obesity Among Adults ??? Hamilton County (ZIP labeled)",
       fill  = "% Obesity") +
  theme_minimal()


############################################################
## Step 13: Heat Map|High Cholesterol (ZIP labeled)
############################################################

# Displays cholesterol risk patterns across ZIPs and tracts.
# Supports cardiovascular risk monitoring and planning.

ggplot() +
  geom_sf(data = conditions_map,
          aes(fill = `High cholesterol among adults who have ever been screened`),
          color = NA) +
  geom_sf(data = hamilton_zips, fill = NA, color = "black", size = 0.2) +
  geom_sf_text(data = hamilton_zip_centroids, aes(label = ZIP),
               size = 3, color = "black") +
  scale_fill_viridis(option = "turbo") +
  labs(title = "High Cholesterol ??? Hamilton County (ZIP labeled)",
       fill  = "% High Cholesterol") +
  theme_minimal()


############################################################
## Step 14: Scatterplot|Obesity vs Depression
############################################################

# Compares two chronic conditions to find relationships.
# Helps identify tracts with multiple co???occurring health burdens.
# Important for integrated chronic disease + behavioral health interventions.

ggplot(places_wide_conditions,
       aes(x = `Obesity among adults`,
           y = `Depression among adults`)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  labs(
    title = "Obesity vs Depression ??? Hamilton County",
    x     = "% Obesity",
    y     = "% Depression"
  ) +
  theme_minimal()

ggplot() +
  geom_sf(data = conditions_map,
          aes(fill = `Depression among adults`),
          color = NA) +
  geom_sf(data = hamilton_zips,
          fill = NA, color = "black", size = 0.2) +
  geom_sf_text(data = hamilton_zip_centroids,
               aes(label = ZIP), size = 3, color = "black") +
  scale_fill_viridis(
    option = "plasma",
    name = "Depression (%)",
    breaks = c(5, 10, 15, 20),
    labels = c("5%", "10%", "15%", "20%")
  ) +
  guides(
    fill = guide_colorbar(
      title = "Depression Among Adults",
      barwidth = 12,
      barheight = 1.2,
      title.position = "top"
    )
  ) +
  labs(title = "Depression Among Adults ??? Hamilton County (ZIP labeled)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )