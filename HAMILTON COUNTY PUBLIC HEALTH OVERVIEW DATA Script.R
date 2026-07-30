############################################################
# HAMILTON COUNTY, INDIANA - Public Health Data Mapping
############################################################

# Load libraries
library(readr)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(ggplot2)
library(viridis)

options(tigris_use_cache = TRUE)
readRenviron("~/.Renviron")

############################################################
# 1. Load PLACES Data
############################################################

url <- "https://data.cdc.gov/resource/cwsq-ngmh.csv?countyname=Hamilton"
places <- read_csv(url)

# Filter to Hamilton, IN
hamilton_places <- places %>%
  filter(stateabbr == "IN", countyname == "Hamilton")

# Select target measures
selected_measures <- c(
  "Depression among adults",
  "Frequent mental distress among adults",
  "Current lack of health insurance among adults aged 18-64 years",
  "Visits to doctor for routine checkup within the past year among adults",
  "Visits to a health professional in past year among adults"
)

hamilton_selected <- hamilton_places %>%
  filter(measure %in% selected_measures)

############################################################
# 2. FIX PLACES GEOID (critical step)
############################################################

places_wide_conditions <- hamilton_selected %>%
  select(GEOID = locationid, measure, data_value) %>%
  
  mutate(
    GEOID = gsub("[^0-9]", "", GEOID),     # remove non-digits
    GEOID = ifelse(nchar(GEOID) == 0 | is.na(GEOID),
                   NA,
                   GEOID
    ),
    GEOID = stringr::str_pad(GEOID, width = 11, pad = "0")  # safe zero-padding
  ) %>%
  
  pivot_wider(names_from = measure, values_from = data_value)

############################################################
# 3. Load ACS Data
############################################################

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
  mutate(GEOID = as.character(GEOID)) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)

############################################################
# 4. Load Census Tract Shapes
############################################################

tract_shapes <- tracts(
  state = "IN",
  county = "Hamilton",
  cb = TRUE
) %>%
  mutate(GEOID = as.character(GEOID))

############################################################
# 5. Safe Join – Remove geometry, join, then restore geometry
############################################################

conditions_map <- tract_shapes %>%
  st_drop_geometry() %>%
  left_join(places_wide_conditions, by = "GEOID") %>%
  left_join(data_clean, by = "GEOID") %>%
  left_join(select(tract_shapes, GEOID, geometry), by = "GEOID") %>%
  st_as_sf()

############################################################
# 6. ZIP Code Shapes
############################################################

zctas <- zctas(year = 2020, cb = TRUE)

hamilton_county <- counties(state = "IN", cb = TRUE) %>%
  filter(NAME == "Hamilton")

hamilton_zips <- st_join(zctas, hamilton_county, join = st_intersects) %>%
  filter(!is.na(NAME)) %>%
  select(ZIP = GEOID20, geometry)

hamilton_zip_centroids <- st_centroid(hamilton_zips)

############################################################
# 7. VISUALIZATION — Example Maps
############################################################

### Depression ###
ggplot() +
  geom_sf(data = conditions_map,
          aes(fill = `Depression among adults`),
          color = NA) +
  geom_sf(data = hamilton_zips, fill = NA, color = "black") +
  geom_sf_text(data = hamilton_zip_centroids, aes(label = ZIP), size = 3) +
  scale_fill_viridis(option = "plasma") +
  labs(title = "Depression Among Adults — Hamilton County",
       fill = "% Depression") +
  theme_minimal()

### Frequent Mental Distress ###
ggplot() +
  geom_sf(data = conditions_map,
          aes(fill = `Frequent mental distress among adults`),
          color = NA) +
  geom_sf(data = hamilton_zips, fill = NA, color = "black") +
  geom_sf_text(data = hamilton_zip_centroids, aes(label = ZIP), size = 3) +
  scale_fill_viridis(option = "magma") +
  labs(title = "Frequent Mental Distress — Hamilton County",
       fill = "% Frequent Distress") +
  theme_minimal()

### Uninsured Adults 18–64 ###
ggplot() +
  geom_sf(data = conditions_map,
          aes(fill = `Current lack of health insurance among adults aged 18-64 years`),
          color = NA) +
  geom_sf(data = hamilton_zips, fill = NA, color = "black") +
  geom_sf_text(data = hamilton_zip_centroids, aes(label = ZIP), size = 3) +
  scale_fill_viridis(option = "cividis") +
  labs(title = "Lack of Health Insurance (18–64) — Hamilton County",
       fill = "% Uninsured") +
  theme_minimal()

### Routine Checkup ###
ggplot() +
  geom_sf(data = conditions_map,
          aes(fill = `Visits to doctor for routine checkup within the past year among adults`),
          color = NA) +
  geom_sf(data = hamilton_zips, fill = NA, color = "black") +
  geom_sf_text(data = hamilton_zip_centroids, aes(label = ZIP), size = 3) +
  scale_fill_viridis(option = "magma") +
  labs(title = "Routine Doctor Checkup — Hamilton County",
       fill = "% Past-Year Checkup") +
  theme_minimal()
