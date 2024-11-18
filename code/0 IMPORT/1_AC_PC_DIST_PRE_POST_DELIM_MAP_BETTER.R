# Header -------------------------------------------------------------------
rm(list = ls())
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)
library(tidyr)

source("code/functions.R")

# This code needs some maintaining starting 
> spatial_results <- compute_intersection(
  +   geom1 = pre_geom,
  +   geom2 = post_geom,
  +   dist_shp = dist_shp_2001,
  +   pc_shp = post_pc_shp
  + )

# Functions ---------------------------------------------------------------
# Function to clean state and PC names
# Pre-Process Names and Ensure CRS Consistency ----------------------------
clean_and_transform <- function(shapefile, state_col, 
                                pc_col) {
  shapefile %>%
    #st_transform(crs = crs) %>%  # Ensure CRS is consistent
    mutate(
      {{ state_col }} := tolower({{ state_col }}),
      {{ pc_col }} := tolower({{ pc_col }}),
      {{ pc_col }} := gsub("\\s*\\(.*\\)", "", {{ pc_col }}),
      {{ pc_col }} := str_trim({{ pc_col }})
    )
}


# Update Compute Intersection Function ------------------------------------
compute_intersection <- function(geom1, geom2, dist_shp, pc_shp, area_threshold = 1) {
  # Ensure CRS consistency is handled before calling the function
  
  # Compute intersections for all geometries
  intersections <- geos_intersection(geom1, geom2)
  
  # Calculate intersection areas
  intersection_areas <- geos_area(intersections)
  
  # Filter non-empty intersections
  valid_intersections <- which(intersection_areas > 0)
  
  if (length(valid_intersections) == 0) {
    return(data.frame())  # Return an empty data frame if no intersections
  }
  
  # Filter valid intersections
  geom1_filtered <- geom1[valid_intersections]
  geom2_filtered <- geom2[valid_intersections]
  intersections_filtered <- intersections[valid_intersections]
  
  # Compute additional metrics
  dist_areas <- geos_area(geom1_filtered)
  pc_areas <- geos_area(geom2_filtered)
  dist_area_pct <- (intersection_areas[valid_intersections] / dist_areas) * 100
  pc_area_pct <- (intersection_areas[valid_intersections] / pc_areas) * 100
  
  # Create the result data frame
  results <- data.frame(
    dist_id = dist_shp$admin_id[valid_intersections],
    dist_name = tolower(dist_shp$DISTRICT[valid_intersections]),
    pc_id = pc_shp$PC_CODE[valid_intersections],
    pc_name = pc_shp$PC_NAME[valid_intersections],
    intx_area = intersection_areas[valid_intersections],
    dist_area = dist_areas,
    dist_area_pct = dist_area_pct,
    pc_area = pc_areas,
    pc_area_pct = pc_area_pct
  )
  
  # Filter based on area thresholds
  results <- results %>%
    filter(dist_area_pct >= area_threshold, pc_area_pct >= area_threshold)
  
  return(results)
}


# Step 1: Load Shapefiles -------------------------------------------------
# Administrative Districts (2001 and 2011)
dist_shp_2001 <- st_read("../Data/MAPS- INDIA/AC/maps-master/Districts/Census_2001/2001_Dist.shp") %>%
  mutate(admin_id = row_number()) %>%
  mutate(admin_area = st_area(geometry))

dist_shp_2011 <- st_read("../Data/MAPS- INDIA/AC/maps-master/Districts/Census_2011/2011_Dist.shp") %>%
  st_make_valid() %>%
  mutate(admin_id = row_number()) %>%
  mutate(admin_area = st_area(geometry))

common_crs <- st_crs(dist_shp_2001)  # Use the CRS of the admin districts as the standard


# Pre-Delimitation PC
pre_pc_shp <- map_dfr(
  list.dirs("../Data/MAPS- INDIA/pre-delim/PC_Data/States", recursive = FALSE),
  ~ {
    shp <- list.files(.x, pattern = "\\.shp$", full.names = TRUE)
    if (length(shp) > 0) st_read(shp) else NULL
  }
)

# Ensure geometries are valid
pre_pc_shp <- pre_pc_shp %>%
  st_make_valid()

# Filter valid geometries
pre_pc_shp <- pre_pc_shp %>%
  filter(st_is_valid(geometry))

# Assign CRS if not set
if (is.na(st_crs(pre_pc_shp))) {
  st_crs(pre_pc_shp) <- 4326  # Assuming original CRS is WGS 84
}

# Transform CRS and calculate admin area
pre_pc_shp <- pre_pc_shp %>%
  st_transform(crs = common_crs) %>%
  mutate(admin_area = st_area(geometry))




# Post-Delimitation PC
post_pc_shp <- st_read("../Data/MAPS- INDIA/AC/maps-master/parliamentary-constituencies/india_pc_2019.shp") %>%
  st_make_valid()

# Assign the CRS first if not already set
if (is.na(st_crs(post_pc_shp))) {
  st_crs(post_pc_shp) <- 4326  # Assuming the original CRS is WGS 84
}

# Then transform to the common CRS
post_pc_shp <- post_pc_shp %>%
  st_transform(crs = common_crs) %>%
  mutate(admin_area = st_area(geometry))

# Apply Cleaning and Transformation

pre_pc_clean <- clean_and_transform(pre_pc_shp, ST_NAME, 
                                    PC_NAME)
post_pc_clean <- clean_and_transform(post_pc_shp, ST_NAME, 
                                     PC_NAME)

# Match by Names ----------------------------------------------------------
name_matched <- pre_pc_clean %>%
  as.data.frame() %>%  # Temporarily convert to data frame for joining
  dplyr::select(ST_NAME, PC_NAME) %>%
  inner_join(
    post_pc_clean %>%
      as.data.frame() %>%
      dplyr::select(ST_NAME, PC_NAME),
    by = c("ST_NAME", "PC_NAME")
  ) %>%
  mutate(name_match = 1)

# Ensure CRS Consistency for Administrative Districts ---------------------
dist_shp_2001 <- st_transform(dist_shp_2001, crs = common_crs)
dist_shp_2011 <- st_transform(dist_shp_2011, crs = common_crs)

# Remove Name Matched Units -----------------------------------------------
pre_geom <- as_geos_geometry(
  pre_pc_clean %>% anti_join(name_matched, by = c("ST_NAME", "PC_NAME"))
)
post_geom <- as_geos_geometry(
  post_pc_clean %>% anti_join(name_matched, by = c("ST_NAME", "PC_NAME"))
)

# Compute Spatial Intersections -------------------------------------------
spatial_results <- compute_intersection(
  geom1 = pre_geom,
  geom2 = post_geom,
  dist_shp = dist_shp_2001,
  pc_shp = post_pc_shp
)

# Construct Final Mapping -------------------------------------------------
final_mapping <- spatial_results %>%
  mutate(
    delim_id = 3,
    match_pct = intx_area / dist_area
  ) %>%
  dplyr::select(dist_id, dist_name, pc_id, pc_name, match_pct, delim_id)

# Append Name Matches -----------------------------------------------------
final_mapping <- bind_rows(
  final_mapping,
  name_matched %>%
    mutate(delim_id = 4, match_pct = 1)
)


# Save Final Outputs ------------------------------------------------------
#qsave(final_mapping, "data/dum.qs")
