library(sf)
library(ggplot2)
library(dplyr)
library(purrr)

# Define the directory path
base_dir <- "../Data/MAPS- INDIA/pre-delim/PC_Data/States"

# List all state folders within the base directory
state_folders <- list.dirs(base_dir, recursive = FALSE)

# Read all shapefiles and combine into a single sf object
all_shapes <- state_folders %>%
  map_dfr(~ {
    # Find the shapefile in each state folder
    shapefile <- list.files(.x, pattern = "\\.shp$", full.names = TRUE)
    # Read the shapefile and add a column for the state ID
    if (length(shapefile) > 0) {
      st_read(shapefile) %>%
        mutate(state = basename(.x))
    } else {
      NULL
    }
  })

# Plot all shapes together
ggplot(data = all_shapes) +
  geom_sf(aes(fill = state), color = NA) +  # Optional: color the states differently
  theme_minimal() +
  labs(title = "Indian States Shapefiles",
       fill = "State ID")
