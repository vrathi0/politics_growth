



# 0 HEADER ----------------------------------------------------------------



rm(list=ls())
source("code/functions.R")


library(sf)
library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)  # For side-by-side plots


# NOTES ON MATCHING FOR PC ACROSS 2008 DELIMINATION
# 1. I have done name matching, and it matches for a good number of PC also
#   but it does not deal with boundary shifting and what variation that might trigger
# for eg, I currently have no idea if this random boundary change trigger something 
#  for a subset of PCs. First metric to still keep handy should be just the 
# area match % like the non name match sample, this itself is likely to be random
# Post note: On the very lower end, it can be a signal for some error also
# For eg Bhopal has 0% overlap between new and old PC boundaries. 
#  25% (91) has less than 60%, 
# > summary(name_match_pct$match_pct1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.6023  0.7829  0.7261  0.9231  0.9945 


# 2. OUtside of name match, the area interaction sample might currently contain 
# some errors. 

# FUNCTIONS ------------------------------------------------

library(sf)
library(ggplot2)
library(dplyr)
library(patchwork)

# Define the function
plot_state <- function(shapefile1, shapefile2, state_name, pc_names) {
  # Filter the first shapefile for the given state name
  state_shapefile1 <- shapefile1 %>%
    filter(tolower(ST_NAME) == tolower(state_name))  # Replace ST_NAME with the actual column name
  
  # Highlight specific PC_NAMEs in shapefile1
  state_shapefile1 <- state_shapefile1 %>%
    mutate(highlight = if_else(tolower(PC_NAME) %in% tolower(pc_names), "Highlighted", "Other"))  # Replace PC_NAME with the actual column name
  
  # Filter the second shapefile for the given state name
  state_shapefile2 <- shapefile2 %>%
    filter(tolower(ST_NAME) == tolower(state_name))  # Replace ST_NAME with the actual column name
  
  # Highlight specific PC_NAMEs in shapefile2
  state_shapefile2 <- state_shapefile2 %>%
    mutate(highlight = if_else(tolower(PC_NAME) %in% tolower(pc_names), "Highlighted", "Other"))  # Replace PC_NAME with the actual column name
  
  # Create the plot for the first shapefile
  plot1 <- ggplot(state_shapefile1) +
    geom_sf(aes(fill = highlight), color = "black", size = 0.2) +
    scale_fill_manual(values = c("Highlighted" = "red", "Other" = "lightblue"), name = "Highlight") +
    theme_minimal() 
    # labs(
    #   title = paste("Geographical Units in", state_name, "- Shapefile 1"),
    #   subtitle = "First Shapefile",
    #   caption = "Source: Shapefile 1"
    # )
  
  # Create the plot for the second shapefile
  plot2 <- ggplot(state_shapefile2) +
    geom_sf(aes(fill = highlight), color = "black", size = 0.2) +
    scale_fill_manual(values = c("Highlighted" = "red", "Other" = "lightgreen"), name = "Highlight") +
    theme_minimal() 
    # labs(
    #   title = paste("Geographical Units in", state_name, "- Shapefile 2"),
    #   subtitle = "Second Shapefile",
    #   caption = "Source: Shapefile 2"
    # )
  
  # Combine the two plots side by side
  combined_plot <- plot1 + plot2
  
  # Display the combined plot
  print(combined_plot)
}

# Example usage:
# shapefile1 <- st_read("path_to_shapefile1.shp")  # Replace with the path to your first shapefile
# shapefile2 <- st_read("path_to_shapefile2.shp")  # Replace with the path to your second shapefile
# plot_state_comparison(shapefile1, shapefile2, "Madhya Pradesh", c("PC1", "PC2"))





# READING AC ELECTION DATA ------------------------------------------------

rawdt=read_csv("../Data/TCPD/AC/All_States_AE.csv") %>% 
  clean_names()

# state_code=read_excel("../Data/TCPD/state_code.xlsx")
# state_code$state_name[state_code$state_name=="Daman & Diu"]="Goa_Daman_&_Diu"
# state_code$state_name[state_code$state_name=="NCT of Delhi"]="Delhi"
# state_code$state_name[state_code$state_name=="Jammu and Kashmir"]="Jammu_&_Kashmir"
# 
# state_code=state_code %>% 
#   mutate(state=str_replace_all(state_name, " ", "_")) %>% 
#   dplyr::select(state, state_id) %>% 
#   mutate(state=tolower(state)) %>% 
#   mutate(state_id=padzero(state_id, 2))

# creating state+assembly_no+ac_id+poll_no+position level dt
# Creating sh_election_id to mimic SHRUG data. 
ac_election=rawdt %>% 
  mutate(state_name=tolower(state_name)) %>% 
  inner_join(state_code) %>% 
  mutate(assembly_no=padzero(assembly_no, 2), 
         constituency_no=padzero(constituency_no, 3),
         state_code=padzero(state_code, 2))

post_ac=ac_election %>% 
  filter(as.numeric(year)>2008) %>% 
  filter(!(state_code=="28" & year==2009)) %>% 
  filter(poll_no==0) %>% 
  dplyr::select( constituency_name, 
                constituency_no, constituency_type, 
                state_name,state_code) %>% distinct() %>% 
  rename(ac_name1=constituency_name) %>% 
  rename(ac_no=constituency_no) %>% 
  rename(ac_type=constituency_type)

# The more important point here is that
# I can get AC-DISTRICT mapping from ac_election
# Then use the PC-AC mapping in pc_raw to complete the PC-AC-District
# mapping in both time period. 
# This ideally should remove the need to any spatial mapping 
# between PC and DISTRICT

ac_dist_map=ac_election %>% 
  dplyr::select(state_name, st_abr, 
                assembly_no, delim_id, 
                constituency_name, constituency_type, 
                district_name) %>% 
  distinct()




## Reading PC data also

pc_raw=read_csv("../Data/TCPD/General Election/AC_LEVEL_All_States_2024-11-14.csv") %>% 
  clean_names()


# 1 READING ADMIN DISTRICT 2001 ---------------------------------------------

# Define the path to the shapefile
shapefile_path <- "../Data/MAPS- INDIA/AC/maps-master/Districts/Census_2001/2001_Dist.shp"

# Read the shapefile
dist_shp <- st_read(shapefile_path)

# Add unique IDs to each geometry
dist_shp <- dist_shp %>%
  mutate(admin_id = paste0( row_number())) 

# Calculate total area for each administrative district
dist_shp <- dist_shp %>%
  mutate(admin_area = st_area(geometry))

# 1 READING ADMIN DISTRICT 2011 ---------------------------------------------

# Define the path to the shapefile
shapefile_path <- "../Data/MAPS- INDIA/AC/maps-master/Districts/Census_2011/2011_Dist.shp"

# Read the shapefile
dist_shp11 <- st_read(shapefile_path)

dist_shp11 <- dist_shp11 %>%
  st_make_valid()
dist_shp11 <- dist_shp11 %>%
  filter(st_is_valid(geometry))


# Add unique IDs to each geometry
dist_shp11 <- dist_shp11 %>%
  mutate(admin_id = paste0( row_number())) 

# Calculate total area for each administrative district
dist_shp11 <- dist_shp11 %>%
  mutate(admin_area = st_area(geometry))



# 2 READING PRE DELIM PC AND INTERSECTING IT WITH DISTRICT SHP---------------------------------



# Define the directory path
base_dir <- "../Data/MAPS- INDIA/pre-delim/PC_Data/States"

# List all state folders within the base directory
state_folders <- list.dirs(base_dir, recursive = FALSE)

# Read all shapefiles and combine into a single sf object
pc_shp <- state_folders %>%
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
# ggplot(data = all_shapes) +
#   geom_sf(aes(fill = state), color = NA) +  # Optional: color the states differently
#   theme_minimal() +
#   labs(title = "Indian States Shapefiles",
#        fill = "State ID")

# pc_shp_pre_rg=pc_shp %>%
#   as.data.frame() %>% 
#   mutate(pc_id=padzero(PC_NO, 2)) %>% 
#   arrange(ST_NAME, pc_id) %>% 
#   dplyr::select(ST_NAME,ST_CODE,PC_NAME, pc_id) %>% 
#   distinct() %>% group_by(ST_NAME) %>% 
#   mutate(pc_count=n_distinct(pc_id)) %>% ungroup() %>% 
#     filter(str_to_title(ST_NAME)!="Andhra Pradesh")
# pc_name_pre=pc_shp_pre_rg %>% 
#   mutate(pc_name=trimws(gsub("\\s*\\(.*\\)", "", PC_NAME))) %>% 
#   mutate(pc_name=tolower(pc_name))
# pc_name_pre=unique(pc_name_pre$pc_name)



# Ensure both layers have the same CRS
st_crs(pc_shp) <- 4326
if (st_crs(dist_shp) != st_crs(pc_shp)) {
  pc_shp <- st_transform(pc_shp, st_crs(dist_shp))
}


# Calculate total area for each election district (optional, if needed)
pc_shp <- pc_shp %>%
  st_make_valid()
pc_shp_valid <- pc_shp %>%
  filter(st_is_valid(geometry))

pc_shp_valid <- pc_shp_valid %>%
  mutate(election_area = st_area(geometry))

pc_shp_valid=pc_shp_valid %>% 
  # Remove the first character from 'state_code_pc'
  mutate(ST_CODE = substr(ST_CODE, 2, nchar(ST_CODE)))# %>% 
#filter(!grepl("^U", ST_CODE))
# (NOT REMOVING) This removes all UTs including Delhi.
# [1] "ANDAMAN & NICOBAR ISLANDS"          
# [2] "CHANDIGARH"                         
# [3] "DADRA & NAGAR HAVELI"               
# [4] "DAMAN & DIU"                        
# [5] "NATIONAL CAPITAL TERRITORY OF DELHI"
# [6] "LAKSHADWEEP"                        
# [7] "PONDICHERRY"  

# # Select the 10th element from dist_shape
# dist_shape_subset <- dist_shp[10, ]
# 
# # Select the specified elements from pc_shp
# pc_shp_subset <- pc_shp_valid[c(378,  380,  428), ]
# 
# # Plot both subsets together
# ggplot() +
#   geom_sf(data = dist_shape_subset, fill = "blue", color = "green",linewidth = 1.5, alpha = 0.5) +
#   geom_sf(data = pc_shp_subset, fill = "red", color = "black", alpha = 0.5) +
#   theme_minimal() +
#   labs(title = "Selected Elements from dist_shape and pc_shp")


# Unique ID for each admin district

pc_shp_valid <- pc_shp_valid %>%
  mutate(ST_NAME=str_to_title(ST_NAME)) %>% 
  mutate(
    ST_NAME = case_when(
      ST_NAME == "Uttaranchal" ~ "Uttarakhand",
      ST_NAME == "Arunachal Pradesh" ~ "Arunanchal Pradesh",
      ST_NAME == "Jammu & Kashmir" ~ "Jammu and Kashmir",
      ST_NAME == "Tamilnadu" ~ "Tamil Nadu",
      ST_NAME == "Delhi" ~ "Delhi & NCR",
      ST_NAME == "Orissa" ~ "Odisha",
      ST_NAME == "Pondicherry" ~ "Puducherry",
      ST_NAME == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar Island",
      ST_NAME == "Dadra & Nagar Haveli" ~ "Dadara & Nagar Havelli",
      ST_NAME == "Daman and Diu" ~ "Daman & Diu",
      TRUE ~ ST_NAME  # Keep the state as-is if no changes are needed
    )
  )



pc_shp_valid <- pc_shp_valid %>%
  mutate(election_id = paste0( row_number()))  # Unique ID for each election district

# Convert sf objects to geos_geometry for faster processing
admin_geos <- as_geos_geometry(dist_shp)
election_geos <- as_geos_geometry(pc_shp_valid)

# Create an empty list to store results
intersection_results <- list()

# Perform intersections
for (i in seq_along(admin_geos)) {
  # Find intersections with each admin district geometry
  admin_geom <- admin_geos[i]
  intersects <- geos_intersects(admin_geom, election_geos)
  
  # For intersecting election districts, calculate intersection areas
  if (any(intersects)) {
    intersected_geoms <- geos_intersection(admin_geom, election_geos[intersects])
    intersection_areas <- geos_area(intersected_geoms)
    
    # Filter out cases with zero intersection area (i.e., only boundaries touch)
    valid_intersections <- intersection_areas > 0
    
    # Only store results with non-zero intersection area
    if (any(valid_intersections)) {
      # Calculate area of each intersected election district geometry
      election_areas <- geos_area(election_geos[intersects][valid_intersections])
      
      results <- data.frame(
        dist_id = dist_shp$DT_CEN_CD[i],  # Use the newly created unique admin ID
        dist_name=dist_shp$DISTRICT[i],
        pc_id = pc_shp_valid$PC_CODE[intersects][valid_intersections],  # Use the unique election ID
        pc_name=pc_shp_valid$PC_NAME[intersects][valid_intersections],
        pc_type=pc_shp_valid$PC_TYPE[intersects][valid_intersections],
        state_pc=tolower(pc_shp_valid$ST_NAME[intersects][valid_intersections]),
        state_dist=tolower(dist_shp$ST_NM[i]),
        state_code_dist=dist_shp$ST_CEN_CD[i],
        state_code_pc=pc_shp_valid$ST_CODE[intersects][valid_intersections],
        intx_area = intersection_areas[valid_intersections],
        dist_area = geos_area(admin_geom),
        dist_area_pct= (intersection_areas[valid_intersections] / geos_area(admin_geom)) * 100,
        pc_area = election_areas,  
        pc_area_pct = (intersection_areas[valid_intersections] / election_areas) * 100
        
        
      )
      
      intersection_results[[i]] <- results
    }
  }
}

# Combine results into a single dataframe
pc_dist_table_pre <- bind_rows(intersection_results)
pc_dist_table_pre=pc_dist_table_pre %>% filter(dist_area_pct>=1) %>% 
  filter(pc_area_pct>=1) %>% 
  mutate(delim_id=3) %>% distinct()
#mutate(pc_id=sub("^S", "", pc_id))






# 3 READING PRE DELIM AC AND INTERSECTING IT WITH DISTRICT---------------

# Define the directory path
shp_path <- "../Data/MAPS- INDIA/pre-delim/ac_all_final_april_2012/AC_All_Final.shp"

# Read the shapefile
ac_shp <- st_read(shp_path)

ac_shp <- ac_shp %>%
  filter(st_is_valid(geometry))

# Add unique IDs to each geometry
ac_shp <- ac_shp %>%
  mutate(ac_id = paste0( row_number())) 

# Calculate total area for each administrative district
ac_shp <- ac_shp %>%
  mutate(ac_area = st_area(geometry))

# ggplot(data = ac_shp) +
#   geom_sf(color = "black", fill = NA, size = 0.2) +
#   theme_minimal() +
#   labs(
#     title = "State Assembly Constituencies",
#     subtitle = "Boundary Map of Assembly Constituencies",
#     caption = "Source: ac_all_final_april_2012 Shapefile"
#   )

# 

ac_shp <- ac_shp %>%
  mutate(
    State = case_when(
      State == "Uttaranchal" ~ "Uttarakhand",
      State == "Arunachal Pradesh" ~ "Arunanchal Pradesh",
      State == "Jammu & Kashmir" ~ "Jammu and Kashmir",
      State == "Tamilnadu" ~ "Tamil Nadu",
      State == "Delhi" ~ "Delhi & NCR",
      State == "Orissa" ~ "Odisha",
      State == "Pondicherry" ~ "Puducherry",
      State == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar Island",
      State == "Dadra & Nagar Haveli" ~ "Dadara & Nagar Havelli",
      State == "Daman and Diu" ~ "Daman & Diu",
      TRUE ~ State  # Keep the state as-is if no changes are needed
    )
  )


pre_ac_pc_dist=ac_shp %>% clean_names() %>% 
  as.data.frame() %>% 
  dplyr::select(dist_name,ac_name,state,pc_name) %>% 
  distinct() %>% rename(st_name=state) %>% 
  mutate_all(tolower) %>% 
  mutate(delim_id=3)


# Convert sf objects to geos_geometry for faster processing
admin_geos <- as_geos_geometry(dist_shp)
election_geos <- as_geos_geometry(ac_shp)

# Create an empty list to store results
intersection_results <- list()

# Perform intersections
for (i in seq_along(admin_geos)) {
  # Find intersections with each admin district geometry
  admin_geom <- admin_geos[i]
  intersects <- geos_intersects(admin_geom, election_geos)
  
  # For intersecting election districts, calculate intersection areas
  if (any(intersects)) {
    intersected_geoms <- geos_intersection(admin_geom, election_geos[intersects])
    intersection_areas <- geos_area(intersected_geoms)
    
    # Filter out cases with zero intersection area (i.e., only boundaries touch)
    valid_intersections <- intersection_areas > 0
    
    # Only store results with non-zero intersection area
    if (any(valid_intersections)) {
      # Calculate area of each intersected election district geometry
      election_areas <- geos_area(election_geos[intersects][valid_intersections])
      
      results <- data.frame(
        dist_id = dist_shp$admin_id[i],  # Use the newly created unique admin ID
        dist_name=tolower(dist_shp$DISTRICT[i]),
        dist_ac=tolower(ac_shp$DIST_NAME[intersects][valid_intersections]),
        ac_id = ac_shp$ac_id[intersects][valid_intersections],  # Use the unique election ID
        ac_name=ac_shp$AC_NAME[intersects][valid_intersections],
        ac_type=ac_shp$AC_TYPE[intersects][valid_intersections],
        state_ac=tolower(ac_shp$State[intersects][valid_intersections]),
        state_dist=tolower(dist_shp$ST_NM[i]),
        intx_area = intersection_areas[valid_intersections],
        dist_area = geos_area(admin_geom),
        dist_area_pct= (intersection_areas[valid_intersections] / geos_area(admin_geom)) * 100,
        ac_area = election_areas,  
        ac_area_pct = (intersection_areas[valid_intersections] / election_areas) * 100
        
        
      )
      
      intersection_results[[i]] <- results
    }
  }
}


ac_dist_table_pre <- bind_rows(intersection_results)


ac_dist_table_pre=ac_dist_table_pre %>% 
          mutate(mismatch_dum1=if_else(state_ac!=state_dist, 0,1),
                 mismatch_dum2=if_else(dist_area_pct<=1,0,1)) %>% 
  mutate(delim_id=3)
        

# THere are bunch of mistmatch particularly at the state borders
#  tagged under mismatch_dum1. Basically, border ACs seems to be 
# mistagged in the AC_shapefile only (from Sandeep). Perhaps a better shapefile 
# might fix this. 


# 4   READING POST DELIM AC  and intersecting-------------------------------------------------



# Define the directory path
base_dir <- "../Data/MAPS- INDIA/AC/maps-master/assembly-constituencies/India_AC.shp"

ac_shp_post=st_read(base_dir)

# # List all assembly files
# flist=list.files(base_dir, 
#                     pattern= "assembly\\.shp$", 
#                     full.names = TRUE, 
#                  recursive = T)
# 
# shp_list=lapply(flist, st_read)
# ac_shp_post=shp_list %>% bind_rows()


# Ensure both layers have the same CRS

if (st_crs(dist_shp) != st_crs(ac_shp_post)) {
  ac_shp_post <- st_transform(ac_shp_post, st_crs(dist_shp))
}


# Calculate total area for each election district (optional, if needed)
ac_shp_post <- ac_shp_post %>%
  st_make_valid()
ac_shp_post <- ac_shp_post %>%
  filter(st_is_valid(geometry))

ac_shp_post <- ac_shp_post %>%
  mutate(election_area = st_area(geometry))

# Unique ID for each admin district

ac_shp_post <- ac_shp_post %>%
  mutate(election_id = paste0( row_number()))  # Unique ID for each election district

post_ac_pc_dist=ac_shp_post %>% clean_names() %>% 
  as.data.frame() %>% 
  dplyr::select(dist_name,ac_name,st_name,pc_name) %>% 
  distinct() %>% 
  mutate_all(tolower) %>% 
  mutate(delim_id=4)


# Convert sf objects to geos_geometry for faster processing
admin_geos <- as_geos_geometry(dist_shp)
election_geos <- as_geos_geometry(ac_shp_post)

# Create an empty list to store results
intersection_results <- list()

# Perform intersections
for (i in seq_along(admin_geos)) {
  # Find intersections with each admin district geometry
  admin_geom <- admin_geos[i]
  intersects <- geos_intersects(admin_geom, election_geos)
  
  # For intersecting election districts, calculate intersection areas
  if (any(intersects)) {
    intersected_geoms <- geos_intersection(admin_geom, election_geos[intersects])
    intersection_areas <- geos_area(intersected_geoms)
    
    # Filter out cases with zero intersection area (i.e., only boundaries touch)
    valid_intersections <- intersection_areas > 0
    
    # Only store results with non-zero intersection area
    if (any(valid_intersections)) {
      # Calculate area of each intersected election district geometry
      election_areas <- geos_area(election_geos[intersects][valid_intersections])
      
      results <- data.frame(
        dist_id = dist_shp$admin_id[i],  # Use the newly created unique admin ID
        dist_name=dist_shp$DISTRICT[i],
        ac_no = ac_shp_post$AC_NO[intersects][valid_intersections],  # Use the unique election ID
        ac_name=ac_shp_post$AC_NAME[intersects][valid_intersections],
        #pc_type=pc_shp_valid$PC_TYPE[intersects][valid_intersections],
        state_ac=tolower(ac_shp_post$ST_NAME[intersects][valid_intersections]),
        state_id=dist_shp$ST_CEN_CD[i],
        state_dist=tolower(dist_shp$ST_NM[i]),
        intx_area = intersection_areas[valid_intersections],
        dist_area = geos_area(admin_geom),
        dist_area_pct= (intersection_areas[valid_intersections] / geos_area(admin_geom)) * 100,
        ac_area = election_areas,  
        ac_area_pct = (intersection_areas[valid_intersections] / election_areas) * 100
        
        
      )
      
      intersection_results[[i]] <- results
    }
  }
}






# Combine results into a single dataframe
ac_dist_table_post <- bind_rows(intersection_results)
ac_dist_table_post=ac_dist_table_post %>% 
  filter(!is.na(ac_name)) %>% 
  mutate(mismatch_dum1=if_else(state_ac!=state_dist, 0,1),
         mismatch_dum2=if_else(dist_area_pct<=1,0,1)) %>% 
  filter(mismatch_dum2==1)

ac_dist_table_post=ac_dist_table_post %>% 
  mutate(ac_no=padzero(ac_no, 3))


# Now bringing AC_TYPE from the TCPD AC ELECTION DATA

ac_dist_table_post=ac_dist_table_post %>% 
  inner_join(post_ac) %>% mutate(delim_id=4)




# 5  READING POST DELIM PC  and intersecting-------------------------------------------------

# Define the directory path
base_dir <- "../Data/MAPS- INDIA/AC/maps-master/parliamentary-constituencies/india_pc_2019.shp"

pc_shp_post=st_read(base_dir)
pc_shp_post <- pc_shp_post %>%
  st_make_valid()
pc_shp_post <- pc_shp_post %>%
  filter(st_is_valid(geometry))

# Rough
# pc_shp_post_rg=pc_shp_post %>%
#   as.data.frame() %>% 
#   mutate(pc_id=padzero(PC_CODE, 2)) %>% 
#   arrange(ST_NAME, pc_id) %>% 
#   dplyr::select(ST_NAME,ST_CODE,PC_NAME, pc_id) %>% 
#   distinct() %>% group_by(ST_NAME) %>% 
#   mutate(pc_count=n_distinct(pc_id)) %>% ungroup() %>% 
#   filter(str_to_title(ST_NAME)!="Andhra Pradesh")
# 
# pc_name_post=pc_shp_post_rg %>% 
#   mutate(pc_name=trimws(gsub("\\s*\\(.*\\)", "", PC_NAME))) %>% 
#   mutate(pc_name=tolower(pc_name))
# pc_name_post=unique(pc_name_post$pc_name)
# 
# not_in_post <- setdiff(pc_name_pre, pc_name_post)
# 
# # Find names in pc_name_post that are not in pc_name_pre
# not_in_pre <- setdiff(pc_name_post, pc_name_pre)


#Rough

# Ensure both layers have the same CRS
st_crs(pc_shp_post) <- 4326
if (st_crs(dist_shp) != st_crs(pc_shp_post)) {
  pc_shp_post <- st_transform(pc_shp_post, st_crs(dist_shp))
}

pc_shp_post <- pc_shp_post %>%
  mutate(election_area = st_area(geometry))


pc_shp_post <- pc_shp_post %>%
  mutate(ST_NAME=str_to_title(ST_NAME)) %>% 
  mutate(
    ST_NAME = case_when(
      ST_NAME == "Uttaranchal" ~ "Uttarakhand",
      ST_NAME == "Arunachal Pradesh" ~ "Arunanchal Pradesh",
      ST_NAME == "Jammu & Kashmir" ~ "Jammu and Kashmir",
      ST_NAME == "Tamilnadu" ~ "Tamil Nadu",
      ST_NAME == "Delhi" ~ "Delhi & NCR",
      ST_NAME == "Orissa" ~ "Odisha",
      ST_NAME == "Pondicherry" ~ "Puducherry",
      ST_NAME == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar Island",
      ST_NAME == "Dadra & Nagar Haveli" ~ "Dadara & Nagar Havelli",
      ST_NAME == "Daman and Diu" ~ "Daman & Diu",
      TRUE ~ ST_NAME  # Keep the state as-is if no changes are needed
    )
  )

# Unique ID for each admin district

pc_shp_post <- pc_shp_post %>%
  mutate(election_id = paste0( row_number()))  # Unique ID for each election district

# Convert sf objects to geos_geometry for faster processing
admin_geos <- as_geos_geometry(dist_shp)
election_geos <- as_geos_geometry(pc_shp_post)

# Create an empty list to store results
intersection_results <- list()

# Perform intersections
for (i in seq_along(admin_geos)) {
  # Find intersections with each admin district geometry
  admin_geom <- admin_geos[i]
  intersects <- geos_intersects(admin_geom, election_geos)
  
  # For intersecting election districts, calculate intersection areas
  if (any(intersects)) {
    intersected_geoms <- geos_intersection(admin_geom, election_geos[intersects])
    intersection_areas <- geos_area(intersected_geoms)
    
    # Filter out cases with zero intersection area (i.e., only boundaries touch)
    valid_intersections <- intersection_areas > 0
    
    # Only store results with non-zero intersection area
    if (any(valid_intersections)) {
      # Calculate area of each intersected election district geometry
      election_areas <- geos_area(election_geos[intersects][valid_intersections])
      
      results <- data.frame(
        dist_id = dist_shp$DT_CEN_CD[i],  # Use the newly created unique admin ID
        dist_name=dist_shp$DISTRICT[i],
        pc_id = pc_shp_post$PC_CODE[intersects][valid_intersections],  # Use the unique election ID
        pc_name=pc_shp_post$PC_NAME[intersects][valid_intersections],
        pc_type=pc_shp_post$Res[intersects][valid_intersections],
        state_pc=tolower(pc_shp_post$ST_NAME[intersects][valid_intersections]),
        state_dist=tolower(dist_shp$ST_NM[i]),
        state_code_dist=dist_shp$ST_CEN_CD[i],
        state_code_pc=pc_shp_post$ST_CODE[intersects][valid_intersections],
        intx_area = intersection_areas[valid_intersections],
        dist_area = geos_area(admin_geom),
        dist_area_pct= (intersection_areas[valid_intersections] / geos_area(admin_geom)) * 100,
        pc_area = election_areas,  
        pc_area_pct = (intersection_areas[valid_intersections] / election_areas) * 100
        
        
      )
      
      intersection_results[[i]] <- results
    }
  }
}

# Combine results into a single dataframe
pc_dist_table_post <- bind_rows(intersection_results)


# # MERGING IN STATE CODE FROM OUTSIDE (CODES ARE SAME AS IN ST_CEN_CODE)
# dd =pc_dist_table_post %>% 
#   mutate(state_pc=str_replace_all(state_pc, " ", "_")) %>% 
#   left_join(state_code, by=c("state_pc"="state"))
# dd=dd %>% dplyr::select(state_pc, state_dist, state_code_dist, 
#                         state_code_pc, state_id) %>% distinct()


pc_dist_table_post=pc_dist_table_post %>% filter(dist_area_pct>=1) %>% 
  filter(pc_area_pct>=1) %>% 
  mutate(delim_id=4) %>% distinct()
 # mutate(pc_id=padzero(pc_id, 2)) %>% 
 # mutate(pc_id=paste0(state_code_pc, pc_id))



# 6 MERGING PRE PC GEO AND POST PC GEO ----------------------------------



## FIRST MATCHING BY NAMES 
# Pre-period data: Extract and clean
pre_pc_df <- pc_shp_valid %>%
  as.data.frame() %>%
  clean_names()

pre_pc_name <- pre_pc_df %>%
  dplyr::select(st_name, pc_name, pc_type) %>%
  mutate(
    st_name = tolower(st_name),
    pc_name = tolower(pc_name)
  ) %>%
  distinct() %>%
  rename(pre_pc_type = pc_type)

pre_pc_name$st_name[pre_pc_name$st_name == "national capital territory of delhi"] <- "delhi  & ncr"

# Post-period data: Extract and clean
post_pc_df <- pc_shp_post %>%
  as.data.frame() %>%
  clean_names()

post_pc_name <- post_pc_df %>%
  dplyr::select(st_name, pc_name, res) %>%
  mutate(
    st_name = tolower(st_name),
    pc_name = tolower(pc_name),
    pc_name = gsub("\\s*\\(.*\\)", "", pc_name),
    pc_name = str_trim(pc_name)
  ) %>%
  distinct() %>%
  rename(post_pc_type = res)

# Merge pre and post data by names
pre_post_name_merge <- pre_pc_name %>%
  inner_join(post_pc_name, by = c("st_name", "pc_name")) %>%
  mutate(name_merge = 1)

# Create a long format dataset with `delim_id` and `pc_type`
pre_post_name_merge <- pre_post_name_merge %>%
  uncount(2) %>%  # Duplicate each row twice
  group_by(st_name, pc_name) %>%
  mutate(
    delim_id = c(3, 4),  # Assign delim_id
    pc_type = if_else(delim_id == 3, pre_pc_type, post_pc_type)  # Select pc_type based on delim_id
  ) %>%
  ungroup() %>% 
  dplyr::select(-pre_pc_type, post_pc_type) %>% distinct()

# 367 are merged here. Can take them out perhaps for spatial match

# So we can already take out these from the shapefile before intersection

pre_shp=pc_shp_valid %>% 
  mutate(ST_NAME=tolower(ST_NAME), 
         PC_NAME=tolower(PC_NAME)) %>% 
  anti_join(pre_post_name_merge, 
            by = c("PC_NAME" = "pc_name", "ST_NAME" = "st_name"))

post_shp=pc_shp_post %>% 
  mutate(ST_NAME=tolower(ST_NAME), PC_NAME=tolower(PC_NAME)) %>% 
   mutate(PC_NAME=gsub("\\s*\\(.*\\)", "", PC_NAME)) %>% 
  mutate(PC_NAME=str_trim(PC_NAME)) %>% 
  anti_join(pre_post_name_merge, 
            by = c("PC_NAME" = "pc_name", "ST_NAME" = "st_name"))

  
# Convert sf objects to geos geometries
pre_delim_geom <- as_geos_geometry(pre_shp)
post_delim_geom <- as_geos_geometry(post_shp)

# Initialize an empty list to store results
intersections_list <- list()

# Loop through each pre-delim geometry and find intersections with post-delim geometries
for (i in seq_along(pre_delim_geom)) {
  # Get intersections of the current pre-delim geometry with all post-delim geometries
  intersected_geoms <- geos_intersection(pre_delim_geom[i], post_delim_geom)
  
  # Calculate the area of each intersection
  intersection_areas <- geos_area(intersected_geoms)
  
  # Filter out empty geometries (no intersection)
  non_empty <- intersection_areas > 0
  
  # Only collect results if there are non-empty intersections
  if (any(non_empty)) {
    # Collect results for the current pre-delim geometry
    intersections_list[[i]] <- data.frame(
      pre_state = pre_shp$ST_NAME[i],
      pre_pc_code = pre_shp$PC_CODE[i],
      pre_pc_name = pre_shp$PC_NAME[i],
      pre_pc_type = pre_shp$PC_TYPE[i],
      post_state = post_shp$ST_NAME[non_empty],
      post_st_code = post_shp$ST_CODE[non_empty],
      post_pc_no = post_shp$PC_CODE[non_empty],
      post_pc_name = post_shp$PC_NAME[non_empty],
      post_pc_type = post_shp$Res[non_empty],
      intersect_area = intersection_areas[non_empty]
    )
  }
}

# Combine results from each pre-delim geometry into a single data frame
pre_post_pc_table <- bind_rows(intersections_list)


pre_post_pc_table=pre_post_pc_table %>% 
  mutate(post_pc_code=paste0(post_st_code, padzero(post_pc_no,2))) %>% 
  rename(pre_st_name=pre_state, post_st_name=post_state)

pre_post_pc_table=pre_post_pc_table %>% 
  group_by(pre_st_name, pre_pc_name) %>% 
  mutate(area_max1=max(intersect_area, na.rm=T)) %>% 
  mutate(area_tot1=sum(intersect_area, na.rm=T)) %>% 
  mutate(max_match1=intersect_area==area_max1) %>% 
  mutate(match_pct1=intersect_area/area_tot1) %>% 
  ungroup()# %>% 
  # group_by(post_st_name, post_pc_code) %>% 
  # mutate(area_max2=max(intersect_area, na.rm=T)) %>% 
  # mutate(area_tot2=sum(intersect_area, na.rm=T)) %>% 
  # mutate(max_match2=intersect_area==area_max2) %>% 
  # mutate(match_pct2=intersect_area/area_tot2) %>% 
  # ungroup() %>% 
  # arrange(pre_st_name, pre_pc_code, desc(intersect_area))




# For each pre-delim constituency, find the post-delim constituency with the maximum intersected area
pre_to_post_max <- pre_post_pc_table %>%
  group_by(pre_st_name, pre_pc_name) %>%
  slice_max(intersect_area, n = 1) %>%
  ungroup() %>%
  dplyr::select(pre_st_name, pre_pc_name, post_st_name, pre_pc_type,
                post_pc_type,
                post_pc_name, intersect_area,match_pct1)


# JUST USING PRE_TO_POST as THAT IS MORE NATURAL TO CARRY ALONG PREVIOUS PC DATA


pre_post_map=pre_to_post_max %>% 
  rename(st_name_3=pre_st_name, 
         st_name_4=post_st_name, 
         pc_type_3=pre_pc_type, 
         pc_type_4=post_pc_type,
         pc_name_3=pre_pc_name, 
         pc_name_4=post_pc_name)

# Step 1: Create unique pc_uid based on st_name_3 and pc_name_3
# Step 1: Create unique pc_uid based on st_name_3 and pc_name_3
pre_post_map <- pre_post_map %>%
  mutate(pc_uid = as.numeric(factor(paste(st_name_3, pc_name_3))))

# Step 2: Pivot to long format with delim_id
pre_post_long <- pre_post_map %>%
  pivot_longer(
    cols = c(starts_with("st_name_"), starts_with("pc_name_"), starts_with("pc_type_")),
    names_to = c(".value", "delim_id"),
    names_sep = "_(?=[^_]+$)"  # Separate at the last underscore
  ) %>%
  mutate(delim_id = as.integer(delim_id)) %>%
  dplyr::select(pc_uid, delim_id, st_name, pc_name, pc_type, 
                match_pct1)


pre_post_long=pre_post_long %>% 
  mutate(
    st_name = case_when(
      st_name == "Uttaranchal" ~ "Uttarakhand",
      st_name == "Arunachal Pradesh" ~ "Arunanchal Pradesh",
      st_name == "Jammu & Kashmir" ~ "Jammu and Kashmir",
      st_name == "Tamilnadu" ~ "Tamil Nadu",
      st_name == "national capital territory of delhi" ~ "delhi & ncr",
      st_name == "orissa" ~ "odisha",
      st_name == "andaman & nicobar island" ~ "andaman & nicobar",
      st_name == "Dadra & Nagar Haveli" ~ "Dadara & Nagar Havelli",
      st_name == "Daman and Diu" ~ "Daman & Diu",
    TRUE ~ st_name  # Keep the state as-is if no changes are needed
  ))
  
  

side_df=pre_post_long %>% filter(delim_id==4) %>% 
  group_by(st_name, pc_name) %>% mutate(post_st_pc_dup=n()>1) %>% 
  ungroup()

pre_post_long=pre_post_long %>% left_join(side_df) %>% 
  arrange(pc_uid, delim_id) %>% 
  mutate(post_st_pc_dup=if_else(is.na(post_st_pc_dup), FALSE, post_st_pc_dup))



# I AM ALSO COMPUTING INTERSECTION % FOR THE NAME MATCHED SAMPLE

name_match=pre_post_name_merge %>%
  dplyr::select(st_name, pc_name) %>% distinct()

# Filter shapefiles to keep only the name-matched entries
pre_shp_matched <- pc_shp_valid %>%
    mutate(ST_NAME = tolower(ST_NAME), PC_NAME = tolower(PC_NAME)) %>%
  mutate(ST_NAME=if_else(ST_NAME=="national capital territory of delhi",
                         "delhi & ncr", ST_NAME),
         ST_NAME=if_else(ST_NAME=="orissa",
                         "odisha", ST_NAME)) %>% 
  inner_join(name_match, 
             by = c("PC_NAME" = "pc_name", "ST_NAME" = "st_name")) %>% 
  arrange(ST_NAME, PC_NAME)

post_shp_matched <- pc_shp_post %>%
  mutate(ST_NAME = tolower(ST_NAME), PC_NAME = tolower(PC_NAME)) %>%
  mutate(PC_NAME = gsub("\\s*\\(.*\\)", "", PC_NAME)) %>%
  mutate(PC_NAME = str_trim(PC_NAME)) %>%
  inner_join(name_match, 
             by = c("PC_NAME" = "pc_name", "ST_NAME" = "st_name")) %>% 
  arrange(ST_NAME, PC_NAME)



#ROUGH

# plot_state(pre_shp_matched, 
#            post_shp_matched, 
#            "madhya pradesh", 
#            "bhopal")
# 


# Ensure CRS consistency
if (st_crs(pre_shp_matched) != st_crs(post_shp_matched)) {
  post_shp_matched <- st_transform(post_shp_matched, st_crs(pre_shp_matched))
}

# Convert sf objects to geos geometries
pre_geoms <- as_geos_geometry(pre_shp_matched)
post_geoms <- as_geos_geometry(post_shp_matched)

# Compute intersections and their areas
intersections <- geos_intersection(pre_geoms, post_geoms)
intersection_areas <- geos_area(intersections)

# Compute individual areas
pre_areas <- geos_area(pre_geoms)
post_areas <- geos_area(post_geoms)

# Create results with match percentages
name_match_pct <- pre_shp_matched %>%
  mutate(
    intersection_area = intersection_areas,
    pre_area = pre_areas,
    post_area = post_areas,
    match_pct1 = (intersection_area / pre_area)) %>%
  dplyr::select(ST_NAME, PC_NAME, 
                match_pct1) %>% 
  clean_names() %>% as.data.frame() 


pre_post_name_merge=pre_post_name_merge %>% 
  inner_join(name_match_pct) 




# FINAL MAPPING 
# THIS IS APPEND OF NAME MERGE AND SPATIAL MERGE
# THIS IS JUST PRE-POST PC MAP:
# PC IN PRE PERIOD ARE LINKED TO PC IN THE MOST PERIOD FIRST BY NAME
# THEN WHATEVER PC IN THE POST HAS MAXIMUM INTERSECTED AREA WITH THE PRE PC

# NOTE: this can lead to SAME PC in the post period getting linked to two
# different PC in the pre period.  There are 19 such PC, leading to 42 rows of PC
# being present
#  OR just a wrong assignment

# For wrong assignment, I can see bottom 25ptile of the match_percentage
# > summary(pre_post_long$match_pct1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4360  0.6861  0.8783  0.8304  0.9896  1.0000 

# By this estimate there are 84 PC wrongly assigned with match percentage 
# of 66% or less. (To be clear, they might be correct even .. 84 just just a 
# very rough estimate)

max_existing_id <- max(pre_post_long$pc_uid, na.rm = TRUE)

# Step 2: Assign new unique IDs to rows in pre_post_name_merge
pre_post_name_merge <- pre_post_name_merge %>%
  mutate(dd=as.numeric(as.factor(paste0(st_name, pc_name)))) %>% 
  mutate( pc_uid = max_existing_id + dd  ) %>% 
  dplyr::select(-dd)
  


final_pc_map=pre_post_long %>% bind_rows(pre_post_name_merge) 

final_pc_map=final_pc_map %>% ungroup() %>% 
  mutate(name_merge=if_else(is.na(name_merge), 0, name_merge)) %>% 
  mutate(post_st_pc_dup=if_else(is.na(post_st_pc_dup), FALSE, 
                                post_st_pc_dup))









# 8 CREATING AC-PC-DIST PANEL WITH PRE/POST PANEL --------------------------------------------

#( OKAY THIS BELOW SEEMS A MUCH BETTER WAY)
ac_pc_dist=pre_ac_pc_dist %>% 
  bind_rows(post_ac_pc_dist)

ac_pc_dist=ac_pc_dist %>% 
  mutate(pc_name = gsub("\\s*\\(.*\\)", "", pc_name)) %>%
  mutate(pc_name = str_trim(pc_name))

ac_pc_dist_delim=final_pc_map %>% 
  left_join(ac_pc_dist) %>% 
  dplyr::select( -geometry) %>% distinct() %>% 
  arrange(pc_uid, st_name, pc_name)
#Joining with `by = join_by(delim_id, st_name, pc_name)`

# Assuming ac_pc_dist_delim_map is your data frame
ac_pc_dist_delim_map <- ac_pc_dist_delim %>%
  # Replace spaces with underscores in st_name
  mutate(
    st_name = str_replace_all(st_name, " ", "_"),
    # Extract the values inside parentheses for ac_name
    ac_type = str_extract(ac_name, "\\(([^)]+)\\)") %>% str_remove_all("[()]"),
    # Remove the parentheses and their content from ac_name
    ac_name = str_trim(str_remove(ac_name, "\\s*\\(.*\\)"))
  )

# Convert ac_category to uppercase (optional, if needed)
ac_pc_dist_delim_map <- ac_pc_dist_delim_map %>%
  mutate(ac_type = str_to_upper(ac_type)) %>% 
  mutate(ac_type=if_else(is.na(ac_type), "GEN", ac_type))



# We can check for two admin rules:
# 1 PC per AC
# 1 District per AC
# 
check_df=ac_pc_dist_delim %>% group_by(st_name, ac_name, delim_id) %>% 
  mutate(pc_per_ac=n_distinct(st_name, pc_name)) %>% 
  group_by(st_name, dist_name, delim_id) %>% 
  mutate(dist_per_ac=n_distinct(st_name, dist_name)) %>% ungroup()

# There are some messups, but by far it looks very good. Can deal with issues later

# > table(check_df$pc_per_ac)
# 
# 1    2    3    5    6    7    9   15 
# 7493   43    3    5    6   18    9   15 
# > table(check_df$dist_per_ac)
# 
# 1 
# 7592 

qsave(ac_pc_dist_delim_map, 
      file="data/2 CLEAN/ac_pc_dist_delim_map.qs")


