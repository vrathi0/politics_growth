

# NOTE: 
# 1. Often I have to deal with getting a time variant list of geo units. 
# for eg, districts often change, or new states are created, 
# or new any type of units (while old one cease to exits). 

#. 2. In this file, I will keep recording various lists, 
# so it would be easier to keep them consistent (as compared to 
#. doing something ad-hoc in some code file and then forgetting about it)

# 3. One such file already exists 1_AC_PC_DIST_PRE_POST_DELIM_MAP.R
#.  but this is solely dedicated to mapping pre and post delim AC/PC. 

# List of sources: 
# 1. LGD: https://lgdirectory.gov.in/


# List of folders:
#./Data/LGD
# /Data/MAPS- INDIA
# /Data/India Govt Structure data

# 0 HEADER ----------------------------------------------------------------

rm(list=ls())
source("code/functions.R")


standardize_state_names <- function(state_name) {
  
  state_name=tolower(state_name) 
  state_name <- dplyr::case_when(
    state_name %in% c("uttarakhand", "uttarkhand") ~ "uttaranchal",
    state_name == "orissa" ~ "odisha",
    state_name %in% c("arunanchal pradesh") ~ "arunachal pradesh",
    state_name %in% c("jammu and kashmir", "jammu & kashmir",
                      "jammu_&_kashmir","jammu kashmir") ~ "jammu and kashmir",
    state_name == "tamilnadu" ~ "tamil nadu",
    state_name %in% c("delhi", "national capital territory of delhi", 
                      "delhi & ncr") ~ "delhi & ncr",
    state_name == "puducherry" ~ "pondicherry",
    state_name %in% c("andaman & nicobar islands") ~ "andaman & nicobar island",
    state_name == "dadra & nagar haveli" ~ "dadara & nagar havelli",
    state_name == "daman and diu" ~ "daman & diu",
    TRUE ~ state_name  # Retain unchanged state names
  )
  
  # Replace spaces with underscores in the resulting state names
  state_name <- stringr::str_replace_all(state_name, " ", "_")
  
  return(state_name)
}




# 1 DISTRICTS ----------------------------------------------------------------

lgd_districts=read_csv("../data/LGD/26Oct2024/districts.csv") %>% 
  clean_names() %>% 
  rename(st_name=state_name_in_english, 
         dist_name=district_name_in_english) %>% 
  mutate(st_name=standardize_state_names(st_name),
         dist_name=tolower(dist_name))#







# 2 STATES ----------------------------------------------------------------

# reading language based classification of states:


# Create a tibble with the state-to-group mapping, including the new "Eastern" group.
lang_st_class <- tibble(
  st_name = c("gujarat", "assam", "meghalaya", "tamil_nadu", "karnataka", "west_bengal",
                 "odisha", "uttar_pradesh", "kerala", "bihar", "assam_meghalya", "arunachal_pradesh",
                 "delhi_&_ncr", "maharashtra", "punjab", "manipur", "-", "rajasthan",
                 "himachal_pradesh", "andhra_pradesh", "madhya_pradesh", "chandigarh",
                 "jharkhand", "haryana", "pondicherry", "telangana", "sikkim",
                 "jammu_and_kashmir", "nagaland", "not_found", "uttaranchal", "mizoram",
                 "chhattisgarh", "others", "tripura", "manipur-tripura", "a_g_m_u_t",
                 "goa", "andaman_&_nicobar", "ladakh"),
  st_lang_grp = c("Indo-Aryan", "North-East", "North-East", "Dravidian", "Dravidian", "Eastern",
                  "Eastern", "Indo-Aryan", "Dravidian", "Eastern", "North-East", "North-East",
                  "Indo-Aryan", "Indo-Aryan", "Indo-Aryan", "North-East", "Unknown", "Indo-Aryan",
                  "Indo-Aryan", "Dravidian", "Indo-Aryan", "Indo-Aryan", "Eastern", "Indo-Aryan",
                  "Dravidian", "Dravidian", "North-East", "Indo-Aryan", "North-East", "Unknown",
                  "Indo-Aryan", "North-East", "Indo-Aryan", "Unknown", "North-East", "North-East",
                  "Unknown", "Indo-Aryan", "Unknown", "Unknown")
)


