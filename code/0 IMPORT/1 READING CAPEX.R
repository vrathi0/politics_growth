


# 0 NOTES -----------------------------------------------------------------

# WHAT DOING?
# 1. Reading Capex data
# 2. Basic stats (maybetter in a notebook)





# 1 HEADER ----------------------------------------------------------------




# 2 READING DATA ----------------------------------------------------------

fpath <- "../Data/UCB/CMIE Capex/All_projects_in_database_since_1995_20240331000000"

flist <- list.files(fpath, full.names = TRUE)

# Extract file names without extension for naming the list elements
file_names <- flist %>%
  basename() %>% # Get the base file name
  tools::file_path_sans_ext() %>%  # Remove the file extension
  tolower()

# Read all files into a list and name the list elements
capex_list <- map(flist, ~ read_delim(.x, delim = "|", 
                                      escape_double = FALSE, 
                                      trim_ws = TRUE) %>% 
                    clean_names()) %>% set_names(file_names)




# names_list=lapply(capex_list, names)
# key_cols=Reduce(intersect, names_list)
# capex_full <- Reduce(function(x, y) full_join(x, y, 
#                                               by = intersect(names(x), 
#                                                              names(y))), capex_list)
# all_names=names(capex_full)



qsave(capex_full, 
      "data/0 RAW/capex_full.qs")


qsave(capex_list, 
      "data/0 RAW/capex_list.qs")




