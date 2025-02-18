




# Function to transform IAS data to a monthly structure
transform_ias_data <- function(ias_data) {
  expanded_data <- ias_data %>%
    # Remove rows with NA start_date or end_date
    filter(!is.na(start_date) & !is.na(end_date) & start_date <= end_date) %>%
    mutate(
      # Adjust end_date to the last day of the month
      end_date = ceiling_date(end_date, "month") - days(1),
      date_seq = map2(start_date, end_date, ~ seq(as.Date(.x), 
                                                  as.Date(.y), 
                                                  by = "month"))
    ) %>%
    unnest(date_seq) %>%  # Expand the date sequences into one row per month
    mutate(
      year   = format(date_seq, "%Y"),
      month  = format(date_seq, "%m"),
      active = 1
    ) %>%
    dplyr::select(-date_seq)  # Drop the intermediate date column if not needed
  
  return(expanded_data)
}

# Function to create district-month grid
create_monthly_grid <- function(district_list, start_date = "1989-01", 
                                end_date = "2021-12") {
  # Generate a sequence of months from start_date to end_date
  month_year_seq <- seq(as.Date(paste0(start_date, "-01")), 
                        as.Date(paste0(end_date, "-01")), 
                        by = "month")
  
  # Create a data frame for each district
  
  dt=district_list %>% dplyr::select(office, st_name) %>% distinct()
  grid <- dt %>%
    tidyr::crossing(date = month_year_seq) %>%
    mutate(
      year =(format(date, "%Y")),
      month = (format(date, "%m"))
    ) %>% dplyr::select(-date) %>% distinct()  
  
  return(grid)
}

# Load AC-PC-District mapping
load_ac_pc_mapping <- function(filepath) {
  read_csv(filepath) %>%
    clean_names() %>%
    mutate(st_name = standardize_state_names(st_name))
}
