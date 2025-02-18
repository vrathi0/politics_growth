# CAPEX Data Notes

# Output:
# 1. A state_district-year level panel with some aggregated stats indicating the state of new project in that year in that geography.


# 1 HEADER ----------------------------------------------------------------



rm(list = ls())
library(here)
source(here("code/functions.R"))

# Load necessary packages
library(lfe)

# Define the function
get_var <- function(data, y_var, fe1, fe2) {
  # Validate inputs
  if (!all(c(y_var, fe1, fe2) %in% names(data))) {
    stop("y_var, fe1, or fe2 not found in the dataset.")
  }
  
  # Ensure no missing values for the specified variables
  data <- data[complete.cases(data[, c(y_var, fe1, fe2)]), ]
  
  # Center y_var for proper variance computation
  data[[y_var]] <- scale(data[[y_var]], center = TRUE, scale = FALSE)
  
  # Construct regression formulas
  full_formula <- as.formula(paste(y_var, "~", fe1, "+", fe2))
  fe1_formula <- as.formula(paste(y_var, "~", fe1))
  fe2_formula <- as.formula(paste(y_var, "~", fe2))
  
  # Fit the models
  full_model <- lm(full_formula, data = data)
  fe1_model <- lm(fe1_formula, data = data)
  fe2_model <- lm(fe2_formula, data = data)
  
  # Compute total variance of y_var
  total_var <- var(data[[y_var]], na.rm = TRUE)
  
  # Compute residual variances for each model
  residual_var_full <- var(residuals(full_model), na.rm = TRUE)
  residual_var_fe1 <- var(residuals(fe1_model), na.rm = TRUE)
  residual_var_fe2 <- var(residuals(fe2_model), na.rm = TRUE)
  
  # Compute R2 values
  r2_full <- 1 - residual_var_full / total_var
  r2_fe1 <- 1 - residual_var_fe1 / total_var
  r2_fe2 <- 1 - residual_var_fe2 / total_var
  
  # Compute shares of explained variance
  share_fe1 <- r2_fe1 / r2_full
  share_fe2 <- r2_fe2 / r2_full
  
  # Return results as a named list
  return(as.data.frame(list(
    outcome=y_var,
    full_r2 = r2_full,
    r2_fe1 = r2_fe1,
    r2_fe2 = r2_fe2,
    share_fe1 = share_fe1,
    share_fe2 = share_fe2
  )))
}

# capex_list loading
capex_list=qread(here("data/0 RAW/capex_list.qs"))
source("code/0 CW/capex_events.R")
# use "cpx_events" data

# Event List
{
  rank1_events=c(
  "Environment clearance sought",
  "Environmental clearance received",
  "Memorandum of Understanding (MoU) signed",
  "State government approval received",
  "Industrial entrepreneurs memorandum (IEM) filed",
  "Land acquired",
  "Implementation stalled on",
  "Central Government approval received",
  "Expert Appraisal Committee (EAC) recommendation received",
  "Shelved on",
  "Abandoned on",
  "Board of Directors' approval received",
  "Forest clearance sought",
  "Land acquisition problem",
  "Power purchase agreement (PPA) signed",
  "Forest clearance received",
  "Letter of Intent (LoI) received",
  "Cabinet Committee on Economic Affairs (CCEA) approval received",
  "Cabinet approval received",
  "Special Economc Zone (SEZ) Board formal approval received",
  "Planning Commission approval received",
  "Coastal regulatory zone (CRZ) clearance received",
  "Special Economic Zone (SEZ) Board in-principle approval received",
  "Central Electricity Authority (CEA) in principle approval received",
  "Formal approval cancelled by SEZ Board (BoA)",
  "Foreign Investment Promotion Board (FIPB) approval received",
  "Coal linkage granted",
  "Issues resolved by Cabinet Committee on Investment (CCI)",
  "Land acquisition problem resolved",
  "Public Investment Board (PIB) approval received",
  "Forest Advisory Committee (FAC) recommendation received",
  "Fuel supply agreement (FSA) signed",
  "MRTP (Monopolistic & Restrictive Trade Practices) clearance received",
  "De-notification request approved by BoA/SEZ Board",
  "Secretariat for Industrial Assistance (SIA) clearance received",
  "Cabinet Committee on Foreign Investment (CCFI) approval received",
  "Memorandum of Understanding (MoU) cancelled",
  "Power purchase agreement (PPA) cancelled",
  "Land allotment cancelled",
  "Rejected by central government",
  "Central Electricity Authority (CEA) initial approval received",
  "Collaboration approved",
  "Project rejected/deferred by the SEZ Board",
  "Acquired land returned",
  "Contract termination revoked"
)
}

# Define a mapping for cleaner abbreviations
stalling_map <- c(
  "fuel/feedstock/raw material supply problem" = "stalled_fuel_issue",
  "lack of clearances (non-environmental)" = "stalled_no_clearance",
  "lack of environment clearance" = "stalled_no_env_clearance",
  "lack of funds" = "stalled_funds_issue",
  "lack of promoter interest" = "stalled_no_promoter_interest",
  "land acquisition problem" = "stalled_land_issue",
  "natural calamity" = "stalled_natural_disaster",
  "not available" = "stalled_not_available",
  "others" = "stalled_other_issues",
  "unfavourable market conditions" = "stalled_market_conditions"
)
# 2 PROJECT EVENT --------------------------------------------------------

# Event Data Processing
event_raw=capex_list$project_events
# event_list=event_raw %>% 
#   #filter(event %in% rank1_events) %>% 
#   dplyr::select(event) %>% 
#   group_by(event) %>% summarise(count=n()) %>%
#   arrange(count) 
# #write.xlsx(event_list, here("data/2 CLEAN/rank1_event_list.xlsx"))

event_raw=event_raw %>%  mutate(
  date = dmy(event_date),
  month = lubridate::month(date, label = TRUE),
  year = year(date)
) %>% 
  mutate(project_id=as.factor(paste0(company, company_code, 
                                     project_name, project_number))) %>%
  group_by(project_id) %>% 
  arrange(date, .by_group = TRUE) %>% 
  mutate(sno=row_number(), 
         tot_event_count=n()) %>% ungroup() %>% 
  mutate(begin=(sno==1) | event=="Date of announcement") %>% 
  group_by(project_id, year) %>% 
  mutate(begin=max(begin, na.rm=T)) %>% ungroup()

event_raw=event_raw %>% 
  mutate(event=gsub("\\b(Environment|Environmental)\\b", 
                    "Environmental", event))

event_raw=event_raw %>% 
  left_join(cpx_events, by="event") 
  

event_df=event_raw %>% 
  mutate(rank1_event=if_else(!is.na(phase_title), 
                             1,0)) %>% 
  mutate(start_year=if_else(begin==1, year, NA )) %>% 
  group_by(project_id) %>% 
  fill(.,start_year) %>% ungroup() %>% 
  dplyr::select(-c("product",
                   "quantity", "quantity_unit" 
                   ))

event_df1=event_df %>% 
  mutate(reason_for_stalling = tolower(reason_for_stalling),
         reason_for_stalling = recode(reason_for_stalling, 
                                      !!!stalling_map)  # Apply abbreviation mapping
  ) %>%  # Ensure consistency
  mutate(dummy = if_else(is.na(reason_for_stalling), 
                         0,1)) %>%  # Create a temporary column to indicate presence
  pivot_wider(
    names_from = reason_for_stalling, 
    values_from = dummy, 
    values_fill = list(dummy = 0)  # Fill missing values with 0
  ) %>% dplyr::select(-"NA") %>% distinct()


prj_yr_r1event=event_df1 %>%
  filter(rank1_event==1) %>%
  group_by(project_id, year,phase_title) %>% 
  mutate_at(vars(starts_with("stalled_"), 
                 "rank1_event"), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  dplyr::select(project_id,company, company_code, 
                project_name, project_number, year,month,
                begin,
                start_year, rank1_event,
                tot_event_count,phase_title,
                phase_order,begin,
                starts_with("stalled_")) %>%
  distinct()




# 3 PROJECT DETAILS --------------------------------------------------------

# Project Details
det_raw=capex_list$project_detail
det_raw=det_raw %>%
  mutate(own_cat = NA_character_) %>%
  mutate(own_cat = if_else(grepl("Group", ownership, ignore.case = TRUE), "Corp Group", own_cat)) %>% 
  mutate(own_cat = if_else(grepl("State", ownership, ignore.case = TRUE), "State", own_cat)) %>% 
  mutate(own_cat = if_else(grepl("Central", ownership, ignore.case = TRUE), "Central", own_cat)) %>% 
  mutate(own_cat = if_else(ownership=="Private (Indian)", "Private (Indian)", own_cat)) %>% 
  mutate(own_cat = if_else(ownership=="Private (Foreign)", "Private (Foreign)", own_cat)) %>%
  mutate(new_unit = if_else(type_of_unit=="New Unit", 1,0)) %>% 
  mutate(renovation = if_else(type_of_unit=="Renovation & Modernisation", 1,0)) %>% 
  mutate(expansion = if_else(type_of_unit=="Substantial Expansion",  1,0))

det_df=det_raw %>% ungroup() %>% 
  mutate(
    complete_date = as.character(completion_date), # Ensure it's a character variable
    complete_year1 = as.numeric(str_extract(complete_date, "\\d{4}")), # Extract year
    complete_month1 = match(word(completion_date, 1), month.name) # Convert month name to number
  ) %>% 
  dplyr::select(company, company_code, 
                project_name, project_number, 
                own_cat, new_unit, renovation, expansion, 
                cost_rs_million,industry,industry_code, 
                ownership,type_of_unit,
                project_status,complete_year1,
                complete_month1) %>% distinct()


   

det_df <- det_df %>%
  group_by(industry_code) %>%
  mutate(ind_count = n()) %>% 
  ungroup() %>%
  mutate(ind_quantile = ntile(ind_count, 4)) 


loc_df=capex_list$project_location
loc_df=loc_df %>% group_by(company, company_code, 
                           project_name, project_number) %>% 
  mutate(n_loc=n_distinct(state, district_code)) %>% 
  ungroup() %>% mutate(multi_loc=if_else(n_loc>1,1,0))

det_loc_df=det_df %>% inner_join(loc_df)



# 4 PRODUCT & CAP --------------------------------------------------------


prod_raw=capex_list$products_and_capacity

# This has capacity numbers also, but there are like 260 different 
# units and the summarize those numbers seems like a time intensive tast
# leaving that for now. For now, can just have number of unique products
# per project. 

prod_df=prod_raw %>% 
  group_by(company,company_code, project_name, project_number) %>% 
  summarise(product_count=n_distinct(product_code)) %>% 
  ungroup()
#only 15% of project has more than one product



# 5 COMPLETION DATE-----------------------------------------------------

complete_raw=capex_list$project_completion_details

complete_raw=complete_raw %>% 
  mutate(
    date = dmy(completion_date), # Convert the string to date
    month = lubridate::month(date, label = TRUE), # Extract month as a labeled factor (e.g., Jan, Feb)
    year = year(date) # Extract year
  ) 

prj_complete_date=complete_raw %>% 
  filter(description=="Completed") %>% 
  dplyr::select(project_name, project_number, 
                company, company_code, year, month) %>%
  distinct() %>% 
  rename(complete_year=year, complete_month=month) 


# 6 PROJECT COST -----------------------------------------------------

#COST

cost_raw=capex_list$project_cost

cost_raw= cost_raw %>% 
  mutate(
    cost_date = dmy(date_of_cost_of_project), # Convert the string to date
    cost_month = lubridate::month(cost_date, label = TRUE), # Extract month as a labeled factor (e.g., Jan, Feb)
    cost_year = year(cost_date) # Extract year
  ) 

cost_df=cost_raw %>% dplyr::select(-date_of_cost_of_project) %>% distinct()

cost_df=cost_df %>% group_by(company, company_code, 
                             project_name, project_number) %>% 
  arrange(cost_date, .by_group = TRUE) %>% 
  mutate(
    order_no=row_number(),
    cost_change = cost_of_project_rs_million - dplyr::lag(cost_of_project_rs_million) # Calculate first difference, use 0 for the first value
  ) %>%   ungroup() %>%   distinct() %>% 
  mutate(cost_went_down=if_else(cost_change<0 &order_no!=1, TRUE, FALSE)) %>%
  mutate(cost_went_up=if_else(cost_change>0 &order_no!=1, 
                              TRUE, FALSE)) %>% 
  mutate(cost_first=if_else(order_no==1, 
                            cost_of_project_rs_million, NA)) %>% 
  group_by(company, company_code,
           project_name, project_number) %>% 
  fill(cost_first) %>% ungroup() %>% 
  dplyr::select(-c(cost_date, 
                   cost_month)) %>% distinct() %>% 
  rename(cost_proj=cost_of_project_rs_million)

# Also creating quantile_indicator for each project based on their first cost
dd=cost_df %>% 
  dplyr::select(company, company_code,
                project_name, project_number, cost_first)%>%
  distinct() %>% 
  mutate(cost_q=ntile(cost_first, 5)) %>% 
  dplyr::select(-cost_first)

cost_df=cost_df %>% inner_join(dd)



# 7 MERGING EVERYTHING  -----------------------------------------------------



# MERGING


proj_state_dist_df= det_loc_df %>% full_join(prod_df) %>% 
  full_join(prj_complete_date) 

cost_event_df=prj_yr_r1event %>% 
  full_join(cost_df,
            by=c(intersect(names(cost_df), 
                           names(prj_yr_r1event)),
                 "year"="cost_year"))


time_geo_panel=proj_state_dist_df %>% 
  full_join(cost_event_df) %>% distinct()

# Joining with `by = join_by(company, company_code, project_name, project_number)`Joining with `by = join_by(company, company_code, project_name, project_number)`Joining with `by = join_by(company, company_code, project_name, project_number)`Warning: Detected an unexpected many-to-many relationship between `x` and `y`. 

#time_geo_panel=proj_state_dist_df %>% full_join(event_df)
#Joining with `by = join_by(company, company_code, project_name, project_number)`

library(dplyr)
# Precompute Unique Project ID



# Add Logical Flags to Avoid Repeated Calculations
time_geo_panel <- time_geo_panel %>%
  mutate(
    is_begin = begin == 1,
    is_central = own_cat == "Central" & is_begin,
    is_corp_group = own_cat == "Corp Group" & is_begin,
    is_priv_foreign = own_cat == "Private (Foreign)" & is_begin,
    is_priv_indian = own_cat == "Private (Indian)" & is_begin,
    is_state = own_cat == "State" & is_begin,
    is_new_unit=new_unit == 1,
    is_renovation=renovation == 1,
    is_expansion=expansion == 1, 
    is_multiloc=multi_loc==1 & is_begin
  )
# Aggregate Using Precomputed Columns
time_geo <- time_geo_panel %>%
  group_by(state, district, district_code, year,phase_title) %>%
  summarise(
    # Projects Started
    projects_started = n_distinct(project_id[is_begin], na.rm = TRUE),
    
    # Rank1 Events Count
    rank1_events_count = sum(rank1_event, na.rm = TRUE),
   
    # Total Cost for New Projects
    new_proj_cost_million = sum(cost_first[is_begin], na.rm = TRUE),
    
    # Cost Changes
    cost_went_up = sum(cost_went_up, na.rm = TRUE),
    cost_went_down = sum(cost_went_up, na.rm = TRUE),
    
    # Project counts by cost quantiles
    n_cost_q1=n_distinct(project_id[is_begin & cost_q==1], na.rm=T),
    n_cost_q2=n_distinct(project_id[is_begin & cost_q==2], na.rm=T),
    n_cost_q3=n_distinct(project_id[is_begin & cost_q==3], na.rm=T),
    n_cost_q4=n_distinct(project_id[is_begin & cost_q==4], na.rm=T),
    n_cost_q5=n_distinct(project_id[is_begin & cost_q==5], na.rm=T),
    
    
    
    # Industry Counts by Quantiles
    count_ind_q1 = n_distinct(industry_code[is_begin & ind_quantile == 1], na.rm = TRUE),
    count_ind_q2 = n_distinct(industry_code[is_begin & ind_quantile == 2], na.rm = TRUE),
    count_ind_q3 = n_distinct(industry_code[is_begin & ind_quantile == 3], na.rm = TRUE),
    count_ind_q4 = n_distinct(industry_code[is_begin & ind_quantile == 4], na.rm = TRUE),
    count_ind_total = n_distinct(industry_code[is_begin], na.rm = TRUE),
    
    # Ownership Counts
    central = n_distinct(project_id[is_central], na.rm = TRUE),
    corp_group = n_distinct(project_id[is_corp_group], na.rm = TRUE),
    priv_foreign = n_distinct(project_id[is_priv_foreign], na.rm = TRUE),
    priv_indian = n_distinct(project_id[is_priv_indian], na.rm = TRUE),
    state_prj = n_distinct(project_id[is_state], na.rm = TRUE),
    
    # Other Categories
    new_units = n_distinct(project_id[is_new_unit], na.rm = TRUE),
    renovations = n_distinct(project_id[is_renovation], na.rm = TRUE),
    expansions = n_distinct(project_id[is_expansion], na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(state_dist=paste0(state, district, sep="-"))


dist_all=time_geo %>% 
  group_by(state, district) %>% 
  summarise(
    new_proj_cost_million=sum(new_proj_cost_million, na.rm=T), 
    projects_started=sum(projects_started, na.rm=T)
  ) %>% ungroup()

state_all=time_geo %>% 
  group_by(state) %>% 
  summarise(
    new_proj_cost_million=sum(new_proj_cost_million, na.rm=T), 
    projects_started=sum(projects_started, na.rm=T)
  ) %>% ungroup()


# 8 WRITING TO DISK  -----------------------------------------------------

qsave(time_geo_panel, 
      file=here("data/2 CLEAN/capex_time_geo_full_panel.qs"))


# qsave(time_geo, 
#       file=here("data/2 CLEAN/capex_time_geo_df.qs"))

