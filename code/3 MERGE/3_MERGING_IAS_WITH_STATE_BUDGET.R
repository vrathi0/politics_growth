rm(list=ls())
source("code/functions.R")


# THIS FILE IS TO MERGE THE IAS DATA FROM TCPD/SUPREMO
# WITH THE STATE BUDGET DATA FROM RBI. 
# THERE CAN BE TWO KINDS OF MERGE: 
  # 1. CROSS SECTIONAL: ONLY ALONG LINE ITEMS OR SECTORS
 # 2.  TIME SERIES: INCLUDING YEARS IN ADDITION OF THE ABOVE. 

# 1 INPUT DATA ------------------------------------------------------------


ias_exp=qread("data/tcpd_ias_exp.qs")
#ias_exp=ias_exp %>% rename(grade=lvl)
qreadm("data/clean/state_budget.qs")
all_budget=qread("data/clean/all_budget.qs")
csec_budget=qread("data/clean/csec_budget.qs")

field_cat_sector_cw=source("code/0 CW/field_cat_sector_tag.R")
field_cat_sector_cw=field_cat_sector_cw$value

ias_cw=ias_exp %>% inner_join(field_cat_sector_cw) %>% 
  mutate(state_name=tolower(cadre))

# Joining with `by = join_by(field_of_experience,
#                            category_of_experience)`

# VERY GOOD ! 
# ias_cw %>% filter(line_item!="" & !grepl("Others", line_item)) %>% 
#   count_fun(field_of_experience, category_of_experience)
# [1] 320 (out of 392)

#ROUGH
dm=ias_exp %>% filter(grepl("District Collector", designation))

dm_st_count1=dm %>% group_by(cadre) %>% 
  mutate(dist_count=n_distinct(office)) %>% 
  group_by(cadre, office) %>% 
  mutate(dist_post_count=n()) %>% ungroup() %>% 
  dplyr::select(cadre, office, dist_count, dist_post_count) %>% 
  distinct() %>% 
  arrange(cadre)

dm_st_count2=dm %>% group_by(cadre) %>% 
  summarise(dist_count=n_distinct(office)) 




# st_budget2 has time series
# st_budget3 has cross section along sector+line_item
st_budget3=st_budget3 %>% 
  rename(st_abr=state) %>% 
  left_join(state_code )

st_budget2=st_budget2 %>% 
  rename(st_abr=state) %>% 
  left_join(state_code )



# field_cat_sector has field and categ of experience from supremo IAS data.
# combined with line_item and sector from the RJ and UP state budget docs. 
# field_cat_sector_cw=field_cat_sector_cw %>% 
#   mutate(sector=if_else(sector=="", NA, sector),
#          line_item=if_else(line_item=="", NA, line_item)) %>% 
#   group_by(category_of_experience) %>% 
#   fill(sector, .direction = "down") #%>% 
#   #fill(line_item, .direction = "down") %>% ungroup()


li_list=all_budget %>% 
  filter(grepl("Total Capital Outlay|EXPENDITURE", section2)) %>% 
  dplyr::select( budget_head_clean) %>% 
  rename(line_item=budget_head_clean) %>% distinct()

non_merge_li=li_list %>% 
  filter(!(line_item %in% field_cat_sector_cw$line_item)) 
# This list above has all the line_item/budget_head that
# are not merged at all and therefore cant be matched to any CS. 


# ISSUES [FIXED] ----------------------------------------------------------



# [FIXED] NEEDS FIX:
# 1. From the IAS data, we seem to be covering all major categories
#  except Personnel and General Administration. I think this can be fixed
# Not really, following categories are not merged to any line item
# (unique(field_cat_sector_cw$category_of_experience[is.na(field_cat_sector_cw$line_item)]))
# [1] "Agriculture & Cooperation"            
# [2] "Corporate Management (New)"           
# [3] "Defence"                              
# [4] "Development of NER"                   
# [5] "External Affairs"                     
# [6] "Information & Broadcasting"           
# [7] "Law and Justice"                      
# [8] "Mail Management"                      
# [9] "Mines & Minerals"                     
# [10] "N.Applicable"                         
# [11] "N.Available"                          
# [12] "Parliamentary Affairs"                
# [13] "Personnel and General Administration" 
# [14] "Petroleum & Natural Gas"              
# [15] "Planning & Prog Implementation"       
# [16] "Project Appraisal and Risk Management"
# [17] "Protocol"                             
# [18] "Public Administration"                
# [19] "Public Policy"                        
# [20] "Public Works"                         
# [21] "Railway Project"                      
# [22] "ST Welfare"                           
# [23] "Science & Technology"                 
# [24] "Service Commn"                        
# [25] "Social Justice & Empowerment"         
# [26] "Staff Officers"                       
# [27] "Training"                             
# [28] "Women & Child Dev"                    
# [29] "Youth Affairs & Sports"               
# [30] NA 


# 2. However, from the all_budget data, we are somehow missing many sectors
# that are not merged. This need to be looked at, the sectors are:
# [1] "Economic Services"                                                           
# [2] "Relief for Natural Calamities"                                               
# [3] "Others"                                                                      
# [4] "Co-operation"                                                                
# [5] "Government Servants"                                                         
# [6] "Miscellaneous"                                                               
# [7] "Transport and Communications"                                                
# [8] "Organs of State"                                                             
# [9] "Fiscal Services"                                                             
# [10] "Other Fiscal Services"                                                       
# [11] "Interest Payments and Servicing of Debt"                                     
# [12] "Interest Payments"                                                           
# [13] "Pensions"                                                                    
# [14] "Miscellaneous General Services"                                              
# [15] "Compensation and Assignments to Local Bodies and Panchayati Raj Institutions"
#
# For some reason, these sectors are not merged to any caterogies 
# in the IAS data, which is likely an error. 




# 2 CROSS SECTIONAL MERGE -------------------------------------------------
 # this will just add measures of cross sectional differences across line_item+sector



# dropping sector from the csec_budget

csec_budget1=csec_budget %>% 
  dplyr::select(-sector) %>% 
  mutate(state_name=tolower(state_ut)) %>% 
  dplyr::select(-state_ut) %>% 
  mutate(state_name=gsub("jammu and kashmir", "jammu & kashmir", state_name)) %>% 
  mutate(state_name=gsub("assam|meghalay", "assam meghalya", state_name))
# Convert 'section2' to a numerical factor
csec_budget1 <- csec_budget1 %>%
  mutate(section2_factor = as.numeric(factor(section2)))

# Create the new 'category' variable by combining 'exp_type' and the numerical factor of 'section2'
csec_budget1 <- csec_budget1 %>%
  mutate(category = paste(exp_type, section2_factor, sep = "_"))

# Pivoting the data to a wide format
wide_csec <- csec_budget1 %>%
  pivot_wider(
    id_cols = c(state_name, line_item), # Keeping 'state_ut' and 'line_item' as the identifier columns
    names_from = category,            # Using the new 'category' for wide transformation
    values_from = starts_with("be_") | starts_with("re_") | starts_with("ac_") ,
    names_glue = "{.value}_{category}" # Format to rename columns appropriately
  )



# MERGING IAS DATA WITH WIDE CSEC BUDGET

ias_budget_csec=ias_cw %>% 
  left_join(wide_csec)
# Joining with `by = join_by(line_item,
#                            state_name)`

# 41 out of original 74 line items are matched

qsave(ias_budget_csec, "data/CS/ias_budget_csec.qs", preset = "high")


# 3 TIME SERIES MERGE -----------------------------------------------------
# I am only taking pre-centre deputation postings here atm. 
# Also limiting to officers after 1988 for now (dont have budget data anyway)


ias_li_merge=ias_cw %>% 
  #filter(grade<14) %>% 
  #filter(batch_yr>=1988) %>% 
  dplyr::select(line_item, state_name, 
                start_date, end_date) %>% 
  filter(line_item!="") %>% 
  filter(!grepl("Others",line_item)) %>% 
  distinct() %>% 
  mutate(id1=row_number())

# Apply the function to each row in dt1 and bind with dt2 for budget calculations
bg_df1 <- all_budget %>%
  ungroup() %>% 
  filter(
    (section2 == "Total Capital Outlay" & 
       exp_type == "capital") |
      (section2 == "DEVELOPMENTAL EXPENDITURE" & 
         exp_type == "revenue") |
      (section2 == "NON-DEVELOPMENTAL EXPENDITURE (General Services)" & 
         exp_type == "revenue") |
      (section2 == "Grants-in-Aid and Contributions" & 
         exp_type == "revenue")
  ) %>% 
  filter(!(exp_type=="capital"& level<=3) & 
           !(exp_type=="revenue" & level<3)) %>% 
   distinct()

li_split =bg_df1 %>% 
  dplyr::select(exp_type, budget_head_clean, sector ) %>% 
  distinct() %>% group_by(budget_head_clean) %>% 
  mutate(li_dup=n()) %>% ungroup()

li_for_wide=li_split %>% filter(li_dup==2) %>% 
  dplyr::select(exp_type, budget_head_clean) 



# Create the new 'category' variable by combining 'exp_type' and the numerical factor of 'section2'
bg_df1 <- bg_df1 %>%  dplyr::select(-section2)
  
# This is just for those line items that are both in CAP and REV
budget_wide=bg_df1 %>% inner_join(li_for_wide) %>% 
  dplyr::select(state_ut, budget_head_clean, exp_type,
                year, 
                account, revised, budget) %>% 
  distinct() %>% 
  group_by(state_ut, budget_head_clean,  year) %>%
  pivot_wider(
    names_from = exp_type,
    values_from = c(account, revised, budget),
    names_sep = "_"
  ) %>%
  ungroup() %>% 
  rename_with(~ sub("_capital$", "_cap", .), ends_with("_capital")) %>%
  rename_with(~ sub("_revenue$", "_rev", .), ends_with("_revenue")) %>%
  ungroup() %>% 
  distinct()


# Now for those line_items that are only in one of CAP OR REV

li_nondup=li_split %>% filter(li_dup==1) %>% 
  dplyr::select(exp_type, budget_head_clean) 

budget_wide2=bg_df1 %>% inner_join(li_nondup) %>% 
  dplyr::select(state_ut, budget_head_clean, exp_type,
                year, 
                account, revised, budget) %>% 
  distinct() %>% 
  group_by(state_ut, budget_head_clean,  year) %>%
  pivot_wider(
    names_from = exp_type,
    values_from = c(account, revised, budget),
    names_sep = "_"
  ) %>%
  ungroup() %>% 
  rename_with(~ sub("_capital$", "_cap", .), ends_with("_capital")) %>%
  rename_with(~ sub("_revenue$", "_rev", .), ends_with("_revenue")) %>%
  ungroup() %>% 
  distinct()



bg_df2=budget_wide %>% bind_rows(budget_wide2)


bg_df3=bg_df2 %>% 
  mutate(year=as.numeric(year)) %>% 
  rename(line_item=budget_head_clean) %>% 
  rename(state_name=state_ut) %>% 
  mutate(state_name=tolower(state_name)) %>% 
  mutate(state_name=gsub("jammu and kashmir", "jammu & kashmir", state_name)) %>% 
  mutate(state_name=gsub("assam|meghalay", "assam meghalya", state_name))


# Vectorized function to calculate financial year impacts
get_mlist <- function(start_dates, end_dates) {
  # Create sequences of dates for all rows in a vectorized manner
  date_list <- map2(start_dates, end_dates, 
                    ~ seq.Date(from = .x, to = .y, by = "month"))
  
  # Flatten the list of dates and expand corresponding rows
  data_expanded <- tibble(
    row_id = rep(seq_along(date_list), lengths(date_list)),
    date = as.Date(unlist(date_list)) # Ensure dates are in Date format
  )
  
  # Calculate financial years
  data_expanded <- data_expanded %>%
    mutate(f_year = year(date) + (month(date) >= 4)) %>%
    count(row_id, f_year, name = "months")
  
  return(data_expanded)
}

# Apply the vectorized function to the entire dataset
mlist <- get_mlist(ias_li_merge$start_date, 
                              ias_li_merge$end_date)

# Merge the financial data with the original dataframe
ias_li_ymo <- ias_li_merge %>%
  mutate(row_id = row_number()) %>%
  left_join(mlist, by = "row_id") %>%
  mutate(budget_year = f_year) %>% distinct()

ts_budget_ias=ias_li_ymo %>%
  left_join(bg_df3, 
            by = c("line_item", "state_name", 
                   "budget_year" = "year")) 

# budget_year and year seems to match in conceptual sense. 



## COMPUTING EXPOSURE TO BUDGET

ts_budget_ias <- ts_budget_ias %>%
  mutate(
    across(
      c(account_cap, account_rev, revised_cap, revised_rev, budget_cap, budget_rev),
      ~ months / 12 * .x,
      .names = "share_{.col}"
    )
  ) %>%
  dplyr::select(-c(account_cap, account_rev, revised_cap, 
            revised_rev, budget_cap, budget_rev, row_id))



ts_budget=ts_budget_ias %>%
  group_by(id1) %>%
  mutate(
    across(
      starts_with("share_"),
      sum,
      .names = "total_{.col}"
    )
  ) %>%
  ungroup() %>% 
  dplyr::select(-c(f_year, months, budget_year), -starts_with("share_")) %>% 
  distinct() 
  # Filter out rows where all "total_" columns are NA
  #filter(!if_all(starts_with("total_"), is.na))


# Now we can merge it back with ias_cw then we will have 
# b'cratic trajectory with budget exposure for every posting. 


ias_budget_ts=ias_cw %>% left_join(ts_budget)

# Joining with `by = join_by(start_date, end_date,
#                            line_item, state_name)`

# THIS DATA NOW HAS BUDGET EXPOSURE FOR EVERY POSTING. 


qsave(ias_budget_ts, "data/CS/ias_budget_ts.qs", preset = "high")





# na_share <- ts_budget_ias %>%
#   summarise(
#     across(
#       c(account_cap, account_rev, revised_cap, revised_rev, budget_cap, budget_rev,
#         share_account_cap, share_account_rev, share_revised_cap, share_revised_rev, 
#         share_budget_cap, share_budget_rev),
#       ~ mean(is.na(.x)),
#       .names = "na_share_{.col}"
#     )
#   )

# Seems like really high NA share, something is wrong. 




# OLD -----------------------------------------------------


# Function to calculate financial year impacts directly from date intervals
get_financial_impacts <- function(start_date, end_date) {
  date_seq <- seq.Date(from = start_date, to = end_date, by = "month")
  
  # Calculate financial years for each date
  financial_years <- year(date_seq) + (month(date_seq) >= 4)
  
  # Return a summarized tibble
  tibble(
    f_year = financial_years,
    months = 1
  ) %>%
    group_by(f_year) %>%
    summarise(months = n(), .groups = 'drop')
}

# Apply the revised function to each row in dt1 using start_date and end_date
result <- ias_li_merge %>%
  mutate(
    financial_data = map2(start_date, end_date, get_financial_impacts)
  ) %>%
  unnest(financial_data) %>%
  mutate(budget_year = f_year ) %>%  # Adjust for financial year ending
  left_join(dt2, by = c("sector", "state_name", 
                        "budget_year" = "year")) %>%
  mutate(share_of_budget = months / 12 * sct_total_acc)



# Print the results
print(result)








  
