rm(list=ls())
source("code/functions.R")

# USing this code to construct features. 
# I have two sources of data at the moment: TCPD IAS and State Budgets
# At some point, I should add politician data also (but not clear how to merge that 
# at the moment) 




# 0 FUNCTIONS ---------------------------------------------------------------


# Function to compute group-specific mean
get_ps <- function(data, group_vars, outcome_var) {
  
  # Loop through the list of group variables to create ps_ variables
  for (group_var in group_vars) {
    
    # Dynamically construct the new variable name with the prefix "ps_"
    new_var_name <- paste0("ps_", group_var)
    
    # Compute the group-specific mean and add the new variable to the data
    data <- data %>%
      group_by(across(all_of(group_var))) %>%
      mutate(!!new_var_name := mean(!!sym(outcome_var), na.rm = TRUE)) %>%
      ungroup()
  }
  
  # Now compute summary statistics for each "ps_" variable by id
  for (group_var in group_vars) {
    new_var_name <- paste0("ps_", group_var)
    
    # Create summary statistics for each id
    data <- data %>%
      group_by(id) %>%
      mutate(
        !!paste0(new_var_name, "_min") := min(!!sym(new_var_name), na.rm = TRUE),
        !!paste0(new_var_name, "_max") := max(!!sym(new_var_name), na.rm = TRUE),
        !!paste0(new_var_name, "_med") := median(!!sym(new_var_name), na.rm = TRUE),
        !!paste0(new_var_name, "_mean") := mean(!!sym(new_var_name), na.rm = TRUE),
        !!paste0(new_var_name, "_sd") := sd(!!sym(new_var_name), na.rm = TRUE)
      ) %>%
      ungroup()
  }
  
  # Return the modified dataframe
  return(data)
}



# 1 READING ---------------------------------------------------------------

all_budget=qread("data/clean/all_budget.qs")

tcpd_ias=qread("data/tcpd_ias_exp.qs")

ias_budget_csec=qread("data/CS/ias_budget_csec.qs")
ias_budget_ts=qread("data/CS/ias_budget_ts.qs")

ias_budget_both=ias_budget_csec %>% 
  inner_join(ias_budget_ts)





# 1. FEATURES: BUDGET -----------------------------------------------------

# NOTE: each state-year budget has two kind of expenses: capital and revenue

# Current method:
# 1. Constructing two main static variables:
    #. A: For each exp_type-line_item: average account amount across all years
    # B : For each exp_type-line_item: trend line coeff across years. 
   # C: Can also add an average revised/budget or account/budget for each exp_type-line_item

bdata=all_budget %>% filter(!is.na(sector))
sector_covar <- bdata %>% ungroup() %>% 
  mutate(total_acc=clipp(account, 0.01, 0.99), 
         total_be=clipp(budget, 0.01, 0.99), 
         total_re=clipp(revised, 0.01, 0.99)) %>% 
  #mutate_at(c("total_acc", "total_be", "total_re"), log)
  group_by(state_ut, exp_type, sector) %>%
  arrange(year) %>%
  mutate(total_acc_delta = total_acc - Lag(total_acc), 
         total_be_delta= total_be - Lag(total_be),
         total_re_delta= total_re - Lag(total_re)) %>%
  group_by(state_ut, exp_type, sector) %>%
  mutate(total_acc_mean=mean(total_acc, na.rm=T), 
         total_be_mean=mean(total_be, na.rm=T),
         total_re_mean=mean(total_re, na.rm=T)) %>% 
  group_by(state_ut, exp_type) %>% 
  mutate(total_acc_q=cut(total_acc_mean, 4, labels=F),
         total_be_q=cut(total_be_mean, 4, labels=F),
         total_re_q=cut(total_re_mean, 4, labels=F)) %>% 
  ungroup()

# Calculate the standard deviation and mean of the YoY changes for each sector
sector_covar <- sector_covar %>%
  group_by(state_ut, exp_type, sector) %>%
  mutate_at(vars(c("total_acc_delta", "total_be_delta", "total_re_delta")),
            list(mean = ~mean(., na.rm=T), sd = ~sd(., na.rm=T))) %>% 
  ungroup() %>% 
  mutate(z_acc = (total_acc_delta - total_acc_delta_mean) / total_acc_delta_sd) %>%
  mutate(abnormal_pos=z_acc>2) %>% 
  mutate(abnormal_neg= z_acc <= -1.5) %>% 
  group_by(state_ut, exp_type, sector) %>% 
  mutate(total_acc_abn_pos=sum(abnormal_pos, na.rm=T),
         total_acc_abn_neg=sum(abnormal_neg, na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::select(state_ut, exp_type, sector, level,
                matches("_mean$|_q$|_abn_pos$|_abn_neg$")) %>% distinct()

names(sector_covar) <- gsub("total", "sector", names(sector_covar))


# SAME FOR LINE ITEM LEVEL
lineitem_covar <- bdata %>% ungroup() %>% 
  rename(line_item=budget_head_clean) %>% 
  mutate(total_acc=clipp(account, 0.01, 0.99), 
         total_be=clipp(budget, 0.01, 0.99), 
         total_re=clipp(revised, 0.01, 0.99)) %>% 
  #mutate_at(c("total_acc", "total_be", "total_re"), log)
  group_by(state_ut, exp_type, line_item) %>%
  arrange(year) %>%
  mutate(total_acc_delta = total_acc - Lag(total_acc), 
         total_be_delta= total_be - Lag(total_be),
         total_re_delta= total_re - Lag(total_re)) %>%
  group_by(state_ut, exp_type, line_item) %>%
  mutate(total_acc_mean=mean(total_acc, na.rm=T), 
         total_be_mean=mean(total_be, na.rm=T),
         total_re_mean=mean(total_re, na.rm=T)) %>% 
  group_by(state_ut, exp_type) %>% 
  mutate(total_acc_q=cut(total_acc_mean, 4, labels=F),
         total_be_q=cut(total_be_mean, 4, labels=F),
         total_re_q=cut(total_re_mean, 4, labels=F)) %>% 
  ungroup()

# Calculate the standard deviation and mean of the YoY changes for each sector
lineitem_covar <- lineitem_covar %>%
  group_by(state_ut, exp_type, line_item) %>%
  mutate_at(vars(c("total_acc_delta", "total_be_delta", "total_re_delta")),
            list(mean = ~mean(., na.rm=T), sd = ~sd(., na.rm=T))) %>% 
  ungroup() %>% 
  mutate(z_acc = (total_acc_delta - total_acc_delta_mean) / total_acc_delta_sd) %>%
  mutate(abnormal_pos=z_acc>2) %>% 
  mutate(abnormal_neg= z_acc <= -1.5) %>% 
  group_by(state_ut, exp_type, line_item) %>% 
  mutate(total_acc_abn_pos=sum(abnormal_pos, na.rm=T),
         total_acc_abn_neg=sum(abnormal_neg, na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::select(state_ut, exp_type, line_item, level,
                matches("_mean$|_q$|_abn_pos$|_abn_neg$")) %>% distinct()

names(lineitem_covar) <- gsub("total", "litem", names(lineitem_covar))



# 2. FEATURES: IAS Data----------------------------------------------------



df=ias_budget_both
last_name=sub(".*\\s", "", df$name)
cadre=df$cadre

# deputization is going down starting 98ish
# and goes to 0 by 2004,2005

dd=tcpd_ias %>% 
  dplyr::select( batch_yr, id_centre_deput)  %>% 
  group_by(batch_yr) %>% 
  summarise(deput_mean=mean(id_centre_deput, na.rm=T))

df=df %>% filter(batch_yr<=2006)


# Keeping only postings before or till level_eq=14

X13_df=df %>% filter(grade<14)
id_list=X13_df %>% dplyr::select(id) %>% distinct()
Y14_df= df %>% filter(grade==14) %>% 
  dplyr::select(id, centre_dum) %>% group_by(id) %>% 
  summarise(Y14=max(centre_dum)) %>% ungroup() %>% 
  right_join(id_list)
Y15_df= df %>% filter(grade>14) %>% 
  dplyr::select(id, centre_dum) %>% group_by(id) %>% 
  summarise(Y15=max(centre_dum)) %>% ungroup() %>% 
  right_join(id_list)

df2=X13_df %>% 
  mutate(mo_idx=grepl("M/o", office)) %>% 
  mutate(dept_idx=grepl("Deptt", office)) %>% 
  mutate(office_wcount=str_count(office, "\\S+")) %>% 
  mutate(last_name=sub(".*\\s", "", name))


office_freq=as.data.frame(table(df$office)) %>% 
  arrange(-Freq)
write.xlsx(office_freq, file = "data/interm/office_freq.xlsx")


 ### FUZZY MATCHING WITHIN OFFICE VARIABLE TO REDUCE LEVELS

office_df=df2 %>% filter(office_wcount>1) %>% 
  dplyr::select(office) %>% 
  mutate(
    # Convert to lowercase
    office_clean = tolower(office),
    # Remove punctuation
    office_clean = str_replace_all(office_clean, "[[:punct:]]", " "),
    office_clean=str_replace(office_clean, "m o",""),
    office_clean=str_replace(office_clean, "d o",""),
    office_clean=str_trim(office_clean),
    # Remove extra whitespace
    office_clean = str_squish(office_clean)
  ) %>% 
distinct()


# Compute the distance matrix
distance_matrix <- stringdistmatrix(
  office_df$office_clean, 
  office_df$office_clean, 
  method = "jw",  # Jaro-Winkler distance
  p = 0.08  # Winkler's adjustment parameter
)

# Convert to a 'dist' object
distance_matrix <- as.dist(distance_matrix)

# Perform hierarchical clustering
hc <- hclust(distance_matrix, method = "average")

# Plot the dendrogram (optional)
 plot(hc, labels = FALSE, main = "Clustering of Office Names")

# Cut the tree at a chosen threshold to form clusters
# Adjust 'h' to set the threshold (between 0 and 1 for Jaro-Winkler)
clusters <- cutree(hc, h = 0.15)

# Add cluster labels to the data
office_df$cluster <- clusters

# For each cluster, select the office name with the highest frequency
cluster_representatives <- office_df %>%
  group_by(office_clean) %>% 
  mutate(off_freq=n()) %>% 
  group_by(cluster) %>%
  arrange(desc(off_freq)) %>%
  summarise(
    standard_office = first(office_clean),
    original_names = paste(unique(office), collapse = "; "),
    total_freq = sum(off_freq)
  ) %>% ungroup()

# Merge the standardized names back to the main data
office_matched <- office_df %>%
  left_join(cluster_representatives %>% 
              dplyr::select(cluster, standard_office), by = "cluster") %>% 
  dplyr::select(-office_clean) %>% distinct()

# at this stage, office_matched contains the original office variable, 
# along with new office string that reduces the number of unique 
# office by matching across similar offices. 
# NOTE that it matches across M/o (centre) and D/o (state). 

df2_std_off=df2 %>%  left_join(office_matched, by="office")

# Now I am renaming any office that has very low frequency
#.  to "Other" on the logic that these small marginalzied 
#. offices might be similar and contain possibly a bad signal that is similar across
# all of them so I can reduce the unique level count further by relabbling 

df3=df2_std_off %>% group_by(standard_office) %>% 
  mutate(office_freq=n()) %>% ungroup() %>% 
  mutate(standard_office2=if_else(office_freq<=10 & mo_idx==F, "Other",
                                  standard_office)) 
# 10 is around 10 ptile of office_freq (excluding NA amd M/o (centre))


# MEAN DURATION : global as well as at each level_eq



id_df1=df3 %>% group_by(id) %>%  
  arrange(id, post_no) %>% 
  mutate(level_diff = grade - dplyr::lag(grade, default = first(grade))) %>% 
  mutate(id_dur_mean=mean(duration, na.rm=T)) %>% 
  mutate(id_total_posts=post_count, 
         id_centre_count=sum(centre_dum)) %>% 
  group_by(id) %>% 
  mutate( ljump_neg2 = sum(level_diff == -2, na.rm = TRUE),
          ljump_neg1 = sum(level_diff == -1, na.rm = TRUE),
          ljump_0 = sum(level_diff == 0, na.rm = TRUE),
          ljump_1 = sum(level_diff == 1, na.rm = TRUE),
          ljump_2 = sum(level_diff == 2, na.rm = TRUE),
          ljump_3 = sum(level_diff == 3, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(id_male = ifelse(gender == "Male", 1, 0),
         id_female = ifelse(gender == "Female", 1, 0)) %>%
  rename(id_age_join=age_at_joining) %>% 
  group_by(batch_yr) %>% 
  # Count the distinct number of ids per batch_yr
  mutate(batch_size = n_distinct(id)) %>% 
  # Calculate percentage of wins and losses
  mutate(id_batch_win = n_distinct(id[id_centre_deput == 1], na.rm = TRUE) / batch_size, 
         id_batch_los = n_distinct(id[id_centre_deput == 0], na.rm = TRUE) / batch_size ) %>% 
  ungroup() %>% 
  dplyr::select(id,id_dur_mean,id_total_posts,id_male, id_female,
                id_centre_count,id_age_join,
                id_batch_win, id_batch_los,
                starts_with("ljump_")) %>% 
  distinct()


df31 <- df3 %>%
  mutate(
    across(
      starts_with("ac_mean_"),
      ~ if (length(.) > 1) {
        sapply(., function(x) if (length(x) == 5 && length(unique(x)) == 1) x[1] else NA)
      } else {
        .
      },
      .names = "{.col}"
    )
  ) %>%
  # Convert all the ac_mean_ variables to numeric
  mutate(
    across(starts_with("ac_mean_"), ~ as.numeric(unlist(.)))
  )



# Ensure columns are numeric and then perform summarisation and pivot
id_df2 <- df31 %>%
  # Convert columns to numeric, if possible
  group_by(id, level_eq) %>%
  summarise(
    id_lvl_dur_mean = mean(duration, na.rm = TRUE),
    id_lvl_post_count = n(),
    id_lvl_ac_mean_cap = sum(ac_mean_capital_4, na.rm = TRUE),
    id_lvl_ac_mean_rev = sum(ac_mean_revenue_3, na.rm = TRUE) +
      sum(ac_mean_revenue_1, na.rm = TRUE) +
      sum(ac_mean_revenue_2, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    id_cols = id,
    names_from = level_eq,
    values_from = c(id_lvl_dur_mean, id_lvl_post_count, id_lvl_ac_mean_cap, id_lvl_ac_mean_rev),
    names_glue = "{.value}_{level_eq}"
  ) %>%
  # Remove the "id_lvl_" prefix from variable names
  rename_with(~ gsub("id_lvl_", "", .), starts_with("id_lvl_")) %>%
  clean_names()







# COMPUTING P SCORES FOR A GROUP OF VARIABLES
varlist=c("last_name","cadre","standard_office","field_of_experience",
          "category_of_experience","place_of_domicile",
          "last_education_division","last_education_subject")
outcome="id_centre_deput"

id_ps=get_ps(data=df3, group_vars = varlist, 
             outcome_var = outcome) %>% 
  dplyr::select(id, matches("_min$|_max$|_mean$|_med$|_sd$")) %>% 
  dplyr::select(id,starts_with("ps_")) %>% 
  distinct()


## NOW WE CAN MERGE ALL id_* dataframes. 

id_x_df=id_df1 %>% inner_join(id_df2) %>% 
  inner_join(id_ps)
  
 

ID_DF=id_x_df %>% inner_join(Y14_df) %>% 
  inner_join(Y15_df)


qsave(ID_DF, 
      file="data/features/ias_deput_features.qs", 
        preset = "high")



# FUNCTION TO CHECK 1-1 MAPPING BETWEEN TWO VARIABLES

check_mapping <-  function(data, var1, var2) {
  # Check if each value in var1 maps to a unique value in var2
  var1_to_var2_counts <- data %>%
    distinct({{ var1 }}, {{ var2 }}) %>%
    group_by({{ var1 }}) %>%
    summarize(count = n(), .groups = "drop")
  
  # Count of var1 values that map to more than one var2
  var1_multiple_var2 <- var1_to_var2_counts %>%
    filter(count > 1) %>%
    nrow()
  
  # Check if each value in var2 maps to a unique value in var1
  var2_to_var1_counts <- data %>%
    distinct({{ var1 }}, {{ var2 }}) %>%
    group_by({{ var2 }}) %>%
    summarize(count = n(), .groups = "drop")
  
  # Count of var2 values that map to more than one var1
  var2_multiple_var1 <- var2_to_var1_counts %>%
    filter(count > 1) %>%
    nrow()
  
  # Determine if there is a one-to-one correspondence
  is_one_to_one <- var1_multiple_var2 == 0 && var2_multiple_var1 == 0
  
  # Return results as a list
  return(list(
    is_one_to_one = is_one_to_one,
    var1_multiple_var2 = var1_multiple_var2,
    var2_multiple_var1 = var2_multiple_var1
  ))
}





