rm(list=ls())
source("code/functions.R")



# I am reading the raw budget docs from RBI website in 0_Budget_Read.R
# Here I am doing a basic descr ana of the numbers

# Should also include a section upto with summary points

#Goal:
# A: The ratio (acc_be or re_be) for any year is a measure of quality of initial 
#. budget expense (be). Higher the ratio, poor the quality.  So then, 
#  the trend in this ratio is a measure of how this quality is trending across year. 

## SUMMARY POINTS ( for quick review)
# 1. (not useful) It seems like RE trend line is weaker than BE trend line. 

        # > quantile(sumstat1_df$trend_coefficient[sumstat1_df$variable==
        #  "total_re"]/sumstat1_df$trend_coefficient[sumstat1_df$variable==
         #. "total_be"], c(0.05,0.25,0.5,0.75, 0.95), na.rm=T)
        
        # 5%       25%       50%       75%       95% 
        # 0.4678937 0.8998270 0.9777135 1.0961906 2.1823252 

# 2. The raw ratio itself seems quite close to 1 for both acc_be, and re_be. 
  # Median is less than 1. 
          # > quantile(st_budget2$re_be_ratio, c(0.1, 0.25, 0.5, 0.7, 0.9), na.rm=T)
          # 10%       25%       50%       70%       90% 
          # 0.8286190 0.9746611 1.0064878 1.0942201 1.4597195 
          # > quantile(st_budget2$acc_be_ratio, c(0.1, 0.25, 0.5, 0.7, 0.9), na.rm=T)
          # 10%       25%       50%       70%       90% 
          # 0.5246705 0.7877265 0.9605364 1.0508424 1.4529098 

# 3. Across both states, the trend line in "R" is much steeper than 
#.  trend line in "C" type expenses. But "R" trend line has a much higher
#.  R2 than "C" line. Meaning, "R" is mostly following some formula and smooth. 

## 4. It seems like the re/be and acc/be ratio are quite close to 1 generally 
## and their trend line is mostly flat. R2 of the trend line seems not interesting 
## and weird to interperate. 

# Final Takeaway: 
# 1. Maybe focus more on capital expenses as they seem more unpredictable. 
# 2. But steeper trend line in R expenses can be used to classification task?

# CRITERIA FOR TAGGING A YOY CHANGE ABNORMAL
#mutate(abnormal_change=abs(z_acc)>2 | z_acc <= -1.5) %>% 


# READING IN --------------------------------------------------------------

qreadm("data/clean/state_budget.qs")



# Calculate the standard deviation and mean of the YoY changes for each sector
st_budget4 <- st_budget3 %>%
  group_by(state, type, line_item) %>%
  mutate_at(vars(c("total_acc_delta", "total_be_delta", "total_re_delta")),
            list(mean = ~mean(., na.rm=T), sd = ~sd(., na.rm=T))) %>% 
  ungroup() %>% 
  mutate(z_acc = (total_acc_delta - total_acc_delta_mean) / total_acc_delta_sd) %>%
  mutate(abnormal_change=abs(z_acc)>2 | z_acc <= -1.5) %>% 
  group_by(state, type, line_item) %>% 
  mutate(ss=sum(abnormal_change, na.rm=T)) %>% 
  mutate(abnormal_line=ss!=0) %>% ungroup() %>% 
  dplyr::select(state, type, line_item, year, total_acc, 
                total_be, total_re, sector, abnormal_line, abnormal_change) %>% 
  distinct()




## List of potential analysis
## 1. For any line-item, after taking out the trend line.. how big are teh shocks. 
## 2. Or, comparing the magnitude of the trend line slope. 
## 3. Characterize the budget revision from BE to RE for each line item. 


# 1. TREND LINES FOR EACH LINE ITEMS --------------------------------------

compute_trend_stats <- function(data, numeric_var, st,t, li) {
  
  
  # data <- data %>%
  #   mutate(across(all_of(numeric_var), ~ . / 10e6))
  
  # Filter the data for the specified state and line item
  filtered_data <- data %>%
    dplyr::filter(state == st & line_item == li & type==t)
  
  # Check if the filtered data has enough data points to perform regression
  if (nrow(filtered_data) < 2) {
    warning("Not enough data points for regression.")
    return(NA)
  }
  
  # Fit a linear model
  trend_reg <- felm(as.formula(paste(numeric_var, "~ year")), data = filtered_data)
  
  # Extract the coefficient for 'year'
  coefficient <- as.numeric(coef(trend_reg)["year"])
  
  # Calculate R-squared
  r_squared <- summary(trend_reg)$r.squared
  
  # Calculate coefficient of variation (CV = standard deviation / mean)
  # Coefficient of Variation = (Standard Deviation / Mean) * 100
  cv <- (sd(filtered_data[[numeric_var]], na.rm = TRUE) / 
           mean(filtered_data[[numeric_var]], na.rm = TRUE)) 
  
  # Return the results as a list
  results <- list(
    trend_coefficient = coefficient,
    r_squared = r_squared,
    coefficient_of_variation = cv, 
    nobs=trend_reg$N
  ) %>% as.data.frame()
  
  return(results)
}


nvec=c("total_be", "re_be_ratio","acc_be_ratio")

input_df=st_budget2 %>% ungroup() %>% 
  dplyr::select(line_item, type,state) %>% 
  distinct() 

expanded_df <- input_df %>%
  mutate(temp_key = 1) %>%
  left_join(data.frame(temp_key = 1, variable = nvec), by = "temp_key") %>%
  dplyr::select(-temp_key)

# Initialize an empty list to store results
sumstat1 <- list()

# Loop over each row in expanded_df
for (i in 1:nrow(expanded_df)) {
  
  
  row <- expanded_df[i, ]
  
  # Use tryCatch to handle errors
  result <- tryCatch({
    # Extracting the values for each parameter from the row
    line_item <- row$line_item
    type <- row$type
    state <- row$state
    variable <- row$variable
    
    # Call the compute_trend_stats function
    compute_trend_stats(numeric_var = variable, st = state, 
                        li = line_item, t = type, data = st_budget2)
  }, error = function(e) {
    # Print error message and return NULL or a specific error value
    message(sprintf("Error in row %d: %s", i, e$message))
    return(data.frame(trend_coefficient = NA,
                      r_squared = NA,
                      coefficient_of_variation = NA, 
                      nobs=NA))
  })
  
  # Store the result
  sumstat1[[i]] <- cbind(row, result)
}


sumstat1_df=sumstat1 %>% bind_rows()


  



# Plotting 

# Assuming your DataFrame is named `df`
# Reshape data to wide format
df=sumstat1_df
wide_df <- df %>%
  pivot_wider(names_from = variable, 
              values_from = c(trend_coefficient, r_squared, coefficient_of_variation))

# Calculate new variables
r1_df <- wide_df %>%
  mutate(
    trend_coefficient_ratio_acc_be = trend_coefficient_total_acc / trend_coefficient_total_be,
    trend_coefficient_ratio_re_be = trend_coefficient_total_re / trend_coefficient_total_be,
    r_squared_ratio_acc_be = r_squared_total_acc / r_squared_total_be,
    r_squared_ratio_re_be = r_squared_total_re / r_squared_total_be,
    coefficient_of_variation_ratio_acc_be = coefficient_of_variation_total_acc / coefficient_of_variation_total_be,
    coefficient_of_variation_ratio_re_be = coefficient_of_variation_total_re / coefficient_of_variation_total_be
  ) %>%
  # Selecting only necessary columns or reformatting as needed
  dplyr::select(line_item, type, state, 
         trend_coefficient_ratio_acc_be, trend_coefficient_ratio_re_be,
         r_squared_ratio_acc_be, r_squared_ratio_re_be,
         coefficient_of_variation_ratio_acc_be, coefficient_of_variation_ratio_re_be)



# Doing similar analysis on st_budget2

input_df=st_budget2 %>% ungroup() %>% 
  dplyr::select(line_item, type,state) %>% 
  distinct() 

nvec=c("re_be_ratio","acc_be_ratio")
expanded_df <- input_df %>%
  mutate(temp_key = 1) %>%
  left_join(data.frame(temp_key = 1, variable = nvec), by = "temp_key") %>%
  dplyr::select(-temp_key)

# Initialize an empty list to store results
sumstat2 <- list()

# Loop over each row in expanded_df
for (i in 1:nrow(expanded_df)) {
  
  
  row <- expanded_df[i, ]
  
  # Use tryCatch to handle errors
  result <- tryCatch({
    # Extracting the values for each parameter from the row
    line_item <- row$line_item
    type <- row$type
    state <- row$state
    variable <- row$variable
    
    # Call the compute_trend_stats function
    compute_trend_stats(numeric_var = variable, st = state, 
                        li = line_item, t = type, data = st_budget2)
  }, error = function(e) {
    # Print error message and return NULL or a specific error value
    message(sprintf("Error in row %d: %s", i, e$message))
    return(data.frame(trend_coefficient = NA,
                      r_squared = NA,
                      coefficient_of_variation = NA, 
                      nobs=NA))
  })
  
  # Store the result
  sumstat2[[i]] <- cbind(row, result)
}


sumstat2_df=sumstat2 %>% bind_rows()

ratio_trend_df=sumstat2_df %>% group_by(state, type, variable) %>% 
  summarise(r_q10=quantile(trend_coefficient, c(0.1), na.rm=T), 
         r_q25=quantile(trend_coefficient, c(0.25), na.rm=T),
         r_q50=quantile(trend_coefficient, c(0.5), na.rm=T),
         r_q75=quantile(trend_coefficient, c(0.75), na.rm=T),
         r_q90=quantile(trend_coefficient, c(0.95), na.rm=T))






# 2. Effort to cluster sectors into different similar bins ----------------

# First, just plotting a simple trend line for each sector in one plot

df=st_budget2
# clipping 1% outlier
df=df %>% mutate(total_acc=clipp(total_acc, 0.01, 0.99)) %>% 
  filter(type=="C") %>% filter(state=="RJ")
li_l=unique(df$line_item)
li_sample=li_l[sample(length(li_l), 22)]
pdf=df %>% filter(line_item %in% li_sample)

ggplot(df, aes(x = year, y = log(total_acc), 
               color = line_item, group = line_item)) +
  geom_line(alpha = 0.5, linewidth = 0.5) +
  theme_minimal() +
  theme(legend.position = "none")

# It seems like most budget trend lines have a uniform slope. 
# There are a few trend lines that have jumps in some years (only for R). 
# Therefore: (1) It is okay to seggregate sectors based on mean slope, 
# (2) I am also going to tag those sector that have an abnormal jumps. These can be potential shocks later on 
# For C, the trend is quite erratic and have a low r2 wrt trend line












# OLD

# PROC-1 ------------------------------------------------------------------

df=state_budget %>%  filter(!is.na(y9)) %>% 
  arrange(state,type,line_item, year ) %>% 
  group_by(state, type, line_item) %>% 
  mutate(line_item_count=n_distinct(year)) %>%  ungroup() %>% 
  filter(line_item_count>=4)

# Getting linear time trend

trend_dt=df 
# Function to fit linear model and extract trend coefficient
fit_trend <- function(data) {
  model <- lm(y9 ~ year, data = data)
  coef(summary(model))["year", "Estimate"]
}

# Group the data and compute trend coefficients
trend_coefficients <- trend_dt %>%
  group_by(state, type, line_item) %>%
  summarise(trend_coefficient = fit_trend(cur_data()), .groups = "drop")


