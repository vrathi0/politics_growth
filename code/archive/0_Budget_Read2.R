rm(list=ls())
source("code/functions.R")

# FUNCTION 

# TO STANDARDIZE HYPHENS
standardize_hyphens <- function(x) {
  # Replace various dash types with the standard hyphen-minus
  str_replace_all(x, "[\u002D\u2010\u2012\u2013\u2014]", "-")
}



# I FOUND THE ENTIRETY OF ALL STATE BUDGET DATA IN ONE EXCEL
#  PROCESSING ALL THE DATA HERE. 

#. https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=State%20Finances%20:%20A%20Study%20of%20Budgets

# I think the data is the line item: e-STATES Database in the recent years tab



# 1. READING IN -----------------------------------------------------------


raw_data=read_excel("../Data/Budget/All_State_All_Year.XLSX", 
                    sheet = "DATA") %>% clean_names()




# > names(raw_data)
# [1] "appendix"    "state_ut"    "budget_head" "fiscal_year"
# [5] "account"     "revised"     "budget"  

# There are 4 kinds of appendix in the data:

# Appendix I: Revenue Receipts of States and Union Territories with Legislature
# Appendix II: Revenue Expenditure of States and Union Territories with Legislature
# Appendix III: Capital Receipts of States and Union Territories with Legislature
# Appendix IV: Capital Expenditure of States and Union Territories with Legislature


# We will for now only pick out 2nd and 4th 


st_data=raw_data %>% filter(appendix %in% c("Appendix-2","Appendix-4")) %>% 
  filter(state_ut !="All States/UT")




# 2. CLEANING -----------------------------------------------------------


# creating RE (apdx 2) and CE  (apdx 4) variables

st_data=st_data %>% 
  mutate(exp_type=if_else(appendix=="Appendix-2","revenue", "capital")) %>% 
  dplyr::select(-appendix) %>% 
  mutate(year=sub(".*-(\\d{4})", "\\1", fiscal_year))

st_data=st_data %>% 
  mutate(budget_head=str_replace(budget_head,"of which:","of which"))

st_data$level <- str_count(st_data$budget_head, "\\.") + str_count(st_data$budget_head, ":") + 1
# st_data=st_data %>% 
#   mutate(level=if_else(exp_type=="revenue" & 
#                          grepl("of which: Power", budget_head),
#                        5, level))
st_data <- st_data %>%
  mutate(level = if_else(str_detect(budget_head, "etc\\.$"), 
                         level - 1, 
                         level))


rev_order_df=st_data %>% 
  filter(exp_type=="revenue") %>% 
  distinct(exp_type,budget_head, level) %>% 
  mutate(order=row_number())


cap_order_df=st_data %>% 
  filter(exp_type=="capital") %>% 
  distinct(exp_type,budget_head, level) %>% 
  mutate(order=row_number())

order_df=rev_order_df %>% bind_rows(cap_order_df)

st_data=st_data %>% inner_join(order_df) %>% 
  arrange(state_ut,exp_type, year, order)

st_data <- st_data %>%
  mutate(budget_head_clean = budget_head %>%
           str_replace_all(".*?([A-Z][a-z].*)", "\\1") %>%  # Remove hierarchical prefixes
           
           # Remove parentheses if they contain numbers, special characters, "+", "=", or Roman numerals
           str_replace_all("\\((?=.*?\\b[IVXLCDMivxlcdm]+\\b|[^a-zA-Z]*[0-9!@#$%^&*()\\[\\]{}<>\\-_=+\\.,:;/?~`+|]).*?\\)", "") %>%
           
           # Remove numbers and special characters from the beginning
           str_replace_all("[0-9]+[:\\.-]*\\s*", "") %>%
           str_replace_all("\\(([A-Za-z]+\\s*\\+\\s*[A-Za-z]+)\\)", "") %>% 
           
           str_trim()  # Trim extra whitespace
  )

st_data=st_data %>% 
  mutate(budget_head_clean=if_else(budget_head_clean=="I: DEVELOPMENTAL EXPENDITURE", 
                                  "DEVELOPMENTAL EXPENDITURE",
                                  budget_head_clean)) %>% 
  mutate(budget_head_clean=if_else(budget_head_clean=="General Services) (A to F)", 
                                    "NON-DEVELOPMENTAL EXPENDITURE (General Services)",
                                    budget_head_clean))

st_data <- st_data %>%
  mutate(section = str_extract(budget_head, "^[IVXLCDM]+")) %>%  # Extract the first Roman numeral
  mutate(section=if_else(is.na(section), "",section)) %>% 
  group_by(state_ut, exp_type, year, section) %>% 
  mutate(sno=row_number()) %>% ungroup()

# Trim extra whitespace

# Your data: st_data
budget_sector <- st_data %>%
  # Select relevant columns
  dplyr::select(budget_head_clean, exp_type, level, order) %>%
    distinct() %>%
  arrange(exp_type,  order) %>% 
  
  # Create the 'sector' variable
  mutate(sector = map_chr(seq_along(level), function(i) {
    
    # For exp_type == "capital"
    if (exp_type[i] == "capital") {
      if (level[i] == 4) {
        # Level 4: Sector is the current budget_head_clean
        return(budget_head_clean[i])
      } else if (level[i] == 5) {
        # Level 5: Find the last preceding level 4
        prev_level_4_index <- max(which(level[1:i] == 4))
        return(budget_head_clean[prev_level_4_index])
      } else if (level[i] == 6) {
        # Level 6: Find the last preceding level 5
        prev_level_5_index <- max(which(level[1:i] == 5))
        return(budget_head_clean[prev_level_5_index])
      }
    }
    
    # For exp_type == "revenue"
    if (exp_type[i] == "revenue") {
      if (level[i] == 3) {
        # Level 3: Sector is the current budget_head_clean
        return(budget_head_clean[i])
      } else if (level[i] == 4) {
        # Level 4: Find the last preceding level 3
        prev_level_3_index <- max(which(level[1:i] == 3))
        return(budget_head_clean[prev_level_3_index])
      } else if (level[i] == 5) {
        # Level 5: Find the last preceding level 4
        prev_level_4_index <- max(which(level[1:i] == 4))
        return(budget_head_clean[prev_level_4_index])
      }
    }
    
    # Return NA if none of the conditions are met
    return(NA_character_)
  }))


budget_sector=budget_sector %>% 
  mutate(sector=if_else(is.na(sector), "", sector))


all_budget= st_data %>% inner_join(order_df) %>% 
  inner_join(budget_sector) %>% 
  arrange(state_ut,exp_type, year, order) %>% 
  dplyr::select(state_ut, exp_type,order, budget_head_clean,level,
                sector,year, account, revised, budget, 
                everything())

all_budget=all_budget %>% 
  group_by(state_ut, exp_type, year) %>% 
  mutate(lead_sector=dplyr::lead(sector, default = first(sector))) %>% 
  mutate(sector=if_else(budget_head_clean==lead_sector, 
                        budget_head_clean, sector)) %>% 
  ungroup() %>% 
  mutate(sector=paste0(section, "-", sector)) %>% 
           dplyr::select(-lead_sector) %>% 
  mutate(section2=if_else(sno==1, budget_head_clean, NA)) %>% 
  fill(section2, .direction="down")

dd=all_budget %>% 
  filter(sno==1)

qsave(all_budget, file="data/clean/all_budget.qs")



# 3. CONSTRUCTING CROSS_SECTIONAL VERSION ---------------------------------

# 1. Limiting to section2 for which we have merged with iAS data
# 

bg_df1=all_budget %>%
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
  distinct() %>% 
  filter(!(exp_type=="capital"& level<=3) & 
           !(exp_type=="revenue" & level<3))# %>% 
 # rename(li=budget_head_clean) %>% 
 # mutate(sector=sub(".*-\\s*", "", sector))



# Step 1: Compute means for 'account', 'budget', and 'revised' at the group level (state_ut, exp_type, line_item)
bg_df1 <- bg_df1 %>%
  ungroup() %>%
  mutate_at(c("budget","revised","account"), as.numeric) %>% 
  rename(line_item = budget_head_clean) %>%
  mutate(account = clipp(account, 0.01, 0.99),
         budget = clipp(budget, 0.01, 0.99),
         revised = clipp(revised, 0.01, 0.99)) %>%
  mutate(re_be_ratio=revised/budget, 
         ac_re_ratio=account/revised, 
         ac_be_ratio=account/budget) %>% 
  group_by(state_ut, exp_type, line_item) %>%
  arrange(state_ut, exp_type, line_item, year) %>%
  mutate(ac_mean = mean(account, na.rm = TRUE),
         be_mean = mean(budget, na.rm = TRUE),
         re_mean = mean(revised, na.rm = TRUE),
         re_be_ratio_mean=mean(re_be_ratio, na.rm=T), 
         ac_re_ratio_mean=mean(ac_re_ratio, na.rm=T),
         ac_be_ratio_mean=mean(ac_be_ratio, na.rm=T)) %>%
  ungroup()

# Step 2: Compute the positional rank for each line_item using rank() or ecdf() within (state_ut, exp_type) groups
bg_df2 <- bg_df1 %>%
  dplyr::select(state_ut, exp_type, line_item, sector, section2,
                  ac_mean, be_mean, re_mean,
                re_be_ratio_mean,ac_re_ratio_mean,
                ac_be_ratio_mean) %>%
  distinct() %>%
  group_by(state_ut, exp_type) %>%
  mutate(ac_q = ecdf(ac_mean)(ac_mean),
         be_q = ecdf(be_mean)(be_mean),
         re_q = ecdf(re_mean)(re_mean)) %>%
  ungroup()

# Step 3: Compute R-squared for the time series of account, budget, and revised across years
compute_r_squared <- function(x, y) {
  model <- lm(y ~ x)
  summary(model)$r.squared
}

bg_df3 <- bg_df1 %>%
  mutate(year=as.numeric(year)) %>% 
  group_by(state_ut, exp_type, line_item) %>%
  summarise(
    ac_r2 = compute_r_squared(year, account),
    be_r2 = compute_r_squared(year, budget),
    re_r2 = compute_r_squared(year, revised),
    .groups = "drop"
  )

# Merging the results to create the final dataset
csec_budget <- bg_df2 %>%
  left_join(bg_df3, by = c("state_ut", "exp_type", "line_item"))



qsave(csec_budget, file="data/clean/csec_budget.qs")





