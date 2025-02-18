

# NOTE: 


# 0 HEADER ----------------------------------------------------------------

rm(list=ls())
source("code/functions.R")

# reading both capex state_dist_year agg data
# and TCPD election data

capex_df=qread("data/2 CLEAN/capex_time_geo_df.qs")
ac_lvl_ge_raw=read_csv("../Data/TCPD/General Election/AC_LEVEL_All_States_2024-11-14.csv") %>% 
  clean_names()

ge_raw=read_csv("../Data/TCPD/General Election/GE_All_States_2024-11-14.csv") %>% 
  clean_names()

ac_pc_dist_delim_map=qread("data/2 CLEAN/ac_pc_dist_delim_map.qs")




# 1. MERGING/ADDING ELEC VAR TO CAPEX-----------------------------------------------------

capex_df=capex_df %>% 
  mutate(delim_id=if_else(year<2009,3,4)) %>% 
  mutate(st_name=tolower(state),
         dist_name=tolower(district)) %>% 
  mutate(st_name=str_replace(st_name," ","_")) %>% 
  mutate(st_name=if_else(st_name=="nct_of delhi",
                         "delhi_&_ncr",st_name)) %>% 
  dplyr::select(-state, -district) %>% distinct()


dd=ac_pc_dist_delim_map %>% 
  dplyr::select(-ac_name, -ac_type, -match_pct1) %>% 
  distinct()
capex_delim=capex_df %>% 
  inner_join(dd, 
             by=c("st_name", "dist_name", "delim_id"))


####   1a  READING PC CLASSIFICATION ----------------------------------------------
# Each PC retains the reservation status or changes it in 2009
# Using pre-post status, we can assign the mobility type to each PC
# Before asembly_no 13, data is not that useful becuase of LS suspension in 1997ish

pc_dt=ge_raw %>% 
  filter(assembly_no  %in% c(13,14,15,16)) %>%  #Using PC level data
  filter(position==1) %>% distinct()

pc_dt=pc_dt %>% 
  rename(st_name=state_name, 
         pc_name=constituency_name, 
         pc_type=constituency_type) %>% 
  mutate(st_name=tolower(st_name),
         pc_name=tolower(pc_name)) %>% 
  mutate(stable=no_terms>1) #

pc_dist_delim=ac_pc_dist_delim_map %>% 
  dplyr::select(-ac_name, -ac_type ) %>% 
  distinct()

el_pc_delim=pc_dt %>% inner_join(pc_dist_delim)
# Joining with `by = join_by(st_name, delim_id,
#                            pc_name)`

mobility_classification <- el_pc_delim %>%
  filter(assembly_no  %in% c(13,14,15)) %>% 
  group_by(pc_uid) %>%
  reframe(
    type_13_14 = paste(unique(pc_type[assembly_no == 13]), unique(pc_type[assembly_no == 14]), sep = " to "),
    type_14_15 = paste(unique(pc_type[assembly_no == 14]), unique(pc_type[assembly_no == 15]), sep = " to "),
    delim_transition = paste(unique(delim_id[assembly_no == 13]), unique(delim_id[assembly_no == 15]), sep = " to ")
  ) %>%
  mutate(
    mob_cls = case_when(
      type_14_15 == "GEN to GEN" ~ "GEN to GEN",
      grepl("GEN to (SC|ST)", type_14_15) ~ "GEN to Res",
      grepl("(SC|ST) to GEN", type_14_15) ~ "Res to GEN",
      grepl("(SC|ST) to (SC|ST)", type_14_15) ~ "Res to Res",
      TRUE ~ "Other"
    )
  ) %>%  filter(mob_cls != "Other")

stable_at_ls14=el_pc_delim %>% 
  filter(assembly_no==14) %>% 
  dplyr::select(pc_uid, stable) %>% distinct()
pc_st=el_pc_delim %>% 
  dplyr::select(pc_uid, st_name) %>% distinct()

mobility_classification1=mobility_classification %>% 
  inner_join(pc_st, by="pc_uid") 



pc_mob_class=mobility_classification %>% 
  dplyr::select(pc_uid, mob_cls) %>% distinct() %>% 
  right_join(el_pc_delim) %>% 
  dplyr::select(pc_uid, mob_cls, assembly_no, 
                incumbent) %>% 
  distinct()

# Assigning assembly_no
# Create the assembly_no mapping
capex_delim <- capex_delim %>%
  mutate(
    assembly_no = case_when(
      year >= 2014 & year <= 2019 ~ 16,
      year >= 2009 & year <= 2013 ~ 15,
      year >= 2004 & year <= 2008 ~ 14,
      year >= 1999 & year <= 2003 ~ 13,
      TRUE ~ NA_real_  # Default NA for unmatched years
    )
  ) %>% filter(!is.na(assembly_no)) %>% distinct()

capex_delim_pc=capex_delim %>% inner_join(pc_mob_class)
# Joining with `by = join_by(pc_uid,
#                            assembly_no)`

# Now here we have PC-DISTRICT level capex data merged with 
# PD EL data so that we have mob_cls for each PC, 
# incumbent status for each PC
# > table(capex_delim_pc$incumbent)/nrow(capex_delim_pc)
# 
# FALSE      TRUE 
# 0.6193518 0.3806482 



# 2 SAVING CAPEX-PC MERGED DATA -------------------------------------------
qsave(capex_delim_pc, 
      file="data/2 CLEAN/capex_delim_pc.qs")




# ROUGH
# making a rough cut aggregating at PC level
# and taking between 2004 and 2013



vlist=names(capex_delim1)[5:28]

pc_capex_df <- capex_delim1 %>%
  filter(!is.na(assembly_no)) %>% 
  group_by( assembly_no, mob_cls) %>%
  mutate(across(all_of(vlist), ~sum(.x, na.rm = TRUE))) %>%
  ungroup() 

pc_capex_df1 <- pc_capex_df %>%
  dplyr::select(all_of(vlist), 
                delim_id, assembly_no, mob_cls,year) %>% 
  distinct()

normalize_y_vars <- function(df, y_vars) {
  df %>%
    group_by(mob_cls) %>%                                # Group by mob_cls
    mutate(across(
      all_of(y_vars),                                    # Apply to all y variables
      ~ . / .[assembly_no == 13],                        # Normalize within group
      .names = "{.col}"                                  # Keep the same column names
    )) %>%
    ungroup()                                            # Remove grouping
}


y="rank1_events_count"
df=pc_capex_df1 %>% 
  dplyr::select(-year) %>%
  filter(!is.na(mob_cls)) %>% 
  distinct() %>% ungroup()

df_normalized <- normalize_y_vars(df, vlist)



# Generating all plots: 

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)  # For grid layout
library(purrr)      # For iteration
library(grid)       # For shared legends

# Function to extract the shared legend from a ggplot object
extract_legend <- function(plot) {
  g <- ggplotGrob(plot)
  legend <- g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
  return(legend)
}

# Function to generate plots for each variable and save them to PDF
plot_variables_to_pdf <- function(df, y_vars, output_file) {
  # Create a list of ggplot objects for each variable
  plots <- map(y_vars, function(var) {
    ggplot(df, aes(x = assembly_no, y = .data[[var]], 
                   group = mob_cls, color = mob_cls)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      geom_vline(xintercept = 14, linetype = "dashed", color = "black", size = 1) + # Add vertical dashed line
      labs(
        title = paste(var),
        x = "",
        y = var,
        color = ""
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        legend.position = "right"
      )
  })
  
  # Extract the shared legend from the first plot
  shared_legend <- extract_legend(plots[[1]])
  
  # Remove legends from all individual plots
  plots_no_legend <- map(plots, ~ .x + theme(legend.position = "none"))
  
  # Save all plots with the shared legend to a PDF
  pdf(output_file, width = 12, height = 10) # Set PDF output dimensions
  
  for (i in seq_along(plots_no_legend)) {
    # Arrange 2 plots per row and add the shared legend to the right
    grid.arrange(
      grobs = list(plots_no_legend[[i]], shared_legend),
      ncol = 2,
      widths = c(5, 1) # Adjust widths: 5 for each plot, 2 for the legend
    )
  }
  
  dev.off() # Close the PDF device
}

# Example usage
output_file <- "fig/capex_mob_cls_time_series.pdf"

# Call the function
#plot_variables_to_pdf(df_normalized, vlist, output_file)


# FOLLOWING CODE CHECKS: 

# 1. For which y-var GEN to GEN growth outstrips GEN to Res

# Assuming df is the dataset used for plotting

# Define the y-vars to analyze
# Filter the dataset for assembly_no > 14
filtered_df <- df_normalized %>%
  filter(assembly_no > 15)

# Identify y-vars where GEN to GEN is above GEN to Res
y_vars_above <- vlist %>%
  keep(function(var) {
    filtered_data <- filtered_df %>%
      filter(mob_cls %in% c("Res to GEN", "Res to Res")) %>%
      group_by(assembly_no) %>%
      summarise(
        gen_to_gen = .data[[var]][mob_cls == "Res to GEN"],
        gen_to_res = .data[[var]][mob_cls == "Res to Res"]
      ) %>%
      ungroup()
    
    # Check if GEN to GEN is consistently above GEN to Res
    all(filtered_data$gen_to_gen > filtered_data$gen_to_res, na.rm = TRUE)
  })

# Output the list of y-vars
y_vars_above


# Learning here right now is that
# 1. Within the samply of GEN PC, converting to RES significantly reduces almost all vlist vars
# 2. The effect is pretty much non-existent for PC starting as RES

## REGRESSION

# Step 1: Aggregate variables in `vlist` at the level of `pc_uid` and `assembly_no`
vlist=names(capex_delim1)[5:28]
pc_agg <- capex_delim1 %>%
  filter(!is.na(assembly_no)) %>% 
  group_by(pc_uid, assembly_no) %>%
  mutate(across(
    all_of(vlist),
    ~ sum(.x, na.rm = TRUE) # Aggregate variables using mutate
  )) %>% ungroup()

pc_agg=pc_agg %>% 
  dplyr::select(all_of(vlist), 
                pc_uid, assembly_no, 
                st_name, mob_cls) %>% 
  distinct()

# Step 2: Normalize variables within each `pc_uid` using values at `assembly_no == 13`
# first removing where min ass_no!~=13
pc_agg_full=pc_agg %>% 
  group_by(pc_uid) %>% 
  mutate(min_ass=min(assembly_no, na.rm=T)) %>% 
  ungroup() %>% 
  filter(min_ass==13)

pc_normalized <- pc_agg_full %>%
  group_by(pc_uid) %>%
  mutate(across(
    all_of(vlist),
    ~ (. + 1) / (. [assembly_no == 13] + 1), # Add 1 to both numerator and denominator
    .names = "{.col}"  # Retain the same variable names
  )) %>%  ungroup()



# Trying to see the same in a regression framework. 
# data: capex_delim1

yvar=vlist[1]
dt=pc_normalized %>% 
  filter(mob_cls %in% c("GEN to Res","GEN to GEN")) %>% 
  mutate(res_dum=if_else(mob_cls=="GEN to Res",1,0)) %>% 
  mutate(post_treatment = if_else(assembly_no >= 15, 1, 0)) %>% 
  mutate(event_time = assembly_no - 15) %>% 
  mutate(event_time_factor = as.factor(event_time))

dt_event=dt %>% 
  filter(event_time != 0)

fml_did <- as.formula(
  paste0(yvar, " ~ post_treatment * res_dum |st_name ")
)
reg=felm(fml_did, dt)
summary(reg)



fml_event<- as.formula(
  paste0(yvar, " ~ event_time_factor * res_dum | st_name")
)

# Run event study regression
reg_event <- felm(fml_event, data = dt_event)
summary(reg_event)


# PC level combined regression -------------------------------------------------------

library(dplyr)
library(lfe)

# Define the function
run_did_analysis <- function(data, yvars) {
  # Prepare the data
  dt <- data %>%
    filter(mob_cls %in% c("GEN to Res", "GEN to GEN")) %>%
    mutate(
      res_dum = if_else(mob_cls == "GEN to Res", 1, 0),
      post_treatment = if_else(assembly_no >= 15, 1, 0),
      event_time = assembly_no - 15,
      event_time_factor = as.factor(event_time)
    )
  
  # Iterate over yvars and collect results
  results <- lapply(yvars, function(yvar) {
    # Define the regression formula
    fml_did <- as.formula(
      paste0(yvar, " ~ post_treatment * res_dum | st_name")
    )
    
    # Run the regression
    reg <- felm(fml_did, data = dt)
    
    # Extract the coefficient and t-value for the interaction term
    coef <- summary(reg)$coefficients["post_treatment:res_dum", "Estimate"]
    t_val <- summary(reg)$coefficients["post_treatment:res_dum", "t value"]
    
    # Return results for this yvar
    data.frame(
      yvar = yvar,
      beta = coef,
      t_val = t_val
    )
  })
  
  # Combine results into a single dataframe
  results_df <- do.call(rbind, results)
  
  return(results_df)
}

# Example usage
results_df <- run_did_analysis(pc_normalized, vlist)

# View results
print(results_df)





# AT mob_cls level

yvar=vlist[1]
dt=capex_delim1 %>% 
  filter(mob_cls %in% c("GEN to Res","GEN to GEN")) %>% 
  mutate(res_dum=if_else(mob_cls=="GEN to Res",1,0)) %>% 
  mutate(post_treatment = if_else(assembly_no >= 15, 1, 0)) %>% 
  mutate(event_time = assembly_no - 15) %>% 
  mutate(event_time_factor = as.factor(event_time))

dt_event=dt %>% 
  filter(event_time != 0)

fml_did <- as.formula(
  paste0(yvar, " ~ post_treatment * res_dum |state ")
)
reg=felm(fml_did, dt)
summary(reg)






# fml=as.formula(paste0(y,"~ mob_cls + as.factor(year)*mob_cls"))
# 
# reg=felm(formula = fml, 
#          data=dt)
# summary(reg)


