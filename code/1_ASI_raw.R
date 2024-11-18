rm(list=ls())
source("code/functions.R")



## Here I am mostly just reading in raw data to construct various metrics like time series of density. 


# CONSTRUCTING LEVELS MEASURE  --------------------------------------------



# READING RAW -------------------------------------------------------------

# Listing files:

flist_A=list.files(path="../Data/ASI", 
                   pattern = "^(A|BlockA|BLOCK-A|blka|blkA).*\\.dta$", 
                   recursive=T, 
                   full.names = T)



# I can choose just a few vars using the following code, but I am just keeping all the vars for now. 
# Selecting states and district variables

# # Pattern for variable selection
# pattern <- "State|District|Year|Inflation|Multiplier|Serial|Dispatch"
# 
# 
# getL=function(x){
#   l=as.character(lapply(as.list(x), attr, "label"))
#   
#   return(l)
#   
# }
#

# Pattern for variable selection
  p <- "despatch|dispatch|psl|scheme|state|district|year|inflation|multiplier|serial|sector|status|units|open|closed"
# 
getSub = function(x, pattern) {
  # Extract labels from the attributes of the input data frame
  l = as.character(lapply(as.list(x), attr, "label"))
  names(x)=l
  x=x %>% clean_names() %>% mutate_all(as.character)
  

  # Subset the data frame based on the labels that match the specified pattern
  d = x[, grep(pattern, names(x))]

  # Return the resulting data frame
  return(d)
}


resetName = function(x) {
  # Extract labels from the attributes of the input data frame
  l = as.character(lapply(as.list(x), attr, "label"))
  
    # Extract column names from the labels and assign them to the subsetted data frame
  names(x) =l
  
  # Clean column names and convert all columns to character type
  x = x %>% clean_names() %>% mutate_all(as.character)
  
  # Return the resulting data frame
  return(x)
}


# Function to rename variables based on pattern match
rename_variables <- function(df, pattern, new_names) {
  old_names <- names(df)
  matching_names <- old_names[grep(pattern, old_names)]
  
  # Rename only if there are matching names
  if (length(matching_names) > 0) {
    df <- df %>% rename_at(vars(matching_names), list(~ new_names))
  }
  
  return(df)
}

#ss=readstata13::read.dta13(flist_A[[1]])

# READING ALL THE A-FiLES


dt0=lapply(flist_A, read_dta)  # 
dt0=lapply(dt0, resetName)

dt0=c(dt0[-1], dt0[1])

# # Now I need to rename all the vars using the labels in the data
# # Pattern for variable selection
# p <- "despatch|dispatch|psl|scheme|state|district|year|inflation|multiplier|serial|sector|status|units|open|closed"
# 
# 
# 
# # Pattern to match in variable names
# pattern_to_match <- "state|district|year|inflation|sector|status"
# 
# # New names for the variables
# new_variable_names <- c("state", "district", "year","weight","rural_urban","status")  # Replace with your actual names
# 
# # Rename variables for each data frame in the list
# renamed_dfs <- lapply(list_of_dfs, function(df) rename_variables(df, pattern_to_match, new_variable_names))
# 
# # Now, all data frames in 'renamed_dfs' have the same variable names# 
# 
# dt0=lapply(dt0, getSub, pattern=p)


# BRUTE FORCE RENAMING VARIABLES

dt1=list()
# y99

df=dt0[[1]]
df=df %>% rename(year=year,
                  state=state_code, 
                 district=district_code, 
                 status=open_closed,
                 mult=inflation_multiplier_factor_in_9999_9999_format,
                 unit_size=no_of_units,
                 rural_urban=sector) %>% 
  mutate(year="1999") %>% mutate(mult=as.numeric(mult))
dt1[[1]]=df


#y00

df=dt0[[2]]
df=df %>% rename(state=state_code, 
                 district=district_code, 
                 status=status_of_unit_code,
                 mult=inflation_multiplier_factor_in_9999_9999_format,
                 unit_size=no_of_units,
                 rural_urban=rural_urban_code,
                 tot_wrk_days=number_of_total_working_days,
                 tot_cost_prd=total_cost_of_production) %>% 
  mutate(year="2000") %>% mutate(mult=as.numeric(mult))
dt1[[2]]=df


# y01

df=dt0[[3]]
df=df %>% rename(state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_code,
                 status=status_of_unit_code_17_to_20_extracted_data_from_asi_03_04,
                 mult=inflation_multiplier_factor_in_9999_9999_format,
                 unit_size=no_of_units,
                 tot_wrk_days=number_of_working_days_total,
                 tot_wrk_days_manuf=number_of_working_days_manufacturing_days,
                 tot_wrk_days_nonmanuf=number_of_working_days_non_manufacturing_days,
                 tot_cost_prd=cost_of_production) %>% 
  mutate(year="2001") %>% mutate(mult=as.numeric(mult))
dt1[[3]]=df

# y02

df=dt0[[4]]
df=df %>% rename(state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_code,
                 status=status_of_unit_code_17_to_20_extracted_data_from_asi_00_01,
                 tot_wrk_days=number_of_working_days_total,
                 tot_wrk_days_manuf=number_of_working_days_manufacturing_days,
                 tot_wrk_days_nonmanuf=number_of_working_days_non_manufacturing_days,
                 mult=inflation_multiplier_factor_in_9999_9999_format,
                 unit_size=no_of_units,
                 tot_cost_prd=cost_of_production) %>% 
  mutate(year="2002") %>% mutate(mult=as.numeric(mult))
dt1[[4]]=df


# y03

df=dt0[[5]]
df=df %>% rename(state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_code,
                 status=status_of_unit_code_17_to_20_extracted_data_from_asi_01_02,
                 tot_wrk_days=number_of_working_days_total,
                 tot_wrk_days_manuf=number_of_working_days_manufacturing_days,
                 tot_wrk_days_nonmanuf=number_of_working_days_non_manufacturing_days,
                 mult=inflation_multiplier_factor_in_9999_9999_format,
                 unit_size=no_of_units,
                 tot_cost_prd=cost_of_production) %>% 
  mutate(year="2003") %>% mutate(mult=as.numeric(mult))
dt1[[5]]=df


# y04

df=dt0[[6]]
df=df %>% rename(state=state_code, 
                 district=district_code, 
                 rural_urban=sector_rural_urban,
                 status=status_of_unit,
                 tot_wrk_days=number_of_working_days_total,
                 tot_wrk_days_manuf=number_of_working_days_manufacturing_days,
                 tot_wrk_days_nonmanuf=number_of_working_days_non_manufacturing_days,
                 mult=inflation_multiplier_factor_in_9999_9999_format,
                 unit_size=no_of_units,
                 tot_cost_prd=cost_of_production) %>% 
  mutate(year="2004") %>% mutate(mult=as.numeric(mult))
dt1[[6]]=df



# y05

df=dt0[[7]]
df=df %>% rename(state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_code,
                 status=status_of_unit_code_17_to_20_extracted_data_from_asi_03_04,
                 tot_wrk_days=number_of_working_days_total,
                 tot_wrk_days_manuf=number_of_working_days_manufacturing_days,
                 tot_wrk_days_nonmanuf=number_of_working_days_non_manufacturing_days,
                 mult=inflation_multiplier_factor_in_9999_9999_format,
                 unit_size=no_of_units,
                 tot_cost_prd=cost_of_production)%>% 
  mutate(year="2005") %>% mutate(mult=as.numeric(mult))
dt1[[7]]=df


# y06

df=dt0[[8]]
df=df %>% rename(state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_sector,
                 status=status_of_units,
                 tot_wrk_days=no_of_total_working_days,
                 tot_wrk_days_manuf=no_of_manufacturing_days,
                 tot_wrk_days_nonmanuf=no_of_non_manufacturing_days,
                 mult=multiplier_factor,
                 unit_size=no_of_factories,
                 tot_cost_prd=cost_of_production)  %>% mutate(mult=as.numeric(mult))
dt1[[8]]=df



# y07

df=dt0[[9]]
df=df %>% rename(year=year_07,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_sector,
                 status=status_of_factory,
                 tot_wrk_days=no_of_working_days_total,
                 tot_wrk_days_manuf=no_of_manufacturing_days,
                 tot_wrk_days_nonmanuf=no_of_non_manufacturing_days,
                 mult=multiplier_factor,
                 unit_size=no_of_factories,
                 tot_cost_prd=cost_of_production) %>% mutate(mult=as.numeric(mult))
dt1[[9]]=df


# y08

df=dt0[[10]]
df=df %>% rename(year=year,
                 state=state, 
                 district=district, 
                 rural_urban=sector,
                 status=status_of_factory,
                 tot_wrk_days=no_of_total_working_days,
                 tot_wrk_days_manuf=no_of_manufacturing_days,
                 tot_wrk_days_nonmanuf=no_of_non_manufacturing_days,
                 mult=multiplier,
                 unit_size=no_of_factories,
                 tot_cost_prd=cost_of_production) %>% mutate(mult=as.numeric(mult))
dt1[[10]]=df




# y09

df=dt0[[11]]
df=df %>% rename(year=year,
                 state=state, 
                 district=district, 
                 rural_urban=sector,
                 status=status_of_factory,
                 tot_wrk_days=number_of_total_working_days,
                 tot_wrk_days_manuf=number_of_manufacturing_days,
                 tot_wrk_days_nonmanuf=number_of_non_manufacturing_days,
                 mult=multiplier_factor,
                 unit_size=no_of_factories,
                 tot_cost_prd=total_cost_of_production)%>% 
  mutate(year="2009") %>% mutate(mult=as.numeric(mult))
dt1[[11]]=df


#y10
df=dt0[[12]]
df=df %>% rename(year=year,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=sector_code,
                 status=status_of_factory_code,
                 tot_wrk_days=number_of_total_working_days,
                 tot_wrk_days_manuf=number_of_manufacturing_days,
                 tot_wrk_days_nonmanuf=number_of_non_manufacturing_days,
                 mult=multiplier_factor,
                 unit_size=no_of_factories,
                 tot_cost_prd=total_cost_of_production)%>% 
  mutate(year="2010") %>% mutate(mult=as.numeric(mult))
dt1[[12]]=df



#y11
df=dt0[[13]]
df=df %>% rename(year=year,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_code,
                 status=status_of_unit,
                 tot_wrk_days=total_no_of_working_days,
                 tot_wrk_days_manuf=no_of_working_days_manufacturing,
                 tot_wrk_days_nonmanuf=no_of_working_days_non_manufacturing,
                 mult=multilplier_factor,
                 unit_size=no_of_units,
                 tot_cost_prd=cost_of_production) %>% mutate(mult=as.numeric(mult))
dt1[[13]]=df


#y12
df=dt0[[14]]
df=df %>% rename(year=year,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_code,
                 status=status_of_unit,
                 tot_wrk_days=total_no_of_working_days,
                 tot_wrk_days_manuf=no_of_working_days_manufacturing,
                 tot_wrk_days_nonmanuf=no_of_working_days_non_manufacturing,
                 mult=multilplier_factor,
                 unit_size=no_of_units,
                 tot_cost_prd=cost_of_production) %>% mutate(mult=as.numeric(mult))
dt1[[14]]=df

#y13
df=dt0[[15]]
df=df %>% rename(year=year,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_code,
                 status=status_of_unit,
                 tot_wrk_days=total_no_of_working_days,
                 tot_wrk_days_manuf=no_of_working_days_manufacturing,
                 tot_wrk_days_nonmanuf=no_of_working_days_non_manufacturing,
                 mult=multilplier_factor,
                 unit_size=no_of_units,
                 tot_cost_prd=cost_of_production) %>% mutate(mult=as.numeric(mult))
dt1[[15]]=df


#y13  ( Y13 IS REPEATED TWICE)
df=dt0[[16]]
df=df %>% rename(year=year,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_code,
                 status=status_of_unit,
                 tot_wrk_days=total_no_of_working_days,
                 tot_wrk_days_manuf=no_of_working_days_manufacturing,
                 tot_wrk_days_nonmanuf=no_of_working_days_non_manufacturing,
                 mult=multilplier_factor,
                 unit_size=no_of_units,
                 tot_cost_prd=cost_of_production) %>% mutate(mult=as.numeric(mult))
dt1[[16]]=df

# y14
df=dt0[[17]]
df=df %>% rename(year=year,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_code,
                 status=status_of_unit,
                 tot_wrk_days=total_no_of_working_days,
                 tot_wrk_days_manuf=no_of_working_days_manufacturing,
                 tot_wrk_days_nonmanuf=no_of_working_days_non_manufacturing,
                 mult=multilplier_factor,
                 unit_size=no_of_units,
                 tot_cost_prd=cost_of_production) %>% mutate(mult=as.numeric(mult))
dt1[[17]]=df


# y15
df=dt0[[18]]
df=df %>% rename(year=year,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_code,
                 status=status_of_unit,
                 tot_wrk_days=total_no_of_working_days,
                 tot_wrk_days_manuf=no_of_working_days_manufacturing,
                 tot_wrk_days_nonmanuf=no_of_working_days_non_manufacturing,
                 mult=multilplier_factor,
                 unit_size=no_of_units,
                 tot_cost_prd=cost_of_production) %>% mutate(mult=as.numeric(mult))
dt1[[18]]=df


# y15  *Y15 is also REPEATED TWICE
df=dt0[[19]]
df=df %>% rename(year=year,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=rural_urban_code,
                 status=status_of_unit,
                 tot_wrk_days=total_no_of_working_days,
                 tot_wrk_days_manuf=no_of_working_days_manufacturing,
                 tot_wrk_days_nonmanuf=no_of_working_days_non_manufacturing,
                 mult=multilplier_factor,
                 unit_size=no_of_units,
                 tot_cost_prd=cost_of_production) %>% mutate(mult=as.numeric(mult))
dt1[[19]]=df



# y17   ( NO Y16)
df=dt0[[20]]
df=df %>% rename(year=year,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=sector_rural_urban,
                 status=status_of_unit,
                 tot_wrk_days=number_of_working_days_total,
                 tot_wrk_days_manuf=number_of_working_days_manufacturing_days,
                 tot_wrk_days_nonmanuf=number_of_working_days_non_manufacturing_days,
                 mult=multiplier_in_9999_99999999,
                 unit_size=no_of_units,
                 tot_cost_prd=total_cost_of_production)%>% 
  mutate(year="2017") %>% mutate(mult=as.numeric(mult))
dt1[[20]]=df

# y18
df=dt0[[21]]
df=df %>% rename(year=year,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=sector_rural_urban,
                 status=status_of_unit,
                 tot_wrk_days=number_of_working_days_total,
                 tot_wrk_days_manuf=number_of_working_days_manufacturing_days,
                 tot_wrk_days_nonmanuf=number_of_working_days_non_manufacturing_days,
                 mult=multiplier_in_9999_99999999,
                 unit_size=no_of_units,
                 tot_cost_prd=total_cost_of_production)%>% 
  mutate(year="2018") %>% mutate(mult=as.numeric(mult))
dt1[[21]]=df



# y18
df=dt0[[22]]
df=df %>% rename(year=year,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=sector_rural_urban,
                 status=status_of_unit,
                 tot_wrk_days=number_of_working_days_total,
                 tot_wrk_days_manuf=number_of_working_days_manufacturing_days,
                 tot_wrk_days_nonmanuf=number_of_working_days_non_manufacturing_days,
                 mult=multiplier_in_9999_99999999,
                 unit_size=no_of_units,
                 tot_cost_prd=total_cost_of_production)%>% 
  mutate(year="2019") %>% mutate(mult=as.numeric(mult))
dt1[[22]]=df


# y98
df=dt0[[25]]
df=df %>% rename(year=accounting_year,
                 state=state_code, 
                 district=district_code, 
                 rural_urban=sector,
                 status=open_closed_code,
                 unit_size=no_of_units)%>% 
  mutate(year="1998") %>% 
  distinct()

# For year 1998, the WGT are in different file
blkI_98=read_dta("../Data/ASI/ASI_1997_98_Detailed_new format/exdir/I-PRODUCTS AND BY_PRODUCTS  (RC81).dta")
blkI_98=resetName(blkI_98)
blkI_98=blkI_98 %>% 
  rename(state=state_code,
         mult=multiplier) %>% 
  dplyr::select(state, cso_rsl_no, psl_no, rtn_industry_code, mult) %>% distinct()

df=df %>% left_join(blkI_98) %>% 
  mutate(mult=as.numeric(mult))

df=df %>% group_by(state, district, rural_urban, frame_industry_code) %>% 
  mutate(mult_fill=mean(mult, na.rm=T),
         mult_p90_10=quantile(mult, 0.9, na.rm=T)/quantile(mult, 0.1, na.rm=T)) %>% 
  ungroup()

df$mult[is.na(df$mult)]=df$mult_fill[is.na(df$mult)]
df$mult=df$mult/100
df$mult[df$scheme_census_1_sample_2==1]=1

dt1[[23]]=df



# TIME SERIES oF LEVELS: no of firms --------------------------------------


dt2=dt1 %>% bind_rows() %>% dplyr::select(year, state, district, rural_urban, status, tot_wrk_days,
                                          tot_wrk_days_manuf, tot_wrk_days_nonmanuf, 
                                          mult, unit_size, tot_cost_prd) %>% distinct() %>% 
  filter(year!="1998")


# STATUS==1 are OPEN

dt2=dt2 %>% filter(status==1) %>% filter(unit_size==1)



# USING WEIGHTS

dt2=dt2 %>% mutate_at(vars(tot_wrk_days,
                           tot_wrk_days_manuf,
                           tot_wrk_days_nonmanuf,
                           tot_cost_prd, 
                           mult), as.numeric)


dt3=dt2 %>% mutate_at(vars(tot_wrk_days,
                                tot_wrk_days_manuf,
                                tot_wrk_days_nonmanuf,
                                tot_cost_prd), list(~ . * mult)) %>% 
  mutate(state=padzero(state, 2), 
         district=padzero(district,2))


qs::qsave(dt3, "data/clean/1_ASI.qs")


asi_dist_yr_lvl=dt3 %>% group_by(year, state, district, rural_urban) %>% 
  summarise(firm_count=sum(mult, na.rm=T),
         tot_cost_prd=mean(tot_cost_prd, na.rm=T),
         tot_wrk_days=mean(tot_wrk_days, na.rm=T),
         tot_wrk_days_manuf=mean(tot_wrk_days_manuf, na.rm=T),
         tot_wrk_days_nonmanuf=mean(tot_wrk_days_nonmanuf, na.rm=T)) %>% ungroup() %>% 
  distinct()

asi_dist_yr_lvl2=dt3 %>% group_by(year, state, district) %>% 
  summarise(firm_count=sum(mult, na.rm=T),
            tot_cost_prd=mean(tot_cost_prd, na.rm=T),
            tot_wrk_days=mean(tot_wrk_days, na.rm=T),
            tot_wrk_days_manuf=mean(tot_wrk_days_manuf, na.rm=T),
            tot_wrk_days_nonmanuf=mean(tot_wrk_days_nonmanuf, na.rm=T)) %>% ungroup() %>% 
  distinct()

  
  
asi_st_yr_lvl=dt3 %>% group_by(year, state, rural_urban) %>% 
    summarise(firm_count=sum(mult, na.rm=T),
              tot_cost_prd=sum(tot_cost_prd, na.rm=T),
              tot_wrk_days=sum(tot_wrk_days, na.rm=T),
              tot_wrk_days_manuf=sum(tot_wrk_days_manuf, na.rm=T),
              tot_wrk_days_nonmanuf=sum(tot_wrk_days_nonmanuf, na.rm=T)) %>% ungroup() %>% 
  distinct()
  
  

# CURRENTLY ALL DISTRICTS ARE MISSING AFTER 2010 (RURAL URBAN IS PRESENT)
  
asi_dist_lvl_2k10=asi_dist_yr_lvl %>% filter(as.numeric(year)<=2010) %>% 
  filter(rural_urban==1 | rural_urban==2)

asi_st_lvl=asi_st_yr_lvl %>% 
  filter(rural_urban==1 | rural_urban==2)





# PLOTTING VARIOUSL TIME SERIES -------------------------------------------

# PLOT FUNC
ts_plot=  function(data, variable, group_var) {
  
  dd=data %>% group_by(year,!!sym(group_var)) %>% 
    summarise(val=mean(!!sym(variable), na.rm=T))
  
  ggplot(dd, aes(x = year, y = val, 
                   group = !!sym(group_var), color = !!sym(group_var))) +
    geom_line() +
   # facet_grid(vars(!!sym(group_var)), scales = "free_y") +
    labs(title = paste("Time Series by", group_var),
         x = "Year",
         y = "Variable Value",
         color = "Grouping Variable") +
    theme_minimal()
}

library(dplyr)
library(ggplot2)

ts_plot2 <- function(data, variable, group_var) {
  
  dd <- data %>% mutate(year=as.numeric(year)) %>% 
    group_by(!!sym(group_var), year) %>% 
    summarise(val = mean(!!sym(variable))) %>%
    arrange(!!sym(group_var), year)  # Arrange the data for computing slopes
  
  dd_slope <- dd %>% 
    group_by(!!sym(group_var)) %>% 
    mutate(slope = (val - lag(val)) / (year - lag(year)))
  
  ggplot() +
    #geom_line(data = dd, aes(x = year, y = val, group = !!sym(group_var), color = !!sym(group_var))) +
    geom_line(data = dd_slope, aes(x = year, y = slope, group = !!sym(group_var), color = !!sym(group_var)),
              linetype = "dashed", color = "black") +
    labs(title = paste("Time Series and Slope by", group_var),
         x = "Year",
         y = "Variable Value / Slope",
         color = "Grouping Variable") +
    theme_minimal()
}

# Example usage:
# Replace 'asi_lvl_2k10', 'firm_count', and 'rural_urban' with your actual variable and group variable names
# ts_plot(asi_lvl_2k10, "firm_count", "rural_urban")


# Example usage:
# Replace 'asi_lvl_2k10', 'firm_count', and 'rural_urban' with your actual variable and group variable names
# ts_plot(asi_lvl_2k10, "firm_count", "rural_urban")



# Example usage:
# Assuming your dataframe is named 'your_data'
# Replace 'your_data', 'your_variable', and 'your_group_var' with your actual variable and group variable names
# plot_time_series(your_data, "your_variable", "your_group_var")


# 1. RURAL-URBAN LEVLES TIME SERIES

dd=asi_yearly_lvl %>%  filter(rural_urban==1 | rural_urban==2)





ts_plot(asi_st_lvl,
        "firm_count", 
        "rural_urban" )



# 1. RURAL-URBAN LEVLES YEARLY CHANGE ( % change at year level)


dd <- asi_st_lvl %>% 
  group_by(rural_urban, year) %>% 
  summarise(val = mean(firm_count)) %>%
  arrange(rural_urban, year)  # Arrange the data for computing slopes

dd_slope <- dd %>% 
  group_by(rural_urban) %>% 
  mutate(slope = (val - Lag(val)) /  Lag(val))


ts_plot(dd_slope,
        "slope", 
       "rural_urban" )





# 2. COST OF PRODUCTIOn


ts_plot(asi_st_lvl,
        "tot_wrk_days", 
        "rural_urban" )





# 3. DISTRICT LEVEL TIME COEFF


dist_df=asi_dist_yr_lvl2 %>% 
  mutate(state_dist=paste0(state, district)) %>% 
  mutate(year=as.numeric(year))


ll=unique(dist_df$state_dist)

beta=list(); ci_upper=list();ci_lower=list(); n=list()

for(l in 1:length(ll)){
  
  df=dist_df %>% filter(state_dist==ll[l])
  
  reg=felm(as.formula("firm_count~year"), df)
  
  beta[[l]]=reg$coefficients[2,1]
  
  ci_upper[[l]]=reg$coefficients[2,1] + 1.96*reg$se[2]
  ci_lower[[l]]=reg$coefficients[2,1] - 1.96*reg$se[2]
  n[[l]]=nrow(df)
  
}

reg_df=cbind(unlist(beta), unlist(ci_upper), unlist(ci_lower), unlist(n)) %>%  as.data.frame()
names(reg_df)=c("beta","ci_upper","ci_lower","n")

reg_df =reg_df %>% filter(n>=7)

# PENDING: CAN PLOT THE BETA ON A MAP





