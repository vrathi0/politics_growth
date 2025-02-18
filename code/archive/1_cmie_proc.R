rm(list=ls())
source("code/functions.R")


## HEADER
#In This code, I am just generating a measure of firm density at geo x time level

###



# READING RAW DATA --------------------------------------------------------


cmie_list=qread("data/cmie_full_dump.qs")



# Constructing measure of flow and wages/expenditure.  --------------------

co_id=c("co_code", "cin_code")
geo_var=c("regdcity","regddname","regdstate","regdpin")
year_var=c("incorporation_year")

exp_var=c("energy_cons_value")
# energy_cons_value:  The value of energy consumed as given by the company in its annual report.

# I am extracting a id_df that will have the company id ( along with geo markers and time)
# This can be used to merge in relevant geo markers at ease in any table

id_df=cmie_list[[2]] %>% 
  dplyr::select(one_of(c(geo_var, co_id, year_var))) %>% 
  distinct()


# Flow at pin-year level

df=cmie_list[[2]] %>% 
  dplyr::select(names(id_df))

df=df %>% filter(incorporation_year>=2000)

flow_dt <- df %>% 
  group_by(!!!syms(geo_var), incorporation_year) %>% 
  summarise(num_firms = n_distinct(cin_code))



## Total energy expenditure at pin-year level


df=cmie_list[[1]] %>% 
  rename(co_code=totener_cocode)

df=df %>%  inner_join(id_df) %>% 
  mutate(year=substr(totener_date, 1,4),
         month=substr(totener_date, 5,6),
         day=substr(totener_date,7,8)) %>% 
  filter(month=="03") %>% 
  filter(as.numeric(year)>=2000)

# Below I am essentially multiplying no of units x cost per unit.

df= df %>% mutate(ecost_usd=energy_cons_qty * energy_cons_rate_per_unit)

ecost_firm=df %>% group_by(co_code,totener_date ) %>% 
  mutate(ecost_usd=sum(ecost_usd, na.rm=T))

ecost_geo=ecost_firm %>% 
  group_by(!!!syms(geo_var), year) %>% 
  summarise(ecost_geo=mean(ecost_usd, na.rm=T))








