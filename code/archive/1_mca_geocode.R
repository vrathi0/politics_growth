rm(list=ls())
source("code/functions.R")


# This file takes the (almost) cleaned MCa data: from /data/clean
# and look for obs with no district/zip values. 


## GOAL: TO use the address file dto generate lat/long so that we can 
# later on assign the district labels to the companies



# REGISTERED COMPANY ------------------------------------------------------

# Reading the data file


reg_comp_df=read_rds("data/clean/MCA_registered_company.rds")


df=reg_comp_df %>% 
  dplyr::select(cin, year, registered_office_address, state, zip) %>% 
  distinct() # %>% 
  #filter(is.na(zip))

# Segmenting the data at state-year level 
# Will loop through it so that easier to keep track


reg_geocode_input= split(df, interaction(df$state, df$year, drop = T))



# Looping through list

for( i in 26:length(reg_geocode_input)){
  
  print(paste0("Current Loop..", i, "of ", length(reg_geocode_input), "| State:", 
               unique(reg_geocode_input[[i]]$state), "| year:", 
               unique(reg_geocode_input[[i]]$year)))
  
  df1=reg_geocode_input[[i]] %>% 
    mutate(add=paste0(registered_office_address, state, sep="  "))
  
  df1=df1 %>% geocode(add, method='arcgis') 
 
  ss= gsub(" ", "", unique(df1$state))
  fname=paste0("all_reg_geocode_",ss, "_", unique(df1$year),".rds")
  fpath=file.path("data", "Rgeocode", fname)
  write_rds(df1, fpath, compress = "gz")
 
  reg_geocode_input[[i]]=df1
  
  
}

write_rds(reg_geocode_input, "data/Rgeocode/all_reg_geocode_full.rds", 
          compress = "gz")


#dd=reg_geocode_input %>%  bind_rows()



# LLP ---------------------------------------------------------------------

llp_df=read_rds("data/clean/MCA_llp_company.rds")


df=llp_df %>% 
  dplyr::select(llpin, year, address, state, zip) %>% 
  distinct()

# Segmenting the data at state-year level 
# Will loop through it so that easier to keep track


llp_geocode_input= split(df, interaction(df$state, df$year, drop = T))



# Looping through list

for( i in 120:length(llp_geocode_input)){
  
  print(paste0("Current Loop..", i, "of ", length(llp_geocode_input), "| State:", 
               unique(llp_geocode_input[[i]]$state), "| year:", 
               unique(llp_geocode_input[[i]]$year)))
  
  df1=llp_geocode_input[[i]] %>% 
    mutate(add=paste0(address, state, sep="  "))
  
  df1=df1 %>% geocode(add, method='arcgis') 
  
  ss= gsub(" ", "", unique(df1$state))
  fname=paste0("all_llp_geocode_",ss, "_", unique(df1$year),".rds")
  fpath=file.path("data", "Rgeocode", fname)
  write_rds(df1, fpath, compress = "gz")
  
  llp_geocode_input[[i]]=df1
  
  
}

write_rds(llp_geocode_input, "data/Rgeocode/all_llp_geocode_full.rds", 
          compress = "gz")
  

