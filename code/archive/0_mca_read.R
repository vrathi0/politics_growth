rm(list=ls())
source("code/functions.R")


# # 

# HEADER ------------------------------------------------------------------

# 1. READING AND PROC REGERETED COMPANY DATA
#     INTENDED OUTPUT: COMPANY DATA WITH DISTRICT MARKER. 


# 2. SAME FOR LLP ( Limited PARTNERSHIP)
#dd=read_excel(ff, col_types = "guess", sheet="Indian Companies")


# Function to read sheets with [PATTERN] in the sheet name
read_sheets_with_pattern <- function(file_path, pattern, skp=0) {
  sheet_names <- excel_sheets(file_path)
  lapply(sheet_names[grep(pattern, sheet_names, ignore.case = T)], function(sheet) {
    read_excel(file_path, sheet = sheet, skip = skp)
  })
}



mca_path="../Data/MCA/dump"


mca_list=list.files(path=mca_path, 
                    pattern = ".xlsx$", 
                    full.names = T) %>% 
  as.list()



# READING REGISTERED COMPANIES --------------------------------------------------------


# Reg companies
reg_comp1=list()

for(i in 1:length(mca_list)){
  
  print(i)
  dd=read_sheets_with_pattern(mca_list[[i]], 
                              pattern="Indian Com")[[1]]
  
  dd=dd %>%
    dplyr::select_if(~ !all(is.na(.)))
  
  skp=min(grep("^\\d|^U|^L", dd[[1]]))-1
  
  df=read_sheets_with_pattern(mca_list[[i]], 
                              pattern="Indian Com", 
                              skp=skp)[[1]]
  
  df =df %>% clean_names() %>% 
    dplyr::select(-any_of("s_no"))
  
  reg_comp1[[i]]=df
  
}


## DROPPING 18,84th ITEM ( NEED TO FIX)
 reg_comp1=reg_comp1[-84]
# reg_comp=reg_comp[-18]
# reg_comp=reg_comp[-25]
# reg_comp=reg_comp[-43]
# reg_comp=reg_comp[-66]

reg_comp=reg_comp1

# Post Proc

for (i in seq_along(reg_comp)) {
  print(i)
  
  
  if ("activity" %in% names(reg_comp[[i]])) {
    names(reg_comp[[i]])[names(reg_comp[[i]]) == "activity"] <- "activity_code"
  }
  
  if ("roc_location" %in% names(reg_comp[[i]])) {
    names(reg_comp[[i]])[names(reg_comp[[i]]) == "roc_location"] <- "roc"
  }
  
  if ("company_email_id" %in% names(reg_comp[[i]])) {
    names(reg_comp[[i]])[names(reg_comp[[i]]) == "company_email_id"] <- "email"
    
  }
  
  
  if ("industrial_description" %in% names(reg_comp[[i]])) {
    names(reg_comp[[i]])[names(reg_comp[[i]]) == "industrial_description"] <- "activity_description"
    
  }
  
  if ("industrial_activity_code" %in% names(reg_comp[[i]])) {
    names(reg_comp[[i]])[names(reg_comp[[i]]) == "industrial_activity_code"] <- "activity_code"
    
  }
  
  
  if ("company_class" %in% names(reg_comp[[i]])) {
    names(reg_comp[[i]])[names(reg_comp[[i]]) == "company_class"] <- "class"
    
  }
  
  if ("address" %in% names(reg_comp[[i]])) {
    names(reg_comp[[i]])[names(reg_comp[[i]]) == "address"] <- "registered_office_address"
    
  }
  
  
  
  # print("1")
  if (any(grepl("authorized", names(reg_comp[[i]])))) {
    names(reg_comp[[i]])[which(grepl("authorized", names(reg_comp[[i]])))] <- "authorized_capital"
  }
  
  # print("2")
  if (any(grepl("paidup", names(reg_comp[[i]])))) {
    
    names(reg_comp[[i]])[which(grepl("paidup", names(reg_comp[[i]])))] <- "paidup_capital"
    
    
  }
  
 # print("3")
  if (is.character(reg_comp[[i]]$date_of_registration)) {
    reg_comp[[i]]$date_of_registration=lubridate::dmy(reg_comp[[i]]$date_of_registration)
  }
  
 # print("4")
  reg_comp[[i]]=reg_comp[[i]] %>%
    dplyr::select(-any_of("month_name"))
  
  if ("activity_code" %in% colnames(reg_comp[[i]])) {
    reg_comp[[i]] = reg_comp[[i]] %>% mutate(activity_code = as.character(activity_code))
  }
  
  
  if ("industrial_activity_code" %in% colnames(reg_comp[[i]])) {
    reg_comp[[i]] = reg_comp[[i]] %>% mutate(industrial_activity_code = as.character(industrial_activity_code))
  }
  
 # print("5")
  if ("authorized_capital" %in% colnames(reg_comp[[i]])) {
    reg_comp[[i]] = reg_comp[[i]] %>% mutate(authorized_capital = as.character(authorized_capital))
  }
  
  if ("paidup_capital" %in% colnames(reg_comp[[i]])) {
    reg_comp[[i]] = reg_comp[[i]] %>% mutate(paidup_capital = as.character(paidup_capital))
  }
  
  
  if ("activity_description" %in% colnames(reg_comp[[i]])) {
    reg_comp[[i]] = reg_comp[[i]] %>% mutate(activity_description = as.character(activity_description))
  }
  

  
}


has_roc_location <- sapply(reg_comp, function(df) "inc_29" %in% colnames(df))


# Binding Rows

reg_comp_df=reg_comp %>% bind_rows() %>% 
  distinct()

# inc_29 has no data and  is a juuknk variable

## Keeping only the first 15 variables/names # I AM DROPPING AROUNF 2 % of obs NEED TO FIX IT. 

reg_comp_df=reg_comp_df %>%  dplyr::select(1:15) %>% 
  distinct()


# REG COMP: ADDRESS EXTRACTION ------------------------------------------------------
# Cleaning the ROC variable

reg_comp_df=reg_comp_df %>% 
  mutate(roc=gsub("\\s+", "", roc)) %>% 
  dplyr::mutate(roc=gsub("RoC-|roc", "", roc, ignore.case = TRUE)) %>% 
  mutate(roc=tolower(roc)) %>% 
  mutate(year=year(date_of_registration))

reg_comp_df=reg_comp_df %>% 
  mutate(zip=str_extract(registered_office_address, "\\d{6}"))


# MAPPING ZIP TO DISTIRCTS ------------------------------------------------

zip_dist_map <- st_read("../Data/MAPS- INDIA/INDIA_PINCODES-master/india_pincodes.shp") %>% 
  st_make_valid() %>% mutate(district=tolower(district),
                             state=tolower(state)) %>% as.data.frame() %>% 
  dplyr::select(pincode, state, district) %>% rename(zip=pincode) %>% 
  distinct()

reg_comp_df2=reg_comp_df #%>% left_join(zip_dist_map)

# FIXING THE STATE CODES
reg_comp=reg_comp_df2
# Fixing the state_codes
reg_comp$state[reg_comp$state=="MH"]="Maharashtra"
reg_comp$state[reg_comp$state=="TG"]="Telangana"
reg_comp$state[reg_comp$state=="GJ"]="Gujarat"
reg_comp$state[reg_comp$state=="CH"]="Chandigarh"
reg_comp$state[reg_comp$state=="DL"]="Delhi"
reg_comp$state[reg_comp$state=="HR"]="Haryana"
reg_comp$state[reg_comp$state=="UP"]="Uttar Pradesh"
reg_comp$state[reg_comp$state=="Lucknow"]="Uttar Pradesh"
reg_comp$state[reg_comp$state=="RJ"]="Rajasthan"
reg_comp$state[reg_comp$state=="CT"]="Chattisgarh"
reg_comp$state[reg_comp$state=="Chhaattisgarh"]="Chattisgarh"
reg_comp$state[reg_comp$state=="Chhattisgarh"]="Chattisgarh"
reg_comp$state[reg_comp$state=="KL"]="Kerala"
reg_comp$state[reg_comp$state=="WB"]="West Bengal"
reg_comp$state[reg_comp$state=="KA"]="Karnataka"

reg_comp$state[reg_comp$state=="MP"]="Madhya Pradesh"
reg_comp$state[reg_comp$state=="PB"]="Punjab"
reg_comp$state[reg_comp$state=="BR"]="Bihar"
reg_comp$state[reg_comp$state=="MN"]="Manipur"
reg_comp$state[reg_comp$state=="TN"]="Tamil Nadu"
reg_comp$state[reg_comp$state=="OR"]="Orissa"
reg_comp$state[reg_comp$state=="Odisha"]="Orissa"
reg_comp$state[reg_comp$state=="HP"]="Himachal Pradesh"
reg_comp$state[reg_comp$state=="UR"]="Uttarakhand"
reg_comp$state[reg_comp$state=="JH"]="Jharkhand"
reg_comp$state[reg_comp$state=="GA"]="Goa"
reg_comp$state[reg_comp$state=="AS"]="Assam"
reg_comp$state[reg_comp$state=="DN"]="Dadar Nagar Haveli"
reg_comp$state[reg_comp$state=="Dadra & Nagar Haveli"]="Dadar Nagar Haveli"

reg_comp$state[reg_comp$state=="TR"]="Tripura"
reg_comp$state[reg_comp$state=="JK"]="Jammu and Kashmir"
reg_comp$state[reg_comp$state=="Jammu & Kashmir"]="Jammu and Kashmir"
reg_comp$state[reg_comp$state=="Srinagar"]="Jammu and Kashmir"
reg_comp$state[reg_comp$state=="PY"]="Pondicherry"
reg_comp$state[reg_comp$state=="Puducherry"]="Pondicherry"
reg_comp$state[reg_comp$state=="MZ"]="Mizoram"
reg_comp$state[reg_comp$state=="NL"]="Nagaland"
reg_comp$state[reg_comp$state=="AN"]="Andaman and Nicobar Islands"
reg_comp$state[reg_comp$state=="Andaman & Nicobar Islands"]="Andaman and Nicobar Islands"
reg_comp$state[reg_comp$state=="Andaman & Nicobar"]="Andaman and Nicobar Islands"
reg_comp$state[reg_comp$state=="AR"]="Arunachal Pradesh"
reg_comp$state[reg_comp$state=="LD"]="Lakshadweep"
reg_comp$state[reg_comp$state=="ML"]="Meghalaya"
reg_comp$state[reg_comp$state=="Andra Pradesh"]="Andhra Pradesh"
reg_comp$state[reg_comp$state=="AP"]="Andhra Pradesh"


#Merging state code from state_code ( see functions.R)

reg_comp=reg_comp %>% mutate(state_name=tolower(state)) %>% 
  mutate(state_name=str_replace_all(state_name, " ", "_"))

reg_comp$state_name[reg_comp$state_name=="jammu_and_kashmir"]="jammu_&_kashmir"

reg_comp=reg_comp %>% left_join(state_code, by="state_name")


# 

# READING IN THE ARCHIVAL VERSION ( PRE 2015) -----------------------------

arc_path="../Data/MCA/Archive"
flist_archive=list.files(path=arc_path, 
                         pattern = "*.xlsx$", 
                         recursive = F, 
                         full.names = T)

arch_list=list()
for(f in 1:length(flist_archive)){
  
  arch_list[[f]]= read_excel(flist_archive[[f]],
                             col_types = c("text"))
  
  print(f)
}

arch_dt0=arch_list %>% bind_rows() %>% 
  clean_names()


arch_dt1= arch_dt0 %>% 
  rename(cin=corporate_identification_number,
         state_name=registered_state, 
         roc=registrar_of_companies, 
         category=company_category,
         class=company_class,
         company_type=sub_category,
         activity_description=principal_business_activity) %>% 
  mutate(state_name=tolower(state_name))%>% 
  mutate(state_name=str_replace_all(state_name, " ", "_"))
        

arch_dt1$date2 <- if_else(grepl("/", arch_dt1$date_of_registration), 
                      as.Date(arch_dt1$date_of_registration,  "%d/%m/%Y"), 
                      as.Date(as.numeric(arch_dt1$date_of_registration), origin = "1899-12-30"))

arch_dt1=arch_dt1 %>%  mutate(year=year(date2)) %>% 
  mutate(date_of_registration=date2) %>% 
  dplyr::select(-date2)

# Getting zips too
arch_dt1=arch_dt1 %>% 
  mutate(zip=str_extract(registered_office_address, "\\d{6}"))

# Merging in state_code
arch_dt1=arch_dt1 %>% left_join(state_code, by="state_name")


  


## NOW I CAN JUST EXPORT THE FILL DATA ( from 1990-2015 + 2016- current)

mca_total=arch_dt1 %>%  bind_rows(reg_comp)


# SAVING THE REGISTERED COMPANY DF

write_rds(mca_total, 
          file="data/clean/MCA_registered_company_total.rds", 
          compress = "gz")

# ROUGH -------------------------------------------------------------------


# ROUGH

# nn=reg_comp_df %>%  filter(is.na(zip)) %>% 
#   dplyr::select(registered_office_address) %>%  distinct()
# 
# 
# dd=reg_comp_df %>% group_by(year) %>%
#   summarize(percentage_missing = mean(is.na(zip)) * 100)
# 
# dd=reg_comp_df %>% dplyr::select(roc, state,registered_office_address) %>% distinct() 
# dd=dd[1:25,]
# 
# dd= dd %>% geocode(registered_office_address, method='here') 
# dd1=dd %>% 
#   filter(!is.na(lat))
# 
# dd1=dd1 %>% inner_join(reg_comp_df, by=c("add"="registered_office_address"))
# 
# ac_shp <- st_read("../Data/MAPS- INDIA/AC/maps-master/assembly-constituencies/India_AC.shp")
# ac_shp=st_make_valid(ac_shp)
# 
# lat_long_sf <- st_as_sf(data.frame(lon = dd1$long, lat = dd1$lat), 
#                         coords = c("lon", "lat"), crs = st_crs(ac_shp))
# 
# result <- st_join(lat_long_sf, ac_shp)
# 
# result_2=result %>% 


# MOST ZIP ARE MISSING FROM 2020, 21, 22 years. 





# zip_dist_map=read_csv("../Data/MAPS- INDIA/district_zipcode.csv") %>% 
#   clean_names() %>% 
#   dplyr::select(pincode, districtname, taluk) %>% 
#   rename(zip=pincode, district=districtname) %>% 
#   mutate(zip=as.character(zip)) %>% 
#   group_by(zip) %>% mutate(dist_per_zip=n_distinct(district)) %>% 
#   ungroup() %>% 
#   distinct()


# merge has to be by zip, only common variable

# Adding additional information from address field. 

# This function extracts the zip code and district. 

# extract_district <- function(address) {
#   # Remove "India" from the address
#   address <- gsub("India", "", address, ignore.case = TRUE)
#   address <- gsub(",", "", address, ignore.case = TRUE)
#   
#   # Extract the last 4 words including any numbers
#   words <- strsplit(address, "\\s+")[[1]]
#   last_words <- tail(words, 4)
#   
#   # Find the 6-digit zip code
#   zip_code <- grep("\\d{6}", last_words, value = TRUE)
#   
#   # Check if zip_code is found, otherwise set it to an empty string
#   if (length(zip_code) == 0) {
#     zip_code <- ""
#   }
#   
#   # Extract the district (excluding the zip code)
#   district <- paste(last_words[last_words != zip_code], collapse = " ")
#   
#   district <- gsub("[^a-zA-Z0-9 ]", "", district)
#   district=trimws(district)
#   
#   return(district)
# }
# 
# reg_comp_df=reg_comp_df %>% 
#   mutate(district=extract_district(registered_office_address))


# There are only 28 ROC offices.





# LLP ---------------------------------------------------------------------


# LLP companies
#  The sheet names often varies, so just reading sheets that has "LLP"in them

library(readxl)

# List of Excel file paths
#mca_list <- c("file1.xlsx", "file2.xlsx", "file3.xlsx")

# Function to read sheets with "LLP" in the name
read_sheets_with_pattern <- function(file_path, pattern, skp=0) {
  sheet_names <- excel_sheets(file_path)
  lapply(sheet_names[grep(pattern, sheet_names)], function(sheet) {
    read_excel(file_path, sheet = sheet, skip = skp)
  })
}

# Read sheets with "LLP" in the name from each file
llp_comp1=list()
for(i in 1:length(mca_list)){
  
 print(i)
   dd=read_sheets_with_pattern(mca_list[[i]], 
                                         pattern="LLP")[[1]]
   
  dd=dd %>%
     dplyr::select_if(~ !all(is.na(.)))

  skp=min(grep("^\\d|^A", dd[[1]]))-1
  
 df=read_sheets_with_pattern(mca_list[[i]], 
                                    pattern="LLP", 
                                    skp=skp)[[1]]
  
 df =df %>% clean_names() %>% 
      dplyr::select(-any_of(c("s_no", "sl_no")))
 
 llp_comp1[[i]]=df
 

 
  
}
# Post Proc

llp_comp=llp_comp1

for (i in seq_along(llp_comp)) {
  print(i)
  
  if ("ro_c" %in% names(llp_comp[[i]])) {
    names(llp_comp[[i]])[names(llp_comp[[i]]) == "ro_c"] <- "roc_location"
  }
  
  if ("roc" %in% names(llp_comp[[i]])) {
    names(llp_comp[[i]])[names(llp_comp[[i]]) == "roc"] <- "roc_location"
  }
  
  if ("limited_liability_partnership_name" %in% names(llp_comp[[i]])) {
    names(llp_comp[[i]])[names(llp_comp[[i]]) == "limited_liability_partnership_name"] <- "llp_name"
  }
  
  
  if ("date_of_incorporation" %in% names(llp_comp[[i]])) {
    names(llp_comp[[i]])[names(llp_comp[[i]]) == "date_of_incorporation"] <- "founded"
  }
  
  
  
 # print("1")
  if (any(grepl("of_partners", names(llp_comp[[i]])))) {
    names(llp_comp[[i]])[which(grepl("of_partners", names(llp_comp[[i]])))] <- "no_of_partners"
  }
  
 # print("2")
  if (any(grepl("designated|number_of", names(llp_comp[[i]])))) {
    
    names(llp_comp[[i]])[which(grepl("designated|number_of", names(llp_comp[[i]])))] <- "no_of_desg_partners"
    
     llp_comp[[i]]=llp_comp[[i]] %>% 
      mutate(no_of_desg_partners=as.character(no_of_desg_partners))
  }
  
 # print("3")
  if (is.character(llp_comp[[i]]$founded)) {
    llp_comp[[i]]$founded=lubridate::dmy(llp_comp[[i]]$founded)
  }
  
  if (any(grepl("address", names(llp_comp[[i]])))) {
    names(llp_comp[[i]])[which(grepl("address", names(llp_comp[[i]])))] <- "address"
  }
  
  if (any(grepl("status", names(llp_comp[[i]])))) {
    names(llp_comp[[i]])[which(grepl("status", names(llp_comp[[i]])))] <- "status"
  }
  
  
 # print("4")
  # Renaming variables to activity_code
  names_to_change <- names(llp_comp[[i]])[grepl("activity|code", names(llp_comp[[i]])) & 
                                            !grepl("description", names(llp_comp[[i]])) &
                                            sapply("\\d", grepl, llp_comp[[i]])]
  if (length(names_to_change) > 0) {
    new_name <- "activity_code"
    names(llp_comp[[i]])[which(names(llp_comp[[i]])==names_to_change)] <- new_name
  }
  
  # Renaming variables to activity_description
  names_to_change <- names(llp_comp[[i]])[grepl("description", names(llp_comp[[i]]))] 
  if(length(names_to_change)>1){names_to_change=names_to_change[names_to_change!="description"]}
  
  if (length(names_to_change) > 0) {
    new_name <- "activity_description"
    names(llp_comp[[i]])[which(names(llp_comp[[i]])==names_to_change)] <- new_name
  }
  

  
  if ("activity_code" %in% names(llp_comp[[i]])) {
    llp_comp[[i]]$activity_code=as.character(llp_comp[[i]]$activity_code)
    }
  
 
  llp_comp[[i]]$no_of_partners=as.character(llp_comp[[i]]$no_of_partners)
  
  # Dropping the obligation_of_contribution for now
  
 # print("5")
  llp_comp[[i]]=llp_comp[[i]] %>% 
    dplyr::select(-any_of(c("obligation_of_contribution","obligation_of_contribution_rs")))
  
}



has_date_var <- sapply(llp_comp, function(df) any(grepl("sl_no", names(df))))


# Binding Rows

llp_comp_df=llp_comp %>% bind_rows() %>% 
  distinct()


# REG COMP: ADDRESS EXTRACTION ------------------------------------------------------
# Cleaning the ROC variable

llp_comp_df=llp_comp_df %>% 
  mutate(roc=gsub("\\s+", "", roc_location)) %>% 
  dplyr::mutate(roc=gsub("RoC-|roc", "", roc, ignore.case = TRUE)) %>% 
  mutate(roc=tolower(roc)) %>% 
  mutate(year=year(founded)) %>% 
  mutate(district=tolower(district))

llp_comp_df=llp_comp_df %>% 
  mutate(zip=str_extract(address, "\\d{6}"))



zip_map=read_csv("../Data/MAPS- INDIA/district_zipcode.csv") %>% clean_names() %>% 
  dplyr::select(pincode, districtname) %>% 
  rename(zip=pincode, district=districtname) %>% 
  mutate(zip=as.character(zip), district=tolower(district)) %>% 
  distinct()

llp_comp_df2=llp_comp_df %>% left_join(zip_map)


llp_comp=llp_comp_df2
# Fixing the state_codes
llp_comp$state[llp_comp$state=="Andra Pradesh"]="Andhra Pradesh"
llp_comp$state[llp_comp$state=="Chhaattisgarh"]="Chattisgarh"
llp_comp$state[llp_comp$state=="Daman & Diu"]="Daman and Diu"
llp_comp$state[llp_comp$state=="GOA"]="Goa"
llp_comp$state[llp_comp$state=="Jammu & Kashmir"]="Jammu and Kashmir"
llp_comp$state[llp_comp$state=="Jharkand"]="Jharkhand"
llp_comp$state[llp_comp$state=="Maharastra"]="Maharashtra"
llp_comp$state[llp_comp$state=="Uttarakand"]="Uttarakhand"
llp_comp$state[llp_comp$state=="Telagana"]="Telangana"




# SAVING THE REGISTERED COMPANY DF

write_rds(llp_comp, 
          file="data/clean/MCA_llp_company.rds", 
          compress = "gz")



