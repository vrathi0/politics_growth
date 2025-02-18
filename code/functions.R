

# This is a helper/function file
# source this for useful functions



# Load Packages -----------------------------------------------------------


ipak <- function(pkg){
  #system("mkdir ~/TMPDIR")
  #Sys.setenv(TMPDIR='~/TMPDIR')
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


# Loads Package
pkg=c("Hmisc","gtools","rlist","stringr","broom","openxlsx",
      "cowplot","ggplot2","tmap","maps","devtools","sp","sf",
     "statar","foreach", "doParallel","tmap","pryr","tidyverse" ,
      "urltools","RCurl", "ncdf4","httr", "downloader","raster", "tiff",
      "R.utils", "tictoc", "stringr", "stargazer","colorspace","mapdata","rworldmap",
      "spData","wesanderson","pglm","lme4","fixest","haven",
      "cluster","fpc","fastDummies","caret","glue", "kableExtra","purrr",
      "corrgram","fastDummies","MASS","discreteRV","hdrcde","np", "readstata13",
      "data.table","janitor", "odbc","DBI","labelled","installr","palmerpenguins","readxl","here", 
      "tigerstats","Hmisc","labelled","multiplex","foreach","rollRegres", "qs",
     "pdftools","RefManageR","ggraph", "igraph","zoo","lubridate","tidygeocoder",
     "rvest","qs","httr2","tabulizer", "binsreg","gridExtra", "stringdist",
     "magick", "readtext","DataEditR","fuzzyjoin","glmnet",
     "pROC","caret", "data.table", "geos")#,"pdftools","rJava")

pkg=unique(pkg)

ipak(pkg)

#remotes ::install_github("karldw/kdw.junk")
#devtools::install_github("kylebutts/fwlplot")

#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")


# dropCheck ---------------------------------------------------------------

# this function will check the % of units drop after any filter

dropcheck=function(bigdf, smalldf, unit){
  
 
  a=1-length(unique(smalldf[[unit]]))/length(unique(bigdf[[unit]]))
  
  spec_dec(a*100,2)
  # c=paste(a,b, sep = "--")
  # 
  # c
  
}



# Specify Decimal ---------------------------------------------------------

spec_dec <- function(x, k) trimws(format(round(x, k), nsmall=k))





# Check empty values ------------------------------------------------------

isBlank=function(x){
  
  if(any(nchar(x)==0)) return(TRUE)
    else  return(nchar(x)!=nchar(str_trim(x)))
  
}



# Following function clips top and bottom 5p
clipp <- function(x, lower_lim,upper_lim){
  quantiles <- as.numeric(quantile( x, c(lower_lim, upper_lim ) , na.rm=TRUE))
  x[ x < quantiles[1] ] <- NA
  x[ x > quantiles[2] ] <- NA
  x
}

# Following function top codes top and bottom 5p
tcode <- function(x, lower_lim,upper_lim){
  quantiles <- as.numeric(quantile( x, c(lower_lim, upper_lim ), na.rm=TRUE ))
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}



# Following is just a masker on sprint to padd zero on the left

padzero=function(x,n){ #n is the total number of desired digits
  
  x=sprintf(paste0("%0",n,"d"),as.numeric(x))
  
  
}


# wrapper on substr to pull out character using just length

sublen=function(x, start, length){
  
  end=start+length-1
  
  substr(x,start, end)
  
}



## Function to see N of unit per group

noff = function(dt, u1, u2){
  dt1 = dt %>% 
    group_by(across(all_of(u1))) %>% 
    summarise(across(all_of(u2), ~n_distinct(.))) %>% 
    ungroup()
  
  stats=apply(dt1[,-1], 2, quantile, c(0.1,0.25,0.5,0.9,0.99)) 
   return(stats)
}




## Get Residual


library(lfe)

getRes <- function(dt, var1, var2){
  fml <- as.formula(paste0(var1, "~ 1 | ", paste(var2,collapse = "+")))
  reg1 <- felm(fml, data = dt)
  return(reg1$residuals)
}


### Get Bivariate beta

getBeta <- function(x, y){
  fit <- lm(y ~ x)
  return(coef(fit)[2])
}


## Clean a string vector

clean_strings <- function(x) {
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^[:alnum:]]", "_")
  x=gsub("_+", "_", x)
  x=gsub("_$", "", x)
  return(x)
}



# Get Label in attributes

getLab=function(x){
  
  attr(x, "label")
  
}

# Getting distinct number of unique combination of multiple variables


cdistinct= function(dt, varlist){
 
  dt %>% dplyr::select((all_of(varlist))) %>% 
    n_distinct() 
  

}


# KEEPING STATE CODESREADY FOR ALL CODE FILES

# Reading file with here() and clean_names
master_places <- readxl::read_excel(file.path(here(),"../Data/MAPS- INDIA/state_dist_town.xlsx")) %>%   # Use a consistent path format %>%
  janitor::clean_names()

master_places=master_places %>%  filter(!str_detect(state_code, "[:alpha:]"))






state_code=master_places %>% dplyr::select(state, state_code) %>% 
  mutate(state_code=padzero(state_code,2)) %>% 
  distinct() %>% rename(state_name=state)

state_code$state_name[state_code$state_name=="Daman & Diu"]="Goa_Daman_&_Diu"
state_code$state_name[state_code$state_name=="NCT of Delhi"]="Delhi"
state_code$state_name[state_code$state_name=="Jammu and Kashmir"]="Jammu_&_Kashmir"
state_code$state_name[state_code$state_name=="Odisha"]="orissa"
state_code$state_name[state_code$state_name=="Puducherry"]="pondicherry"

state_code=state_code %>% 
  mutate(state_name=str_replace_all(state_name, " ", "_")) %>% 
  mutate(state_name=tolower(state_name)) %>% 
  distinct() %>% filter(state_name!="telangana")

state_code = state_code %>%
  mutate(
    region = case_when(
      as.numeric(state_code) <= 10 ~ "north",
      as.numeric(state_code) >= 27 ~ "south",
      TRUE ~ NA_character_
    )
  )



st_abr=read_excel(file.path(here(),"../Data/MAPS- INDIA/state_codes.xlsx")) %>% 
  clean_names()
names(st_abr)=c("state_name", "st_abr","st_abr2")

st_abr=st_abr %>% 
  mutate(state_name=str_replace_all(state_name, " ", "_")) %>% 
  mutate(state_name=tolower(state_name))

st_abr$state_name[st_abr$state_name=="andaman_and_nicobar_islands"]="a_&_n_islands"
st_abr$state_name[st_abr$state_name=="dadra_and_nagar_haveli"]="d_&_n_haveli"
st_abr$state_name[st_abr$state_name=="daman_and_diu"]="goa_daman_&_diu"
st_abr$state_name[st_abr$state_name=="odisha"]="orissa"
st_abr$state_name[st_abr$state_name=="telangana"]="telengana"
st_abr$state_name[st_abr$state_name=="puducherry"]="pondicherry"
st_abr$state_name[st_abr$state_name=="jammu_and_kashmir"]="jammu_&_kashmir"


state_code=state_code %>% left_join(st_abr)


# state_code=read_excel("../Data/TCPD/state_code.xlsx")
# state_code$state_name[state_code$state_name=="Daman & Diu"]="Goa_Daman_&_Diu"
# state_code$state_name[state_code$state_name=="NCT of Delhi"]="Delhi"
# state_code$state_name[state_code$state_name=="Jammu and Kashmir"]="Jammu_&_Kashmir"
# state_code$state_name[state_code$state_name=="Odisha"]="orissa"
# state_code$state_name[state_code$state_name=="Puducherry"]="pondicherry"
# 
# state_code=state_code %>% 
#   mutate(state_name=str_replace_all(state_name, " ", "_")) %>% 
#   dplyr::select(state_name, state_id) %>% 
#   mutate(state_name=tolower(state_name)) %>% 
#   distinct()


# FUNCTION TO COUNT UNIQUE COMBINATION

count_fun <- function(.data, ...) {
  .data %>%
    dplyr::select(...) %>%
    n_distinct()
}


# CHECKING IF MAPPING IS 1-1

check_mapping <- function(data, var1, var2) {
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

# Following function views the column names of a dataframe  
v=function(df){
  View(as.data.frame(names(df)))
}


# STANDARD STATE NAMES
# Function to standardize state names
standardize_state_names <- function(state_name) {
  
  state_name=tolower(state_name) 
  state_name <- dplyr::case_when(
    state_name %in% c("uttarakhand", "uttarkhand") ~ "uttaranchal",
    state_name == "orissa" ~ "odisha",
    state_name %in% c("arunanchal pradesh") ~ "arunachal pradesh",
    state_name %in% c("jammu and kashmir", "jammu & kashmir",
                      "jammu_&_kashmir","jammu kashmir") ~ "jammu and kashmir",
    state_name == "tamilnadu" ~ "tamil nadu",
    state_name %in% c("delhi", "national capital territory of delhi", 
                      "delhi & ncr") ~ "delhi & ncr",
    state_name == "puducherry" ~ "pondicherry",
    state_name %in% c("andaman & nicobar islands") ~ "andaman & nicobar island",
    state_name == "dadra & nagar haveli" ~ "dadara & nagar havelli",
    state_name == "daman and diu" ~ "daman & diu",
    TRUE ~ state_name  # Retain unchanged state names
  )
  
  # Replace spaces with underscores in the resulting state names
  state_name <- stringr::str_replace_all(state_name, " ", "_")
  
  return(state_name)
}


## LANGUAGE ST CLASSIFICATION

# Create a tibble with the state-to-group mapping, including the new "Eastern" group.
lang_st_class <- tibble(
  st_name = c("gujarat", "assam", "meghalaya", "tamil_nadu", "karnataka", "west_bengal",
              "odisha", "uttar_pradesh", "kerala", "bihar", "assam_meghalya", "arunachal_pradesh",
              "delhi_&_ncr", "maharashtra", "punjab", "manipur", "-", "rajasthan",
              "himachal_pradesh", "andhra_pradesh", "madhya_pradesh", "chandigarh",
              "jharkhand", "haryana", "pondicherry", "telangana", "sikkim",
              "jammu_and_kashmir", "nagaland", "not_found", "uttaranchal", "mizoram",
              "chhattisgarh", "others", "tripura", "manipur-tripura", "a_g_m_u_t",
              "goa", "andaman_&_nicobar", "ladakh"),
  st_lang_grp = c("Indo-Aryan", "North-East", "North-East", "Dravidian", "Dravidian", "Eastern",
                  "Eastern", "Indo-Aryan", "Dravidian", "Eastern", "North-East", "North-East",
                  "Indo-Aryan", "Indo-Aryan", "Indo-Aryan", "North-East", "Unknown", "Indo-Aryan",
                  "Indo-Aryan", "Dravidian", "Indo-Aryan", "Indo-Aryan", "Eastern", "Indo-Aryan",
                  "Dravidian", "Dravidian", "North-East", "Indo-Aryan", "North-East", "Unknown",
                  "Indo-Aryan", "North-East", "Indo-Aryan", "Unknown", "North-East", "North-East",
                  "Unknown", "Indo-Aryan", "Unknown", "Unknown")
)
