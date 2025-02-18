rm(list=ls())
source("code/functions.R")
source("code/myneta_scrap.R")


# Looking into the output of myneta_scrap2

# Currently using the output of safe_extract_table function
adr_dt=read_dta("../Data/SHRUG/ELECTION/shrug-adr-elections-cand-dta/affidavits_clean.dta")

uid=unique(adr_dt$adr_unique_id) # 412/94k missing

url_df=as.data.frame(cbind(uid)) %>% 
  separate(remove = F,uid, sep = "-", 
           into=c("state","year","cid")) %>% 
  dplyr::filter(uid!="") %>% 
  dplyr::filter(year!=".")


# Reading state code table

place_codes=read_excel("../Data/MAPS- INDIA/state_dist_town.xlsx") %>% 
  clean_names() %>% 
  dplyr::select(state, state_code) %>% distinct() %>% 
  filter(!str_detect(state_code,"[:alpha:]" )) %>% 
  rename(state_name=state)

place_codes$state_name[place_codes$state_name=="Daman & Diu"]="Goa_Daman_&_Diu"
place_codes$state_name[place_codes$state_name=="NCT of Delhi"]="Delhi"
place_codes$state_name[place_codes$state_name=="Jammu and Kashmir"]="Jammu_&_Kashmir"


place_codes=place_codes %>% 
  mutate(state_name=str_replace_all(state_name, " ", "_")) %>% 
  dplyr::select(state_name, state_code) %>% 
  mutate(state=padzero(state_code, 2)) %>% 
  dplyr::select(-state_code)

place_codes_sub=place_codes %>% 
  dplyr::filter(state_name %in% c("Punjab",
                                  "Rajasthan",
                                  "Uttar_Pradesh",
                                  "Bihar",
                                  "Madhya_Pradesh")) %>% 
  mutate(state_name=tolower(state_name)) %>% 
  distinct()

## MERGING IN 

url_df_sub=url_df %>% inner_join(place_codes_sub)

# Another df to record the year-wise state name ADR site uses. 

dt=as.data.frame(rbind(c("09","uttarpradesh2022","2022"),
                       c("09","uttarpradesh2017","2017"),
                       c("09","up2012","2012"),
                       c("09","up2007","2007"),
                       c("03","punjab2022","2022"),
                       c("03","punjab2017","2017"),
                       c("03","pb2012","2012"),
                       c("03","pb2007","2007"),
                       c("23","madhyapradesh2018","2018"),
                       c("23","mp2013","2013"),
                       c("23","2008mp","2008"),
                       c("08","rajasthan2018","2018"),
                       c("08","rajasthan2013","2013"),
                       c("08","rj2008","2008"),
                       c("10","Bihar2020","2020"),
                       c("10","bihar2015","2015"),
                       c("10","bih2010","2010"),
                       c("10","bih2005","2005")))
names(dt)=c("state","st_url","year")

url_df_sub=url_df_sub %>% inner_join(dt)
# Joining, by = c("state", "year")


#https://myneta.info/punjab2022/candidate.php?candidate_id=742

url_df_sub=url_df_sub %>% 
  mutate(url=paste0("https://myneta.info/",
                    st_url,
                    "/candidate.php?candidate_id=",
                    cid))

url_list=as.list(unique(url_df_sub$url))



# temp output rn
data_dump=read_rds("data/daily_output/data_dump1100.rds")

css_list=list('#immovable_assets',
              '#liabilities',
              '#profession',
              '#incomesource',
              '#contractdetails',
              '#cases')


ima_table=list();liab_table=list()
prof_table=list()
inc_table=list()
contract_table=list()
case_table=list()

for(i in 1:length(data_dump)){
  
  ima_table[[i]]=data_dump[[i]][[1]]
  
  liab_table[[i]]=data_dump[[i]][[2]]
  
  prof_table[[i]]=data_dump[[i]][[3]]
  
  inc_table[[i]]=data_dump[[i]][[4]]
  
  contract_table[[i]]=data_dump[[i]][[5]]
  
  case_table[[i]]=data_dump[[i]][[6]]
  
  
}



# CLEANING IMA TABLE ------------------------------------------------------

#(rn its aggregated and without any id marker)

split_string <- function(input_string) {
  
  pattern <- ",\\d{3}" # )\\s*(.*),\\d{3}"
  #pattern <- "Rs\\s(\\d{1,3}(?:,\\d{3})*)\\s*(\\d{1,3}(?:,\\d{3})*)([^\\d]+)$"
  text_val <- sub(paste0("(.*)", pattern), "", input_string)
  num_val=str_remove(input_string, text_val)
  num_val= gsub("[^0-9]", "", num_val)
  
  return(list(num_val, text_val))
  
}


land_class=list();tot_val=list()
for(l in 1:length(ima_table)){
  print(l)
  #l=50
  if(ncol(ima_table[[l]])>0){
      land_class[[l]]=c(unlist(ima_table[[l]][-1,]$X2),"url_id")
     
      
      land_value_tot=(ima_table[[l]][-1,ncol(ima_table[[l]])])
      names(land_value_tot)="val"
      tot_val[[l]] <- land_value_tot %>%
        rowwise() %>%
        mutate(num_val = split_string(val)[[1]]) %>% 
        dplyr::pull(num_val)
      tot_val[[l]]=c(tot_val[[l]],paste0(l))

  }
  
}

land_class=land_class[lapply(land_class,length)>0]
land_class=unique(unlist(land_class))
tot_val=tot_val[lapply(tot_val, length)>0]

tot_val= lapply(tot_val, function(x){names(x)=land_class;x})
tot_val=  tot_val %>%  bind_rows()

