rm(list=ls())
source("code/functions.R")
source("code/myneta_scrap.R")

# This is where the actual scraping will happen. 

# TO DO:
# 1. Read in the SHURG afidavit clean data, 
#.   I will extract all the unique candidate IDs from the data

# 2. The unique ID are of the form: "state_code-year-cand_id"
#     So, will need to parse out and create URL from this info

# 3. Then Will need to pass the URLs into the scrap function



# 1. READING IN SHURG ADR DATA -----------------------------------------------


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

url_df=as.data.frame((sapply(url_list, function(x) unlist(x))))
names(url_df)="links"
#write_csv(url_df, file="data/myneta_url_link.csv")
uulist=url_list[1:50]

# ROUGH

getcontent=function(url){
 
   # # Perform a GET request to the URL
  response <- request(url) |>
    req_perform()
  
  # Check if the request was successful
  # if (rsp$status_code == 200) {
  # Parse the content as HTML
  content <- response |> 
    resp_body_html(encoding = "UTF-8") 
  
  return(content)
  
}

clist=lapply(uulist, getcontent)

#ROUGH

# UUSE PROXY in HTTR2??

data_dump=list()

for(i in 1:300){
  
  
  css_list=list('#immovable_assets',
                '#liabilities',
                '#profession',
                '#incomesource',
                '#contractdetails',
                '#cases')
  
  # # Perform a GET request to the URL
  response <- request(url_list[[i]]) |>
    req_perform()
  
  # Check if the request was successful
 # if (rsp$status_code == 200) {
    # Parse the content as HTML
  content <- response |> 
      resp_body_html(encoding = "UTF-8")
  
data_dump[[i]]=dd=lapply(css_list,safe_extract_table, webpage=clist[[47]])
  
  if(round(i/100)==i/100){print(paste0("Done with ",i," of ",length(url_list) ))
    write_rds(data_dump, paste0("data/daily_output/adr_scrap2/data_dump",i,".rds"), compress = "gz")
    Sys.sleep(10)
  }
  
}
  

write_rds(data_dump, "data/daily_output/adr_scrap2/data_dump_tot.rds", compress = "gz")




# ss=url_df[1:50,]
# 
# wp_list=lapply(ss, read_html)
# 
# url="https://myneta.info/uttarpradesh2022/candidate.php?candidate_id=2908"
# wp=read_html(url)
# ptable=safe_extract_table(wp, css_selector="#profession")
# 
# #profession
# 
# prof_table=lapply(wp_list, safe_extract_table, css_selector="#profession")
# 
# table_ss=lapply(ss, table_scrap)







