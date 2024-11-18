
rm(list=ls())
source("code/functions.R")

#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
standardize_hyphens <- function(x) {
  # Replace various dash types with the standard hyphen-minus
  str_replace_all(x, "[\u002D\u2010\u2012\u2013\u2014]", "-")
}

# HEADER:
# I have RJ Govt Dir data scrapped. This is the code to read that in. 

master_places=master_places %>% mutate_at(c("state","district_name","town_name"), toupper)

# FUNCTIONS ---------------------------------------------------------------

catclp = function(dat, v, uvar, cutoff1, cutoff2) {
  
  dt = dat %>%
    group_by({{ v }}) %>%
    mutate(nn1 = n_distinct({{ uvar }}),
           nn2=n()) %>%
    ungroup()
  
  # printing out the first few elements in nn series
  
  message(paste0("The bottom ",deparse(substitute(v))," has these unique ", deparse(substitute(pid)) ," ", 
                 paste(sort(unique(dt$nn1))[1:5], collapse = ",")))
  
  # removing those departments with only one unique job ( small department) and less than 3 postings for that job. 

  dt = dt %>% filter(nn1 > cutoff1 & nn2>=cutoff2) 
  
  dt
}


sub_dist <- read_csv("~/Dropbox/BurkeLab Dropbox/Vaibhav Rathi/SU/Research/Data/LGD/15Nov2023/subdistricts.csv")
rj_sub_dist=sub_dist %>% clean_names() %>% 
  filter(state_name=="Rajasthan") %>% 
 mutate(district_name=tolower(district_name), 
        sub_district_name=tolower(sub_district_name)) %>% 
  rename(district=district_name, 
         post_city=sub_district_name)

# READING DISTRICT LIST
fcon=file("../Data/Civil Service Dir/RJ/dist_list.rtf", open="r")
dlist=as.data.frame(readLines(con = fcon))

dlist=dlist %>% 
  filter(grepl("option value", dlist[[1]])) 
dlist=dlist %>% 
  mutate(dname=gsub("<.*?>", "", dlist[[1]])) %>% mutate(dname=gsub("\n", ":", dname)) %>% 
  mutate(dname=gsub("Select","", dname)) %>% 
  mutate(dname=str_trim(dname)) %>% dplyr::select(dname) %>% 
  filter(dname!="") %>% mutate(dname=sub("\\\\$","", dname)) %>% distinct()

tlist=toupper(unique(master_places$town_name[master_places$state=="RAJASTHAN"]))
dislist=toupper(unique(master_places$district_name[master_places$state=="RAJASTHAN"]))
#tname1=tolower(unique(rj_sub_dist$post_city[rj_sub_dist$state_name=="Rajasthan"]))



# OFFICER LEVEL DATA ------------------------------------------------------
w_par=3
off_rj=readxl::read_excel("../Data/Civil Service Dir/RJ/Final_Inservice_Officer_Details.xlsx") %>% 
  clean_names() %>% 
  mutate(sno=gsub("\\.$", "", sno, perl = TRUE)) %>% mutate(sno=as.numeric(sno)) %>% 
  mutate(oid=cumsum(sno==1))

off_rj=off_rj %>% group_by(oid) %>% 
  fill(c(name_of_officer, date_of_birth,
         home_town,
        qualification , 
        unique_id, 
        service_type), .direction="down") %>% ungroup() %>% 
  dplyr::select(oid, everything())

off_rj=off_rj %>% 
  rename(post_history=post_held_by_officer) %>% 
  dplyr::select(oid,post_history, everything()) %>% 
  rename(officer_name=name_of_officer) %>% 
  mutate(officer_posting=gsub(".*\\((.*)\\).*", "\\1", officer_name)) %>% 
  mutate(officer_posting=str_replace(officer_posting, "\\s+","")) %>% 
  dplyr::select(officer_posting, officer_name, everything()) %>% 
  mutate(post_city=word(post_history, -1, sep = ",")) %>% 
  mutate(post_city=str_trim(post_city)) %>% 
  mutate(title=sub(",.*", "", post_history)) %>% 
  mutate(officer_name=gsub("\\((.*)\\)", "", officer_name)) %>% 
  rename(officer_hometown=home_town) %>% 
  mutate(mkey=paste0(officer_name, officer_posting, officer_hometown)) %>% 
  mutate(mkey=str_replace(mkey, "\\s+","")) %>% 
  mutate(sno=as.numeric(gsub("[\n]|\\.|\\s+", "", sno))) %>% 
  separate(officer_posting, 
           into = c("posting", "batch_year"), 
           sep = ":",remove = F) %>% 
  mutate(batch_year=if_else(as.numeric(batch_year)>50,
                            paste0("19",batch_year), paste0("20",batch_year))) %>% 
  mutate(batch_year=as.numeric(batch_year))

off_rj=off_rj %>% group_by(oid) %>% 
  mutate(max_lvl=max(sno)) %>% ungroup() %>% 
  mutate(lvl=max_lvl-sno +1) 
# Above the lvl variable is prolly not correct, 
# A better apraoch would be to count the actual IAS levels, like JS, Director, etc


# Dates variable

off_rj=off_rj %>% 
  mutate(order_date=str_sub(order_joining,1,10 ),
         join_date=str_sub(order_joining, 11,20)) %>% 
  mutate(order_date=as.Date(order_date, format = "%d/%m/%Y"), 
         join_date=as.Date(join_date, format="%d/%m/%Y")) %>% 
  mutate(end_date=as.Date(date_to, format="%d/%m/%Y")) %>% 
  #mutate(end_date=if_else(max_lvl==lvl, 
  #                        as.Date("01/01/2100", format="%d/%m/%Y"), end_date)) %>% 
  mutate(start_date=if_else(is.na(join_date), order_date, join_date)) %>% 
  mutate(join_year=year((join_date)),
          end_year=year((end_date)), 
         end_month=month(end_date),
         start_year=year(start_date), 
         start_month=month(start_date)) %>% 
  mutate(duration=as.numeric(end_date-start_date))

off_rj$duration[off_rj$end_year==2100]=NA
off_rj$duration[off_rj$duration<0]=NA


# The end date can be later or earlier than the next order data

off_rj=off_rj %>% group_by(officer_name) %>% 
  arrange(officer_name, start_date) %>% 
  mutate(next_order_date=Lag(order_date, -1)) %>% ungroup() %>% 
  mutate(next_order_year=year(next_order_date)) %>% 
  mutate(early_order=next_order_date<=end_date) %>% 
  mutate(end_date2=if_else(is.na(end_date), next_order_date, end_date)) %>% 
  mutate(end_year2=year(end_date2)) %>% 
  mutate(duration2=as.numeric(end_date2-start_date))

# Viewing

# View(off_rj %>% dplyr::select(oid,lvl, post_history, officer_name, 
#                               order_date, 
#                               join_date, end_date, next_order_date, early_order) %>% distinct() %>% 
#         arrange(oid,lvl) %>% ungroup())




off_rj=off_rj %>%  group_by(oid) %>% 
  mutate(off_start_year=min(start_year, na.rm=T), 
         off_end_year=max(end_year2, na.rm=T)) %>% ungroup()

off_rj=off_rj %>% mutate(tenure_length=start_year-batch_year)

off_rj=off_rj %>% 
  mutate(tenure_length2=start_year-off_start_year) %>% 
  mutate(tenure_length=if_else(tenure_length<=0, tenure_length2, tenure_length))

# Off_end_year seems to be inf/NA for lot of obs and current for rest of them
# It just means that its a dataset of currently active officers. 

## Dealing the Designation(title)

off_rj=off_rj %>% mutate(title1 = str_extract(title, ".*(?=\\s&|\\sAND)"),
    title2 = str_extract(title, "(?<=\\s&|\\sAND).*")) %>% 
  mutate(title1=if_else(is.na(title1), title, title1)) %>% 
  dplyr::select(-title) %>% group_by(oid, start_date) %>% 
  pivot_longer(cols = starts_with("title"), 
               names_to = "tno", 
               values_to = "title") %>% 
  ungroup() %>% filter(!is.na(title)) %>% mutate(title=str_trim(title)) %>% 
  mutate(under_training=if_else(grepl("UNDER TRAINING", title),1,0)) %>% 
  mutate(tno=sub("title","", tno)) %>% group_by(oid, start_date) %>% 
  mutate(under_training=max(under_training, na.rm=T)) %>% ungroup() %>% 
  mutate(title=gsub("\\s*\\([^)]+\\)", "", title)) %>% 
  mutate(title=gsub("ASSTT.","ASSISTANT",title)) 


lvl_df=off_rj %>% 
  group_by(title,  service_type, under_training) %>% 
  summarise(title_order_mean=mean(tenure_length, na.rm=T), 
         title_order_sd=sd(tenure_length, na.rm=T), 
         count=n()) %>% 
  mutate(title_order_cov=title_order_sd/title_order_mean) %>% ungroup() %>% 
  dplyr::select(title, service_type, title_order_mean,under_training) %>% 
    distinct() %>% 
  mutate(title_lvl=floor(title_order_mean/w_par)+1) %>% 
  dplyr::select(-title_order_mean) %>% distinct()
  
# Merging back in the main data

off_rj=off_rj %>% inner_join(lvl_df)

# Now categorizing shifts

off_rj=off_rj %>% group_by(oid, start_date) %>%
  mutate(title_lvl=min(title_lvl, na.rm = T)) %>% 
  group_by(oid) %>%  arrange(start_date) %>% 
  mutate(next_title_lvl=Lag(title_lvl, -1), 
         prev_title_lvl=Lag(title_lvl, 1)) %>% ungroup() %>% 
  group_by(oid, start_date) %>% 
  mutate(next_title_lvl=max(next_title_lvl, na.rm=T), 
         prev_title_lvl=min(prev_title_lvl, na.rm=T)) %>% ungroup()


# Now we can measure each shift category per unit time (year)

off_rj=off_rj %>% 
  mutate(shift_indc=if_else(title_lvl==prev_title_lvl,0, 
                            if_else(title_lvl>prev_title_lvl,1,-1))) %>% 
  mutate(shift_indc=if_else(prev_title_lvl==Inf| prev_title_lvl==-Inf,NA_real_, shift_indc))




# GETTING DISTRICT INFO FROM POST HISTOR VAR

matches <- sapply(as.list(dlist[[1]]), 
                  function(dept) grepl(dept, off_rj$post_history, ignore.case = TRUE))
mlist <- apply(matches, 2, which)
off_rj$pdistrict=NA
for(m in 1:length(mlist)){
  
  rows=mlist[[m]]
  off_rj$pdistrict[rows]=dlist[[1]][m]
  
}


rj_places=master_places %>% filter(state=="RAJASTHAN")
off_rj_m=off_rj %>% left_join(rj_places, by=c("pdistrict"="town_name"))
off_rj_m$district_name[off_rj_m$pdistrict=="JHUNJHUNU"]="JHUNJHUNUN"
off_rj_m$district_name[off_rj_m$pdistrict=="JALORE"]="JALOR"
off_rj_m$district_name[off_rj_m$pdistrict=="SRIGANGANAGAR"]="GANGANAGAR"
off_rj_m$district_name[off_rj_m$pdistrict=="CHITTORGARH"]="CHITTAURGARH"
off_rj_m$district_name[off_rj_m$pdistrict=="DHOLPUR"]="DHAULPUR"
off_rj_m$district_name[off_rj_m$pdistrict=="KHAIRTHAL-TIJARA"]="ALWAR"
off_rj_m$district_name[off_rj_m$pdistrict=="JHALAWAR"]="JHALAWAR"
off_rj_m$district_name[off_rj_m$pdistrict=="SAWAIMADHOPUR"]="SAWAI MADHOPUR"
off_rj_m$district_name[off_rj_m$pdistrict=="JODHPUR (RURAL)"]="JODHPUR"
off_rj_m$district_name[off_rj_m$pdistrict=="JAIPUR (RURAL)"]="JAIPUR"
off_rj_m$district_name[off_rj_m$pdistrict=="KOTPUTLI - BEHROR"]="ALWAR"
off_rj_m$district_name[off_rj_m$pdistrict=="DEEDWANA - KUCHAMAN"]="NAGAUR"
off_rj_m$district_name[off_rj_m$pdistrict=="NEEM KA THANA"]="SIKAR"
off_rj_m$district_name[off_rj_m$pdistrict=="DUDU"]="DUDU"





# REMOVING SOME VARIABLES


off_rj_clean=off_rj_m %>% 
  dplyr::select(-order_joining, -present_posting, 
                -date_to, -additional_post_held_by_officer, 
                -additional_officer_order_joining, -relieving, 
                -year, -training_name, -institute, 
                -place, -duration_days, -mkey, -max_lvl)


# CREATING NEW KEY INDICATING PERSON-TITLE LEVEL

off_rj_clean=off_rj_clean %>% mutate(officer_name=str_squish(officer_name)) %>% 
          mutate(person_post=paste0(officer_name, title))


# CONSTRUCTING CHURN
# Restricting to officer who started on or before 2006
rj_off_churn=off_rj_clean %>% filter(off_start_year<=2006) %>% 
  group_by(district_name, start_year) %>% #filter(pdistrict!="JAIPUR") %>% 
  mutate(dist_syear_churn=n_distinct(person_post)) %>% ungroup() %>% 
  group_by(district_name, end_year) %>% 
  mutate(dist_eyear_churn=n_distinct(person_post)) %>% ungroup() %>% 
  mutate(duration1=as.numeric(end_date-start_date), 
         duration2=as.numeric(next_order_date-start_date)) %>% 
  group_by(district_name, end_year) %>% 
  mutate(dist_eyear_dur1=mean(duration1, na.rm=T), 
         early_order_avg=mean(as.numeric(early_order), na.rm=T)) %>% 
  ungroup() %>% 
  group_by(district_name, next_order_year) %>% 
  mutate(dist_eyear_dur2=mean(duration2)) %>% ungroup() %>% 
  dplyr::select(pdistrict, district_name, start_year, end_year,next_order_year,
                dist_syear_churn, 
                dist_eyear_churn, dist_eyear_dur1, 
                dist_eyear_dur2, early_order_avg) %>% distinct()



# Merging to get district also
# off_rj=off_rj %>% mutate(post_city=tolower(post_city)) %>% 
#   separate(post_city, into = c("post_city", "dist"), 
#            sep = "\\s*\\(\\s*|\\s*\\)\\s*")
# 
# 
# off_rj=off_rj %>% left_join(rj_sub_dist) %>% 
#   mutate(dist2=coalesce(district, dist)) %>% 
#   mutate(dist2=if_else(is.na(dist2), post_city, dist2))

 #ABOVE IS A POOR JOB TO CREATE DISTRICT VARIABLE. NEED TO FIX IT

# POST LEVEL DATA ---------------------------------------------------------



# POST LEVEL DATA

post_rj_raw=read_csv("../Data/Civil Service Dir/RJ/Final_PostHistroy_Services.csv")
post_rj_raw=post_rj_raw %>% clean_names()

# Cleaning

post_rj_raw=post_rj_raw %>% mutate(officer_name= gsub("[\n]|\\s+", " ", officer_name)) %>% 
    mutate(sno=gsub("[\n]|\\.|\\s+", "", sno))

# CREATING POST ID

post_rj=post_rj_raw %>% mutate(pid=cumsum(sno=="1")) %>% 
  dplyr::select(pid, everything())


post_rj=post_rj %>% 
  mutate(officer_detail=gsub(".*\\((.*)\\).*", "\\1", officer_name)) %>% 
  dplyr::select(officer_detail, officer_name, everything()) %>% 
  separate(officer_detail, 
           into = c("officer_posting", "officer_hometown"), 
           sep = "/") %>% 
  mutate_at(vars(officer_posting, officer_hometown), str_trim) %>% 
  mutate(officer_name=gsub("\\((.*)\\)", "", officer_name)) %>% 
  fill(c(department, designation, service_type), .direction="down") %>% 
  mutate(post_city=word(post_description, -1, sep = ",")) %>% 
  dplyr::select(post_city, everything()) %>% 
  mutate(officer_name=str_trim(officer_name), 
         post_city=str_trim(post_city)) %>% 
  mutate(officer_name=str_squish(officer_name)) %>% 
  mutate(mkey=paste0(officer_name, officer_posting, officer_hometown)) %>% 
  mutate(mkey=str_replace(mkey, "\\s+","")) %>% 
  separate(officer_posting, 
           into = c("posting", "batch_year"), 
           sep = ":",remove = F) %>% 
  mutate(batch_year=if_else(as.numeric(batch_year)>50,
                            paste0("19",batch_year), paste0("20",batch_year))) %>% 
  mutate(batch_year=as.numeric(batch_year))


# breaking up the post_description var
## TO DO: The idea here is to extract dept, desig etc 
## and standardize 

# Getting dates and years

post_rj=post_rj %>% 
  mutate(order_date=as.Date(order_date, format = "%d/%m/%Y"), 
         join_date=as.Date(join_date, format="%d/%m/%Y")) %>% 
  mutate(end_date=as.Date(relieving_date, format="%d/%m/%Y")) %>% 
  mutate(start_date=if_else(is.na(join_date), order_date, join_date)) %>% 
  mutate(join_year=year((join_date)), order_year=year(order_date),
         end_year=year((end_date)), start_year=year(start_date), 
         end_month=month(end_date), start_month=month(start_date)) 

post_rj=post_rj %>% group_by(officer_name, post_description, 
                             order_date, start_date) %>% 
  fill(c(end_date,
         relieving_date,
         end_year,end_month),.direction = "downup") %>% ungroup()


# Removing obs where order_date and start_date are more than 50 days apart. 
## These are outliers in the data, I can skip this step but then I will have to 
## count the churn differently 

post_rj=post_rj %>% mutate(diff=as.numeric(order_date-start_date)) %>% 
  filter(abs(diff)<=50)


# REMOVING SOME VARIABLES

post_rj=post_rj %>%  
  dplyr::select(-join_date, -relieving_date,  -sno, -remarks,  -order_year) %>% 
  distinct()

post_rj=post_rj %>% mutate(tenure_length=start_year-batch_year)

post_rj=post_rj %>% group_by(mkey) %>% 
  mutate(off_start_year=min(start_year)) %>% ungroup() %>% 
  mutate(tenure_length2=start_year-off_start_year) %>% 
  mutate(tenure_length=if_else(tenure_length<=0, tenure_length2, tenure_length))


# Most variation is at designation-service_type level only, so ranking along that (ie removing department)
desig_lvl_df=post_rj %>% group_by(designation,  service_type) %>% 
  mutate(desig_order_mean=mean(tenure_length, na.rm=T), 
         desig_order_sd=sd(tenure_length, na.rm=T)) %>% 
  mutate(desig_order_cov=desig_order_sd/desig_order_mean) %>% 
  ungroup() %>% dplyr::select(designation, service_type, desig_order_mean) %>% distinct() %>% 
  mutate(desig_lvl=floor(desig_order_mean/w_par)+1) %>% 
  dplyr::select(-desig_order_mean) %>% distinct()

desig_vec=desig_lvl_df %>% dplyr::select(designation) %>% distinct()
write.xlsx(desig_vec, "data/interm/desig_list.xlsx")



# Merging back in the main data

post_rj=post_rj %>% inner_join(desig_lvl_df)

# Now categorizing shifts

post_rj=post_rj %>% group_by(mkey, start_date) %>%
                mutate(desig_lvl=min(desig_lvl, na.rm = T)) %>% 
      group_by(mkey) %>%  arrange(start_date) %>% 
          mutate(next_desig_lvl=Lag(desig_lvl, -1), 
                 prev_desig_lvl=Lag(desig_lvl, 1)) %>% ungroup() %>% 
  group_by(mkey, start_date) %>% 
  mutate(next_desig_lvl=max(next_desig_lvl, na.rm=T), 
         prev_desig_lvl=min(prev_desig_lvl, na.rm=T)) %>% ungroup()


# Now we can measure each shift category per unit time (year)

post_rj=post_rj %>% 
  mutate(shift_indc=if_else(desig_lvl==prev_desig_lvl,0, 
                            if_else(desig_lvl>prev_desig_lvl,1,-1))) %>% 
  mutate(shift_indc=if_else(prev_desig_lvl==Inf| prev_desig_lvl==-Inf,NA_real_, shift_indc))




mob_df=post_rj %>% 
  tabyl(desig_lvl,next_desig_lvl) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) 


# VIEW

# View(post_rj1 %>% dplyr::select(mkey, start_date, designation, desig_lvl, next_desig_lvl) %>% arrange(mkey, start_date))
# 
# 
# dd=post_rj1 %>%  dplyr::select(designation, department, service_type, post_order_mean,
#                                 post_order_sd, post_order_cov) %>% distinct() %>% 
#   group_by(designation, service_type) %>%
#   mutate(mean_mean=mean(post_order_mean, na.rm=T), 
#          mean_sd=sd(post_order_mean, na.rm=T)) %>% 
#   mutate(mean_cov=mean_sd/mean_mean) %>% 
#   ungroup()
# 
# dd=(post_rj1 %>%  arrange(start_date)) %>% 
#   dplyr::select(mkey,post_city, officer_posting, posting, batch_year, officer_hometown, officer_name, post_description, 
#                 start_date, end_date, department, designation) %>%  distinct()
# ddd=dd %>% group_by(mkey,start_date) %>%
#   mutate(row_id = row_number()) %>%
#   pivot_wider(names_from = row_id, values_from = c(department, designation) ) %>% distinct() %>% ungroup()


# VIEW

tab_df=post_rj %>% filter(grepl("DR|PR|RR|SC|SP|SF|SL|EC", posting)) %>% 
  filter(nchar(posting)==2) %>% dplyr::select(posting, service_type) %>% 
  tabyl(service_type,posting) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) 


# GETTING DEPT+DESIG level stats to be used to order them along the heiriarchy. 



# REMOVING "UNDER TRAINING" STINTS

#post_rj1=post_rj %>% filter(!grepl("UNDER TRAINING", post_description))


# READING DEPARTMENT LIST

# fcon <- file("../Data/Civil Service Dir/RJ/rj_dept_list.txt", open = "r")
# dlist=as.data.frame(readLines(con = fcon)) %>% 
#   mutate(dname=gsub("<.*?>", "", dlist[[1]])) %>% mutate(dname=gsub("\n", ":", dname)) %>% 
#                  mutate(dname=gsub("Select","", dname)) %>% 
#             mutate(dname=str_trim(dname)) %>% dplyr::select(dname) %>% 
#       filter(dname!="")
# dlist1=dlist$dname


# Assigning post districts

matches <- sapply(as.list(dlist[[1]]), 
                  function(dept) grepl(dept, post_rj$post_description, ignore.case = TRUE))
mlist <- apply(matches, 2, which)
post_rj$pdistrict=NA
for(m in 1:length(mlist)){

  rows=mlist[[m]]
  post_rj$pdistrict[rows]=dlist[[1]][m]

}


post_rj$pdistrict[post_rj$post_city=="SAWAI MADHOPUR"]="SAWAI MADHOPUR"

rj_places=master_places %>% filter(state=="RAJASTHAN")
post_rj_m=post_rj %>% left_join(rj_places, by=c("pdistrict"="town_name"))
post_rj_m$district_name[post_rj_m$pdistrict=="JHUNJHUNU"]="JHUNJHUNUN"
post_rj_m$district_name[post_rj_m$pdistrict=="JALORE"]="JALOR"
post_rj_m$district_name[post_rj_m$pdistrict=="SRIGANGANAGAR"]="GANGANAGAR"
post_rj_m$district_name[post_rj_m$pdistrict=="CHITTORGARH"]="CHITTAURGARH"
post_rj_m$district_name[post_rj_m$pdistrict=="DHOLPUR"]="DHAULPUR"
post_rj_m$district_name[post_rj_m$pdistrict=="KHAIRTHAL-TIJARA"]="ALWAR"
post_rj_m$district_name[post_rj_m$pdistrict=="JHALAWAR"]="JHALAWAR"
post_rj_m$district_name[post_rj_m$pdistrict=="SAWAIMADHOPUR"]="SAWAI MADHOPUR"
post_rj_m$district_name[post_rj_m$pdistrict=="JODHPUR (RURAL)"]="JODHPUR"
post_rj_m$district_name[post_rj_m$pdistrict=="JAIPUR (RURAL)"]="JAIPUR"
post_rj_m$district_name[post_rj_m$pdistrict=="KOTPUTLI - BEHROR"]="ALWAR"
post_rj_m$district_name[post_rj_m$pdistrict=="DEEDWANA - KUCHAMAN"]="NAGAUR"
post_rj_m$district_name[post_rj_m$pdistrict=="NEEM KA THANA"]="SIKAR"
post_rj_m$district_name[post_rj_m$pdistrict=="DUDU"]="DUDU"


# Estimating avg tenure time to reach per position*department
# The idea here is that, each position should have an avg time to reach plus a (hopefully) small spread around it
# This can be used to order the positions in the hierarchy. 

# MOVED TO THE FIRST BLOCK IN POST SECTION



# STRING CLEANING

post_rj_m=post_rj_m #%>% mutate(officer_name=str_squish(officer_name))

# REMOVING SNO, PID, DESIGNNATION
post_rj_clean=post_rj_m# %>%  dplyr::select(-sno,   
                                 #   -remarks, -mkey) %>% distinct()

# Getting Duration also, 
post_rj_clean=post_rj_clean %>% group_by(officer_name) %>% 
  arrange(officer_name, start_date) %>% 
  mutate(duration=as.numeric(end_date-start_date)) %>% ungroup()



# CREATING NEW KEY
post_rj_clean=post_rj_clean %>% mutate(person_post=paste0(officer_name,
                                              post_description))


# 

# Now to measure churn we can just count unique person_post at whatever level


# STEPS TO CLEAN POST DATA AT THIS POINT:

# 1. One post should have one location. 
# 2. If JPR is post_city then JAIPUR is district
# 3. Get administrative districts from district listing, pdistrict is from the dropdown
# 4. Construct measure of churn here only

# 1. 
# post_rj1=post_rj %>% group_by(post_description) %>% 
#   fill(pdistrict, .direction = "downup") %>% ungroup()
#^ Not working right now. 
# I will need to break open the text in post_description to extract dept labels. 
# Currently, its mixed up with different other location texts etc 

rj_post_churn=post_rj_clean %>% group_by(district_name, start_year) %>% #filter(pdistrict!="JAIPUR") %>% 
  mutate(dist_syear_churn=n_distinct(person_post)) %>% ungroup() %>% 
  group_by(district_name, end_year) %>% 
  mutate(dist_eyear_churn=n_distinct(person_post)) %>% ungroup() %>% 
  group_by(officer_name) %>% 
  arrange(officer_name, start_date) %>% 
  mutate(next_order_date=Lag(order_date, -1)) %>% ungroup() %>% 
  mutate(next_order_year=year(next_order_date)) %>% 
  mutate(early_order=next_order_date<=end_date) %>% 
  mutate(duration1=as.numeric(end_date-start_date), 
         duration2=as.numeric(next_order_date-start_date)) %>% 
  group_by(district_name, end_year) %>% 
  mutate(dist_eyear_dur1=mean(duration1, na.rm=T), 
         early_order_avg=mean(as.numeric(early_order), na.rm=T)) %>% 
  ungroup() %>% 
  group_by(district_name, next_order_year) %>% 
  mutate(dist_eyear_dur2=mean(duration2)) %>% ungroup() %>% 
  dplyr::select(pdistrict, district_name, start_year, end_year,next_order_year,
                dist_syear_churn, 
                dist_eyear_churn, dist_eyear_dur1, 
                dist_eyear_dur2, early_order_avg) %>% distinct()




# TAGGING SECTORS TO THE DEPARTMENT ---------------------------------------
# I have manually tagged all the dept with sectors and the result can be directly called in

post_dept_sector=source("code/post_dept_sector_tag.R")
post_dept_sector=post_dept_sector$value

post_dept_sector=post_dept_sector %>% mutate(sector1=standardize_hyphens(sector1), 
                                             sector2=standardize_hyphens(sector2))

post_dept_sector$sector1[post_dept_sector$sector1=="POWER"]="ENERGY"


# Now merging it with the main post data

post_sector_rj=post_rj_clean %>% inner_join(post_dept_sector)




qsavem(off_rj_clean, post_sector_rj, file="data/clean/RJ_CS_wpar3.qs")


## OLD BELOW



# CLIPPING
## Now we can clip-clean the data to weed out departments, location (?) or designations that dont have many posts

# removing those departments with only one unique job ( small department) and less than 3 postings for that job. 

post_rj1=post_rj %>% catclp(v=department, pid, 1,3)

post_rj2=post_rj1 %>% catclp(v=district, pid, 5,1)

post_rj_clipped=post_rj #post_rj2 # NOT DOING IT ATM, seems random

# Adding state name and cleaning district name
post_rj_clipped=post_rj_clipped %>%  mutate(st_name="RAJASTHAN")
post_rj_clipped=post_rj_clipped %>% mutate(district=if_else(str_detect(district, "JAIPUR"), "JAIPUR", district))


ac_shp <- st_read("../Data/MAPS- INDIA/AC/maps-master/assembly-constituencies/India_AC.shp") %>% 
  st_make_valid()

ac_df=ac_shp %>%  as.data.frame() %>%  clean_names() %>% rename(district=dist_name)

ac_df=ac_df %>%  filter(st_name=="RAJASTHAN") %>% 
   mutate(district=str_extract(district,"\\w+")) %>% 
  dplyr::select(st_name, district, ac_no, ac_name, pc_no, pc_name) %>% distinct()

#post_rj_clipped=post_rj_clipped %>% left_join(ac_df, by=c("st_name", "district"))
# Putting AC information here. Is it needed?

# Since this is an unbalanced panel, we can create a post start date also. 
# pid indicates an unique post. 
# In the raw data, we have order_date and join_date, both of them have some missing data
# but concatinating them gives much higher density of data, therefore using start_date/year

post_rj_clipped=post_rj_clipped %>% group_by(pid) %>% 
  mutate(post_start_year=min(start_year, na.rm=T), 
         post_end_year=max(end_year, na.rm=T)) %>% ungroup() %>%  distinct()


# Adding department level size and churn measure --------------------------

dt=post_rj_clipped

dlist=unique(dt$department); ylist=c(1995:2023)
dy_count=expand_grid(department=dlist, year=ylist, dept_size=NA_integer_)




dept_year_churn=dt %>% group_by(department, start_year) %>% 
  summarise(dy_churn=n_distinct(pid)) %>% arrange(start_year, -dy_churn) %>% ungroup()


# %>% 
#   group_by(start_year) %>% mutate(rank=row_number()) %>% filter(start_year>=2000 & rank<=20) %>% group_by(department) %>% summarise(count_in20=n()) %>% arrange(-count_in20)


post_count_rj=dt %>% group_by(department, designation, post_city) %>%
  summarise(post_year1=min(start_year), post_year2=max(start_year)) %>% ungroup() %>% 
  mutate(post_year2=if_else(post_year2>=2018, 2030, post_year2))

# mutate(start_count=n()) %>% group_by(department, end_year) %>% 
#   mutate(end_count=n()) %>% ungroup() %>% mutate(size_t=start_count-end_count) %>% 
#   dplyr::select(pid, department, start_year, start_count, end_year, end_count, size_t) %>% distinct() %>% arrange(pid, department, start_year)



# Create an empty data frame to store results


# Loop through each department and year
for (d in dlist) {
  for (y in ylist) {
    # Filter positions for the current department and year
    filtered_positions <- post_count_rj %>%
      filter(department == d, post_year1 <= y, post_year2 >= y)
    
    # Count the number of active positions
    num_active_positions <- nrow(filtered_positions)
    
    # Append results to the data frame
    # active_positions <- rbind(active_positions, data.frame(department = dept, year = year, count = num_active_positions))
    
    dy_count=dy_count %>%  mutate(dept_size=if_else(department==d & year==y, 
                                                    num_active_positions, 
                                                    dept_size))
    
  }
}


dept_level_df=dy_count %>% left_join(dept_year_churn, by=c("department", "year"="start_year"))
dept_level_df$dy_churn[is.na(dept_level_df$dy_churn)]=0
dept_level_df=dept_level_df  %>% mutate(churn_frac=dy_churn/dept_size)


dept_level_df=dept_level_df %>% group_by(department) %>% 
  mutate(dept_size_avg=mean(dept_size[year>=2015], na.rm=T), 
         dept_churn=mean(churn_frac, na.rm=T)) %>% ungroup() %>% 
  arrange(-dept_size_avg) %>% group_by(department) %>% 
  mutate(dd=row_number()) %>% ungroup() %>% 
  mutate(rank=cumsum(dd==1))

dept_churn=dept_level_df %>% dplyr::select(department, dept_churn,dept_size_avg) %>% distinct() %>% 
  arrange(-dept_size_avg,-dept_churn)

top_dept_churn=dept_level_df %>% filter(dept_size_avg>=10)




#qsavem(off_rj, post_rj_clipped,top_dept_churn, file="data/clean/RJ_CS.qs")


# checking how much I can merge the post_rj with ias_exp/profile to merge in the ias_id from that data
# name matching is not great, need to figure out a better way. Merged 187 out of 227  ( or 253 from dt1)

# Around 82 % merge rate 

# dt1=off_rj %>% filter(lvl==1) %>% 
#   dplyr::select(officer_posting, 
#                 officer_name, 
#                 date_of_birth, join_year, officer_hometown, service_type) %>% distinct() %>% 
#   mutate(name=tolower(officer_name), 
#          date_of_birth=as.Date(date_of_birth, format = "%d/%m/%Y"), 
#          birth_year=year(date_of_birth)) %>% 
#   filter(service_type=="IAS")  %>% 
#   filter(birth_year<=1996) %>% arrange(birth_year,name)
# 
# prefixes_to_remove <- c("dr.", "smt", "shri", "shree", "ms", "dr.(ms.)", "miss", "ms.")
# dt1$name <- gsub(paste0("^(", paste(prefixes_to_remove, collapse = "|"), ")\\s+"), "",
#                             dt1$name, ignore.case = TRUE)
# 
# durga_dt1=dt1 %>% filter(grepl("durga", name))
# 
# durga_post=post_rj_clipped %>% filter(grepl("DURGA SHANKAR",officer_name))
# 
# dt2=ias_profile %>% 
#   dplyr::select(id, name, date_of_birth,cadre, date_of_joining,
#                 place_of_domicile) %>%  distinct() %>% 
#   mutate(name=tolower(name)) %>% 
#   filter(cadre=="Rajasthan") %>% 
#     mutate(date_of_birth=as.Date(date_of_birth), 
#            date_of_joining=as.Date(date_of_joining)) %>% 
#   mutate(join_year1=year(date_of_joining), 
#          birth_year=year(date_of_birth)) %>%
#   filter(birth_year>=1963) %>% 
#            arrange(birth_year, name) %>% distinct()
# 
# dt2$name <- gsub(paste0("^(", paste(prefixes_to_remove, collapse = "|"), ")\\s+"), "",
#                  dt2$name, ignore.case = TRUE)
# 
# dt_merge=dt1 %>% inner_join(dt2)


# OLD


# # NOTE: This file is being used to read all the pdfs that are being exported from: 
# # RJ DOP tracking at post level. 
# # Link: https://dop.rajasthan.gov.in/forms/newhistoryform.aspx
# 
# 
# 
# # District Excise Officer -------------------------------------------------
# # For tabulizer:extract_tables use method==lattice"
# 
# raw=tabulizer::extract_tables("../Data/IAS-tcpd/RJ/Excise/DEO/All_districts.pdf",
#                              method = "lattice")
# 
# column_names <- as.character(raw[[1]][1, ])
# 
# # Remove the first row from the first matrix
# raw[[1]] <- raw[[1]][-1, ]
# 
# # Combine matrices into a single matrix by row binding
# combined_matrix <- do.call(rbind, raw)
# 
# # Create a data frame and set column names
# dt <- as.data.frame(combined_matrix, stringsAsFactors = FALSE)
# colnames(dt) <- column_names
# 
# deo_dt=dt


