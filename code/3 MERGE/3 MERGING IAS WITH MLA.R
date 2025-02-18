rm(list=ls())
source("code/functions.R")

# 1. FUNCTIONS -----------------------------------------------------------




# 1. READING DATA -----------------------------------------------------------

# these are new keys from SHRUG, might be useful. Have not used them yet though

# shrug_ac07_key=read_dta("../Data/SHRUG/keys/shrug-con-keys-dta/shrid_frag_con07_key.dta") %>% 
#   clean_names()
# shrug_ac08_key=read_dta("../Data/SHRUG/keys/shrug-con-keys-dta/shrid_frag_con08_key.dta") %>% 
#   clean_names()
# 
# names_ac07=read_dta("../Data/SHRUG/keys/shrug-con-keys-dta/ac07_name_key.dta") %>% 
#   clean_names()
# names_ac08=read_dta("../Data/SHRUG/keys/shrug-con-keys-dta/ac08_name_key.dta") %>% 
#   clean_names()

# Turns out not needed atm

source("code/0 IMPORT/state_political_unrest.R")
source("code/0 IMPORT/new_state_df.R")
source("code/0 IMPORT/state_ruling_party.R")
source("code/3 MERGE/3 IAS MP MLA MERGE PRE-PROC.R")

state_unrest=state_unrest %>% 
  mutate(state_name=standardize_state_names(state_name)) 

new_state_df=new_state_df %>% 
  mutate(state_name=standardize_state_names(state_name))

state_party_df=state_ruling_party %>%
  rename(st_name=state) %>% 
  mutate(st_name=standardize_state_names(st_name)) %>% 
  mutate(year=str_extract(term_duration, "^\\d{4}")) %>% 
  rename(state_party=party_acronym)  %>% 
  dplyr::select(st_name, year,  state_party) #


ac_lvl_ge_raw=read_csv("../Data/TCPD/General Election/AC_LEVEL_All_States_2024-11-14.csv") %>% 
  clean_names()
ac_pc_map=ac_lvl_ge_raw %>% 
  dplyr::select(state_name, constituency_name, pc_name, 
                 delim_id) %>% 
  distinct() %>% 
  mutate_at(c("constituency_name", "pc_name"), tolower) %>%
  mutate(state_name=standardize_state_names(state_name)) %>% 
  rename(st_name=state_name)#

ae_raw=read_csv("../Data/TCPD/AC/TCPD_AE_All_States_2023-7-6.csv") %>% 
  clean_names()

ac_pc_dist_delim_map=qread("data/2 CLEAN/ac_pc_dist_delim_map.qs")

shrug_candX=read_dta("../Data/SHRUG/ELECTION/shrug-adr-elections-ac-dta/affidavits_ac.dta") %>% 
  clean_names()

shrug_candX=shrug_candX %>% 
  left_join(ac_pc_dist_delim_map %>% 
              dplyr::select(pc01_state_id, 
                            pc01_district_id, 
                            dist_name) %>% 
              distinct(), 
            by=c("pc01_state_id","adr_district_name"="dist_name"))




dist_winner_X=shrug_candX %>% 
  dplyr::select(pc01_state_id, pc01_state_name,
                adr_district_name, year, pc01_district_id, ac_id,
                winner_net_assets,winner_assets,
                winner_liabilities, winner_any_crim,winner_num_crim) %>% 
  distinct() %>% 
  group_by(pc01_state_id, pc01_state_name,
           adr_district_name, year) %>% 
  mutate_at(c("winner_net_assets",
                 "winner_assets",
                 "winner_liabilities",
                 "winner_any_crim",
                 "winner_num_crim"), sum, na.rm=T) %>% 
  mutate(n_ac=n_distinct(ac_id)) %>% 
  ungroup() %>% 
  dplyr::select(-ac_id) %>%distinct()
 

dist_winner_X=dist_winner_X %>% 
  mutate(st_name=standardize_state_names(pc01_state_name)) %>% 
  rename(dist_name=adr_district_name) %>% 
  mutate(year=as.character(year))#

# Afdivit data is only available post 2003 hence fewer obs

## IAS DATA

ias_exp=qread("data/2 CLEAN/tcpd_ias+state_exp.qs")
dist_collector_df=qread("data/2 CLEAN/IAS +StateCS DATA MERGED WITH DELIM MAP_AT DIST LEVEL.qs")


## CHIEF MINISTERS 
cm_df=read_csv("../Data/TCPD/Chief Ministers/TCPD-CMID_1962_2022.csv") %>% 
  clean_names() %>% 
  rename(st_name=state_name, cm_party=party) %>% 
  mutate(st_name=standardize_state_names(st_name)) %>% 
  group_by(st_name, assembly_no) %>%
  mutate(cm_count=max(sequence_no, na.rm=T)) %>% ungroup() %>% 
  mutate(cm_year1=year(as.Date(start_date, "%d-%b-%y")),
         cm_year2=year(ymd(end_date)))

cm_party_df=cm_df %>% 
  dplyr::select(st_name, assembly_no, cm_party, cm_count, 
                cm_year1, cm_year2) %>% 
  distinct() %>% group_by(st_name, assembly_no) %>%
  mutate(cm_year1=min(cm_year1, na.rm=T),
         cm_year2=max(cm_year2, na.rm=T)) %>% ungroup() 




#  2. MERGING ----------------------------------------------------------------


ae_raw=ae_raw %>% 
  arrange(pid, assembly_no) %>% # Ensure data is ordered by pid and assembly_no
  group_by(pid) %>%
  mutate(win_counter = cumsum(position == 1)) %>%
  ungroup() %>% 
  mutate(constituency_name= gsub("\\s*\\(.*?\\)", "", 
                                 constituency_name)) %>% 
  mutate(constituency_name= tolower(constituency_name)) %>%
  mutate(district_name=tolower(district_name)) %>% 
  mutate(state_name=standardize_state_names(state_name)) %>% 
  rename(st_name=state_name) %>%
  rename(dist_name=district_name) %>%
  mutate(bizz=if_else(tcpd_prof_main %in% c("Business", 
                                            "Small Business or Self-employed"), 1, 0)) %>%
  mutate(politics=if_else(tcpd_prof_main %in% c("Politics"), 1, 0)) %>%
  mutate(ex_gov=if_else(tcpd_prof_main %in% c("Former Government"), 1, 0)) %>%
  mutate(agri=if_else(tcpd_prof_main %in% c("Agriculture",
                                            "Agricultural Labour"), 1, 0)) %>%
  mutate(other=if_else(tcpd_prof_main %in% c("Other"), 1, 0)) %>%
  mutate(year=as.character(year)) %>% 
  filter(constituency_type %in% c("GEN","SC","ST"))# %>%)

# TAGGING /REMOVING EXCEPTIONS
ae_raw=ae_raw %>% 
  filter(!(st_name=="bihar" & assembly_no==13)) 

dd=ae_raw %>% 
  filter(as.numeric(year)>=1989) %>% 
  filter(poll_no==0) %>% 
  filter(position==1) %>% #count_fun(st_name, assembly_no, year)
  inner_join(state_party_df, by=c("st_name", "year")) %>% 
  dplyr::select(st_name,  year) %>%distinct() %>% 
  arrange(st_name, year)

ae_st_ass_year=ae_raw %>% 
  filter(as.numeric(year)>=1989) %>% 
  filter(poll_no==0) %>% 
  filter(position==1) %>% #count_fun(st_name, assembly_no, year)
  dplyr::select(st_name, year) %>%distinct() %>% 
  arrange(st_name,  year)

ss=setdiff(unique(ae_st_ass_year$st_name), unique(dd$st_name))
ae_st_ass_year1=ae_st_ass_year %>% 
  filter(!(st_name %in% ss))



# Includeing state_unrest, new_state_df, state_party and cm_party
ae_raw=ae_raw %>% 
  left_join(state_unrest, by=c("st_name"="state_name", "year")) %>%
  left_join(new_state_df, by=c("st_name"="state_name", "year")) %>%
  left_join(state_party_df, by=c("st_name", "year")) %>%
  left_join(cm_party_df, by=c("st_name", "assembly_no")) %>%
  mutate(unrest=if_else(is.na(unrest), 0, unrest)) %>%
  mutate(new_state=if_else(is.na(new_state), 0, new_state)) #

# # Including PC names to help with merge with ac_pc_dist_map
# ae_raw2=ae_raw %>% 
#   left_join(ac_pc_map, by=c("st_name", "constituency_name","delim_id"))# %>%

ae_win_nobypoll=ae_raw %>% mutate(year=as.numeric(year)) %>% 
  filter(year>=1989) %>% 
  filter(position==1) %>% 
  mutate(year=as.character(year)) %>% 
  filter(poll_no==0) %>% 
  mutate(month=padzero(month, 2))  %>% 
  mutate(election=1)#
  

ac_dist_map=ac_pc_dist_delim_map %>% 
  dplyr::select(pc_uid, dist_name, st_name, 
                ac_name,ac_type, delim_id, pc01_state_id, 
                pc01_district_id) %>% 
  distinct() %>% 
  filter(ac_type %in% c("GEN","SC","ST"))

ae_win_nobypoll= ae_win_nobypoll %>% 
  mutate(constituency_name= tolower(constituency_name)) %>%
  left_join(ac_dist_map, 
            by=c("st_name", "constituency_name"="ac_name",
                 "delim_id", "constituency_type"="ac_type")) %>%
  mutate(dist_name = coalesce(dist_name.x, dist_name.y)) %>%
  dplyr::select(-dist_name.x, -dist_name.y) %>% 
  distinct() 

ae_win_nobypoll=ae_win_nobypoll %>%
  group_by(st_name) %>%
  mutate(st_min_year=min(as.numeric(year), na.rm=T), 
         st_max_year=max(as.numeric(year), na.rm=T)) %>%
  ungroup()
  
# ----Set of variables that define an unique row:----

# st_name, assembly_no, dist_name, constituency_no, 
# constituency_name,year, month, pc01_state_id,
# pc01_district_id, pc_uid, pid, cm_party



#  2b. CREATING DIST-YEAR-MONTH LEVEL GRID ----------------------------------------------------


matched_collector_df=dist_collector_df %>% 
  filter(cadre_st_mismatch==TRUE) # Around 18% obs dropped here 

# Following is a common pool of districts both in AE and IAS data
st_dist_df=matched_collector_df %>%
  dplyr::select(st_name,office ) %>% #, year, month,assembly_no, 
                #st_min_year) %>% 
  distinct() 



# Create the grid
dist_month_grid <- create_monthly_grid(st_dist_df)

dist_month_grid=dist_month_grid %>% 
  left_join(ae_win_nobypoll %>% 
              dplyr::select(st_name, dist_name, year, month, 
                            assembly_no,st_min_year, cm_year1,
                            cm_year2) %>% distinct(), 
            by=c("st_name", "office"="dist_name", "year", "month"))#

dd=dist_month_grid %>% 
  group_by(st_name) %>%
  arrange(st_name, office,year, month) %>%
  fill(assembly_no, .direction = "down") %>% 
  fill(st_min_year, .direction = "down") %>%
  fill(cm_year1, cm_year2, .direction = "down") %>%
  ungroup() %>% 
  mutate(year=as.numeric(year)) %>%
  filter(year>st_min_year) %>% 
  filter(year>=cm_year1 ) %>% 
  filter( year<=cm_year2) 




dist_month_grid=dist_month_grid %>% 
  group_by(st_name) %>%
  arrange(st_name, office,year, month) %>%
  fill(assembly_no, .direction = "down") %>% 
  fill(st_min_year, .direction = "down") %>%
  fill(cm_year1, cm_year2, .direction = "down") %>%
  ungroup()

dist_month_grid=dist_month_grid %>% 
  mutate(year=as.numeric(year)) %>%
  filter(year>st_min_year) %>% 
  filter(year>=cm_year1 ) %>% 
  filter( year<=cm_year2) 

dist_month_grid=dist_month_grid %>% 
 dplyr::select(-st_min_year)


  
# Step 2: Transform IAS data to monthly  structure

expand_ias_df=matched_collector_df 
ias_month_matched_dist= transform_ias_data(expand_ias_df) %>% 
  distinct()
  #dplyr::select(-st_name)

# Step 3: Merge the grid with the IAS data
# Combine with the grid 
dist_level_posting <- dist_month_grid %>%
  mutate(year=as.character(year)) %>% 
  left_join(ias_month_matched_dist, 
            by = c("office",
                     "year", "month",
                     "st_name")) %>%
  mutate(active = ifelse(is.na(active), 0, active)) 

dist_level_posting=dist_level_posting %>% 
  mutate(delim_id=if_else(year>=2008, 4,3)) %>% distinct()

dist_level_posting <- dist_level_posting %>% 
  group_by(st_name,office) %>% 
  arrange(year, month) %>%  # Ensure data is ordered by year and month
  mutate(
    first = cumsum(active == 1) > 0  # Cumulative sum ensures all rows after the first `active == 1` are also `TRUE`
  ) %>%   ungroup() %>% 
  filter(first == T) %>%
  rename(dist_name=office) #
# The benefit of filter (first==T)
# is that now whenever active==0, we can 
# perhaps assume that the position is empty (for now, would need to recheck
# later on, not a great assumption in long term)

dist_level_posting=dist_level_posting %>% 
  arrange(st_name, dist_name, year, month) 

qsave(dist_level_posting, 
      file="data/2 CLEAN/2 AE-DISTRICT YEAR GRID WITH IAS POSTING.qs")


# 🔥 BIG CHANGE: NOW CANNOT MERGE AT AC LEVEL 🔥

# Step 4: Merge with AE ELECTION data
# election data is at AC-district-candidate-assembly_no level. 
#.  one assembly_no is 5 years. 
# ae_ias_merged=ae_win_nobypoll %>% 
#   mutate(dist_name=tolower(dist_name)) %>%
#   right_join(dist_level_posting,
#              by=c("year","month","delim_id",
#                   "dist_name"="office", "assembly_no",
#                   "constituency_no","st_name"))
# 
# ae_ias_merged=ae_ias_merged %>% 
#   distinct() %>% 
#   arrange(st_name,constituency_name, constituency_no) %>% 
#   fill((names(ae_win_nobypoll)),
#        .direction = "down") #%>%
#   
# ae_ias_merged=ae_ias_merged %>% group_by(dist_name, st_name, assembly_no) %>% 
#   mutate(dist_ac_count=n_distinct(st_name, constituency_no)) %>% 
#   ungroup() %>% 
#   dplyr::select(st_name, dist_name, assembly_no, 
#                         year, month, constituency_no,id, pid,
#                 everything()) %>% 
#   arrange(st_name, dist_name, 
#           constituency_no, constituency_name, year, month)#
# dd=ae_ias_merged %>% 
#   dplyr::select(st_name, dist_name,  
#                 constituency_no, constituency_name, year, month, id, pid) %>% 
#   distinct() %>% filter(st_name=="bihar")

# AT THIS POINT ae_ias_merged can be messed up unfortunately :/ 

# SUM STAT FOR AC PER DIST
# > summary(dd$dist_ac_count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   4.000   7.000   8.328  10.000  51.000 
# > quantile(dd$dist_ac_count, 0.95, na.rm=T)
# 95% 
# 20 
# > quantile(dd$dist_ac_count, 0.9, na.rm=T)
# 90% 
# 15 

## SOLVED
# constituency_name vs constituency_no
# ass_year_df=ae_win_nobypoll %>% 
#   dplyr::select(st_name, assembly_no, year) %>% distinct()
# 
# 🔥
# con_df=ae_ias_merged %>% 
#   dplyr::select(st_name, assembly_no, 
#                 constituency_name, constituency_no) %>% 
#   distinct() %>% 
#   arrange(st_name, assembly_no, constituency_name, constituency_no) %>% 
#   group_by(st_name, assembly_no, constituency_no) %>% 
#   mutate(two_names=n_distinct(constituency_name)) %>% 
#   group_by(st_name, assembly_no, constituency_name) %>% 
#   mutate(two_no=n_distinct(constituency_no)) %>% 
#   ungroup() %>% filter(two_names>1 | two_no >1) %>% 
#   left_join(ass_year_df) %>% 
#   arrange(st_name, assembly_no, -two_names) 
# 
# two_no_names_df=con_df %>% filter(two_no>1, two_names>1)
# 
# two_no_df=two_no_df %>% left_join(ae_win_nobypoll)
# 
# atmakur_ap_10=ae_ias_merged %>% 
#   mutate(constituency_name=tolower(constituency_name)) %>% 
#   filter(st_name=="andhra_pradesh", 
#          year %in% c(1999,2000)) %>% 
#   dplyr::select(st_name, constituency_name, constituency_no, constituency_type, 
#                 year, month, assembly_no, party, id, 
#                 ias_name, delim_id, delim_id.x, delim_id.y, 
#                 margin_percentage) %>%
#   distinct() %>% 
#   arrange(st_name,constituency_name, constituency_no) %>% 
#   fill(c(constituency_no,constituency_type,
#          assembly_no,party),
#        .direction = "down")
#   
# dd=atmakur_ap_10 %>% 
#   filter(constituency_name=="ichapuram" | constituency_no==4)
  
## SOLVED
  

# 3. AGGREGATING TO GENERATE SOME DISTRICT LEVEL STATS-----------------

# now it is essentially have become an AC level data
# in this section I will aggregate some stuff to district level
# List:
# 1. No. of State party aligned MLAs. 
# 1b: No of MLA aligned with MP party 
#      ( in face of multiple MP, MLA.. it can be % of everyone aligned with state party 
#   or % of everyone belonging to the same party (can be opposition also))
# 2. No of constituencies. 
# 3. No of stable MLAs. (no_terms>2 & same_constituency==T or incumbent==T)
# 4. Total criminal charges
# 5. Total assets
# 6. Total liabilities
# 7. No. of SC/ST ACs (or candidates)
# 8. Average turn out percentage
# 9. Average Margin
# 10. No. of closely contested ACs
#    Strong Winner (margin_percent >15)
# 11. Avg ENOP
# 12. For each prof category: Count of people in that category


ae_data=ae_win_nobypoll %>% 
  filter(!is.na(dist_name))#

# dd=ae_data %>%
#   group_by(st_name, dist_name, constituency_no, 
#            constituency_name, year, month) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   filter(count > 1) %>%
#   arrange(desc(count))
# 
# tt=ae_data %>% filter(constituency_no==37, 
#                       constituency_name=="kottur", 
#                       year==2004)
# 
# dup_col=tt %>%
#   summarise(across(everything(), n_distinct)) %>%
#   pivot_longer(cols = everything(), names_to = "variable", values_to = "unique_values") %>%
#   filter(unique_values > 1)


# 1. No. of State party aligned MLAs.


dist_party_list=ae_data %>%
  group_by(dist_name, st_name, year, month, assembly_no) %>%
  summarise(
    party_list = str_c((party), collapse = ", "),  # Collect unique parties into a string
    ae_pid_list=str_c(pid, collapse=", "),
    .groups = "drop"
  )

# 2. No of constituencies. 
# this is dist_ac_count already in the data

# 3. No of stable MLAs. (no_terms>2 & same_constituency==T or incumbent==T)



dist_stable_mla=ae_data %>% 
  mutate(stable_mla0=if_else(same_constituency==T & incumbent==T,1,0),
         stable_mla1=if_else((no_terms>2),1, 0),
         stable_mla2=if_else((no_terms>2 & incumbent==T ), 1, 0),
         stable_mla3=if_else((no_terms>2 & same_constituency==T ), 1, 0),
         stable_mla4=if_else((no_terms>2 & same_constituency==T & incumbent==T),
                             1, 0)) %>%
  dplyr::select(dist_name, st_name, year, month, 
                assembly_no, constituency_no, constituency_name,
                stable_mla0,stable_mla1, stable_mla2, stable_mla3, 
                stable_mla4) %>% distinct() %>%
  group_by(dist_name, st_name, year, month,assembly_no) %>%
  summarise(stb_mla0_count=sum(stable_mla0, na.rm=T),
            stb_mla1_count=sum(stable_mla1, na.rm=T),
            stb_mla2_count=sum(stable_mla2, na.rm=T),
            stb_mla3_count=sum(stable_mla3, na.rm=T),
            stb_mla4_count=sum(stable_mla4, na.rm=T)) %>%
  ungroup() 
  
# 4,5,6. Total criminal charges and asset info
# not in TCPD data
# can just merge in: dist_winner_X 


# 7. No. of SC/ST ACs


dist_res_ac_count=ae_data %>% 
  dplyr::select(dist_name, st_name, year, month, 
                assembly_no, constituency_no, constituency_name,
                constituency_type) %>% distinct() %>% 
  group_by(dist_name, st_name, year, month,assembly_no) %>% 
  summarise(res_ac_count=sum(str_detect(constituency_type,"SC|ST"), 
                             na.rm=T),
    ac_count = n_distinct(paste0(constituency_no, "_", 
                                 constituency_name))) %>% 
  ungroup()

dd=ae_data %>% 
  dplyr::select(dist_name, st_name, year, month, 
                assembly_no, constituency_no, constituency_name,
                constituency_type) %>% distinct()


# 8. Average turn out percentage


dist_turnout=ae_data %>% 
  dplyr::select(dist_name, st_name, year, month, 
                assembly_no, constituency_no, constituency_name,
                constituency_type,turnout_percentage) %>% 
  distinct() %>% 
  group_by(dist_name, st_name, year, month,assembly_no) %>% 
  summarise(turnout_percent=mean(turnout_percentage,na.rm=T)) %>% 
  ungroup()


# 9 Average Margin


dist_margin=ae_data %>% 
  dplyr::select(dist_name, st_name, year, month, 
                assembly_no, constituency_no, constituency_name,
                constituency_type,margin_percentage) %>% 
  distinct() %>% 
  group_by(dist_name, st_name, year, month,assembly_no) %>% 
  summarise(margin_percent=mean(margin_percentage,na.rm=T)) %>% 
  ungroup()

# 10 No. of closely contested AC

dist_close=ae_data %>% 
  dplyr::select(dist_name, st_name, year, month, 
                assembly_no, constituency_no, constituency_name,
                constituency_type,margin_percentage) %>% 
  distinct() %>% 
  group_by(dist_name, st_name, year, month,assembly_no) %>% 
  summarise(closely_contested_ac=sum(margin_percentage<5,na.rm=T)) %>% 
  ungroup()


# 11 Strong Winner (margin_percent >15)

dist_strong_winner=ae_data %>% 
  dplyr::select(dist_name, st_name, year, month, 
                assembly_no, constituency_no, constituency_name,
                constituency_type,margin_percentage) %>%
  distinct() %>%
  group_by(dist_name, st_name, year, month,assembly_no) %>% 
  summarise(strong_winner_ac=sum(margin_percentage>=15,na.rm=T)) %>% 
  ungroup()


# 12 Avg ENOP

dist_enop=ae_data %>% 
  dplyr::select(dist_name, st_name, year, month, 
                assembly_no, constituency_no, constituency_name,
                constituency_type,enop) %>%
  distinct() %>%
  group_by(dist_name, st_name, year, month,assembly_no) %>% 
  summarise(enop=mean(enop,na.rm=T)) %>% 
  ungroup()

# 13. For each prof category: Count of people in that category

dist_prof_count=ae_data %>% 
  dplyr::select(dist_name, st_name, year, month, 
                assembly_no, constituency_no, constituency_name,
                constituency_type, bizz, politics, 
                ex_gov, agri, other) %>%
  distinct() %>%
  group_by(dist_name, st_name, year, month,assembly_no) %>% 
  summarise(ae_bizz_count=sum(bizz,na.rm=T),
            ae_politics_count=sum(politics,na.rm=T),
            ae_ex_gov_count=sum(ex_gov,na.rm=T), 
            ae_agri_count=sum(agri, na.rm=T), 
            ae_other_count=sum(other, na.rm=T)) %>% 
  ungroup()


## Combining all the stats

dist_ae_data=dist_stable_mla %>% 
  inner_join(dist_res_ac_count) %>% 
  inner_join(dist_turnout) %>% 
  inner_join(dist_margin) %>%
  inner_join(dist_close) %>% 
  inner_join(dist_strong_winner) %>% 
  inner_join(dist_enop) %>% 
  inner_join(dist_prof_count) %>% 
  inner_join(dist_party_list) %>%
  left_join(dist_winner_X, 
            by=c("dist_name", "st_name", "year")) %>%
  arrange(st_name, dist_name) %>% 
  rename(ae_year=year, 
         ae_month=month)#



# MERGING WITH DIST LEVEL YEARLY GRID

dist_year_panel=dist_level_posting %>% 
  left_join(dist_ae_data ,
            by=c("dist_name", "st_name", "assembly_no")) 
  
ae_ias_merged=dist_year_panel
date="2025-02-10"
qsave(ae_ias_merged, paste0("data/3 MERGED SAMPLES/ae_ias_merged_",date,".qs"))


dd=dist_year_panel %>% 
  dplyr::select(st_name, dist_name, year, month, assembly_no,
                id, ias_name,
                stb_mla1_count, stb_mla2_count, stb_mla3_count, 
                stb_mla4_count, res_ac_count, turnout_percent, 
                margin_percent, closely_contested_ac, 
                strong_winner_ac, enop, bizz_count, politics_count, 
                ex_gov_count, agri, other, winner_net_assets, 
                winner_assets, winner_liabilities, winner_any_crim, 
                winner_num_crim) %>%
  arrange(st_name, dist_name, year, month) #

total_unit=ae_ias_merged %>% 
  count_fun(year, month, st_name, dist_name)#
total_pol_unit=ae_ias_merged %>% 
  filter(!is.na(pid)) %>% 
  count_fun(year, month, st_name, dist_name)#
total_ias_unit=ae_ias_merged %>% 
  filter(active==1) %>% 
  count_fun(year, month, st_name, dist_name)#
# > total_pol_unit/total_unit
# [1] 0.9410572
# > total_ias_unit/total_pol_unit
# [1] 0.6128989
# > total_ias_unit/total_unit
# [1] 0.5767729




# state_election_list=ae_win %>% 
#   filter(poll_no==0) %>% 
#   filter(year>=2000) %>% 
#   dplyr::select(state_name, year) %>% 
#   distinct() %>%
#   arrange(state_name, year) 
# 
# win_count_distr= ae_win %>% 
#   filter(poll_no==0) %>% 
#   filter(position == 1) %>% 
#   filter(year>=2002) %>% 
#   group_by(state_name,year, win_counter) %>% 
#   summarise(pid_count = n_distinct(pid), 
#             .groups = "drop") %>%  # Count pids at each win_counter level
#   group_by(state_name,year) %>%
#   mutate(
#     total_pids = sum(pid_count),                  # Total pids for this assembly_no
#     percent_pids = (pid_count / total_pids) * 100 # Compute percentage
#   ) %>%
#   ungroup() #%>% filter(win_counter>1) 
# 
# pid_on_3rd_win=win_count_distr %>% 
#   filter(win_counter>2) %>%
#   group_by(state_name,year) %>%
#   summarise_at(c("pid_count","percent_pids"), sum) %>% 
#   ungroup()#
# 
# pid_on_3rd_win <- pid_on_3rd_win %>%
#   group_by(state_name) %>%                # Group by state_name
#   arrange(year, .by_group = TRUE) %>%     # Arrange by year within each group
#   mutate(
#     percent_pids_diff = percent_pids - dplyr::lag(percent_pids)  # Calculate first difference
#   ) %>%
#   ungroup() 
# 
# # DOESNT SEEM TO HAVE ANY EFFECT AROUND 2008 DELIMITATION
# # QUITE UNLIKE PC ELECTIONS
# 
# library(plotly)
# 
# p <- ggplot(pid_on_3rd_win, 
#             aes(x = year,
#                 y = percent_pids, 
#                 color = state_name)) +
#   geom_line(size = 1) +
#   labs(
#     title = "Trend of Percentage PIDs Across States Over Years",
#     x = "Year",
#     y = "Percentage of PIDs",
#     color = "State"
#   ) +
#   theme_minimal()
# 
# # Convert to interactive plot
# plotly::ggplotly(p)



#
# 🔥 BIG CHANGE: NOW CANNOT MERGE AT AC LEVEL 🔥
# ---------------------------------------------
# Step 4: Merge with AE ELECTION data
# ---------------------------------------------


