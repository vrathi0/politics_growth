rm(list=ls())
source("code/functions.R")







# 1. Cross sectional (state) differences in churn frequency of officers -----------

ias_exp=qread("data/tcpd_ias_exp.qs")

ball_last=ias_exp %>% #filter(batch_yr %in% c(1997, 1998, 1999)) %>% 
  filter(retired=="0") %>% 
  group_by(id) %>% mutate(last_post=max(lvl_no)) %>% ungroup() %>% 
  dplyr::select(id, last_post, lvl_no,max_lvl, everything()) %>% distinct() %>% 
  arrange(id) %>% 
  filter(last_post==lvl_no) %>% 
  arrange(cadre, id) %>% distinct() %>% 
  group_by(cadre,batch_yr ) %>% mutate(cadre_size=n()) %>% 
  ungroup() %>% 
  dplyr::select(last_post, cadre, cadre_size, batch_yr) %>% 
  distinct()
#mutate(last_post=if_else(cadre=="A G M U T", 18, last_post))

# Adding base category
new_row <- data.frame(
  last_post = 10,
  cadre = "0ABASE",
  cadre_size = 5,
  batch_yr = 0
)

ball_last=ball_last %>% bind_rows(new_row)

# Trying to see if there are any meaningfull cross state differences 
# in posting counter across states and within batch

fml=as.formula(paste0("last_post~ cadre + cadre_size| batch_yr"))
reg=felm(fml, ball_last);summary(reg)



# 2. PLAYING WITH LGD DATA -----------

lgd_path="../Data/LGD/26Oct2024/"

state_org=read_csv(paste0(lgd_path, "state_orgs.csv")) %>% 
  clean_names()%>% group_by(state_name) %>% 
  mutate(state_org_count=n()) %>% ungroup() 

state_org_units=read_csv(paste0(lgd_path, "state_org_units.csv")) %>% 
  clean_names() %>%  group_by(state_name) %>% 
  mutate(state_porg_unit_count=n_distinct(parent_org_unit_code)) %>% ungroup() 
rj_org_unit=state_org_units %>% filter(state_name=="Rajasthan")

state_admin=read_csv(paste0(lgd_path, "state_admin_depts.csv")) %>% 
  clean_names() %>% group_by(state_name) %>% 
  mutate(state_admin_count=n()) %>% ungroup() 

state_admin_units=read_csv(paste0(lgd_path, "state_admin_dept_units.csv")) %>%   
  clean_names() %>% group_by(state_name) %>% 
  mutate(state_admin_dept_count=n_distinct(admin_department_code)) %>% 
  ungroup()
rj_admin_unit=state_admin_units %>% filter(state_name=="Rajasthan")
up_admin_unit=state_admin_units %>% filter(state_name=="Uttar Pradesh")
hr_admin_unit=state_admin_units %>% filter(state_name=="Haryana")


state_org_desig=read_csv(paste0(lgd_path, "state_org_designations.csv")) %>% 
  clean_names() %>% group_by(state_name) %>% 
  mutate(state_org_desig_count=n_distinct(designation_code)) %>% 
  ungroup()
rj_org_desig=state_org_desig %>% filter(state_name=="Rajasthan")



changes_df=read_csv(paste0(lgd_path, "changes.csv")) %>% 
  clean_names() #%>% group_by(state_name) %>% 
 # mutate(state_change_count=n()) %>% ungroup()


new_dist_txt=c(
"District has contributed partly in creation of new district.",
"New District is Created By Splitting a District.",
"New District is Created by Contribution from Two or More Districts.",
"District is shifted to newly created State."
               )

new_dist_chg_df=changes_df %>% filter(operation %in% new_dist_txt)