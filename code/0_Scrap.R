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