rm(list=ls())
source("code/functions.R")




# Merging all EC across years ---------------------------------------------

# Notes: 
# Algo right now ( May 20)
# Just summing the total number of Econ Est for each geog unit ( state, district, eb)
# We get one number per year, and then after merging we get three numbers in total

# Lets first try merging with SHRUG. 

# Reading in 2013 EC

ec2013=read_rds("../Data/Economics Census/clean/ec2013_ran.rds")


ec2013 = ec2013 %>% mutate(total_hired=as.numeric(m_h)+ as.numeric(f_h))


# Sample selection:
## Picking obs with greater than 9 hired people ( 99p)
## and choosing select broad activity

filter_bact=c("06","09","10","11","12","13","14","16")

ec2013_flt=ec2013 %>% filter(total_hired>=9 & (bact %in% filter_bact))

# Getting no of est per unnit

ec2013_count =ec2013_flt %>% ungroup() %>%  group_by(st, dt, eb) %>% 
            summarise(total_est=n())

d= ec2013_count %>% mutate(key=paste0(st, dt)) %>% pull(key)
d=unique(d)


# 2005

ec2005 <- read_rds("../Data/Economics Census/clean/ec2005_ran.rds")

# Creating new state and district code based on sector code (1: rural; 2: urban)

ec2005= ec2005 %>% 
  mutate(stcode=ifelse(sector_code==1, v_stateut_code, c_stateut_code)) %>% 
  mutate(distcode=ifelse(sector_code==1, v_dist_code, c_dist_code)) 


# Subsetting it using the same criteria ( broad activity & no. of hired people)

ec2005_flt=ec2005 %>% filter(classf_code==2) %>% 
  mutate(total_hired=as.numeric(amale_no)+as.numeric(afemale_no)
         + as.numeric(cmale_no) + as.numeric(cfemale_no) - as.numeric(extra_no)) %>% 
  filter(total_hired>=9)


ec2005_count=ec2005_flt %>% group_by(stcode, distcode) %>% 
              summarise(total_est=n())



## 1998

ec1998=  read_rds("../Data/Economics Census/clean/ec1998_ran.rds")


ec1998= ec1998 %>% rename(stcode=state_code, distcode=dist_code)


# Subsetting it using the same criteria ( non-agri & no. of hired people)


ec1998_flt=ec1998 %>% filter(classf_code==2) %>% 
            filter(total_hired>=9)


ec1998_count=ec1998_flt %>% group_by(stcode, distcode) %>% 
  summarise(total_est=n())



# rad SHP file

distshape=st_read("../Data/SHRUG/geometries_shrug-v1.5.samosa-open-polygons-shp/district.shp")
