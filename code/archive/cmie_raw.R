rm(list=ls())
source("code/functions.R")



cmie_root="../Data/CMIE"
cmie_extr="full_20230623"
cmie_sec="identity"


fpath=file.path(cmie_root, cmie_extr, cmie_sec)
flist=list.files(fpath, 
                 pattern = "_dat.txt$", 
                 full.names = T)



raw_dt=read_delim(flist, 
                delim = "|", 
                escape_double = FALSE, 
                trim_ws = TRUE)





# TASK 1: Plotting the district level trajectory of raw count
#   Can create time points: 2000, 2005, 10, 15, 20 and just count the number of companies existing at that time

dist_time_count=raw_dt %>% 
            mutate(year_lvl=if_else(incorporation_year<=2000, 1, 
                                    if_else(incorporation_year<=2005,2, 
                                            if_else(incorporation_year<=2010, 3, 
                                                    if_else(incorporation_year<=2015,4,5))))) %>% 
  group_by(regddname, year_lvl) %>% 
  mutate(firm_count=n_distinct(co_code)) %>% 
  ungroup()


# filtering out the districts that has less than 100 firms in total from 2-5


dt_count_sub=dist_time_count %>% dplyr::filter(year_lvl>1 & !is.na(year_lvl)) %>% 
  dplyr::select(regddname, year_lvl, firm_count) %>% 
  distinct() %>% 
  group_by(regddname) %>% 
  mutate(tot_count=base::sum(firm_count)) %>% 
  dplyr::filter(tot_count>=50) %>% 
  ungroup() %>% 
  arrange(regddname, year_lvl) %>% 
  group_by(regddname) %>% 
  mutate(firm_growth=mean(firm_count)) %>% 
  ungroup()


                                                        
