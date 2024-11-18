rm(list=ls())
source("code/functions.R")


# This is to merge in ZIP-AC mapping into MCA data 



# IMPORTING ALL DATA ------------------------------------------------------

mca_raw=read_rds("data/clean/MCA_registered_company_total.rds") %>% 
  mutate(state_name=tolower(state_name)) %>% 
  dplyr::select(-state)

zip_ac_map4=read_rds("data/interm/zip_ac_map4.rds") %>% 
  mutate(st_code_ac=padzero(st_code_ac, 2), 
         ac_no=padzero(ac_no, 3))
zip_ac_map4$state[zip_ac_map4$state=="nct of delhi"]="delhi"
zip_ac_map4$state=NULL

mca_ac=mca_raw %>% inner_join(zip_ac_map4) # (old)DELHI IS MISSING IN ZIP_AC_MAP ( perhaps some other merge flaws also in ZIP_AC_MERGE )
#Joining, by = c("zip")

# > length(unique(mca_ac$zip))/length(unique(mca_raw$zip))
#[1] 0.7169272

# Around 4428 zips are present in mca_raw but not in zip_ac_map4. So they wont be matched. 


# SAVING
qsave(mca_ac, "data/clean/MCA_AC_MAP4_REG_COMP.qs", nthreads = 1)


# GENERATING YEARLY FLOW AT VARIOUS LEVELS: ZIP, AC, DISTRICT, ROC
mca_ac=qread("data/clean/MCA_AC_MAP4_REG_COMP.qs")
setDT(mca_ac)
tic()
tt=mca_ac[, c("zip_year_act_count", "zip_year_act_authcap", "zip_year_act_paidcap") :=
         .(n_distinct(cin),
           mean(as.numeric(authorized_capital), na.rm = TRUE),
           mean(as.numeric(paidup_capital), na.rm = TRUE)),
       by = .(year, zip, activity_description)] %>%
  .[, c("ac_year_act_count", "ac_year_act_authcap", "ac_year_act_paidcap") :=
      .(n_distinct(cin),
        mean(as.numeric(authorized_capital), na.rm = TRUE),
        mean(as.numeric(paidup_capital), na.rm = TRUE)),
    by = .(st_code_ac, ac_no,ac_name, year, activity_description)] %>% 
  .[, c("dist_year_act_count", "dist_year_act_authcap", "dist_year_act_paidcap") :=
      .(n_distinct(cin),
        mean(as.numeric(authorized_capital), na.rm = TRUE),
        mean(as.numeric(paidup_capital), na.rm = TRUE)),
    by = .(state_name, district, year,activity_description)] %>% 
  .[, c("roc_year_act_count", "roc_year_act_authcap", "roc_year_act_paidcap") :=
      .(n_distinct(cin),
        mean(as.numeric(authorized_capital), na.rm = TRUE),
        mean(as.numeric(paidup_capital), na.rm = TRUE)),
    by = .(roc, year,activity_description)] 

mca_ac_year_act=tt %>% dplyr::select(state_name, zip, year, dist_code_ac, dist_name_ac, 
                                 st_code_ac, st_name_ac, district,roc,
                        activity_description,
                        ac_name, ac_no, ac_at_dist_bnd, 
                        ac_at_st_bnd, zip_at_dist_bnd, 
                        dist_at_st_bnd, 
                        starts_with("zip_year_"), starts_with("ac_year_"),
                        starts_with("dist_year_"),
                        starts_with("roc_year_")) %>% distinct()
rm(tt)
rm(mca_ac)
gc()
toc()



qsave(mca_ac_year_act, "data/clean/MCA_ACYEAR_REG_COMP.qs", nthreads = 1)

  

