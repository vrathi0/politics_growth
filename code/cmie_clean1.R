
## This is jsut an exploratory code file to poke around 
# CMIE data


rm(list=ls())
source("code/functions.R")


dat_flist=list.files(path="../Data/CMIE/full_20230623",
                pattern = "dat.txt$",  full.names = T)

dt_list=lapply(dat_flist,
               read_delim, delim="|",
               escape_double=F, trim_ws=TRUE)

map_flist=list.files(path="../Data/CMIE/full_20230623",
                     pattern = "map.txt$",  full.names = T)

map_list=lapply(map_flist, 
               read_delim, delim="|",
               escape_double=F, trim_ws=TRUE,col_names = FALSE)

map_df=map_list %>% bind_rows()
names(map_df)=c("var_name","var_lab","var_type")


# Saving the full data dump for each access
qsave(dt_list , file="data/cmie_full_dump.qs")

# Reading it back
cmie_dt=qread("data/cmie_full_dump.qs")


# Variables of interest
var_fortable=c("co_industry_gp_code","state_code","totener_cocode")


filtered_dat <- lapply(dt_list, function(df) {
  if (any(colnames(df) %in% var_fortable)) {
    df
  } else {
    NULL
  }
})

filtered_dat <- filtered_dat[!sapply(filtered_dat, is.null)]
filtered_dat[[1]]=filtered_dat[[1]] %>% 
  rename(co_code=totener_cocode)



dt = Reduce(function(x, y) inner_join(x, y, by="co_code"), filtered_dat)

# Constructing measure of demand variation at sector level


dt= dt %>% mutate(year=substr(totener_date, start = 1, stop = 4), 
                  month=substr(totener_date, start = 5, stop = 6),
                  date=substr(totener_date, start = 7, stop = 8)) %>% 
            mutate(fine_industry=paste(product_name_mst.y, state_code, sep="-"))


dt=dt %>% 
  mutate(energy_exp=energy_cons_qty*energy_cons_rate_per_unit)

dt = dt %>% group_by(co_code, year) %>% 
  mutate(yearly_energy_cost= sum(energy_exp, na.rm=T)) 


# Industry level mean energy expenditure

industry_demand= dt %>% group_by(fine_industry, year) %>% 
    mutate(yearly_industry_energy_exp=mean(yearly_energy_cost)) %>% 
      dplyr::select(fine_industry, year, yearly_industry_energy_exp ) %>% 
      distinct()

# Getting measures of demand shocks by residuals from trend line

fml=as.formula(paste0("yearly_industry_energy_exp~as.numeric(year)"))

dat_indus_list=split(industry_demand, industry_demand$fine_industry)
nrow_dist=unlist(lapply(dat_indus_list, nrow))


# Dropping all outliers
for(i in 1:length(dat_indus_list)){
  
  df=dat_indus_list[[i]]
  df$yearly_industry_energy_exp=clipp(df$yearly_industry_energy_exp, 0.05,0.95)
  df=df %>% filter(!is.na(yearly_industry_energy_exp))
  dat_indus_list[[i]]=df
  
}


# dropping all fine_industry that has less than 5 years of data
dat_indus_list <- keep(dat_indus_list, ~ nrow(.x) >= 5)

# drop


regF=function(df){
  
  fml=as.formula(paste0("yearly_industry_energy_exp~as.numeric(year)"))
  
  reg= summary(lfe::felm(fml, df))
  
  return(reg$r.squared)
  
}
  

r2_list=lapply(dat_indus_list, regF)

fine_ind_name=names(r2_list)
r2_vals=unlist(r2_list)

r2_df=data.frame(cbind(fine_ind_name, r2_vals)) %>% 
  mutate(r2_vals=as.numeric(r2_vals)) %>% 
      arrange(r2_vals)


# At this point, r2 values are arranged at industry+state, can break them up

r2_df=r2_df %>%
  mutate(industry = str_extract(fine_ind_name, "^[^\\d]+"),
         state = str_extract(fine_ind_name, "\\d+$")) %>% 
    mutate(industry=substr(industry, 1, nchar(industry) - 1))



# Arbitrarily picking r2_vals between 0.01 and 0.4

r2_df_subset=r2_df %>%  filter(r2_vals>0.01 & r2_vals<0.4)



# Now idea is that to pull firms from thse industries and states


firm_extract=list()
var_extr=c("co_code",
           "company_name",
           "short_name",
           "state_code",
           "co_industry_type",
           "co_product_gp_code",
           "product_name_mst",
           "co_industry_gp_code",
           "co_industry_name",
           "incorporation_year",
           "regsaddr",
           "regsemail",
           "regdcity",
           "regdtele",
           "hoaddr",
           "hocity",
           "hostate")

dd=filtered_dat[[2]] %>%  dplyr::select(all_of(var_extr))


for (i in 1:nrow(r2_df_subset)){
  
  line=r2_df_subset[i,]
  firm_extract[[i]]=dd %>% 
    filter(product_name_mst==line$industry & state_code==line$state) %>% 
    mutate(r2=line$r2_vals) %>% 
    mutate(fine_industry=paste(product_name_mst, state_code, sep="-"))
  
  
  if(nrow(firm_extract[[i]])>0){
     names(firm_extract)[i]=unique(firm_extract[[i]]$fine_industry)
  }
  
}



## Saving the firm_extract that potentially contains 
##    firms that we can reach out to. 

## In other sense, this can also be a POC of the baseline sample frame

write_rds(firm_extract, 
          file="data/firms_volatile_demand.rds", 
          compress = "gz")





## Looking at firms in Agra

ff=firm_extract %>%  bind_rows()

ff1= dd %>%  filter(str_detect(regdcity , "Agra")) %>% 
  dplyr::select(company_name, short_name,product_name_mst,
                regsaddr, regdtele)

write.xlsx(ff1, file="data/agra_firms.xlsx")






