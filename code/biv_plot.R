source("code/functions.R")


# Scatterplot script:
## Bivariate plot between incumbant wealth increase and trend in # of firms


# Collating ASI data

# This file is just to merge A module of the ASI data

# Listing files:

flist=list.files(path="../Data/ASI", 
                 pattern = "^(A|BlockA|BLOCK-A|blka|blkA).*\\.dta$", recursive=T, 
                 full.names = T)


# Reading the files

dt=lapply(flist, read_dta)


# Selecting states and district variables

# Pattern for variable selection
pattern <- "State|District|Year|Inflation|Multiplier|Serial|Dispatch|2020"


getL=function(x){
  l=as.character(lapply(as.list(x), attr, "label"))
  
  return(l)
  
}

getX=function(x){
  l=as.character(lapply(as.list(x), attr, "label"))
  
  d=x[,grep(pattern, l)]
  names(d)=str_extract(l[grep(pattern, l)], "\\w+")
  d=d %>% clean_names() %>% 
    mutate_all(as.character)
  return(d)
  
}


dt_sub=lapply(dt, getX) %>% bind_rows() 

dt_sub$year[dt_sub$year=="2006"]="06"
dt_sub$year[dt_sub$year=="2007"]="07"
dt_sub$year[dt_sub$year=="2008"]="08"
dt_sub$year[dt_sub$year=="2011"]="11"
dt_sub$year[dt_sub$year=="2012"]="12"
dt_sub$year[dt_sub$year=="2013"]="13"
dt_sub$year[dt_sub$year=="2014"]="14"

dt_sub=dt_sub %>% mutate(state=padzero(state, 2), 
                         district=padzero(district,2)) %>% 
  mutate(year=coalesce(year, x20)) %>% 
  mutate(state_dist=paste0(state, district)) %>% 
  mutate(year_n=as.numeric(year)+1) %>% 
  mutate(wt = coalesce(inflation, multiplier),
         dsl=coalesce(dispatch, despatch)) %>% 
  filter(!is.na(year))





ac_shp=st_read("raw_data/maps-master/assembly-constituencies/India_AC.shp")

dist_ac=ac_shp %>% as.data.frame() %>% 
  dplyr::select(ST_CODE, ST_NAME, DT_CODE, DIST_NAME, AC_NAME) %>% distinct() %>% 
  rename(sname=ST_NAME, dname=DIST_NAME, constituency=AC_NAME, state=ST_CODE, district=DT_CODE) %>% 
  mutate_all(tolower) %>% #filter(grepl("bihar|madhya pradesh|rajasthan|uttar pradesh", sname)) %>% 
  mutate(district=padzero(district,2), state=padzero(state, 2))





dt_count= dt_sub %>% group_by(year,year_n, state, district, state_dist) %>% 
  summarise(nobs=sum(as.numeric(wt), na.rm=T)) %>% group_by(state, year) %>% 
  mutate(state_year_nobs=mean(nobs, na.rm=T)) %>% 
  group_by(state) %>% 
  mutate(state_cv=sd(state_year_nobs, na.rm = T)/mean(state_year_nobs, na.rm=T)) %>% 
  group_by(state_dist) %>% 
  mutate(state_dist_cv=sd(nobs, na.rm = T)/mean(nobs, na.rm=T)) %>% 
  mutate(state_dist_cv_dm=state_dist_cv-state_cv) %>% 
  group_by(state, year) %>% 
  mutate(state_year_cv=sd(nobs, na.rm=T)/mean(nobs, na.rm=T))




# removing those state_dist with less than 10 years of data
dt_count= dt_count %>% group_by(state_dist) %>% mutate(nn=n_distinct(year)) %>% 
          filter(nn>=6) %>% dplyr::select(-nn) %>% filter((district!="99"))

# Now to compute the 5 or 4 year trend line ( rolling)

library(rollRegres)

# Assuming you have a data frame named 'df' with columns 'state', 'year', and 'variable'
# 'state' represents the state names, 'year' represents the year, and 'variable' represents the variable values

# Create a new column 'trend_coefficient' in the original dataset
dt_count$tc <- NA

# Get the unique states from the data frame
sd <- unique(dt_count$state_dist)

# Loop over each state
for (s in sd) {
  # Subset the data for the current state
  print(s)
  dt <- dt_count[dt_count$state_dist == s, ]
  
  if(nrow(dt)>4 & (sd(dt$nobs)!=0)){
  # Sort the data by year in ascending order
    dt <- dt[order(dt$year_n), ]
    
    # Compute the rolling regression using a window size of 4
    roll_lm_result <- rollRegres::roll_regres(nobs ~ year_n, data = dt, width = 5,
                                              do_downdates = TRUE)
    
    # Extract the slope coefficients (trend coefficients) from the rolling regression
    trend_coefficients <- roll_lm_result$coefs[, 2]
    
    # Append the trend coefficients to the original dataset for the corresponding state
    dt$tc=c(trend_coefficients)
    dt= dt %>% mutate(tc=Lag(tc, -4))
    
    dt_count[dt_count$state_dist == s, "tc"] <- Lag(c(trend_coefficients),-4)
    
    nobs_cv=sd(dt$nobs,na.rm=T)/mean(dt$nobs, na.rm=T)
    
    dt_count[dt_count$state_dist == s, "nobs_cv"]=nobs_cv
    
  }
}


#dt_count=dt_count %>% inner_join(dist_ac) %>% mutate(constituency=str_trim(constituency))
#Joining with `by = join_by(state, district)`









# Myneta 


can_df2=read_dta("../Data/SHRUG/dta_shrug-v1.5.samosa-assembly-dta/shrug-v1.5.samosa-assembly-dta/assembly_candidates_clean.dta")

can_df3=read_dta("../Data/SHRUG/dta_shrug-v1.5.samosa-assembly-dta/shrug-v1.5.samosa-assembly-dta/assembly_elections_clean.dta")



can_df1=read_dta("../Data/SHRUG/dta_shrug-v1.5.samosa-affidavits-dta/shrug-v1.5.samosa-affidavits-dta/affidavits_clean.dta")
# can_df= can_df1 %>% rename(state=pc01_state_id, constituency=adr_con_name, 
#                             dname=adr_district_name) %>% 
#               filter(winner==1) %>% mutate(year=as.character(year)) %>% 
#               mutate(year=substr(year, 3,4))
# 
# can_df =can_df %>%  inner_join(dist_ac)
# can_df=can_df %>% group_by(state,district, year) %>% 
#   mutate(ave_asset=mean(assets, na.rm=T)) %>% 
#   mutate(cv_asset=sd(assets, na.rm=T)/mean(assets, na.rm=T)) %>% 
#   distinct(state, district, year,ave_asset,cv_asset )
  



can_df11=can_df1 %>% group_by(adr_cand_name, adr_con_name) %>% 
  mutate(rpt=n_distinct(year)) %>% 
     arrange(adr_cand_name,adr_con_name, year) %>% 
     mutate(asset_inc=-(assets-Lag(assets,-1))) %>% 
  rename(state=pc01_state_id, constituency=adr_con_name, 
              dname=adr_district_name) %>% group_by(state, dname, year) %>% 
    mutate(asset_inc=mean(asset_inc, na.rm=T)) %>% 
  mutate(year=as.character(year)) %>% 
  mutate(year=substr(year, 3,4)) %>% ungroup() 
  
can_df12 =can_df11 %>%  inner_join(dist_ac)
#Joining with `by = join_by(state, dname, constituency)`


asi_shr=can_df12 %>% inner_join(dt_count)
#Joining with `by = join_by(year, state, district)`


asi_district=asi_shr %>% distinct(district, state, year, tc, nobs_cv, asset_inc) %>% 
    distinct()
asi_district$tc_clip=clipp(asi_district$tc, 0.05,0.95)
high_extr=quantile(asi_district$asset_inc, 0.5, na.rm=T)
asi_district$high_dum=ifelse(asi_district$asset_inc>high_extr,1,0)

#asi_shr=asi_shr %>% mutate(win_ass=)#=cut(num_crim, breaks=c(0,3,1000)))

fml=as.formula(paste0("nobs_cv~ high_dum"))

reg=felm(fml, asi_district); summary(reg)

ggplot(asi_district, aes(log(asset_inc), tc_clip, color = factor(high_dum))) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE) +
  labs(color = "Dummy") +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  ylim(0,80)+
  xlab("log asset increase")+ ylab("rate of firm entry")


pp=asi_shr %>% dplyr::select(tc, asset_inc)
pp=pp[complete.cases(pp),]
plot(pp$asset_inc, pp$tc)

ggplot(asi_shr, aes(x = -asset_inc, y = tc)) +
  geom_point() + ylim(c(-20,50))+ xlim(c(-4.490e+06 ,1.209e+07))





win_df=read_rds("data/winner_4states.rds") %>% 
  mutate(candidate=tolower(candidate), constituency=tolower(constituency)) %>%
  mutate(candidate=str_trim(candidate), constituency=str_trim(constituency)) %>% 
  mutate(year=substr(year, 3,4))%>% 
  mutate(asset_value=as.numeric(asset_value)) %>% rename(sname=state)

win_df$state=""
win_df$state[win_df$sname=="UP"]="09"
win_df$state[win_df$sname=="RJ"]="08"
win_df$state[win_df$sname=="BH"]="10"
win_df$state[win_df$sname=="MP"]="23"
win_df$sname=NULL

win_df=win_df %>%  inner_join(dist_ac) %>% 
        group_by(state, district, year) %>% 
        mutate(ave_asset=mean(asset_value, na.rm=T)) %>% 
  mutate(cv_asset=sd(asset_value, na.rm=T)/mean(asset_value, na.rm=T)) %>% 
  distinct(state, district, year,ave_asset,cv_asset )

#Joining with `by = join_by(constituency, state)`



incum_df=read_rds("data/incum_4states.rds") %>% 
  mutate(candidate=gsub("\\(.*\\)", "", names_party)) %>% 
  mutate(party=str_extract(names_party, "\\((.*?)\\)")) %>% 
  mutate(party=gsub("\\(|\\)", "", party)) %>% 
  mutate(candidate=tolower(candidate)) %>% 
  dplyr::select(candidate, party, year, asset_increase, pct_increase) %>% 
  distinct() %>% 
  mutate(candidate=str_trim(candidate))

can_df=read_rds("data/can_4states.rds") %>% 
  mutate(candidate=tolower(candidate)) %>% 
  dplyr::select(candidate, party, constituency,year) %>% distinct() 

incum_win_merge=incum_df %>% inner_join(win_df) %>% mutate(year=substr(year, 3,4)) %>% 
      mutate(constituency=tolower(constituency)) %>% 
  mutate(constituency=str_trim(constituency)) %>% dplyr::select(-state)


incum_can_merge=incum_df %>% inner_join(can_df) %>% group_by(constituency, year) %>% 
  mutate(nn=n()) %>% filter(nn==1) %>% mutate(year=substr(year, 3,4)) %>% 
    mutate(constituency=tolower(constituency)) %>% 
  mutate(constituency=str_trim(constituency))


incum_can_merge$year[incum_can_merge$year=="12"]="10"
incum_can_merge$year[incum_can_merge$year=="13"]="10"

incum_win_merge$year[incum_win_merge$year=="12"]="10"
incum_win_merge$year[incum_win_merge$year=="13"]="10"

## Merging


asi_ec1=dt_count %>% inner_join(win_df) %>% 
      mutate(ave_asset=ave_asset/1e5)
#`by = join_by(year, state, district)`

# asi_ec1$asset_value=clipp(asi_ec1$asset_value, 0.50,0.95)
# asi_ec1=asi_ec1 %>% filter(!is.na(asset_value)) %>% mutate(log_asst=log(asset_value))
#          


asi_ec2=dt_count %>% inner_join(incum_win_merge) %>% 
    mutate(pct_increase=as.numeric(pct_increase), 
         asset_increase=log(as.numeric(asset_increase)))


fml=as.formula(paste0("tc~cv_asset"))

reg=felm(fml, asi_ec1); summary(reg)

dd= asi_ec1 %>%  filter(!is.na(tc) & !(is.na(ave_asset)))
ggplot(dd, aes(x = ave_asset, y = tc)) +
  geom_point() + xlim(c(1874326 ,28651537 ))

  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = paste("coef =", round(coef(summary(lm(tc ~ asset_value, data = asi_ec1)))[2, 1], 2))),
            x = Inf, y = -Inf, hjust = 1, vjust = 0)


#by = join_by(candidate, party, year)`


# incum_can_merge=incum_can_merge %>% mutate(constituency=tolower(constituency)) %>% 
#           mutate(constituency=str_trim(constituency))
# 
# 
 c1=unique(incum_win_merge$constituency)
# 
 c2=unique(dt_count$constituency)
# 
sum(c1 %in% c2)
