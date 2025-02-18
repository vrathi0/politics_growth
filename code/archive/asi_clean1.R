rm(list=ls())
source("code/functions.R")

# Rough

asi_panel=read_dta("/Users/vaibhavrathi/Dropbox/ASI/generated_data/Clean/Final/asi_plant_panel1998-2016_cleanset.dta")
  
  #read_dta("/Users/vaibhavrathi/Dropbox/ASI/generated_data/Clean/Final/ASI1980-2016_panel.dta")


# ROUGH



##ROUGH
blka=read_dta("../Data/ASI/ASI201920data/ASI201920Study/blkA201920.dta")
blkb=read_dta("../Data/ASI/ASI201920data/ASI201920Study/blkB201920.dta")
blkc=read_dta("../Data/ASI/ASI201920data/ASI201920Study/blkC201920.dta")
blkd=read_dta("../Data/ASI/ASI201920data/ASI201920Study/blkD201920.dta")
blke=read_dta("../Data/ASI/ASI201920data/ASI201920Study/blkE201920.dta")
blkf=read_dta("../Data/ASI/ASI201920data/ASI201920Study/blkF201920.dta")

name_df=as.data.frame(cbind(names(blka), names(blkb), names(blkc), 
                            names(blkd), names(blke), names(blkf)))

##

# This file is just to merge A module of the ASI data

# Listing files:

flist_A=list.files(path="../Data/ASI", 
                 pattern = "^(A|BlockA|BLOCK-A|blka|blkA).*\\.dta$", 
                 recursive=T, 
                 full.names = T)

flist_B=list.files(path="../Data/ASI", 
                   pattern = "^(B-|BlockB|BLOCK-B|blkb|blkB).*\\.dta$", 
                   recursive=T, 
                   full.names = T)


# Reading the files

dt=lapply(flist_B, read_dta)


# Selecting states and district variables

# Pattern for variable selection
pattern <- "State|District|Year|Inflation|Multiplier|Serial|Dispatch"


getL=function(x){
  l=as.character(lapply(as.list(x), attr, "label"))
  
  return(l)
  
}


getX = function(x) {
  # Extract labels from the attributes of the input data frame
  l = as.character(lapply(as.list(x), attr, "label"))
  
  # Subset the data frame based on the labels that match the specified pattern
  d = x[, grep(pattern, l)]
  
  # Extract column names from the labels and assign them to the subsetted data frame
  names(d) = str_extract(l[grep(pattern, l)], "\\w+")
  
  # Clean column names and convert all columns to character type
  d = d %>% clean_names() %>% mutate_all(as.character)
  
  # Return the resulting data frame
  return(d)
}



dt_sub=lapply(dt, getX) %>% bind_rows() 

dt_sub$year[dt_sub$year=="2006"]="06"
dt_sub$year[dt_sub$year=="2007"]="07"
dt_sub$year[dt_sub$year=="2008"]="08"


dt_sub=dt_sub %>% mutate(state=padzero(state, 2), 
                         district=padzero(district,2)) %>% 
  mutate(state_dist=paste0(state, district)) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(wt = coalesce(inflation, multiplier),
         dsl=coalesce(dispatch, despatch))

## Counting and basic summ stat

## 1. R2 in state vs state_dist
## 2. Dispersion in trend coeff at state_dist level. 
## 3. 

# summing weights for each year-state-district

dt_count= dt_sub %>% group_by(year, state, district, state_dist) %>% 
  summarise(nobs=sum(as.numeric(wt))) %>% group_by(state, year) %>% 
  mutate(state_year_nobs=mean(nobs, na.rm=T)) %>% 
  group_by(state) %>% 
  mutate(state_cv=sd(state_year_nobs, na.rm = T)/mean(state_year_nobs, na.rm=T)) %>% 
  group_by(state_dist) %>% 
  mutate(state_dist_cv=sd(nobs, na.rm = T)/mean(nobs, na.rm=T)) %>% 
  mutate(state_dist_cv_dm=state_dist_cv-state_cv) %>% 
  group_by(state, year) %>% 
  mutate(state_year_cv=sd(nobs, na.rm=T)/mean(nobs, na.rm=T))


fml=as.formula("nobs~state")
reg=lfe::felm(fml, dt_count);summary(reg)


# Making CV density plots

ggplot(dt_count, aes(x = state_dist_cv)) +
  geom_density( alpha = 0.5, lwd=1) +
  labs(x = "CV  State-District level", y = "Density")+
  geom_vline(aes(xintercept = median(state_dist_cv, na.rm=T)), color = "red", linetype = "dashed", size = 1)+
scale_x_continuous(breaks = c(0, 0.4,1,2,3))


ggplot(dt_count, aes(x = state_dist_cv_dm)) +
  geom_density( alpha = 0.5, lwd=1) +
  labs(x = "CV  District level", y = "Density")+
  geom_vline(aes(xintercept = median(state_dist_cv_dm, na.rm=T)), color = "red", linetype = "dashed", size = 1)+
  scale_x_continuous(breaks = c(-2,-1,-0.5,0,0.5,1,2,3))+
  ggtitle("After taking out state level CV")


# Trend line at district level

dt_trend=dt_count %>% 
  group_by(state_dist) %>% mutate(nn=n()) %>% filter(nn>2)
dt_trend_list=split(dt_trend, dt_trend$state_dist)

getrend=function(x){
  
  fml=as.formula(paste0("nobs~year"))
  reg=felm(fml, x)
  
  return(reg$coefficients[2,1])
}

trend=unlist(lapply(dt_trend_list, getrend))
trend=clipp(trend, 0.05, 0.95)

state_dist_trend_df=as.data.frame(trend)

ggplot(state_dist_trend_df, aes(x = trend)) +
  geom_density( alpha = 0.5, lwd=1) +
  labs(x = "", y = "Density")+
  geom_vline(aes(xintercept = median(trend, na.rm=T)), color = "red", linetype = "dashed", size = 1)+
   ggtitle("State-District trend in unit count")



## Trend in State level CV across years


dt_trend_list=split(dt_count, dt_trend$state)

getrendcv=function(x){
  
  fml=as.formula(paste0("state_year_cv~year"))
  reg=felm(fml, x)
  
  return(reg$coefficients[2,1])
}

trend=unlist(lapply(dt_trend_list, getrend))



stateCV_trend_df=as.data.frame(trend) %>% 
          filter(!is.na(trend))

ggplot(stateCV_trend_df, aes(x = trend)) +
  geom_density( alpha = 0.5, lwd=1) +
  labs(x = "", y = "Density")+
  geom_vline(aes(xintercept = median(trend, na.rm=T)), color = "red", linetype = "dashed", size = 1)+
  ggtitle("Regression Coeff (state_CV~year)")

