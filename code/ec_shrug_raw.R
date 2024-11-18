rm(list=ls())
source("code/functions.R")


# READING IN ALL THE EC DATA FROM SHRUG

flist=list.files(path="../Data/SHRUG/EC",
                 pattern = ".con08.dta$", 
                 recursive = T, 
                 full.names = T)


dt=lapply(flist, read_dta)





## Just keeping the total firm count

firm_var=c("ec90_count_all",
           "ec98_count_all",
           "ec05_count_all",
           "ec13_count_all")

firm_count=lapply(dt, function(x){ dd=x %>% dplyr::select("ac08_id",any_of(firm_var))
return(dd)})

firm_count_df= Reduce(function(x, y) merge(x, y,  all = TRUE), firm_count)


firm_count_lg=firm_count_df %>%
  pivot_longer(cols = starts_with("ec"), 
               names_to = "year", 
               names_pattern = "ec(\\d\\d)_count_all", 
               values_to = "count")


firm_count_lg$year[firm_count_lg$year=="90"]="1990"
firm_count_lg$year[firm_count_lg$year=="98"]="1998"
firm_count_lg$year[firm_count_lg$year=="05"]="2005"
firm_count_lg$year[firm_count_lg$year=="13"]="2013"


plot(density(firm_count_lg$count[firm_count_lg$year=="2005"], na.rm=T), col = "blue", main = "Density Plot")
lines(density(firm_count_lg$count[firm_count_lg$year=="2013"], na.rm=T), col = "red")
legend("topright", legend = c("2005", "2013"), col = c("blue", "red"), lwd = 1)


# Seeing teh growth in pure countes from 2005 to 2013
quantile(firm_count_lg$count[firm_count_lg$year=="2013"], c(0.1,0.9), na.rm=T)/quantile(firm_count_lg$count[firm_count_lg$year=="2005"], c(0.1,0.9), na.rm=T)


