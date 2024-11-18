rm(list=ls())
source("code/functions.R")


# This code is to read pdf tables of monthly cost indices




# GETTING FILE PATHS ------------------------------------------------------

fpath=list.files("../Data/CCI/Cost Indices", 
                 pattern = ".pdf$", 
                 full.names = T)

pattern <- "([A-Za-z]+) (\\d{4})"

dates=str_extract(fpath, pattern = pattern)

matches <- regexec("([A-Za-z]+)\\s*(\\d+)", dates)
months <- regmatches(dates, matches)

# Create a data frame
date_df <- do.call(rbind.data.frame, months)
colnames(date_df) <- c("date", "mon", "yr")


# Format the dates in "mon yyyy" format
f_dates <- sprintf("%s %04d", substr(date_df$mon, 1, 3), as.numeric(date_df$yr))


fpath_df=as.data.frame(cbind(fpath, f_dates))

# Nov 2019 and Dec 2019 Needs to be read separately, removing them

novdec2019=fpath_df %>%  filter((f_dates %in%c("Nov 2019", "Dec 2019")))
fpath_df1=fpath_df %>% filter(!(f_dates %in%c("Nov 2019", "Dec 2019")))
                                

left=26.72; width=545.47;top=125.63;height=679.15
bottom=top+height; right=left+width

# Reading files

read_tab=function(fp){
  
  # left=26.72; width=545.47;top=125.63;height=679.15
  # bottom=top+height; right=left+width
  
  # p1=tabulizer::extract_tables(as.character(fp[1]), pages = 1, guess = F,
  #                             area= list(c(top, left, bottom, right)))
  
  
  p1=tabulizer::extract_tables(as.character(fp[1]), pages = 1, guess = T,
                               method = "stream")
  
  rno=which(p1[[1]][,1]=="1")
  p1=p1[[1]][-c(1:rno-1),]
  
  p1_l=list()
  for(d in 1:dim(p1)[1]){
    
    cp1=p1[d,]
    p1_l[[d]]=unlist(str_split(cp1, pattern = "\\s"))
    
  }
  
 
  
  p1_df=as.data.frame(do.call(rbind, p1_l))
  p1_df=p1_df %>% filter(V2!="Jorhat") %>% dplyr::select(1:13) # This is in Assam and creating 
  # a mess because of space and (), just dropping it.
 # p1_df=p1_df %>% separate(V4, into = paste0("V4_",1:7), sep = " ")
  
  colnames_df <- c("sno", "city", "Building", "Road", "Bridge",
                   "Dam", "Power", "Railway", "Mineral Plant",
                   "Medium Industry", "Transmission", "Urban Infra",
                   "Maintenance")
  
  colnames(p1_df) <- colnames_df
  
  p2=tabulizer::extract_tables(as.character(fp[1]), 
                               pages = 2, guess = T, method = "stream")
  p2_df=as.data.frame(p2); colnames(p2_df)=colnames_df
  
  df=rbind(p1_df, p2_df)
  
  df$date=as.character(fp[2])
  
  df
  
}


cci_dt1=list()

for(i in 1:nrow(fpath_df1)){
  
  print(i)
  ff=fpath_df1[i,]
  
  cci_dt1[[i]]=read_tab(ff)
  
}


# Now reading the novdec2019, both have only 1 page

cci_dt2=list()
for(n in 1:nrow(novdec2019)){

    fp=novdec2019[n,]
    
    raw_read=tabulizer::extract_tables(as.character(fp[1]), pages = 1, guess = T,
                                          method = "stream")
    raw_read=raw_read[[1]]
    # Clipping away the top jumbled up rows
    rno=min(which(str_detect(raw_read[,1],"^1")))-1
    raw_read=raw_read[-c(1:rno),]
    raw_read=as.data.frame(raw_read)
    raw_read=raw_read %>% separate(V1, into=c("V1a", "V1b"), 
                                   sep="\\s", extra = "merge")
    
    colnames_df <- c("sno", "city", "Building", "Road", "Bridge",
                     "Dam", "Power", "Railway", "Mineral Plant",
                     "Medium Industry", "Transmission", "Urban Infra",
                     "Maintenance")
    
    colnames(raw_read) <- colnames_df
    
    raw_read$date=as.character(fp[2])
    
    cci_dt2[[n]]=raw_read
    
    
}



# Merging all the data into one dataframe

cci_df1=cci_dt %>% bind_rows()
cci_df2=cci_dt2 %>% bind_rows()

cci_total=cci_df1 %>%  bind_rows(cci_df2)


## Even now there are some mess in the data, but there are only about 75 corrupt rows, 
# maybe can jsut let them be for now since its monthly data anyway, not worth the effort. 

# removing them

cci_total= cci_total %>%  filter(city!="") %>% filter(grepl("\\d", Building))
# about 150 rows are getting dropped here

cci_total=cci_total %>%  clean_names()

cci_total=cci_total %>% 
  mutate(month=format(as.Date(paste0(date, "-01"), format = "%b %Y-%d"), "%m"),
         year=format(as.Date(paste0(date, "-01"), format = "%b %Y-%d"), "%Y") )


write_rds(cci_total, file="data/clean/cci_total.rds", 
          compress = "gz")





