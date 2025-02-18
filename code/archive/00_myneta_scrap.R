
source("code/functions.R")
not_all_na <- function(x) any(!is.na(x))

url_extract1=function(u){ # This function is for winner and candidates
  webpage <- rvest::read_html(u)
  
  yr=str_extract(u,pattern = "\\d{4}")
  
  table_nodes <- rvest::html_nodes(webpage, "table")
  df<- rvest::html_table(table_nodes[[3]])
  colnames(df) <- df[2, ]
  df <- df[-c(1:2), ]
  
  df=df %>% clean_names() %>% dplyr::select(where(not_all_na)) %>% 
    mutate(asset_value=str_extract(total_assets, "\\d[\\d,]+"),
           assert_lvl=str_extract(total_assets, "\\d+[:space:][:alpha:]+"),
           liab_value=str_extract(liabilities, "\\d[\\d,]+"),
           liab_lvl=str_extract(liabilities, "\\d+[:space:][:alpha:]+")) %>% 
    mutate(asset_value=gsub(",", "", asset_value),
           liab_value=gsub(",", "", liab_value))
  
  df$liab_value[is.na(df$liab_value)]=0
  df$liab_lvl[is.na(df$liab_lvl)]=0
  
  df$year=yr
  
  return(df)
  
}

url_extract2=function(u){ # This function is for incumbants
  webpage <- rvest::read_html(u)
  
  yr=str_extract(u,pattern = "\\d{4}")
  
  table_nodes <- rvest::html_nodes(webpage, "table")
  df<- rvest::html_table(table_nodes[[3]])
  #colnames(df) <- df[2, ]
  df <- df[-c(1), ]
  
  names(df)=c("sno","names_party","asset_current","asset_last","asset_increase",
              "pct_increase","remarks")
  
  df=df %>% clean_names() %>% dplyr::select(where(not_all_na)) %>% 
    mutate(asset_current=str_extract(asset_current, "\\d[\\d,]+"),
           asset_last=str_extract(asset_last, "\\d[\\d,]+"),
           asset_increase=str_extract(asset_increase, "\\d[\\d,]+")) %>% 
    mutate(pct_increase=gsub("%", "", pct_increase),
           asset_current=gsub(",", "", asset_current),
           asset_last=gsub(",", "", asset_last),
           asset_increase=gsub(",", "", asset_increase))
  
 
  
  df$year=yr
  
  return(df)
  
}


# Uttar Pradesh



bit1a="https://myneta.info/up"
bit1b="https://myneta.info/uttarpradesh"
yr1=c(2007,2012); yr2=c(2017,2022)
candidate="/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary"
winner="/index.php?action=summary&subAction=winner_analyzed&sort=candidate#summary"
incumbent="/index.php?action=recontestAssetsComparison"

url_list1=paste0(bit1a, yr1, candidate)
url_list2=paste0(bit1b, yr2, candidate)
can_url_list=c(url_list1,url_list2) %>% as.list()

url_list1=paste0(bit1a, yr1, winner)
url_list2=paste0(bit1b, yr2, winner)
win_url_list=c(url_list1,url_list2) %>% as.list()

url_list1=paste0(bit1a, yr1, incumbent)
url_list2=paste0(bit1b, yr2, incumbent)
incum_url_list=c(url_list1,url_list2) %>% as.list()


up_can_df=lapply(can_url_list, url_extract1) %>% bind_rows()
up_can_df$state="UP"


up_win_df=lapply(win_url_list, url_extract1) %>% bind_rows()
up_win_df$state="UP"


up_incum_df=lapply(incum_url_list, url_extract2) %>% bind_rows()
up_incum_df =up_incum_df %>% filter(!str_detect(sno, "^\\D"))
up_incum_df$state="UP"




## Rajasthan

# https://myneta.info/rj2008/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary
# https://myneta.info/rj2008/index.php?action=summary&subAction=winner_analyzed&sort=candidate#summary
# 
# https://myneta.info/rajasthan2013/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary
# 
# https://myneta.info/rajasthan2018/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary


bit1a="https://myneta.info/rj"
bit1b="https://myneta.info/rajasthan"
yr1=c(2008); yr2=c(2013,2018)
candidate="/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary"
winner="/index.php?action=summary&subAction=winner_analyzed&sort=candidate#summary"
incumbent="/index.php?action=recontestAssetsComparison"

url_list1=paste0(bit1a, yr1, candidate)
url_list2=paste0(bit1b, yr2, candidate)
can_url_list=c(url_list1,url_list2) %>% as.list()

url_list1=paste0(bit1a, yr1, winner)
url_list2=paste0(bit1b, yr2, winner)
win_url_list=c(url_list1,url_list2) %>% as.list()

url_list1=paste0(bit1a, yr1, incumbent)
url_list2=paste0(bit1b, yr2, incumbent)
incum_url_list=c(url_list1,url_list2) %>% as.list()

rj_can_df=lapply(can_url_list, url_extract1) %>% bind_rows()
rj_can_df$state="RJ"


rj_win_df=lapply(win_url_list, url_extract1) %>% bind_rows()
rj_win_df$state="RJ"


rj_incum_df=lapply(incum_url_list, url_extract2) %>% bind_rows()
rj_incum_df =rj_incum_df %>% filter(!str_detect(sno, "^\\D"))
rj_incum_df$state="RJ"



# Madhya Pradesh

# https://myneta.info/2008mp/
#   https://myneta.info/mp2013/
#   https://myneta.info/madhyapradesh2018/
#   
#   https://myneta.info/2008mp/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary
# https://myneta.info/madhyapradesh2018/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary

bit1a="https://myneta.info/2008mp"
bit1b="https://myneta.info/mp2013"
bit1c="https://myneta.info/madhyapradesh2018"
yr1=c(2008); yr2=c(2013,2018)
candidate="/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary"
winner="/index.php?action=summary&subAction=winner_analyzed&sort=candidate#summary"
incumbent="/index.php?action=recontestAssetsComparison"

url_list1=paste0(bit1a, candidate)
url_list2=paste0(bit1b,  candidate)
url_list3= paste0(bit1c,  candidate)
can_url_list=c(url_list1,url_list2, url_list3) %>% as.list()

url_list1=paste0(bit1a, winner)
url_list2=paste0(bit1b,  winner)
url_list3= paste0(bit1c,  winner)
win_url_list=c(url_list1,url_list2, url_list3) %>% as.list()

url_list2=paste0(bit1b,  incumbent)
url_list3= paste0(bit1c,  incumbent)
incum_url_list=c(url_list2, url_list3) %>% as.list()

mp_can_df=lapply(can_url_list, url_extract1) %>% bind_rows()
mp_can_df$state="MP"


mp_win_df=lapply(win_url_list, url_extract1) %>% bind_rows()
mp_win_df$state="MP"


mp_incum_df=lapply(incum_url_list, url_extract2) %>% bind_rows()
mp_incum_df =mp_incum_df %>% filter(!str_detect(sno, "^\\D"))
mp_incum_df$state="MP"


## Bihar

# https://myneta.info/bih2005/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary
# https://myneta.info/bih2010/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary
# https://myneta.info/bihar2015/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary
# https://myneta.info/Bihar2020/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary

bit1a="https://myneta.info/bih2005"
bit1b="https://myneta.info/bih2010"
bit1c="https://myneta.info/bihar2015"
bit1d="https://myneta.info/Bihar2020"


candidate="/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary"
winner="/index.php?action=summary&subAction=winner_analyzed&sort=candidate#summary"
incumbent="/index.php?action=recontestAssetsComparison"

url_list1=paste0(bit1a, candidate)
url_list2=paste0(bit1b,  candidate)
url_list3= paste0(bit1c,  candidate)
url_list4= paste0(bit1d,  candidate)
can_url_list=c(url_list1,url_list2, url_list3, url_list4) %>% as.list()

url_list1=paste0(bit1a, winner)
url_list2=paste0(bit1b,  winner)
url_list3= paste0(bit1c,  winner)
url_list4= paste0(bit1d,  winner)
win_url_list=c(url_list1,url_list2, url_list3, url_list4) %>% as.list()

url_list1=paste0(bit1a, incumbent)
url_list2=paste0(bit1b,  incumbent)
url_list3= paste0(bit1c,  incumbent)
url_list4= paste0(bit1d,  incumbent)
incum_url_list=c(url_list1,url_list2, url_list3, url_list4) %>% as.list()

bh_can_df=lapply(can_url_list, url_extract1) %>% bind_rows()
bh_can_df$state="BH"


bh_win_df=lapply(win_url_list, url_extract1) %>% bind_rows()
bh_win_df$state="BH"


bh_incum_df=lapply(incum_url_list, url_extract2) %>% bind_rows()
bh_incum_df =bh_incum_df %>% filter(!str_detect(sno, "^\\D"))
bh_incum_df$state="BH"


## Appending all the dfs

can_df = up_can_df %>% bind_rows(rj_can_df) %>% bind_rows(mp_can_df) %>% 
          bind_rows(bh_can_df)
#can_df$winner=0
can_df$sno=NULL

write_rds(can_df, "data/can_4states.rds", compress = "gz")

win_df = up_win_df %>% bind_rows(rj_win_df) %>% bind_rows(mp_win_df) %>% 
  bind_rows(bh_win_df)
win_df$winner=1

write_rds(win_df, "data/winner_4states.rds", compress = "gz")



total_can_df=can_df %>% left_join(win_df) %>% 
        mutate(criminal_case=as.numeric(criminal_case)) %>% 
          mutate(criminal_case=tcode(criminal_case, 0,0.99))
total_can_df$winner[is.na(total_can_df$winner)]=0
write_rds(total_can_df, "data/totcan_4states.rds", compress = "gz")



incum_df = up_incum_df %>% bind_rows(rj_incum_df) %>% bind_rows(mp_incum_df) %>% 
  bind_rows(bh_incum_df)

write_rds(incum_df, "data/incum_4states.rds", compress = "gz")





# SHRUG -------------------------------------------------------------------

can_df1=read_dta("../Data/SHRUG/dta_shrug-v1.5.samosa-affidavits-dta/shrug-v1.5.samosa-affidavits-dta/affidavits_clean.dta")

can_df2=read_dta("../Data/SHRUG/dta_shrug-v1.5.samosa-assembly-dta/shrug-v1.5.samosa-assembly-dta/assembly_candidates_clean.dta")

can_df3=read_dta("../Data/SHRUG/dta_shrug-v1.5.samosa-assembly-dta/shrug-v1.5.samosa-assembly-dta/assembly_elections_clean.dta")

can_df1= can_df1 %>% rename(state=pc01_state_id, constituency=adr_con_name)






# Graph 1

dd=total_can_df %>% group_by(state,winner) %>% 
        mutate(obs_count=n()) %>%  
  mutate(criminal_case = ifelse(criminal_case > 4, 4, criminal_case)) %>% 
        group_by(state,criminal_case, winner) %>% 
                mutate(share=n()*100/obs_count) %>% 
        distinct(state, criminal_case, share, winner)



ggplot(data = dd, aes(x = criminal_case, y = log(share), color = factor(winner))) +
  geom_point(size = 3)+
  facet_wrap(~state, scales = "free_x", ncol = 2) +
  labs(y="log_share", x="# criminal cases (top coded at 4)", color="Winner")




# Graph 2

dd=incum_df %>% group_by(state) %>% mutate(year=as.numeric(factor(year))) %>% 
      group_by(state, year) %>% 
     summarise(asset1=median(as.numeric(asset_last)),
               asset2=median(as.numeric(asset_current)),
               asset_delta=median(as.numeric(asset_increase), na.rm = T))

# ff=incum_df %>% group_by(state) %>% mutate(year=as.numeric(factor(year))) %>% 
#   group_by(state, year) %>% 
#   summarise(asset1=as.numeric(quantile(as.numeric(asset_last), 0.9)),
#             asset2=as.numeric(quantile(as.numeric(asset_current), 0.9)),
#             asset_delta=as.numeric(quantile(as.numeric(asset_increase), 0.9, na.rm=T)))



newrow <- tibble(state = unique(dd$state),
                 year = 0, asset1=0, asset2=0, 
                 asset_delta=0)

dd=dd %>%  bind_rows(newrow) %>% group_by(state) %>% 
        arrange(state, year) %>% 
  mutate(asset1=Lag(asset1, shift = -1)) %>% 
  mutate(asset_f=if_else((asset2==0), asset1, asset2)) %>% 
  mutate(pop=0) %>% 
  distinct(state, year, pop, asset_f) 

raj=tibble(state="RJ", year=c(0:2), pop=1, 
           asset_f=c(20000,44000,83000))
up=tibble(state="UP", year=c(0:3), pop=1, 
          asset_f=c(14000,35000,52000,65000))
mp=tibble(state="MP", year=c(0:2), pop=1, 
          asset_f=c(16000,32000,62000))
bh=tibble(state="BH", year=c(0:3), pop=1, 
          asset_f=c(8000,19000,30000,44000))

dd=dd %>% bind_rows(raj) %>% bind_rows(up) %>%  bind_rows(mp) %>% 
  bind_rows(bh)


ggplot(data=dd, aes(x=factor(year), y=log(asset_f), group = factor(pop))) +
  geom_line(aes(linetype=factor(pop), color=factor(pop)), linewidth=1.5)+
  scale_linetype_manual(values=c("solid", "longdash"))+
  geom_point(size=2)+
  facet_wrap(~state, scales = "free_x", ncol = 2) +
  labs(title="Self disclosed assets", x="year", y="log_assets")+
  theme(legend.position = "none")





### State Assembly Constituency Map

# Read shapefile
district_shape <- st_read("raw_data/maps-master/assembly-constituencies/India_AC.shp")
state_shape=st_read("raw_data/maps-master/States/Admin2.shp")

# Plot shapefile

# Set color and thickness for state boundary
state_color <- "black"
state_thickness <- 1.5

# Set color and thickness for district boundary
district_color <- "red"
district_thickness <- 0.5

ggplot() +
  geom_sf(data = state_shape, fill = "white", color = "black", lwd = 1) +
  geom_sf(data = district_shape, fill = NA, color = "azure4", lwd = 0.01)+
  theme_void() +
  theme(
    panel.background = element_rect(fill = "azure2"),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  )

  



### SCRAp

win2=win_df %>% group_by(state, constituency) %>% mutate(ecount=n()) %>% 
          group_by(state, constituency, candidate) %>% mutate(can_count=n())


win2=win2 %>% filter(can_count>1) %>% distinct(state, constituency) %>% nrow()

## trying to merge incum_df with can_df


incum_df2= incum_df %>% mutate(candidate=gsub("\\(.*\\)", "", names_party)) %>% 
              mutate(party=str_extract(names_party, "\\((.*?)\\)")) %>% 
              mutate(party=gsub("\\(|\\)", "", party)) %>% 
              mutate(candidate=tolower(candidate)) %>% 
            dplyr::select(candidate, party, year, asset_increase, pct_increase) %>% 
            distinct() %>% 
            mutate(candidate=str_trim(candidate))


can_df2=can_df %>% mutate(candidate=tolower(candidate)) %>% 
  dplyr::select(candidate, party, constituency,year) %>% distinct() 
incum_can_merge=incum_df2 %>% left_join(can_df2)


d_shp=st_read("../Data/SHRUG/geometries_shrug-v1.5.samosa-open-polygons-shp/district.shp")

ac_shp=st_read("raw_data/maps-master/assembly-constituencies/India_AC.shp")

dist_ac=ac_shp %>% as.data.frame() %>% 
  dplyr::select(ST_NAME, DIST_NAME, AC_NAME) %>% distinct() %>% 
  rename(state=ST_NAME, dist=DIST_NAME, constituency=AC_NAME) %>% 
  mutate_all(tolower) %>% filter(grepl("bihar|madhya pradesh|rajasthan|uttar pradesh", state))

