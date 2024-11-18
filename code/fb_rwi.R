
rm(list=ls())
source("code/functions.R")


fb_ac07=read_dta("../Data/SHRUG/FB-RWI/facebook_rwi_con07.dta")
fb_ac08=read_dta("../Data/SHRUG/FB-RWI/facebook_rwi_con08.dta")


elec_cand=read_rds("data/clean/elec_cand.rds")


fb_elec_cand=elec_cand %>% 
        inner_join(fb_ac08)




# COMPUTING MEAN NET WEALTH ACROSS TIME OF WINNERS

fb_elec_win=fb_elec_cand %>% filter(win==1)


fb_elec_win=fb_elec_win %>% group_by(ac08_id) %>% 
            mutate(lg_avg_asset=mean(log(assets), na.rm=T)) %>% ungroup()
fb_elec_cand=fb_elec_cand %>% 
  mutate(bimaru=if_else(state_id %in% c("08",
                                        "09",
                                        "10",
                                        "21",
                                        "23"),1,0))
          
dd=fb_elec_cand %>% filter(bimaru==1) %>% 
  dplyr::select(facebook_mean_rwi, avg_asset) %>% distinct()

fml=as.formula(paste("log(avg_asset)~
                     facebook_mean_rwi*as.factor(bimaru)"))

reg=lfe::felm(fml, fb_elec_cand)
summary(reg)



# COMPUTING WEALTH GROWTH FOR INCUMBENTS

inc_cand=elec_cand %>% 
  dplyr::select(state_id, state_const,year, ac07_id, ac08_id, 
                pid, assembly_no, 
                assets,candidate,  win, incumbent,
                margin_percentage,constituency_name) %>% 
  dplyr::filter(win==1)

# %>% 
#   distinct() %>% 
#   mutate(assembly_no=as.numeric(assembly_no)) %>% 
#   group_by(pid) %>% 
#   arrange(pid, assembly_no) %>% 
#   mutate(asset_delta=assets-Lag(assets),
#          assembly_delta=assembly_no-Lag(assembly_no)) %>% 
#   filter(incumbent==T) %>% 
#   filter(!is.na(asset_delta)) %>% 
#   ungroup()


fb_inc=inc_cand %>% 
  inner_join(fb_ac08)

fb_inc=fb_inc %>% group_by(ac08_id) %>% 
  mutate( mean_margin=mean(margin_percentage, na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::select(facebook_mean_rwi, 
                 mean_margin,
                state_id, 
                constituency_name) %>% 
  distinct()

fb_inc=fb_inc %>% 
  mutate(bimaru=if_else(state_id %in% c("08",
                                        "09",
                                        "10",
                                        "21",
                                        "23"),1,0))


# Joining with `by = join_by(ac08_id)`
fml=as.formula(paste("mean_margin~ 
                     facebook_mean_rwi"))

reg=lfe::felm(fml, fb_inc);summary(reg)

inc_cand1=inc_cand %>% ungroup() %>% 
  mutate(asset_delta=clipp(asset_delta, 0.1, 0.95)) %>% 
  dplyr::filter(!is.na(asset_delta))

fb_inc1=fb_inc %>% 
  mutate(mean_asset_delta=clipp(mean_asset_delta, 0.1, 0.95)) %>% 
  dplyr::filter(!is.na(mean_asset_delta))

plot(density(log(inc_cand1$asset_delta)), col = "blue", main = "Density Plot")
lines(density(log(fb_inc1$mean_asset_delta)), col = "red")
legend("topright", legend = c("Dataset 1", "Dataset 2"), col = c("blue", "red"), lwd = 1)



# AVG WIN MARGIN



# Plotting the AC08 level mean asset increase on a map


ac_shp <- st_read("../Data/MAPS- INDIA/AC/maps-master/assembly-constituencies/India_AC.shp")
ac_shp=ac_shp %>% 
  mutate(constituency_name=toupper(AC_NAME)) %>% 
  inner_join(fb_elec_win, by="constituency_name")

ac_shp=ac_shp %>% 
  dplyr::filter(lg_avg_asset>0) %>% 
  mutate(asset_dummy=if_else(lg_avg_asset>16.6,1,0))

# Plot the shapefile
custom_breaks <- c(0, 5,7,10,15,20,80 )
tmap_options(check.and.fix = TRUE)
tm_shape(ac_shp) +
  tm_borders()+
  tm_fill(col="asset_dummy")






