rm(list=ls())
source("code/functions.R")


# THIS CODE IS TO CONSTRUCT ZIP-AC Mapping

# [Sept 18th] This is done in three mutually redundant ways

# 1.  Mapping the zip centroid to nearest AC centroid for each district
# 2.  Mapping the zip centroid to nearest AC centroid for each state
# 3.  For each zip polygon, recording the AC polygon that intersects with it ( this includes zip that lies within)


# THIS AC SHP is likely post 2008

ind_shp <- st_read("../Data/MAPS- INDIA/india/IND_adm0.shp") %>% 
  st_make_valid()


zip_shp <- st_read("../Data/MAPS- INDIA/INDIA_PINCODES-master/india_pincodes.shp") %>% 
  st_make_valid() %>% mutate(district=tolower(district),
                             state=tolower(state))
#zip_shp$state[zip_shp$state=="nct of delhi"]="delhi"

ac_shp <- st_read("../Data/MAPS- INDIA/AC/maps-master/assembly-constituencies/India_AC.shp") %>% 
  st_make_valid() %>% mutate(DIST_NAME=tolower(DIST_NAME), 
                             ST_NAME=tolower(ST_NAME))

dist_shp=st_read("../Data/MAPS- INDIA/OFFICIAL_INDIA_CENSUS_2011_DISTRICT_POLYGONS-main/india-district.gpkg") %>% 
  st_make_valid() %>% mutate(name=tolower(name)) 
valid_status <- st_is_valid(dist_shp)
# There is one poly that is problamatic, dropping it for now:
# cartodb_id censuscode dt_cen_cd st_cen_cd   st_nm    name                           geom
# 400         73        488        21        24 Gujarat bharuch MULTIPOLYGON (((72.71736 21...                       
dist_shp <- dist_shp[valid_status, ] # Dropping problematic geometries


state_shp=st_read("../Data/MAPS- INDIA/state_and_country/India_State_Boundary.shp") %>% 
  st_make_valid()
state_shp=st_transform(state_shp, crs = st_crs(ac_shp))



zip_centroids=st_centroid(zip_shp); ac_centroids=st_centroid(ac_shp)
zip_centroids=st_transform(zip_centroids, crs=st_crs(ac_shp))

# Each AC is uniquely mapped to a admin district, so it makes best sense to do the zip-AC mapping 
#  at the district level. 

zip_cent_df=zip_centroids %>%  separate_rows(district, sep=",") %>% 
  mutate(district=str_trim(district)) %>% group_by(pincode) %>% 
  mutate(dist_perzip=n_distinct(district), zip=pincode) %>% 
  mutate(border_dum1=if_else(dist_perzip>1, 1, 0)) %>% # ZIP at the boundary of district
  ungroup() %>% as.data.frame() %>% 
  dplyr::select(zip, state, district, border_dum1) %>% distinct() 

zip_cent_df$state[zip_cent_df$state=="nct of delhi"]="delhi"


# CREATING BOUNDARY INDICATOR FOR AC --------------------------------------

dist_ac_boundary=st_contains_properly(dist_shp, ac_shp, sparse = TRUE, prepared = TRUE)
bid=unlist(dist_ac_boundary)
ac_shp$border_dum2=1
ac_shp$border_dum2[bid]=0 # AC at the boundary of district


state_ac_boundary=st_contains_properly(state_shp, ac_shp, sparse = TRUE, prepared = TRUE)
bid=unlist(state_ac_boundary)
ac_shp$border_dum3=1
ac_shp$border_dum3[bid]=0 # AC at the boundary of states

state_dist_boundary=st_contains_properly(state_shp, dist_shp, sparse = TRUE, prepared = TRUE)
bid=unlist(state_dist_boundary)
dist_shp$border_dum4=1
dist_shp$border_dum4[bid]=0 # Dist at the boundary of states

loc_dist=st_contains_properly(ind_shp, dist_shp, sparse = T, prepared = T)
bid2=(1:nrow(dist_shp))[!1:nrow(dist_shp) %in%unlist(loc_dist)]
dist_shp$border_dum4[bid2]=0



# Attempt 1 : Matching using min distance at district level ---------------


dist_list=intersect(unique(zip_cent_df$district), 
                    unique(ac_centroids$DIST_NAME))

# Now we have a common list of 453 districts ( out of around 750+) 
# Delhi is missing in this, that can be done at state level

zip_ac_map_dist=list()
for(d in 1: length(dist_list)){
  
  z1=zip_cent_df %>%  dplyr::filter(district==dist_list[d]) # CHANGE HERE TO ZIP_SHP
  a1=ac_centroids %>%  filter(DIST_NAME==dist_list[d])
  a1_name=unique(a1$AC_NAME)
  
  dmat=st_distance(z1, a1)
  
  min_column <- apply(dmat, 1, which.min)
  ac_name=a1_name[min_column]
  
  z1$ac_name=a1_name[min_column]
  
  zip_ac_map_dist[[d]]=z1
  
  if(round(d/25)==d/25){
    print(paste0("Done with ", d, " of ", length(dist_list)))
  }
  
}

write_rds(zip_ac_map_dist, "data/daily_output/zip_ac_map_dist_lvl_0916.rds", 
          compress = "gz")

# Attempt 2 : Matching using min distance at state level ---------------


# ATTEMPTING THE SAME AT STATE LEVEL TO INCREASE COVERAGE POTENTIALLY

state_list=intersect(unique(zip_centroids$state), unique(ac_centroids$ST_NAME))

# Now we have a common list of 453 districts ( out of around 750+) 
# Delhi is missing in this, that can be done at state level
zip_ac_map_state=list()
for(d in 1: length(state_list)){
  
  z1=zip_centroids %>%  dplyr::filter(state==state_list[d])
  a1=ac_centroids %>%  filter(ST_NAME==state_list[d])
  a1_name=unique(a1$AC_NAME)
  
  dmat=st_distance(z1, a1)
  
  min_column <- apply(dmat, 1, which.min)
  ac_name=a1_name[min_column]
  
  z1$ac_name=a1_name[min_column]
  
  zip_ac_map_state[[d]]=z1
  
  #if(round(d/25)==d/25){
  print(paste0("Done with ", d, " of ", length(state_list)))
  # }
  
}



write_rds(zip_ac_map_state, "data/daily_output/zip_ac_map_st_lvl_0916.rds", 
          compress = "gz")


# NOW WE CAN MERGE THEM BOTH TO GET TWO ZIP-AC MAPPING USING TWO LEVELS
#  This is redundancy, 

smap=zip_ac_map_state %>% bind_rows() %>% 
  rename(state_ac=ac_name) %>% as.data.frame()
dmap=zip_ac_map_dist %>% bind_rows() %>% 
  rename(dist_ac=ac_name) %>% as.data.frame()

zip_ac_map=smap %>% full_join(dmap)

# Here 6400 ( around 30% ) zip code has different AC 
#  depending on state level mapping or district level mapping

# This is a measure of around 30% are at the boundary of distrct?


# 
##   between zip_shp and ac_shp --------

# ATTEMPT 3: Intersecting ZIP Polygons with AC Polygons  -------------


zsub=zip_shp %>%  filter(state=="rajasthan")
acsub=ac_shp %>% filter(ST_NAME=="rajasthan")

#pol_within=st_within(zip_shp, ac_shp, sparse = F, prepared = T)
pol_intersect=st_intersects(zip_shp, ac_shp, sparse=F)
dd=st_intersects(zip_shp, ac_shp, sparse=T)

#idx_within=apply(pol_within, 1, function(row) which(row))
idx_intersects=apply(pol_intersect, 1, function(row) which(row))

zip_ac_intx_map=list()

for (i in 1:length(idx_intersects)) {
  num_copies <- length(idx_intersects[[i]])
  # Repeat the ith feature from sf_object1 'num_copies' times
  rf <- zip_shp[rep(i,num_copies),]
  rf$ac_intersects=ac_shp$AC_NAME[idx_intersects[[i]]]
  rf$district_ac=ac_shp$DIST_NAME[idx_intersects[[i]]]
  rf$state_ac_id=ac_shp$ST_CODE[idx_intersects[[i]]]
  rf$ac_no=ac_shp$AC_NO[idx_intersects[[i]]]
  rf$pc_no=ac_shp$PC_NO[idx_intersects[[i]]]
  rf$ac_at_dist_bnd=ac_shp$border_dum2[idx_intersects[[i]]]
  rf$ac_at_st_bnd=ac_shp$border_dum3[idx_intersects[[i]]]
  
  zip_ac_intx_map[[i]] <- rf
}

zip_ac_intx_map=zip_ac_intx_map %>%  bind_rows()

# dd=zip_ac_intx_map %>% group_by(ac_intersects, district_ac, state_ac_id) %>% 
#   summarise(zip_per_intx=n_distinct(pincode))
# setDT(zip_ac_intx_map)
# tt <- zip_ac_intx_map[, zip_per_intx := .(n_distinct(ac_intersects, district_ac, state_ac_id)),
#                       by = .(pincode)] %>% 
#   dplyr::select(ac_intersects, district_ac, state_ac_id, zip_per_intx) %>% 
#   distinct()



# Adding back all the boundary dummies

dist_df=dist_shp %>%  as.data.frame() %>% 
  dplyr::select(name, border_dum4) %>%  rename(district=name) %>% distinct() 

df=zip_ac_intx_map %>% separate_rows(district, sep=",") %>% 
  mutate(district=str_trim(district)) %>%  rename(zip=pincode)
df = df %>% inner_join(zip_cent_df) %>% rename(zip_at_dist_bnd=border_dum1)
df=df %>% inner_join(dist_df) %>% 
      rename(dist_at_st_bnd=border_dum4) # THERE IS A LOSS OF DISTRICTS HERE 

zip_ac_intx_map=df


write_rds(zip_ac_intx_map, "data/interm/zip_ac_map_intx.rds", 
          compress = "gz")


# ATTEMPT 4: LOCATING ZIP CENTROID IN THE AC  -------------

#( this will ensure 1-1 mapping)

#ac_zip_map=st_contains(ac_shp, zip_centroids)

# First, Lets get one AC for each ZIP 

zip_ac_map=st_nearest_feature(zip_centroids, ac_shp)

# Now, we can run a loop and get distance from each zip to the boundary of AC
zip_ac_dist=list()
for( i in 1:nrow(zip_centroids)){
  
  nr=st_nearest_points(zip_centroids[i,], ac_shp[zip_ac_map[i],])
  pts = st_cast(nr[1], "POINT")
  
  
  dd=st_distance(zip_centroids[i,],pts)
  
  zip_ac_dist[[i]]=dd[2]
  
  
}

zip_shp2=zip_shp
zip_shp2$st_code_ac=ac_shp$ST_CODE[zip_ac_map] 
zip_shp2$st_name_ac=ac_shp$ST_NAME[zip_ac_map] 
zip_shp2$dist_code_ac=ac_shp$DT_CODE[zip_ac_map] 
zip_shp2$dist_name_ac=ac_shp$DIST_NAME[zip_ac_map] 
zip_shp2$ac_no=ac_shp$AC_NO[zip_ac_map]
zip_shp2$ac_name=ac_shp$AC_NAME[zip_ac_map]
zip_shp2$ac_at_dist_bnd=ac_shp$border_dum2[zip_ac_map]
zip_shp2$ac_at_st_bnd=ac_shp$border_dum3[zip_ac_map]
zip_shp2$dist_to_ac=unlist(zip_ac_dist)

zip_ac_map4=zip_shp2


# Adding back all the boundary dummies

dist_df=dist_shp %>%  as.data.frame() %>% 
  dplyr::select(name, border_dum4) %>%  rename(district=name) %>% distinct() 

df=zip_ac_map4 %>% separate_rows(district, sep=",") %>% 
  mutate(district=str_trim(district))  %>%  rename(zip=pincode)
df = df %>% inner_join(zip_cent_df) %>% rename(zip_at_dist_bnd=border_dum1)
# Joining, by = c("zip", "state", "district")
df=df %>% inner_join(dist_df) %>% 
  rename(dist_at_st_bnd=border_dum4) # THERE IS A LOSS OF DISTRICTS HERE 
#Joining, by = "district"

zip_ac_map4=df

write_rds(zip_ac_map4, "data/interm/zip_ac_map4.rds", 
          compress = "gz")
