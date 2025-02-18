rm(list=ls())
source("code/functions.R")

# This code file contains first cut of MCA analysis. 

# Input data characteristics:
# Comes from 1_mca_ac_merge.R
# Currently only reg_companies with AC location. 




# LOADING THE DATA --------------------------------------------------------
tic()
mca_acyr=qread("data/clean/MCA_ACYEAR_REG_COMP.qs") %>% 
  mutate(state_name=str_replace_all(state_name, " ", "_")) %>% 
  rename(fi_year=year)
mca_acyr$state_name[mca_acyr$state_name=="jammu_and_kashmir"]="jammu_&_kashmir"

# including state code for state-zip


mca_acyr=mca_acyr %>% inner_join(state_code)

toc()



# Currently I have only flow of firms. Possible analysis points:
# 1. 


#llp_comp=read_rds("data/clean/MCA_llp_company.rds")



# REGISTERED COMPANY ------------------------------------------------------

# RATE OG FLOW AT DISTRICT LEVEL:

df=mca_acyr

dist_yr=df %>% group_by(state_name, district, fi_year) %>% 
  summarise(dist_yr_flow=sum(dist_year_act_count),
            dist_yr_autcap=mean(dist_year_act_authcap, na.rm=T),
            dist_year_act_paidcap=mean(dist_year_act_paidcap, na.rm=T))





# DIST AND AC LEVEL SPATIAL MAP -------------------------------------------

df=mca_acyr

dist_df=df %>% group_by(district) %>% 
  summarise(lg_dist_tot_count=log(sum(dist_year_act_count))) %>% 
  rename(name=district) %>%
  mutate(
    bin_var = case_when(
      lg_dist_tot_count < quantile(lg_dist_tot_count, 0.45,na.rm=T) ~ 0,
      lg_dist_tot_count > quantile(lg_dist_tot_count, 0.55,na.rm=T) ~ 1,
        TRUE ~ NA_real_
    )
  )
 # mutate(bin_var = as.integer(lg_dist_tot_count > median(lg_dist_tot_count, na.rm=T))) 

dist_shp=st_read("../Data/MAPS- INDIA/OFFICIAL_INDIA_CENSUS_2011_DISTRICT_POLYGONS-main/india-district.gpkg") %>% 
  st_make_valid() %>% mutate(name=tolower(name)) 
valid_status <- st_is_valid(dist_shp)
# There is one poly that is problamatic, dropping it for now:
# cartodb_id censuscode dt_cen_cd st_cen_cd   st_nm    name                           geom
# 400         73        488        21        24 Gujarat bharuch MULTIPOLYGON (((72.71736 21...                       
dist_shp <- dist_shp[valid_status, ] # Dropping problematic geometries
dist_shp=dist_shp %>% inner_join(dist_df)

ggplot(dist_shp) +
  geom_sf(aes(fill = factor(bin_var))) +  # Fill based on binary index
  scale_fill_manual(values = c("0" = "chocolate", "1" = "blanchedalmond")) +  # Specify colors
  labs(
    title = "Shapefile Plot with Binary Fill",
    fill = "Binary Index (0: Below Median, 1: Above Median)"
  ) +
  theme_minimal()

# AC
ac_df=df %>% group_by(st_code_ac, ac_name) %>% 
  summarise(lg_ac_tot_count=log(sum(ac_year_act_count))) %>% 
  rename(AC_NAME=ac_name) %>% 
  mutate(bin_var = as.integer(lg_ac_tot_count > median(lg_ac_tot_count, na.rm=T))) 

ac_shp <- st_read("../Data/MAPS- INDIA/AC/maps-master/assembly-constituencies/India_AC.shp") %>% 
  st_make_valid() %>% mutate(DIST_NAME=tolower(DIST_NAME), 
                             ST_NAME=tolower(ST_NAME))


ac_shp=ac_shp %>% inner_join(ac_df)

ggplot(ac_shp) +
  geom_sf(aes(fill = factor(bin_var))) +  # Fill based on binary index
  scale_fill_manual(values = c("0" = "chocolate", "1" = "blanchedalmond")) +  # Specify colors
  labs(
    title = "Shapefile Plot with Binary Fill",
    fill = "Binary Index (0: Below Median, 1: Above Median)"
  ) +
  theme_minimal()



# ESTIMATING THE BOUNDARY EFFECT ------------------------------------------

# NOTE: I have several boundary indicators that 
#  indicates if a geography is at the boundary of another bigger geography
# the point of this section is to see if there is any boundary effect at al ( why should there be?)

# zip at dist boundayr

df=mca_acyr

zip_count_df=df %>% filter(!is.na(district)) %>% 
  group_by(zip, zip_at_dist_bnd) %>% 
  mutate(zip_count=sum(zip_year_act_count)) %>% 
  dplyr::select(zip, zip_at_dist_bnd, district, state_name, zip_count) %>% 
  distinct() 
zip_count_df$zip_count=clipp(zip_count_df$zip_count, 0, 0.95)

fml=as.formula(paste0("zip_count~zip_at_dist_bnd|district"))
reg=felm(fml, zip_count_df); summary(reg) # beta= 17 tstat=6.4  mean_Y=40


# AC AT THE DIST BOUNDARY

df=mca_acyr

ac_count_df=df %>% filter(!is.na(district)) %>% 
  group_by(st_code_ac,ac_no, ac_at_dist_bnd) %>% 
  mutate(ac_count=sum(ac_year_act_count)) %>% 
  dplyr::select(st_code_ac,ac_no, ac_at_dist_bnd, district, ac_count) %>% 
  distinct() 
ac_count_df$ac_count=clipp(ac_count_df$ac_count, 0.25, 0.75)

fml=as.formula(paste0("ac_count~ac_at_dist_bnd|district"))
reg=felm(fml, ac_count_df); summary(reg) # beta= -776 tstat=-15  mean_Y=636


# AC AT THE STATE BOUNDARY

df=mca_acyr

ac_count_df=df %>% filter(!is.na(state_name)) %>% #filter(dist_at_st_bnd==1) %>% 
  group_by(st_code_ac,ac_no, ac_at_st_bnd) %>% 
  mutate(ac_count=sum(ac_year_act_count)) %>% 
  dplyr::select(st_code_ac,ac_no, dist_code_ac, ac_at_st_bnd, state_name, region,
                ac_count) %>% 
  mutate(state_dist_ac_code=paste(st_code_ac, dist_code_ac, sep="-")) %>% 
  distinct() 
ac_count_df$ac_count=clipp(ac_count_df$ac_count, 0, 0.95)
ac_north=ac_count_df %>% filter(region=="north")
ac_south=ac_count_df %>% filter(region=="south")


fml=as.formula(paste0("ac_count~ac_at_st_bnd|state_dist_ac_code"))
reg=felm(fml, ac_count_df); summary(reg) # beta= -423 tstat=-7  mean_Y=825




# DIST AT THE STATE BOUNDARY

df=mca_acyr

dist_count_df=df %>% filter(!is.na(state_name) & !(is.na(district))) %>% 
  group_by(district, dist_at_st_bnd) %>% 
  mutate(dist_count=sum(dist_year_act_count)) %>% 
  dplyr::select(district, dist_at_st_bnd, state_name, region, dist_count) %>% 
  distinct() 
dist_count_df$dist_count=clipp(dist_count_df$dist_count, 0.5, 0.95)
dist_north=dist_count_df %>% filter(region=="north")
dist_south=dist_count_df %>% filter(region=="south")

fml=as.formula(paste0("dist_count~dist_at_st_bnd|state_name"))
reg=felm(fml, dist_count_df); summary(reg) # beta= 175.4 tstat=0.69  mean_Y: 2663





# GROWTH RATE AT DISTRICT LEVEL -------------------------------------------------

dist_year_df=mca_acyr %>% 
            group_by(district, fi_year) %>% 
  summarise(dist_year_count=sum(dist_year_act_count)) %>% 
  filter(!(fi_year %in% c(2020,2021, 2023, 2022,NA))) %>% 
  filter(!is.na(district)) %>% ungroup() 
dist_year_df$dist_year_count=  tcode(dist_year_df$dist_year_count, 0.05, 0.95)
  
# Step 1: Calculate the quantile position for each district at the first year
quantile_data <- dist_year_df %>%
  filter(fi_year == min(fi_year))
quantile_data$q_lvl=cut(quantile_data$dist_year_count, 
                        breaks=quantile(quantile_data$dist_year_count, c(0,0.25,
                                                                         0.5,0.9,1)),
                         labels=F)

quantile_data$q_lvl[is.na(quantile_data$q_lvl)]=1


# Step 2: Compute growth rate for each district
growth_data <- dist_year_df %>%
  group_by(district) %>%
  arrange(fi_year) %>%
  mutate(growth_rate = (dist_year_count - Lag(dist_year_count)) / Lag(dist_year_count)) %>%
  filter(!is.na(growth_rate)) %>% 
  group_by(district) %>% 
  summarise(grate=mean(growth_rate)*100)

plot_df=growth_data %>%  inner_join(quantile_data)

# Step 3: Create a scatter plot
ggplot(plot_df, aes(x = 1, y = grate, 
                    color = q_lvl)) +
  geom_point(size = 1) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(
    title = "Growth Rates by District",
    x = "",
    y = "Growth Rate",
    color = "Quantile Position"
  ) +
  guides(color = guide_legend(title = "Quantile Position"))



ggplot(plot_df, aes(x = factor(q_lvl), 
                    y = grate, fill = factor(q_lvl))) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribution of Growth Rates by Quantile",
    x = "Quantile Position",
    y = "Growth Rate (Coefficient on Year)",
    fill = "Quantile Position"
  )


# zip has already been used to merge in district


df=reg_comp %>%  filter(!is.na(zip)) %>% 
  dplyr::select(cin, state, district, category, zip,
                activity_description, activity_code, year,
                authorized_capital, paidup_capital) %>% 
  rename(district_zip=district) %>% 
  distinct()

# int=st_intersection(zsub, acsub)
# int_area=st_area(int)

# zip_centroids_sample=zip_centroids %>%  sample_frac(0.5)
# ac_centroids_sample=ac_centroids %>%  sample_frac(0.5)
# 
# tic()
# dist=st_distance(zip_centroids_sample, ac_centroids_sample)
# toc()




district_year_counts <- df %>%
  group_by(district_zip, year) %>%
  summarise(lg_num_firms = log(n_distinct(cin)),
            num_firms = n_distinct(cin)) 

district_stat=district_year_counts %>% filter(year<=2019) %>% 
  group_by(district_zip) %>% 
  summarise(sd_nfirms=sd(num_firms, na.rm=T), 
            mean_nfirms=mean(num_firms, na.rm=T))
            

# Create a histogram with density measure
ggplot(district_year_counts, aes(x = lg_num_firms)) +
  #geom_histogram(binwidth = .5, fill = "blue", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "red") +
  labs(
    title = "Histogram Distribution of Number of Firms at District Level",
    x = "Number of Firms in District",
    y = "Density"
  ) +
  theme_minimal()






# ANA: YEAR WISE DISTRIBUTION ---------------------------------------------

df=mca_acyr

setDT(df)  # Convert 'df' to a data table

yearly_count <- df[!is.na(fi_year), .(num_firms = sum(roc_year_act_count)), by = .(fi_year)]


yearly_count=df %>% 
  dplyr::select(fi_year, roc_year_act_count) %>% 
  distinct() %>% 
  group_by(fi_year) %>% 
  summarise(num_firms=base::sum(roc_year_act_count)) %>% 
  filter(!is.na(fi_year))

ggplot(yearly_count, aes(x = fi_year, y = num_firms)) +
  geom_bar(stat = "identity", fill = "chocolate", 
           color = "black", width=0.3) +
  labs(
    title = "Year-wise New Firms Count",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal()





# ANA: ZIP LEVEL VARIATION ------------------------------------------------

zip_year_count=df %>% 
  group_by( zip, year) %>% 
  summarise(num_firms=n_distinct(cin)) %>% 
  ungroup()
  



# ANA: AC LEVEL VARIATION -------------------------------------------------




#OLD:
# This above is AC being pulled in by zipcode that are on the boundary. 
# These AC are not actually split, rather these AC are at the boundary of district
# For eg, 4 or 5 dist_count mean there is a 4 or 5 zip codes, each belonging to 
#  separate districts that intersects with this one AC. Therefore, this AC is super at the boundary ( of amy districts)


df=mca_acyr

firm_count_df=df %>% mutate(state_ac=as.factor(paste(st_code_ac, ac_no, sep="-")),
                      state_dist=as.factor(paste(st_code_ac, district, sep="-"))) %>% 
        group_by(state_ac, fi_year) %>%  filter(fi_year<=2019) %>% 
  mutate(ac_year_firm_count=sum(ac_year_act_count)) %>% ungroup() %>% 
  group_by(state_dist, fi_year) %>% 
  mutate(dist_year_firm_count=sum(dist_year_act_count)) %>% 
  ungroup() %>% 
  group_by(state_ac) %>% 
  mutate(ac_firm_count=mean(ac_year_firm_count)) %>% ungroup() %>% 
  mutate(ac_demeaned=ac_year_firm_count-ac_firm_count) %>% 
  dplyr::select(state_ac, state_dist, fi_year, ac_year_firm_count, 
                dist_year_firm_count, ac_firm_count, ac_demeaned) %>% 
  distinct()


fml=as.formula(paste0("ac_firm_count~ 1| state_dist"))
reg1=felm(fml, firm_count_df)
fe_df=getfe(reg1)

# > var(fe_df$effect)/var(firm_count_df$ac_year_firm_count)
# [1] 0.1914182.  ( for till 2019: 0.3538774)

# > var(fe_df$effect)/var(firm_count_df$dist_year_firm_count)
# [1] 0.1410943

# > var(fe_df$effect)/var(firm_count_df$ac_firm_count).  #"ac_firm_count~ 1| state_dist"
# [1] 0.1440171



# Running the same regression separately for each district
dist_vec=unique(ac_year_count_pre19$district_ac)
beta=list();tval=list()
for(d in 1:length(dist_vec)){
  print(d)
  dt=ac_year_count_pre19 %>%  filter(district_ac==dist_vec[d])
  fml=as.formula("num_firms~boundary_idx|year")
  if(var(dt$boundary_idx)!=0){
    reg=lfe::felm(fml, dt )
    beta[[d]]=summary(reg)$coefficients[1,1]
    tval[[d]]=summary(reg)$coefficients[1,3]
  }
  
}

summary(unlist(beta))

              
            


dd=ac_year_count_pre19 %>%  group_by(district, year) %>% 
  summarise(dy_mean=mean(num_firms))


# Create a histogram with density measure
ggplot(df_ac, aes(x = lg_num_firms)) +
  #geom_histogram(binwidth = .5, fill = "blue", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "red") +
  labs(
    title = "Histogram Distribution of Number of Firms at District Level",
    x = "Number of Firms in District",
    y = "Density"
  ) +
  theme_minimal()


