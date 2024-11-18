
rm(list=ls())
source("code/functions.R")



## NOTE:
##
# I am trying to merge the following pieces:
# 1. Trivedi election data- it has candidate level 
#   election information 
# 2. Candidate affidavit information 
#   ( wealth, criminal status, education, etc)
# 3. Constituency level social and economic indicators. 

# 4. MCA yearly firm flow at AC level


# OUTPUT 1: SPATIAL MAP OF AVG CRIMINAL CHARGES
# OUTPUT 2: SPATIAL MAP OF AVG CANDIDATE WEALTH
# OUTPUT 3: SPATIAL MAP OF Avg Incumbant wealth growth rate
# OUTPUT 4: SPATIAL CORRELATION WITH SEC

# TRIVEDI ELECTION DATA ---------------------------------------------------

# From 1961 to 2023, election data for every AC. 
# This data contains, candidate level election outcome
# for each of these AC elections( including bye-elections)
# 


rawdt=read_csv("../Data/TCPD/AC/All_States_AE.csv") %>% 
              clean_names()


tcpd_varlist=c("pid", "assembly_no", "tcpd_year", 
               "contested", "vote_share_percentage",
               "position", "constituency_name", 
               "state_name", "candidate","sex","tcpd_party",
               "candidate_type","n_cand","no_terms",
               "turncoat","incumbent","recontest","margin_percentage")


state_code=read_excel("../Data/TCPD/state_code.xlsx")
state_code$state_name[state_code$state_name=="Daman & Diu"]="Goa_Daman_&_Diu"
state_code$state_name[state_code$state_name=="NCT of Delhi"]="Delhi"
state_code$state_name[state_code$state_name=="Jammu and Kashmir"]="Jammu_&_Kashmir"


state_code=state_code %>% 
  mutate(state_name=str_replace_all(state_name, " ", "_")) %>% 
  dplyr::select(state_name, state_id)

# creating state+assembly_no+ac_id+poll_no+position level dt
# Creating sh_election_id to mimic SHRUG data. 
ac_election=rawdt %>% inner_join(state_code) %>% 
        mutate(assembly_no=padzero(assembly_no, 2), 
               constituency_no=padzero(constituency_no, 3),
               state_id=padzero(state_id, 2)) %>% 
        mutate(sh_election_id=paste(state_id, assembly_no, 
                                    constituency_no,poll_no, sep="-")) %>% 
        mutate(sh_election_id=str_replace(sh_election_id, "-0$","")) %>% 
        mutate(sh_cand_id=paste(sh_election_id, position, sep="-")) %>% 
        rename(tcpd_age=age) %>% 
        rename(tcpd_party=party) %>% 
        rename(tcpd_year=year, ac_no=constituency_no) %>% 
        mutate(win=(position==1)) %>% 
         mutate(state_const=paste(state_name, constituency_name, 
                                  sep="-")) %>% ungroup() %>% 
  mutate(win_margin=if_else(position==1, margin_percentage, NA_real_))
# RECONSIDER: the name: sh_election_id or 
# sh_cand_id ( this is not consistent across election for same individual)

# Fixing NOTA observation
ac_election=ac_election %>% 
  mutate(pid=if_else(is.na(pid), "NOTA", pid))


cand_lvl=ac_election %>% 
  mutate(state_const=paste(state_name, constituency_name, sep="-")) %>% 
  dplyr::select(pid, assembly_no, tcpd_year, contested, 
                vote_share_percentage,
                position, 
                win, state_const, constituency_name, 
                state_name, all_of(tcpd_varlist)) %>% distinct()
# This above is a candidate level dataset. 
# For every candidate( pid), it has election outcomes.

              


# CANDIDATE CHARACTERISTICS -----------------------------------------------

adr_cand_x=read_dta("../Data/SHRUG/ELECTION/shrug-adr-elections-cand-dta/affidavits_clean.dta")
# Data on political candidate characteristics 
# (chiefly, assets and criminal charges) from affidavits filed 
# with the Electoral Commission of India from 2003-2017. 
# Merges to election results data on sh_cand_id / sh_election_id, 
# and to SHRUG on ac07_id / ac08_id.
# NEED TO EXTEND IT TO 2022. 



adr_elec=read_dta("../Data/SHRUG/ELECTION/shrug-adr-elections-ac-dta/affidavits_ac.dta")
tr_cand=read_dta("../Data/SHRUG/ELECTION/shrug-triv-cand-dta/trivedi_candidates_clean.dta")

#tr_cand2= tr_cand %>% filter(sh_cand_id %in% c2_s)




# CONSITTUENCY SOCIO ECONOMIC INDICATOR -----------------------------------

## SHRUG ECONOMIC CENSUS
ec98a= read_dta("../Data/SHRUG/EC/shrug-ec98-dta/ec98_con07.dta")
ec98b= read_dta("../Data/SHRUG/EC/shrug-ec98-dta/ec98_con08.dta")
ec98=bind_rows(ec98a, ec98b) %>% distinct() %>% 
      mutate(ac_id=coalesce(ac07_id, ac08_id)) %>% 
  dplyr::select(-ac07_id, -ac08_id)


ec05a=read_dta("../Data/SHRUG/EC/shrug-ec05-dta/ec05_con07.dta")
ec05b=read_dta("../Data/SHRUG/EC/shrug-ec05-dta/ec05_con08.dta")
ec05=bind_rows(ec05a, ec05b) %>%  distinct()%>% 
  mutate(ac_id=coalesce(ac07_id, ac08_id)) %>% 
  dplyr::select(-ac07_id, -ac08_id)

ec13a=read_dta("../Data/SHRUG/EC/shrug-ec13-dta/ec13_con07.dta")
ec13b=read_dta("../Data/SHRUG/EC/shrug-ec13-dta/ec13_con08.dta")
ec13=bind_rows(ec13a, ec13b) %>%  distinct()%>% 
  mutate(ac_id=coalesce(ac07_id, ac08_id)) %>% 
  dplyr::select(-ac07_id, -ac08_id)

ec_all=ec98 %>% full_join(ec05, by="ac_id") %>% full_join(ec13, by="ac_id")



# POPULATION CENSUS
pc11_shrid= read_dta("../Data/SHRUG/PC/shrug-td11-dta/pc11_td_clean_shrid.dta")
pc11_ac07= read_dta("../Data/SHRUG/PC/shrug-td11-dta/pc11_td_clean_con07.dta")








# MERGE SECTION -----------------------------------------------------------

## MERGE 1: ELECTION DATA WITH CANDIDATE X ( both inner and full)

elec_candX_full=ac_election %>% left_join(adr_cand_x)
elec_candX_inner=ac_election %>% inner_join(adr_cand_x)




##

# -----  MERGE 2: Merging with AC level yearly firm flow from MCA ---------------



zip_ac_map=read_rds("data/interm/zip_ac_map_intx.rds") %>% as.data.frame() %>% 
  dplyr::select(pincode, ac_intersects, district_ac, 
                state_id, ac_no, pc_no) %>% 
  rename(zip=pincode) %>% 
  mutate(state_id=padzero(state_id, 2), 
         ac_no=padzero(ac_no, 3)) %>% 
  distinct()

reg_comp=read_rds("data/clean/MCA_registered_company.rds")

# zip has already been used to merge in district

df=reg_comp %>%  filter(!is.na(zip)) %>% 
  dplyr::select(cin, state, district, category, zip,
                activity_description, activity_code, year,
                authorized_capital, paidup_capital) %>% 
  rename(district_zip=district) %>% 
  distinct()


ac_firms=df %>% left_join(zip_ac_map) # This is at zip-year level of firm flow
# Joining with `by = join_by(zip)`
ac_firms= ac_firms %>% 
  group_by(state_id, ac_no) %>% 
  mutate(boundary_idx=n_distinct(district_zip)) %>% ungroup()
# This above is AC being pulled in by zipcode that are on the boundary. 
# These AC are not actually split, rather these AC are at the boundary of district
# For eg, 4 or 5 dist_count mean there is a 4 or 5 zip codes, each belonging to 
#  separate districts that intersects with this one AC. Therefore, this AC is super at the boundary ( of amy districts)


ac_year_flow=ac_firms %>% group_by(state_id, ac_no,year) %>% 
  mutate(firm_flow=n_distinct(cin)) %>% 
  dplyr::select(ac_intersects, state_id, ac_no, year, 
                district_ac, pc_no, firm_flow, boundary_idx) %>% ungroup() %>% 
  distinct()

ac_avg_flow=ac_year_flow %>% group_by(state_id, ac_no) %>% 
  mutate(firm_flow=mean(firm_flow, na.rm=T)) %>% 
  dplyr::select(ac_intersects, state_id, ac_no, 
                district_ac, pc_no, firm_flow, boundary_idx) %>% 
  distinct()

ac_avg_winmargin=ac_election %>% group_by(ac_no, state_id) %>% 
  filter(position==1) %>% 
  mutate(avg_win_margin=mean(win_margin, na.rm=T)) %>% 
              dplyr::select(sh_election_id,state_id, ac_no, 
                  constituency_name, state_name,
                  avg_win_margin) %>%
    distinct()

ac_ele_firm=ac_avg_winmargin %>%  inner_join(ac_avg_flow, 
                                            by=c( "state_id", "ac_no"))

# Stats for inner_join: MOSTLY JUST YEAR MISMATCH
# > (ac_ele_firm_flow %>% dplyr::select(ac_year, state_id, ac_no) %>% distinct() %>% nrow())/(ac_election %>% dplyr::select(ac_year, state_id, ac_no) %>%  distinct() %>%  nrow())
# [1] 0.1014211
# > (ac_ele_firm %>% dplyr::select( state_id, ac_no) %>% distinct() %>% nrow())/(ac_avg_winmargin %>% dplyr::select(state_id, ac_no) %>%  distinct() %>%  nrow())
# [1] 0.8635846

# > (ac_ele_firm %>% dplyr::select( state_id, ac_no) %>% distinct() %>% nrow())/(ac_avg_winmargin %>% dplyr::select(state_id, ac_no) %>%  distinct() %>%  nrow())
# [1] 0.9300843


# CAUTION: AC_NO might mean different AC in different dataset. CHECK


fml=as.formula(paste0("avg_win_margin~firm_flow|district_ac"))
reg=lfe::felm(fml, ac_ele_firm); summary(reg)


#Joining with `by = join_by(sh_election_id, sh_cand_id)`
# This merge is good inner_join is 0.97 success rate

# In the above merge, there is only one snag that is because 
#  adr_cand_x has a mistagged entry for bye-election. See it by: 
# View(adr_cand_x %>% filter(sh_cand_id=="28-13-122-2"))


# Getting election id and win_margin

# ac_winmargin=ac_election %>% filter(position==1) %>% 
#   dplyr::select(sh_election_id,state_id, constituency_no, ac_year,
#                 constituency_name, state_name, 
#                 win_margin) %>% 
#   distinct()
# 
# state_ac_map=ac_election %>% dplyr::select(state_id, constituency_no) %>%  distinct()
# ac_shp <- st_read("../Data/MAPS- INDIA/AC/maps-master/assembly-constituencies/India_AC.shp") %>% 
#   st_make_valid() %>% mutate(DIST_NAME=tolower(DIST_NAME), 
#                              ST_NAME=tolower(ST_NAME))
# state_ac_map2=ac_shp %>% as.data.frame() %>% clean_names() %>% 
#   mutate(st_code=padzero(st_code, 2), 
#          ac_no=padzero(ac_no, 3)) %>% 
#   rename(state_id=st_code, 
#          constituency_no=ac_no) %>% dplyr::select(state_id, constituency_no) %>% 
#   distinct()
#   
# mm=state_ac_map %>%  inner_join(state_ac_map2)

# ELECTION DATA ANALYSIS --------------------------------------------------

## Here I am attempting basical electoral profile at 
#   constituency level. I am computing the following for last 3 AC, before 2003:
# 1. Pr(switching out the incumbent )
# 2. Avg Win margin across three election


p_incwin=elec_candX_full %>% 
  mutate(inc_win=(incumbent==1 & win==1)) %>% 
  mutate(inc_win=as.numeric(inc_win)) %>% 
  group_by(sh_election_id) %>% 
  mutate(inc_win=max(inc_win)) %>% 
  group_by(state_name, assembly_no) %>% 
  mutate(state_party=names(which.max(table(ac_party)))) %>% 
  dplyr::filter(win==1) %>% ungroup() %>% 
  dplyr::select(sh_election_id, assembly_no,state_const,
                inc_win, margin_percentage, state_party) %>% 
  distinct() %>% 
  group_by(state_const) %>% 
  arrange(state_const, assembly_no) %>% 
  mutate(p_inc_win=rollapply(inc_win, 
                             3, mean, fill = NA, align = "right"),
         avg_margin=rollapply(margin_percentage, 
                              3, mean, fill = NA, align = "right")) %>% 
  dplyr::select(-inc_win, -margin_percentage)


elec_candX_full2=elec_candX_full %>% inner_join(p_incwin)

write_rds(elec_candX_full2, 
          file="data/clean/elec_cand.rds", 
          compress = "gz")


dd=read_html("https://myneta.info/uttarpradesh2022/candidate.php?candidate_id=4621")

# shp_path="../Data/MAPS- INDIA/AC/maps-master/assembly-constituencies/India_AC.shp"
# 
# shp <- st_read(shp_path)
# 
# # Set tmap_options to check and fix invalid polygons
# tmap_options(check.and.fix = TRUE)
# # Create a simple map using tmap
# tm_shape(shp) +
#   tm_borders()











## SECTION CREATING CONSTITUENCY LEVEL DATA
elec_candX_inner=election_df
winner_crime_df= elec_candX_inner %>% filter(win==T) %>% 
      #filter(!is.na(ac08_id)) %>%
      #filter(ac08_id!="") %>% # picking only elections post 08 delination
          dplyr::select(ac_id, year, num_crim, state_id, crime_major,
                        constituency_name,assembly_no) %>% 
  distinct() %>% mutate(assembly_no=as.numeric(assembly_no)) %>% 
  group_by(state_id) %>% 
  mutate(e_count=n_distinct(assembly_no)) %>% 
  ungroup()


ff <- ff %>%
  group_by(state_id) %>%
  arrange(state_id, assembly_no) %>%
  mutate(change = assembly_no - Lag(assembly_no,)) %>%
  mutate(change=if_else(is.na(change),0,change)) %>% 
  mutate(new_var = cumsum(change == 1) +1) %>%
  dplyr::select(-change) %>%  # Remove the 'change' column, it was just an auxiliary column
  ungroup()  # To remove the grouping

state_crime_trend=winner_crime_df %>% group_by(state_id) %>% 
  arrange(state_id, assembly_no) %>% 
  mutate(change = assembly_no - Lag(assembly_no,)) %>%
  mutate(change=if_else(is.na(change),0,change)) %>% 
  mutate(eno = cumsum(change == 1) +1) %>% 
  group_by(state_id, assembly_no) %>% 
  mutate(total_crime=sum(num_crim),
         avg_crime_major=mean(crime_major, na.rm=T)) %>% ungroup() %>% 
  dplyr::select(state_id, eno, total_crime, avg_crime_major) %>% distinct()

agg_crime_trend=state_crime_trend %>% group_by(eno) %>% 
  summarise(total_crime=mean(total_crime, na.rm=T), 
            avg_crime_major=mean(avg_crime_major, na.rm=T))





# winner_crime_df2= elec_candX_inner %>% filter(winner==1) %>% 
#   filter(!is.na(ac08_id)) %>%
#   filter(ac08_id!="") %>% # picking only elections post 08 delination
#   dplyr::select( ac08_id,year, num_crim, state_name, 
#                  constituency_name,assembly_no) %>% 
#   distinct() %>% mutate(assembly_no=as.numeric(assembly_no)) %>% 
#   group_by(ac08_id) %>% 
#   mutate(e_count=n_distinct(assembly_no)) %>% 
#   ungroup()


# ISSUE: It seems like ac_id is not consistent for 
#  same state+constituency. Which is weird and needs to be looked into further


# USing state_name+ constituency_name for now

                        
View(winner_crime_df %>% ungroup() %>%
       arrange(state_name,constituency_name, assembly_no,year,ac_id,e_count) %>%
       dplyr::select(ac_id,assembly_no,constituency_name, year, state_name,e_count ) %>%
       distinct() %>% filter(state_name=="Andhra_Pradesh"))

  
# Compute baseline value and trend for each geographical unit
  baseline_trend <- winner_crime_df %>% filter(e_count>1) %>% 
    filter(!is.na(num_crim)) %>% 
    group_by(state_name, constituency_name) %>%
    arrange(state_name, constituency_name, assembly_no) %>% 
    mutate(base_crime=first(num_crim), 
           end_crime=last(num_crim)) %>% 
    mutate(crime_delta=end_crime-base_crime) %>% 
    dplyr::select(state_name, constituency_name, 
                  base_crime, end_crime, crime_delta) %>% 
    distinct() %>% 
    mutate(base_crime=base_crime+1, 
           end_crime=end_crime+1)
  
  
  
  
  
  # Assign values greater than 6 to 7
  baseline_trend$base_crime[baseline_trend$base_crime > 7] <- 8
  baseline_trend$end_crime[baseline_trend$end_crime > 7] <- 8
  
  # Create a matrix to store mobility counts
  mobility_matrix <- matrix(0, ncol = 8, nrow = 8)  # 0-7 values
  
  # Loop through the dataset and update mobility counts
  for (i in 1:nrow(baseline_trend)) {
   #print(i)
     mobility_matrix[baseline_trend$base_crime[i],
                    baseline_trend$end_crime[i]] <- mobility_matrix[baseline_trend$base_crime[i],
                                                                    baseline_trend$end_crime[i]] + 1
  }

mobility_df <- as.data.frame(mobility_matrix)

mobility_df <- mobility_df %>% 
    mutate(row_sum = rowSums(.)) %>% 
    mutate(across(starts_with("V"), ~ . / row_sum )) %>%
    dplyr::select(-row_sum) %>% 
    mutate(across(starts_with("V"), spec_dec, 2))



# Define row and column names (replace with your actual names)
row_names <- c("0", "1", "2", "3", "4",
               "5","6","7")
col_names <- c("0", "1", "2", "3", "4",
               "5","6","7")
heatmap_data <- expand.grid(row = row_names, col = col_names)
heatmap_data$value <- as.vector(as.matrix(mobility_df))
heatmap_data=heatmap_data %>% mutate(value=as.numeric(value))

# Create the heatmap using ggplot2
heatmap_plot <- ggplot(heatmap_data, aes(x = col, y = row, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "# of charges -end", y = "# of charges -baseline",
       title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Display the heatmap
print(heatmap_plot)


  
  # Add row and column names
  row.names(mobility_df) <- colnames(mobility_df) <- 0:7
  
  mdf <- expand.grid(base_crime = 1:8, end_crime = 1:8, prop=NA)
  
  for( i in 1:nrow(mdf)){
    mdf$prop[i]=mobility_df[mdf$base_crime[i], mdf$end_crime[i]]
    
  }
mdf$prop=as.numeric(mdf$prop)*5
  
  
  plot(NA, NA, xlim = c(0, 1), ylim = c(0, 8), 
       xlab = "", ylab = "Start Value")
  axis(2, at = 0:8, labels = 0:8)
  axis(4, at = 0:8, labels = 0:8, col.axis = "red")
  segments(0,mdf$base_crime, 1, mdf$end_crime, col="azure4",
           lwd=mdf$prop)
  
 
  
  
 # Education
  
winner_df= elec_candX_inner %>% filter(winner==1)
edu_trend=winner_df %>% 
  filter(!is.na(ed)) %>% 
  group_by(state_name, constituency_name) %>% 
  mutate(e_count=n_distinct(assembly_no)) %>% 
  filter(e_count>1) %>% 
  #filter(!is.na(num_crim)) %>% 
  group_by(state_name, constituency_name) %>%
  arrange(state_name, constituency_name, assembly_no) %>% 
  mutate(base_ed=first(ed), 
         end_ed=last(ed)) %>% 
  #mutate(crime_delta=end_crime-base_crime) %>% 
  dplyr::select(state_name, constituency_name, 
                base_ed, end_ed) %>% 
  distinct() 

# Assets 
winner_df= elec_candX_inner %>% filter(winner==1)
asset_trend=winner_df %>% 
  filter(!is.na(assets)) %>% 
  group_by(state_name, constituency_name) %>% 
  mutate(e_count=n_distinct(assembly_no)) %>% 
  filter(e_count>1) %>% 
  #filter(!is.na(num_crim)) %>% 
  group_by(state_name, constituency_name) %>%
  arrange(state_name, constituency_name, assembly_no) %>% 
  mutate(base_asset=first(assets), 
         end_asset=last(assets)) %>% 
  #mutate(crime_delta=end_crime-base_crime) %>% 
  dplyr::select(state_name, constituency_name, 
                base_asset, end_asset) %>% 
  distinct() 


  

## Merging with EC

ele_ec=ele_cand_x %>%  full_join(ec_all, by="ac_id")





# DSCRIPTIVES ----------------------------------------------------------------

# 1. Winning Vote share across time 
# 2. Number of candidates per election across time

df=cand_elec_df
df1= df %>% group_by(pid) %>% 
      mutate(count=n()) %>% 
      group_by(state_name) %>% 
      summarise(count_perstate=mean(count))



