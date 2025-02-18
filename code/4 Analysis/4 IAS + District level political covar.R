
rm(list=ls())
source("code/functions.R")



#----------------------------------------------
  # NOTE
# 1. This is yet another attempt to merge the IAS and 
   # the district level political covariates. 
  
# 2. The idea now is to have both MP and MLA covar in the 
   # same dataset and try to construct individual as well as joint measures
  
# 3. Then the idea is to see how doe sthe IAS level metrics responds to the 
  # political changes. 
  
#----------------------------------------------

# 0 HEADER ---------------------------------------------------------------

standardize_state_names <- function(state_name) {
  
  state_name=tolower(state_name) 
  state_name <- dplyr::case_when(
    state_name %in% c("uttarakhand", "uttarkhand") ~ "uttaranchal",
    state_name == "orissa" ~ "odisha",
    state_name %in% c("arunanchal pradesh") ~ "arunachal pradesh",
    state_name %in% c("jammu and kashmir", "jammu & kashmir",
                      "jammu_&_kashmir","jammu kashmir") ~ "jammu and kashmir",
    state_name == "tamilnadu" ~ "tamil nadu",
    state_name %in% c("delhi", "national capital territory of delhi", 
                      "delhi & ncr") ~ "delhi & ncr",
    state_name == "puducherry" ~ "pondicherry",
    state_name %in% c("andaman & nicobar islands") ~ "andaman & nicobar island",
    state_name == "dadra & nagar haveli" ~ "dadara & nagar havelli",
    state_name == "daman and diu" ~ "daman & diu",
    TRUE ~ state_name  # Retain unchanged state names
  )
  
  # Replace spaces with underscores in the resulting state names
  state_name <- stringr::str_replace_all(state_name, " ", "_")
  
  return(state_name)
}

# Define a lookup table for abbreviations
phase_abbreviations <- c(
  "Preliminary Filing and Expression of Interest" = "pre_filing",
  "Negative or Termination Events" = "neg_term",
  "Additional/Parallel Agreements and Approvals" = "add_agree",
  "Final Regulatory Clearances" = "final_clear",
  "Land-Related Processes" = "land_proc",
  "Initial Regulatory Engagement (Application/Preliminary Approvals)" = "init_reg",
  "Detailed Review and Expert Scrutiny" = "review_scrutiny",
  "Post-Clearance Contractual and Implementation Adjustments" = "post_clear",
  "In-Principle Approvals and Further Governmental Endorsements" = "principle_appr"
)

# 1 READING THE MERGED PANEL ----------------------------------------------

date="2025-02-13"
ge_ias=qread(paste0("data/3 MERGED SAMPLES/ge_ias_merged_",date,".qs"))
ge_ias=ge_ias %>% 
  mutate_at(c("year"), as.character) %>%
  mutate_at(c("month"), as.character) %>% 
  mutate_at(c("month"),padzero, 2)



ge_ias_select=ge_ias %>% 
  dplyr::select(-assembly_no,  -ias_name,  -id,
                -cadre_st, -active, -id_centre_deput, -post_no, -cohort,
                -month_in_service, -grade) %>% 
  distinct()

date="2025-02-10"
ae_ias=qread(paste0("data/3 MERGED SAMPLES/ae_ias_merged_",date,".qs"))
ae_ias=ae_ias %>% 
  mutate_at(c("year","start_year", "end_year"), as.character) %>%
  mutate_at(c("month", "start_month", "end_month"), as.character) %>% 
  mutate_at(c("month", "start_month", "end_month"), padzero, 2)

#capex_df=qread("data/2 CLEAN/capex_time_geo_df.qs")
capex_panel=qread("data/2 CLEAN/capex_time_geo_full_panel.qs")


ias_df=qread("data/2 CLEAN/tcpd_ias+state_exp.qs")

# id="AP043100"
# BOTTOMLINE: We have 144391 st_dist_year_month units that are 
# matched across both AE and GE merges with same set of IAS officers.

# ROUGH
# Comparing AE and GE merges
# The geo, time and geo_time units should be same across both data


# 2 EFFECT ON IAS POSTINGS (START/END) AS A FUNCTION OF ELECTIONS------------------------

# here in the end I am mapping how does the count of 
# post start and end instances vary over the political assembly. 
# and does that pattern vary by some politician X

 # --- AE==----------
df=ae_ias
aeyear_idx_df = df %>% 
  dplyr::select(st_name, assembly_no, year, month) %>% 
  distinct() %>% 
  # Group by st_name to process within each state
  group_by(st_name, assembly_no) %>%
  arrange(st_name, assembly_no, year, month) %>%
  mutate(ass_time=row_number(), 
         max_ass_time=max(ass_time,na.rm=T)) %>% ungroup() %>% 
  mutate(eyear_idx=ass_time<=12, 
         eyear_idx_lead=max_ass_time-ass_time<=12) %>% 
  dplyr::select(-max_ass_time) %>% distinct() %>% 
  mutate(ass_year=floor(ass_time/12)+1)


ae_ias_posting_count=df %>% 
  mutate(dist_time=paste0(st_name, "_", dist_name, "_", year, "_", month)) %>%
  mutate(dist_state=paste0(st_name, "_", dist_name)) %>%
  dplyr::select(st_name,dist_state,  assembly_no,
                year, month, dist_time, id,  ac_count,
                start_year, start_month, end_year, end_month,
                stb_mla0_count,stb_mla4_count) %>% 
  distinct() %>% 
  inner_join(aeyear_idx_df) %>% 
  group_by(dist_state, year) %>% 
  mutate(stb_mla0_ind=any(stb_mla0_count>2, na.rm=T),
         stb_mla4_ind=any(stb_mla4_count>1,na.rm=T)) %>% ungroup() %>%
  mutate(post_start=if_else(year==start_year & month==start_month, 
                            1, 0)) %>% 
  mutate(post_end=if_else(year==end_year & month==end_month,
                          1, 0)) %>%
  group_by(dist_time) %>% 
  mutate(ae_dist_time_id_count = n_distinct(id[!is.na(id)])) %>% 
  ungroup() %>%  group_by(dist_state, year) %>% 
  mutate(mean_off_pccount_distyear =mean(ae_dist_time_id_count, 
                                       na.rm=T)/mean(ac_count, 
                                                     na.rm=T), 
         mean_off_count_distyear =mean(ae_dist_time_id_count, 
                                        na.rm=T),
         sd_off_count_distyear=sd(ae_dist_time_id_count, na.rm=T)) %>% 
  group_by(st_name,year) %>% 
         mutate(post_start_count=sum(post_start, na.rm=T), 
         post_end_count=sum(post_end, na.rm=T)) %>%
  ungroup() %>% group_by(dist_state) %>%
  mutate(mean_off_count_dist =mean(ae_dist_time_id_count, na.rm=T),
         mean_off_pccount_dist =mean(ae_dist_time_id_count, 
                                     na.rm=T)/mean(ac_count, 
                                                   na.rm=T),
         sd_off_count_dist=sd(ae_dist_time_id_count, na.rm=T)) %>%
  ungroup() %>% 
  mutate(has_officer = if_else(ae_dist_time_id_count > 0, 1, 0)) %>% 
  group_by(dist_time) %>%
  mutate(has_officer = max(has_officer, na.rm=T)) %>% ungroup()

# Now, create an indicator at the dist_time (year_month) resolution:
dist_year_extensive <- ae_ias_posting_count %>% 
  dplyr::select(dist_state, year, dist_time, has_officer) %>% 
  distinct() %>% 
  group_by(dist_state, year) %>%
  summarise(months_with_any_officer = sum(has_officer, na.rm=T)) %>% 
  ungroup()#

# Now, following data should have info 
#. about density of time series at state_district level. 

ae_ias_posting_count=ae_ias_posting_count %>% 
  inner_join(dist_year_extensive) #

ls_year1=c("1991", "1996", "1999", "2004", "2009", "2014","2019")
ls_year2=c("1993","2001","2006","2011","2016","2021","2007")
dd=ae_ias_posting_count %>% #filter(post_end_count>0) %>% 
  dplyr::select(st_name,  ass_year, year,stb_mla4_ind,
                stb_mla0_count, stb_mla4_count,
                post_start_count, post_end_count) %>% distinct() 

fml=as.formula(paste0("post_end_count~ass_year*stb_mla0_count |st_name"))
summary(felm(fml, data=dd  ))

# for both start and end, the coeff is consistent
# around 15% more churn right after election and decreasing 
# monotonically

# --- GE==----------


df=ge_ias
geyear_idx_df = df %>% 
  dplyr::select(st_name, assembly_no, year, month) %>% 
  distinct() %>% 
  # Group by st_name to process within each state
  group_by(st_name, assembly_no) %>%
  arrange(st_name, assembly_no, year, month) %>%
  mutate(ass_time=row_number(), 
         max_ass_time=max(ass_time,na.rm=T)) %>% ungroup() %>% 
  mutate(eyear_idx=ass_time<=12, 
         eyear_idx_lead=max_ass_time-ass_time<=12) %>% 
  dplyr::select(-max_ass_time) %>% distinct() %>% 
  mutate(ass_year=floor(ass_time/12)+1)

geyear_idx_df=geyear_idx_df %>% 
  filter(!(st_name=="chhattisgarh" & ass_year>7))


ias_posting_count=df %>% 
  mutate(dist_time=paste0(st_name, "_", dist_name, "_", year, "_", month)) %>%
  mutate(dist_state=paste0(st_name, "_", dist_name)) %>%
  dplyr::select(st_name,dist_state,  assembly_no,
                year, month, dist_time, id,  
                start_year, start_month, end_year, end_month,
                stb_mp0_count,stb_mp4_count) %>% 
  distinct() %>% 
  inner_join(geyear_idx_df) %>% 
  group_by(dist_state, year) %>% 
  mutate(stb_mp0_ind=any(stb_mp0_count>2, na.rm=T),
         stb_mp4_ind=any(stb_mp4_count>1,na.rm=T)) %>% ungroup() %>%
  mutate(post_start=if_else(year==start_year & month==start_month, 
                            1, 0)) %>% 
  mutate(post_end=if_else(year==end_year & month==end_month,
                          1, 0)) %>%
  group_by(dist_time) %>% 
  mutate(ge_dist_time_id_count = n_distinct(id[!is.na(id)])) %>% 
  ungroup() %>% 
  group_by(st_name,year) %>% 
  mutate(post_start_count=sum(post_start, na.rm=T), 
         post_end_count=sum(post_end, na.rm=T)) %>%
  ungroup() 

# Now, create an indicator at the dist_time (year_month) resolution:
#PENDING

# Now, following data should have info 
#. about density of time series at state_district level. 


ls_year1=c("1991", "1996", "1999", "2004", "2009", "2014","2019")
ls_year2=c("1993","2001","2006","2011","2016","2021","2007")
dd=ias_posting_count %>% #filter(post_end_count>0) %>% 
  dplyr::select(st_name,  ass_year, year,stb_mp4_ind,
                stb_mp0_count, stb_mp0_ind, stb_mp4_count,
                post_start_count, post_end_count) %>% distinct() 

fml=as.formula(paste0("post_start_count~stb_mp4_ind |st_name"))
summary(felm(fml, data=dd  ))






ge_merge=ge_ias %>%
  mutate(dist_time=paste0(st_name, "_", dist_name, "_", year, "_", month)) %>%
  mutate(dist_state=paste0(st_name, "_", dist_name)) %>%
  dplyr::select(st_name,dist_state,  year, month, dist_time, id) %>% 
  distinct() %>% 
  group_by(dist_time) %>%
  mutate(ge_dist_time_id_count= n_distinct(id[!is.na(id)])) %>% 
  ungroup()

ae_merge=ae_ias %>% 
  mutate(dist_time=paste0(st_name, "_", dist_name, "_", year, "_", month)) %>%
  mutate(dist_state=paste0(st_name, "_", dist_name)) %>%
  dplyr::select(st_name,dist_state,  year, month, dist_time, id) %>% 
  distinct() %>% 
  group_by(dist_time) %>%
  mutate(ae_dist_time_id_count= n_distinct(id[!is.na(id)])) %>% 
  ungroup()

ae_dt=ae_merge %>% 
  dplyr::select(dist_time, 
                ae_dist_time_id_count, id) %>% distinct() 

dd=ge_ias %>% filter(id %in% ae_dt$id) %>% 
  group_by(id) %>% summarise(n_id=n_distinct(dist_name)) %>%
  ungroup()


ge_dt=ge_merge %>% 
  dplyr::select(dist_time, 
                ge_dist_time_id_count, id) %>% distinct() 

ae_ge_join=ae_dt %>% inner_join(ge_dt) %>% 
  mutate(diff=ae_dist_time_id_count-ge_dist_time_id_count) %>% 
  inner_join(ae_merge) #

ae_ge_join2=ae_dt %>% dplyr::select(-id) %>% distinct() %>%
  inner_join(ge_dt %>% dplyr::select(-id) %>% distinct()) %>% 
  mutate(diff=ae_dist_time_id_count-ge_dist_time_id_count) #
  
ae_ge_antijoin=ae_dt %>% anti_join(ge_dt) %>% 
  inner_join(ae_merge) #
  
ge_ae_antijoin=ge_dt %>% anti_join(ae_dt) %>% 
  inner_join(ge_merge) #

# 1. THere are more districts_months for Karnataka in AE than in GE for years 
# around 1998-2002 ish, most for 2000. 
# 2. THere are more districts_months for Maha in AE than in GE for years 
# around 2013-2017

# Overall there are around 24k more records in GE than AE merge. 
# most of it seems to be an artifact of new states
# There are around 10k unmatched records in AE than GE. 
# details of it are written just above. 

# Overall doesnt seem to be that big of a proble. Moving on. 

# ROUGH END


 

# ge_raw=read_csv("../Data/TCPD/General Election/GE_All_States_2024-11-14.csv") %>% 
#   clean_names()
# 
# ge_raw=ge_raw %>% 
#   arrange(pid, assembly_no) %>% # Ensure data is ordered by pid and assembly_no
#   group_by(pid) %>%
#   mutate(win_counter = cumsum(position == 1)) %>%
#   ungroup()
# 
# ge_win=ge_raw %>% filter(assembly_no>=9) %>% 
#   filter(position==1) %>% 
#   mutate(constituency_name= gsub("\\s*\\(.*?\\)", "", 
#                                  constituency_name)) %>% 
#   mutate(bizz=if_else(tcpd_prof_main %in% c("Business", 
#                                             "Small Business or Self-employed"), 1, 0)) %>%
#   mutate(politics=if_else(tcpd_prof_main %in% c("Politics"), 1, 0)) %>%
#   mutate(ex_gov=if_else(tcpd_prof_main %in% c("Former Government"), 1, 0)) %>%
#   rename(st_name=state_name) %>% 
#   mutate(st_name=standardize_state_names(st_name))




# 2 MEAUSRING CHURN AND OTHER STATS IN IAS POSTINGS -------------------

df=dist_level_posting#

# First, I want to count holes in a district level TS. 
  # For each district-year, % of months vacant
# Then holes in the IAS (id) level TS. 
#.  For each IAS oficer-year, % of months vacant

dist_year_count=df %>% 
  group_by(st_name, dist_name, year) %>% 
  summarise(dist_year_nid=n_distinct(id)) %>% ungroup() #

dist_year_holes=df %>% 
  group_by(st_name, dist_name, year) %>%
  summarise(
    dist_year_nid=n_distinct(id, na.rm=T),
    total_months = n_distinct(month, na.rm=T),  # Total number of months in the group
    missing_months = n_distinct(month[is.na(id)]),  # Count of months where id is NA
    percent_missing = (missing_months / total_months) * 100  # Compute percentage
  ) %>%
  ungroup()

# Around 47% of state-dist-year combination have 0 IAS officer
# For an average st-dist-year, no of missing months:
# > summary(dist_year_holes$missing_months)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   9.000   6.958  12.000  12.000 



ias_dist_yr_count=df %>% 
  filter(!is.na(id)) %>% 
  dplyr::select(id, st_name, dist_name, 
                 year) %>% distinct() %>% 
  group_by(st_name,dist_name, year) %>% 
  mutate(dist_year_count=n_distinct(id)) %>% ungroup() %>%
  arrange(st_name, dist_name, year) %>% 
  mutate(dist_year_delta=dist_year_count-dplyr::lag(dist_year_count)) %>% 
  dplyr::select(-id) %>% distinct()
# > summary(ias_dist_yr_count$dist_year_delta)
# Min. 1st Qu.  Median    Mean 3rd Qu. 
# -34       0       0       0       0 
# Max.    NA's 
     # 29       1 

ias_churn_intensive=df %>% 
  filter(!is.na(id)) %>% 
  filter(as.numeric(year) >=1999) %>%
  group_by(id, post_no) %>%
  mutate(post_start_year=min(year, na.rm=T), 
         post_end_year=max(year, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(st_name,dist_name, year) %>%
  mutate(id_incoming=sum(year==post_start_year), 
         id_outgoing=sum(year==post_end_year)) %>%
  ungroup() %>% 
  dplyr::select(st_name,dist_name, year,
                id_incoming, id_outgoing) %>% distinct()#




# 3. Linking MP ID (pid) with IAS id (id)------------------------------

## idea here is to see if a MP-IAS match can be a unit. 
## remember what I am trying to do is to find some notion of 
#  better coordination vs worse coordination. 
## length of a  MP-IAS match can be a good proxy for that, to start with
# one issue is that any match depends on all the other matches, so.. (solution?)


# 3.1. MP-IAS match length ----------------------------------------------


#candidate_type
df=ge_ias
ias_gpid_match=df %>% 
  ungroup() %>% 
  filter(!is.na(pid)) %>%
  filter(!is.na(id)) %>%
  mutate(portfolio_count=if_else(is.na(portfolio_count),0,
                                 portfolio_count),
         mos_count=if_else(is.na(mos_count),0,mos_count),
         mos_ic_count=if_else(is.na(mos_ic_count),0,mos_ic_count),
         cm_count=if_else(is.na(cm_count),0,cm_count)) %>%
  mutate(portfolio_count_ind=if_else(portfolio_count>0,1,0),
         mos_count_ind=if_else(mos_count>0,1,0),
         mos_ic_count_ind=if_else(mos_ic_count>0,1,0),
         cm_count_ind=if_else(cm_count>0,1,0)) %>%
  group_by(id, pid) %>% 
  mutate(pair_duration=n_distinct(interaction(year, month, drop = TRUE)), 
         pair_ndist=n_distinct(interaction(st_name,dist_name, drop = TRUE)), 
         pair_assembly=n_distinct(assembly_no),
         pair_posting=n_distinct(post_no),
         bizz=sum(bizz, na.rm=T),
         no_terms=max(no_terms, na.rm=T),
         politics=sum(politics, na.rm=T),
         ex_gov=sum(ex_gov, na.rm=T),
         id_centre_deput=max(id_centre_deput, na.rm=T)) %>%
  ungroup() %>% 
  dplyr::select(st_name,id, pid, starts_with("pair_") , no_terms, 
                bizz, 
                politics, ex_gov, portfolio_count, mos_count,
                mos_ic_count,cm_count,
                portfolio_count_ind, mos_count_ind,
                mos_ic_count_ind, cm_count_ind, id_centre_deput) %>%
                 distinct()

fml=as.formula(paste0("id_centre_deput~pair_assembly |st_name"))
summary(felm(fml, 
             data=ias_gpid_match  ))



# 2 MERGING GE IAS + AE IAS + CAPEX --------------------------------

ias_exp=qread("data/2 CLEAN/tcpd_ias+state_exp.qs")
ias_exp=ias_exp %>% 
  mutate(cadre_st=standardize_state_names(cadre_st)) %>% 
  mutate(st_name=cadre_st) %>% 
  mutate(office=str_trim(str_remove_all(office, "\\s*\\(.*?\\)"))) %>% 
  mutate(office=tolower(office))

dist_level_posting=qread("data/2 CLEAN/2 AE-DISTRICT YEAR GRID WITH IAS POSTING.qs")

ac_pc_dist_delim_map=qread("data/2 CLEAN/ac_pc_dist_delim_map.qs")



#capex_df=qread("data/2 CLEAN/capex_time_geo_df.qs")
capex_df=capex_panel %>% 
  rename(st_name=state, dist_name=district) %>%
  mutate(st_name=standardize_state_names(st_name))  %>% 
  mutate(dist_name=tolower(dist_name)) %>% 
  mutate(delim_id=if_else(year>=2008, 4,3)) %>%
  mutate(year=as.character(year))#

capex_df=capex_df %>% 
  filter(!is.na(rank1_event)) %>% 
  filter(rank1_event>0) %>%
  dplyr::select(project_id,st_name, dist_name, 
                year, month,
                cost_rs_million,industry, 
                ownership, own_cat, project_status,
                ind_count,ind_quantile,multi_loc,
                complete_year,complete_month,begin,
                tot_event_count,phase_title,rank1_event, 
                starts_with("stalled_")) %>% 
  distinct()

capex_df=capex_df %>% 
    rowwise() %>%
  mutate(prj_stalled = sum(c_across(starts_with("stalled_")), na.rm = TRUE)) %>%
  ungroup()

dd=capex_df %>% 
 dplyr::select(industry, ind_count) %>% distinct() %>% 
  arrange(-ind_count) 

capex_wide=capex_df %>%
  mutate(phase_title = recode(phase_title, !!!phase_abbreviations)) %>% # Rename categories
  mutate(dummy = 1) %>% # Create a placeholder column
  pivot_wider(names_from = phase_title, values_from = dummy, values_fill = list(dummy = 0)) %>%
  rename_with(~ paste0("phase_", .), 
              starts_with("pre_filing"):starts_with("principle_appr")) # Add prefix

state_capex=capex_wide %>% filter(own_cat=="State" |own_cat=="Central" )

state_event_df <- state_capex %>% 
  group_by(st_name, dist_name, year) %>%
  summarise(
    across(
      c("rank1_event", "cost_rs_million", "begin", 
        starts_with("phase_")), 
      \(x) sum(x, na.rm = TRUE)  # Use an anonymous function
    ), 
    .groups = "drop"
  ) %>% mutate(bad_event=phase_post_clear+phase_neg_term,
               good_event=phase_final_clear,
               land_event=phase_land_proc) %>% 
  mutate(cat="state")

priv_capex=capex_wide %>% 
  filter(!(project_id %in% state_capex$project_id))

priv_event_df=priv_capex %>% 
  group_by(st_name, dist_name, year) %>%
  summarise(
    across(
      c("rank1_event", "cost_rs_million", "begin", 
        starts_with("phase_")), 
      \(x) sum(x, na.rm = TRUE)  # Use an anonymous function
    ), 
    .groups = "drop"
  ) %>% mutate(bad_event=phase_post_clear+phase_neg_term,
               good_event=phase_final_clear,
               land_event=phase_land_proc) %>% 
    mutate(cat="private")


event_df=state_event_df %>% bind_rows(priv_event_df)
event_df=event_df %>% 
  mutate(delim_id=if_else(year>=2008, 4,3))

# ggplot(capex_st_yr, aes(x = year, y = ratio)) +
#   geom_line() +  # Line plot
#   geom_point(color = "red", size = 2) +  # Highlight points
#   theme_minimal() +
#   labs(
#     title = "Ratio: events to count",
#     x = "Year",
#     y = ""
#   ) +
#   theme(
#     text = element_text(size = 12)  # Adjust text size
#   )

geo_map=ac_pc_dist_delim_map %>% 
  dplyr::select(-ac_name, -ac_type,
                -match_pct1, -pc01_state_id, 
                -pc01_district_id) %>% 
  distinct()

event_delim_df=event_df %>% 
  inner_join(geo_map, 
             by=c("st_name", "dist_name", "delim_id"))






# 3. Constructing PC mobility measures --------------------------------

pc_dt=ge_ias %>% 
  filter(assembly_no  %in% c(13,14,15,16)) #%>%  #Using PC level data
  

pc_dt=pc_dt %>% 
  rename(pc_name=constituency_name) %>% 
  mutate(st_name=tolower(st_name),
         pc_name=tolower(pc_name)) %>% 
  mutate(stable=no_terms>2) #

pc_dist_delim=ac_pc_dist_delim_map %>% 
  dplyr::select(-ac_name, -ac_type ) %>% 
  distinct()

el_pc_delim=pc_dt %>% inner_join(pc_dist_delim)
# Joining with `by = join_by(st_name, delim_id,
#                            pc_name)`

mobility_classification <- el_pc_delim %>%
  filter(assembly_no  %in% c(13,14,15)) %>% 
  group_by(pc_uid) %>%
  reframe(
    type_13_14 = paste(unique(pc_type[assembly_no == 13]), unique(pc_type[assembly_no == 14]), sep = " to "),
    type_14_15 = paste(unique(pc_type[assembly_no == 14]), unique(pc_type[assembly_no == 15]), sep = " to "),
    delim_transition = paste(unique(delim_id[assembly_no == 13]), unique(delim_id[assembly_no == 15]), sep = " to ")
  ) %>%
  mutate(
    mob_cls = case_when(
      type_14_15 == "GEN to GEN" ~ "GEN to GEN",
      grepl("GEN to (SC|ST)", type_14_15) ~ "GEN to Res",
      grepl("(SC|ST) to GEN", type_14_15) ~ "Res to GEN",
      grepl("(SC|ST) to (SC|ST)", type_14_15) ~ "Res to Res",
      TRUE ~ "Other"
    )
  ) %>%  filter(mob_cls != "Other")

stable_at_ls14=el_pc_delim %>% 
  filter(assembly_no==14) %>% 
  dplyr::select(pc_uid, stable) %>% distinct()
pc_st=el_pc_delim %>% 
  dplyr::select(pc_uid, st_name) %>% distinct()

mobility_classification1=mobility_classification %>% 
  inner_join(pc_st, by="pc_uid") 

pc_mob_class=mobility_classification %>% 
  dplyr::select(pc_uid, mob_cls) %>% distinct() %>% 
  right_join(el_pc_delim) %>% 
  dplyr::select(pc_uid, mob_cls, assembly_no          ) %>% 
  distinct() %>% 
  mutate(treat1=if_else(mob_cls=="GEN to Res", 1, 0),
         treat2=if_else(mob_cls=="Res to GEN", 1, 0)) %>% 
  mutate(treat_both=treat2+treat1)

# names(event_delim_df)
# [1] "st_name"               "dist_name"            
# [3] "year"                  "rank1_event"          
# [5] "cost_rs_million"       "begin"                
# [7] "phase_pre_filing"      "phase_neg_term"       
# [9] "phase_add_agree"       "phase_final_clear"    
# [11] "phase_land_proc"       "phase_init_reg"       
# [13] "phase_review_scrutiny" "phase_post_clear"     
# [15] "phase_principle_appr"  "bad_event"            
# [17] "good_event"            "cat"                  
# [19] "delim_id"              "pc_uid"               
# [21] "pc_name"               "pc_type"              
# [23] "post_st_pc_dup"        "post_pc_type"         
# [25] "name_merge"            "ac_count_pre"         
# [27] "ac_count_post"  





# 4. WHAT HAPPENS AROUND ELECTION AND AROUND (STRONG) INCUMBENT-------------------------


ge_ias_capex=ge_ias %>% 
  inner_join(event_delim_df)
ae_ias_capex=ae_ias %>%
  inner_join(event_delim_df)


ge_anly_df=ge_ias_capex %>% 
  group_by( assembly_no) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(geyear=min(year, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(ge_time=year-geyear) %>% 
  filter(ge_time<=5) %>% 
  mutate(ge_time2=if_else(ge_time>=3,ge_time-5,ge_time)) %>% 
  dplyr::select(st_name, dist_name, year, 
                rank1_event, cost_rs_million, 
                begin, bad_event, good_event, incumbent,
                cat, delim_id, pc_uid, pc_name, margin_percentage,
                pc_type, geyear, ge_time, ge_time2,
                starts_with("stb_"), starts_with("lag_stb")) %>% 
  distinct() %>% 
  mutate(
    stb_mp_total = rowSums(across(starts_with("stb_mp"), ~ replace_na(.x, 0))), 
    lag_stb_mp_total = rowSums(across(starts_with("lag_stb_mp"), ~ replace_na(.x, 0)))
  ) 

dd=ge_anly_df %>% filter(cat=="private")
fml=as.formula("rank1_event ~ stb_mp4_count | year+ st_name")
summary(felm(fml, data=dd ))



treatment_df=ge_anly_df %>% 
  mutate(treat1=if_else(stb_mp_total>0,1,
                        if_else(stb_mp_total==0 & lag_stb_mp_total>0,
                                0,NA)))


library(dplyr)
library(ggplot2)
library(fixest)
library(broom)

# ---- Step 1: Preprocessing ----
ge_anly_df <- ge_ias_capex %>% 
  group_by(assembly_no) %>% 
  mutate(year = as.numeric(year),
         geyear = min(year, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    ge_time = year - geyear,
    ge_time2 = if_else(ge_time >= 3, ge_time - 5, ge_time)
  ) %>% 
  filter(ge_time2 >= -2, ge_time2 <= 2) %>%  # Focus on event window (-2 to +2)
  dplyr::select(st_name, dist_name, year, constituency_name,
                assembly_no,
                rank1_event, cost_rs_million, land_event,
                begin, bad_event, good_event, incumbent,
                cat, delim_id, pc_uid, pc_name, margin_percentage,
                pc_type, geyear, ge_time, ge_time2,
                starts_with("stb_"), starts_with("lag_stb")) %>% 
  distinct() %>% 
  mutate(
    stb_mp_total = rowSums(across(starts_with("stb_mp"), ~ replace_na(.x, 0))), 
    lag_stb_mp_total = rowSums(across(starts_with("lag_stb_mp"), ~ replace_na(.x, 0)))
  )

# ---- Step 2: Define Treatment Variable ----
treatment_df <- ge_anly_df %>% 
  mutate(treat1 = case_when(
    stb_mp4_count > 0 ~ 0,
    stb_mp4_count == 0 & lag_stb_mp4_count > 0 ~ 1,
    TRUE ~ NA_real_
  ))

lead_treat_df=treatment_df %>% 
  dplyr::select(st_name,constituency_name, assembly_no, treat1) %>%
  distinct() %>% 
  group_by(st_name,constituency_name ) %>% 
  arrange(st_name,constituency_name, assembly_no, .by_group = TRUE) %>% 
  mutate(lead_treat1=dplyr::lead(treat1, order_by=assembly_no)) %>% 
  ungroup() %>% dplyr::select(-treat1) %>% distinct()

ltreatment_df=treatment_df %>%
  inner_join(lead_treat_df, 
             by=c("st_name", "constituency_name", "assembly_no")) %>% 
  mutate(treat2=if_else(ge_time2<0,lead_treat1,treat1)) %>% 
  mutate(inc_treat=incumbent==1) 

  
# ---- Step 3: Regression Model ----
reg_df <- ltreatment_df %>% filter(cat == "private")

fml <- as.formula("land_event ~ i(ge_time2, treat2, ref = -1) + 
                  treat2 +ge_time2 |st_name")

event_study_model <- feols(fml, data = reg_df, cluster = ~st_name)

# ---- Step 4: Extract Coefficients for Plot ----
event_study_results <- broom::tidy(event_study_model, conf.int = TRUE)

# Extract event-time coefficients and clean variable names
event_study_results <- event_study_results %>%
  filter(str_detect(term, "ge_time2:")) %>%
  mutate(event_time = c(-2,0,1,2))
        
# ---- Step 5: Compute Mean for NA group ----
na_group <- treatment_df %>%
  filter(is.na(treat1)) %>%
  group_by(ge_time2) %>%
  summarise(mean_rank1_event = mean(rank1_event, na.rm = TRUE)) %>%
  mutate(treat_group = "No Treatment (NA)")

# ---- Step 6: Merge Data for Plot ----
# plot_data <- bind_rows(
#   event_study_results %>% 
#     dplyr::select(event_time, estimate, conf.low, conf.high, treat_group) %>% 
#     rename(mean_rank1_event = estimate),
#   na_group %>% rename(event_time = ge_time2)
# )
plot_data=event_study_results

# ---- Step 7: Event Study Plot (All Three Groups) ----
ggplot(plot_data, aes(x = event_time, y = estimate))+
geom_point() +  # Plot point estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Add confidence intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Reference line for event
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line for no effect
  geom_point(aes(x = -1, y = 0), color = "blue", size = 4, shape = 16) +  # Solid thick dot
  labs(title = "land actions | pooled data, around all elections",
       x = "Event Time",
       y = "beta:",
       caption = "FE: state+treatment+time") +
  theme_minimal()















# 4.RUNNING A EVENT STUDY/DIFFnDIFF regression --------------------------------
# Currently this is around 2008 change of state information
# Y variable will come from capex_df

ge_ias_capex_mob=ge_ias_capex %>% 
  inner_join(pc_mob_class)

ge_ias_capex_mob=ge_ias_capex_mob %>% 
  mutate(year=as.numeric(year)) %>% 
  filter(year %in% c(2004:2014)) %>%
  mutate(time =year-2009) %>% 
  filter(cat=="private")

reg_df=ge_ias_capex_mob %>% 
    mutate(y=land_event) %>% 
  filter(!is.na(y)) %>% 
  filter(!is.na(treat_both))  %>% 
  dplyr::select(y, time, treat_both, treat1, treat2, 
                st_name, dist_name,year) %>% 
  distinct() %>% 
  mutate(y=clipp(y, 0, 0.98))
table(ge_ias_capex_mob$stb_mp1_count)/nrow(ge_ias_capex_mob)


dd=ge_ias_capex_mob %>% 
  filter(treat_both==1) 
table(dd$stb_mp1_count)/nrow(dd)

# Running the regression

# Load necessary packages
library(fixest)
library(ggplot2)
library(dplyr)

rr=felm(y ~ treat_both:factor(time) | st_name | 0 | st_name, 
                             data = reg_df)
# Define the event-study model using feols()
event_study_model <- feols(y ~ i(time, treat_both, ref = -1)+
                             time+treat_both | st_name , 
                           data = reg_df, 
                           cluster = ~st_name)


# Extract coefficients and confidence intervals for plotting
event_study_results <- broom::tidy(event_study_model, 
                                   conf.int = TRUE) %>%
  filter(grepl("time::", term) & grepl(":treat_both", term)) %>%  # Adjusted filter condition
  mutate(event_time = as.numeric(gsub("time::(-?[0-9]+):treat_both", "\\1", term)))  # Extract numeric time values

# Generate Event Study Plot
estudy_plot=ggplot(event_study_results, 
       aes(x = event_time+2009, y = estimate)) +
  geom_point() +  # Plot point estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Add confidence intervals
  geom_vline(xintercept = 2009, linetype = "dashed", color = "red") +  # Reference line for event
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line for no effect
  geom_point(aes(x = 2008, y = 0), color = "blue", size = 4, shape = 16) +  # Solid thick dot
  labs(title = "project interrupted",
       x = "Event Time",
       y = "beta:",
       caption = "FE: event_time+ treatment +State") +
  theme_minimal()

estudy_plot

# ggsave("fig/esplot_rank1events_stFE.pdf", plot = estudy_plot, 
#        width = 8, height = 6)




### PRE-TREND

# Load necessary libraries
library(fixest)
library(ggplot2)
library(dplyr)

# Ensure time is numeric for proper ordering
reg_df$time <- as.numeric(reg_df$time)

# === Step 1: Visual Parallel Trends Check ===

# Compute mean outcomes by time for treated vs. control groups
parallel_trends_data <- reg_df %>%
  group_by(time, treat_both) %>%
  summarise(mean_y = mean(y, na.rm = TRUE), .groups = "drop")

# Plot mean outcomes over time for treated vs. control
ggplot(parallel_trends_data, aes(x = time, y = mean_y, 
                                 color = as.factor(treat_both))) +
  geom_line() + geom_point() +
  geom_vline(xintercept = -1, linetype = "dashed", color = "red") + # Reference time
  labs(title = "Parallel Trends Check", x = "Event Time", y = "Mean Outcome",
       color = "Treatment Status") +
  theme_minimal()

# === Step 2: Formal Placebo Test (Pre-Treatment Only) ===

# Subset dataset for pre-treatment period only
pre_treatment_data <- reg_df %>% filter(time <= 0)

# Run regression using only pre-treatment data
placebo_model <- feols(y ~ i(time, treat_both, ref = 0) | st_name, 
                       data = pre_treatment_data, cluster = ~st_name)

# Extract and plot pre-treatment coefficients
placebo_results <- broom::tidy(placebo_model, conf.int = TRUE) %>%
  filter(grepl("time::", term) & grepl(":treat_both", term)) %>%
  mutate(event_time = as.numeric(gsub("time::(-?[0-9]+):treat_both", "\\1", term)))

ggplot(placebo_results, aes(x = event_time, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "red") +
  labs(title = "Placebo Test: Pre-Treatment Effects",
       x = "Event Time (Pre-Treatment Only)", y = "Estimated Effect",
       caption = "If pre-treatment estimates are significant, parallel trends may be violated.") +
  theme_minimal()




