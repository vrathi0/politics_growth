rm(list=ls())
source("code/functions.R")



# THIS FILE IS TO MERGE TCPD IAS DATA 
# WITH MP CANDIDATE + ELECTION DATA
# OUTPUT: IDENTIFYING THE CONNECTION BETWEEN MP AND IAS. 

# KEEP IN MIND: 
# 1. MP can move constituencies across election. 
# 2. MP Classification (initial): 
  #  a. MP incumbent + won in the same PC
  #  b. MP incumbent + lost in the same PC
  #  c. MP incumbent + moved to another PC
  #  d. MP X having a ministry portfolio
  #  e. MP X length of the career (# years in parliament)
  #  f. MP x no. of questions in Parliament
  #  g. MP x attendance in Parliament

# 3. IAS Classification:
   # a. IAS x level (Directory, JS, etc). 
   # b. IAS x department (Revenue/Finance, Health, etc) (can be useful for identifying specific policy)
   # c. IAS x length of the career (# years in service)
   # d. IAS x running average of duration per posting (is freq moved, or valued?)
          # the above can go either way, is IAS being pulled or pushed?
   # e. ??


# 1. READING DATA -----------------------------------------------------------

source("code/3 MERGE/3 IAS MP MLA MERGE PRE-PROC.R")


ac_lvl_ge_raw=read_csv("../Data/TCPD/General Election/AC_LEVEL_All_States_2024-11-14.csv") %>% 
  clean_names()

ge_raw=read_csv("../Data/TCPD/General Election/GE_All_States_2024-11-14.csv") %>% 
  clean_names()

ge_raw=ge_raw %>% 
  arrange(pid, assembly_no) %>% # Ensure data is ordered by pid and assembly_no
  group_by(pid) %>%
  mutate(win_counter = cumsum(position == 1)) %>%
  ungroup() %>%
  rename(st_name=state_name) %>% 
  mutate(st_name=standardize_state_names(st_name)) #

ge_win=ge_raw %>% filter(assembly_no>=9) %>% 
  filter(position==1) %>% 
  mutate(constituency_name= gsub("\\s*\\(.*?\\)", "", 
                                 constituency_name)) %>% 
  mutate(bizz=if_else(tcpd_prof_main %in% c("Business", 
                                                "Small Business or Self-employed"), 1, 0)) %>%
  mutate(politics=if_else(tcpd_prof_main %in% c("Politics"), 1, 0)) %>%
  mutate(ex_gov=if_else(tcpd_prof_main %in% c("Former Government"), 1, 0)) #%>%
  

# win_count_distr= ge_win %>% 
#   filter(position == 1) %>% 
#   group_by(assembly_no, win_counter) %>% 
#   summarise(pid_count = n_distinct(pid), 
#             .groups = "drop") %>%  # Count pids at each win_counter level
#   group_by(assembly_no) %>%
#   mutate(
#     total_pids = sum(pid_count),                  # Total pids for this assembly_no
#     percent_pids = (pid_count / total_pids) * 100 # Compute percentage
#   ) %>%
#   ungroup() #%>% filter(win_counter>1) 
# 
# pid_on_3rd_win=win_count_distr %>% 
#   filter(win_counter>2) %>%
#   group_by(assembly_no) %>%
#   summarise_at(c("pid_count","percent_pids"), sum) %>% 
#   ungroup()#
# 
# ggplot(pid_on_3rd_win, 
#        aes(x = assembly_no, y = percent_pids)) +
#   geom_line(color = "grey", size = 1) +  # Add a line
#   geom_point(color = "black", size = 3) +  # Optional: Add points for emphasis
#   labs(
#     title = "",
#     x = "Assembly Number",
#     y = "Percentage of PIDs"
#   ) +
#   theme_minimal() +
#   theme(
#     text = element_text(size = 12)
#   )
#   
# 
# library(ggplot2)
# 
# # Create the grouped bar plot
# ggplot(win_count_distr %>% filter(win_counter>1),
#        aes(x = factor(win_counter), 
#            y = percent_pids, fill = factor(assembly_no))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(
#     title = "Percentage of PIDs by Win Counter and Assembly Number",
#     x = "Win Counter",
#     y = "Percentage of PIDs",
#     fill = "Assembly Number"
#   ) +
#   theme_minimal() +
#   theme(
#     text = element_text(size = 12),
#     legend.position = "right"
#   )

  



# Question hour data
tcpd_qh=read_delim("../Data/TCPD/Question Hour data/TCPD_QH.tsv", 
               delim = "\t", escape_double = FALSE, 
               trim_ws = TRUE)#


com_raw=read_csv("../Data/TCPD/Indian Council of Ministers/TCPD_Indian_Cabinet_1990_2021.csv") %>% 
  clean_names()

# AC-PC-DIST-DELIM MAP
ac_pc_dist_delim_map=qread("data/2 CLEAN/ac_pc_dist_delim_map.qs") %>% 
  mutate(dist_name=str_trim(str_remove_all(dist_name, "\\*"))) %>% 
  rename(old_new_pc_match=match_pct1) %>% 
  mutate(st_name=standardize_state_names(st_name)) 


dist_pc_map=ac_pc_dist_delim_map %>% 
  filter(!is.na(dist_name)) %>%
  group_by(pc_uid, delim_id) %>% 
  mutate(dist_per_pc=n_distinct(st_name,dist_name)) %>% 
  ungroup() %>% 
  group_by(st_name,dist_name, delim_id) %>%
  mutate(pc_per_dist=n_distinct(pc_uid)) %>% ungroup() %>% 
  dplyr::select(pc_uid, delim_id,st_name, dist_name, 
                pc01_state_id, pc01_district_id,
                dist_per_pc, pc_per_dist) %>%
  distinct() %>% arrange(pc_uid, delim_id) %>% 
  group_by(st_name,dist_name) %>% 
  mutate(dist_lvl_change=n_distinct(pc_per_dist)) %>% 
  group_by(pc_uid) %>%mutate(pc_lvl_change=n_distinct(dist_per_pc)) %>% 
  ungroup()#

# Counting the change at PC level
pc_dist_counts <- ac_pc_dist_delim_map %>%
  filter(!is.na(dist_name)) %>%
  group_by(pc_uid, delim_id) %>%
  summarise(dist_name_count = n_distinct(st_name,dist_name),
            .groups = "drop")
# Step 2: Pivot data to wide format for comparison
pc_dist_wide <- pc_dist_counts %>%
  pivot_wider(names_from = delim_id, values_from = dist_name_count,
              names_prefix = "delim_")
# Step 3: Calculate the change in the number of `dist_name` for each `pc_uid`
pc_dist_changes <- pc_dist_wide %>%
  mutate(change = delim_4 - delim_3)
# > table(pc_dist_changes$change)
# 
# -4  -3  -2  -1   0   1   2 
# 1   2   2  83 290  71   5 

# Counting the change at district level
dist_pc_counts <- ac_pc_dist_delim_map %>%
  filter(!is.na(dist_name)) %>%
  group_by(dist_name, delim_id) %>%
  summarise(pc_uid_count = n_distinct(pc_uid), .groups = "drop")
# Step 2: Pivot data to wide format for comparison
dist_pc_wide <- dist_pc_counts %>%
  pivot_wider(names_from = delim_id, values_from = pc_uid_count,
              names_prefix = "delim_")
# Step 3: Calculate the change in the number of `pc_uid` for each `dist_name`
dist_pc_changes <- dist_pc_wide %>%
  mutate(change = delim_4 - delim_3)
# > table(dist_pc_changes$change)
# -3  -2  -1   0   1   2   3 
# 1  12  74 273  63  13   1 


## IAS DATA

ias_exp=qread("data/2 CLEAN/tcpd_ias+state_exp.qs")
dist_collector_df=qread("data/2 CLEAN/IAS +StateCS DATA MERGED WITH DELIM MAP_AT DIST LEVEL.qs")



# 2. GE ELECTION DATA: creating important MP level covariates---------------------

# KEEP IN MIND: 
# 1. MP can move constituencies across election. 
# 2. MP Classification (initial): 
#  a. MP incumbent + won in the same PC
#  b. MP incumbent + lost in the same PC
#  c. MP incumbent + moved to another PC
#  d. MP X having a ministry portfolio
#  e. MP X length of the career (# years in parliament)
#  f. MP x no. of questions in Parliament
#  g. MP x attendance in Parliament

# 
# ### QUESTION HOUR DATA
# tcpd_qh_long <- tcpd_qh %>%
#   # Split all relevant columns into lists
#   mutate(across(c(member, party, state, 
#                   constituency, constituency_type, gender),
#                 ~ strsplit(as.character(.), ","))) %>%
#   rowwise() %>%
#   # Compute max length for the current row
#   mutate(max_length = max(c(length(member), length(party), 
#                             length(state), length(constituency), 
#                             length(constituency_type),
#                             length(gender)))) %>%
#   # Pad shorter lists with NA directly
#   mutate(
#     member = list(c(member, rep(NA, max_length - length(member)))),
#     party = list(c(party, rep(NA, max_length - length(party)))),
#     state = list(c(state, rep(NA, max_length - length(state)))),
#     constituency = list(c(constituency, rep(NA, max_length - length(constituency)))),
#     constituency_type = list(c(constituency_type, rep(NA, max_length - length(constituency_type)))),
#     gender = list(c(gender, rep(NA, max_length - length(gender))))
#   ) %>%
#   ungroup() %>%
#   # Unnest all columns
#   unnest(cols = c(member, party, state, 
#                   constituency, constituency_type, gender)) %>% 
#   filter(!is.na(member))  %>% 
#   # Apply str_trim to clean leading/trailing whitespaces
#   mutate(across(c(member, party, state, 
#                   constituency, 
#                   constituency_type, gender), ~ str_trim(.)))#


# Step 1: Split columns into lists and pad with NAs
tcpd_qh_long <- tcpd_qh %>%
  # Split all relevant columns into lists
  mutate(across(c(member, party, state, constituency, constituency_type, gender),
                ~ strsplit(as.character(.), ","))) %>%
  # Compute max length for each row and pad shorter lists with NAs
  mutate(max_length = pmap_int(across(c(member, party, state, constituency, constituency_type, gender)),
                               ~ max(lengths(list(...))))) %>%
  mutate(across(c(member, party, state, constituency, constituency_type, gender),
                ~ map2(., max_length, ~ c(.x, rep(NA, .y - length(.x)))))) %>%
  unnest(cols = c(member, party, state, constituency, constituency_type, gender)) %>%
  filter(!is.na(member)) %>%
  # Clean leading/trailing whitespaces
  mutate(across(c(member, party, state, constituency, constituency_type, gender), str_trim))


rm(tcpd_qh)

# Now just getting number of questions
# at MP-assembly_no level


# Step 2: Count questions at MP-assembly_no level
qcount_df <- tcpd_qh_long %>%
  filter(!is.na(member)) %>%
  group_by(member, ls_number, party, state, constituency, constituency_type, gender) %>%
  summarise(n_question = n_distinct(id), .groups = "drop") %>%
  arrange(member, ls_number, n_question) %>%
  group_by(member, ls_number) %>%
  mutate(dup = n(), dup_no = row_number()) %>%
  ungroup() %>%
  filter(!(dup == 2 & dup_no == 1)) %>%
  dplyr::select(-dup, -dup_no) %>%
  # Clean and standardize constituency and state names
  rename(st_name=state) %>% 
  mutate(
    constituency = toupper(constituency),
    st_name=standardize_state_names(st_name),
    constituency = gsub("\\s*\\(.*?\\)", "", constituency)
  )


ge_qhcount_df=ge_win %>% left_join(qcount_df,
                                       by=c("party","constituency_type",
                                            "constituency_name"="constituency", 
                                            "assembly_no"="ls_number", 
                                            "st_name"))# %>%


### COM DATA

# Ensure `appointment_end` is converted to Date

# Transform the data
com_df <- com_raw %>%
  mutate(
    appointment_end = coalesce(as.Date(appointment_end), as.Date("2024-12-01")),
    min_appt_begin = as.Date(appointment_begin)
  ) %>%
  filter(!ls_number %in% c(11, 12, 17), house == "Lok Sabha") %>%
  dplyr::select(-start_source, -end_source) %>%
  # Calculate first appointment lag
  group_by(ls_number) %>%
  mutate(
    min_appt_begin = min(min_appt_begin, na.rm = TRUE),
    first_apt_lag = as.numeric(as.Date(appointment_begin) - min_appt_begin),
    type_begin = if_else(first_apt_lag <= 10, "start_of_term", type_begin)
  ) %>%
  ungroup() %>%
  # Filter and calculate ranks
  filter(type_end != "death_of_minister") %>%
  mutate(
    rank_perceived = case_when(
      type_begin == "mid_term" & type_end == "mid_term" ~ 4,
      type_begin == "mid_term" & type_end == "end_of_term" ~ 3,
      type_begin == "start_of_term" & type_end == "mid_term" ~ 2,
      type_begin == "start_of_term" & type_end == "end_of_term" ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  # Calculate portfolio metrics
  group_by(ls_number, pid) %>%
  mutate(min_rank_perc=min(rank_perceived, na.rm=T)) %>%
  mutate(
    cm_count = sum(rank == "CM", na.rm = TRUE),
    mos_count = sum(rank == "MoS", na.rm = TRUE),
    mos_ic_count = sum(rank == "MoS(IC)", na.rm = TRUE),
    pm_count=sum(rank=="PM", na.rm=T)) %>% 
  mutate(portfolio_count=cm_count+mos_count+mos_ic_count+pm_count) %>%
  ungroup()#
  
# Here (above), I have changed the values of lot of obs from 
# "mid_term" to "start_of_term" as they are the first appointment
# the only reason they were tagged as "mid_term" was that the ceremony 
# was held with a delay of less than 10 days. Likely because they 
# were either MoS or DeptyCM. 



# ---- Create Cross Tabulation ----
cross_ls_tab <- com_df %>%
  tabyl(type_begin, type_end) %>%
  adorn_percentages("row") %>%
  adorn_totals("both") %>%
  adorn_ns()#

# Now I can add another self-made perceived importance at minister level
# Full term > Start-mid> Mid-End> Mid-Mid 


# ---- Expand to Monthly Sequence ----
com_df2 <- com_df %>%
  mutate(
    month_seq = map2(
      floor_date(appointment_begin, "month"),
      floor_date(appointment_end, "month"),
      ~ seq(.x, .y, by = "month")
    )
  ) %>%
  unnest(month_seq) %>%
  mutate(
    pid_com = pid,
    cand_name2 = name,
    appt_year = year(month_seq),
    appt_month = month(month_seq) # Add appt_month to match com_df2
  ) %>%
  rename(ls_year=year) %>% 
  dplyr::select(-month_seq, -appointment_begin,
                -appointment_end, -pid, -name) %>%
  distinct()


com_ls_pid=com_df2 %>% 
  #filter(house=="Lok Sabha") %>%  # will deal with RS separately
 dplyr::select(cand_name2, pid_com, 
               min_rank_perc,cm_count, 
               mos_count, mos_ic_count, 
               pm_count, portfolio_count, 
               appt_year) %>% distinct()


# Function to apply standardization to the state name in pid_com
standardize_pid_com <- function(pid_com) {
  
  
  # Extract the state name (middle word characters between underscores)
  state_name <- str_extract(pid_com, "(?<=_)[A-Za-z_]+(?=_)")
  
  # Apply the standardize_state_names function
  standardized_state <- standardize_state_names(state_name)
  
  # Replace the old state name with the standardized state name in pid_com
  pid_com_standardized <- str_replace(pid_com, 
                                      paste0("(?<=_)", state_name, "(?=_)"), 
                                      standardized_state)
  
  return(pid_com_standardized)
}

# Apply function to the pid_com variable in com_df2
com_ls_pid <- com_ls_pid %>%
  mutate(pid_com = standardize_pid_com(pid_com))

  
ge_qhcount_df=ge_qhcount_df %>% 
  mutate(pid_com=paste0(assembly_no,"_",
                        st_name,"_",
                        constituency_no,"_",
                        poll_no,"_",position))# 

full_elec_df=ge_qhcount_df %>% 
  left_join(com_ls_pid , 
            by=c("pid_com","year"="appt_year"))# %>% 
  #dplyr::select(-pid_com)#
# full_elec_df contains : Votes + Candidate X + Question Hour data+
  #.                       Cabinet Minister status


full_elec_df=full_elec_df %>% 
  group_by(st_name) %>% 
  mutate(st_min_year=min(year, na.rm=T))

# 2. CONSTRUCTING A DISTRICT-YEAR-MONTH MASTER & MERING IAS & GE ELECTION DATA--------------------


matched_collector_df=dist_collector_df %>% 
  filter(cadre_st_mismatch==TRUE) # Around 18% obs dropped here 

# dd=cadre_mismatch %>% 
#   group_by(cadre_st, st_name) %>%
#   summarise(count=n_distinct(id), row_count=n()) %>% ungroup() %>% 
#   arrange(cadre_st, -count)
dist_list=as.character(unique(matched_collector_df$office))

# Following is a common pool of districts both in GE and IAS data
st_dist_df=matched_collector_df %>%
  dplyr::select(st_name,office) %>% 
  distinct()

# Create the grid
dist_month_grid <- create_monthly_grid(st_dist_df) %>% 
  distinct() 

# Transform the IAS data
ias_month_matched_dist= transform_ias_data(matched_collector_df) %>% 
  distinct() #dplyr::select(-st_name) 

# Step 3: Merge the grid with the IAS data
# Combine with the grid 
dist_level_posting <- dist_month_grid %>%
  left_join(ias_month_matched_dist, by = c("st_name","office", 
                                           "year", "month")) %>%
  mutate(active = ifelse(is.na(active), 0, active)) 

dist_level_posting=dist_level_posting %>% 
 mutate(delim_id=if_else(year>=2008, 4,3)) %>% 
  distinct() %>% mutate(dist_name=office)

pc_dist_level_posting=dist_level_posting %>% 
  inner_join(ac_pc_dist_delim_map %>% #Explore this mapping more, descr stats
               dplyr::select(-post_pc_type, 
                             -ac_name, -ac_type, 
                             -post_st_pc_dup) %>% 
               distinct() , 
             by=c("st_name","dist_name", "delim_id")) %>% 
  mutate_at(c("year","month","delim_id"), as.numeric) %>% 
  distinct()

pc_dist_level_posting=pc_dist_level_posting %>% 
  group_by(st_name,pc_name, delim_id) %>%
  mutate(dist_per_pc = n_distinct(interaction(st_name, dist_name, drop = TRUE))) %>% 
  ungroup() %>% group_by(delim_id, st_name, dist_name) %>% 
  mutate(pc_per_dist = n_distinct(interaction(st_name, pc_name, drop = TRUE))) %>% 
  ungroup() 
# here we have pc-dist-year-month filled with corresponding posting
# we can now merge it with GE data along pc-year-month

# Step 4: Merge with GE ELECTION data
 # election data is at PC-candidate-assembly_no level. 
#.  one assembly_no is 5 years. 
ge_ias_merged=full_elec_df %>% 
  mutate(constituency_name=tolower(constituency_name)) %>%
  right_join(pc_dist_level_posting,
            by=c("year","month","delim_id",
                  "constituency_name"="pc_name","st_name")) 

fill_up=ge_ias_merged %>% 
  dplyr::select(st_name, constituency_name, year, month, 
                assembly_no, st_min_year) %>% 
  distinct() %>% 
  group_by(st_name,constituency_name) %>% 
  arrange(st_name,constituency_name, year, month) %>% 
  fill(assembly_no, .direction = "down") %>%
  fill(st_min_year, .direction = "down") %>%
  ungroup() #

ge_ias_merged=ge_ias_merged %>% dplyr::select(-assembly_no, -st_min_year) %>%
  inner_join(fill_up) %>% 
  arrange(st_name,constituency_name, year, month)

ge_ias_merged=ge_ias_merged %>% 
  filter(year>=st_min_year) 

# now we need to fill down as GE (election) data is only 
# once 5 years but the pol-ias match it generates is relevant till 
# next election

# first limiting the data to bare minimum vars

ge_post_fill=ge_ias_merged %>% 
  dplyr::select(st_name,assembly_no,constituency_no, 
                constituency_name, recruit_type,home_state,
                constituency_type,delim_id, candidate, 
                candidate_type, party, pid,ias_name,cand_name2,
                no_terms,dist_name,id, cadre_st,active, pc_uid,
                 pc_type, year, id_centre_deput,
                start_year, start_month, end_year, end_month,
                month, old_new_pc_match, n_question, 
                pid_com, min_rank_perc, post_no, cohort,
                cm_count, mos_count, mos_ic_count, 
                pm_count, portfolio_count,month_in_service,
                grade, bizz, politics, ex_gov, margin_percentage,
                field_of_experience, category_of_experience,
                designation,organisation,incumbent,same_constituency,
                month_in_service,level_eq,grade,
                st_lang_grp,centre_deput_grd14,state_cs,
                pc01_state_id,pc01_district_id) %>% 
  distinct() %>% 
  group_by(st_name,constituency_name,assembly_no) %>% 
  arrange(st_name,constituency_name, year, month) %>% 
  fill(st_name,  constituency_no, 
       constituency_type, candidate,cand_name2, candidate_type,
       n_question,pid_com, min_rank_perc, 
       cm_count, mos_count, mos_ic_count, 
       pm_count, portfolio_count,
       party,pid, no_terms,bizz, politics, ex_gov,
       incumbent,same_constituency,margin_percentage,
       .direction = "down" )  %>% 
  ungroup() %>% 
  dplyr::select(year, month, everything())#

total_unit=ge_post_fill %>% 
  count_fun(year, month, st_name, constituency_name)#
total_pol_unit=ge_post_fill %>% 
  filter(!is.na(pid)) %>% 
  count_fun(year, month,  st_name, constituency_name)#
total_ias_unit=ge_post_fill %>% 
  filter(active==1) %>% 
  count_fun(year, month, st_name, constituency_name)#
# > total_pol_unit/total_unit
# [1] 1
# > total_ias_unit/total_pol_unit
# [1]0.77
# > total_ias_unit/total_unit
# [1] 0.77

# ge_ias_merged is now a district-year-month level data
# that has both officer posting as well as MP linked to that district-year-month



# 3. No of stable MPs (no_terms>2 & same_constituency==T or incumbent==T)

pc_stable_mp=ge_post_fill %>% 
  mutate(stable_mp0=if_else(same_constituency==T & incumbent==T,1,0),
         stable_mp1=if_else((no_terms>2),1, 0),
         stable_mp2=if_else((no_terms>2 & incumbent==T ), 1, 0),
         stable_mp3=if_else((no_terms>2 & same_constituency==T ), 1, 0),
         stable_mp4=if_else((no_terms>2 & same_constituency==T & incumbent==T),
                             1, 0)) %>%
  dplyr::select(constituency_name, st_name, year, month, 
                assembly_no, stable_mp0,
                stable_mp1,
                stable_mp2,
                stable_mp3,
                stable_mp4 ) %>% distinct() %>%
  group_by(constituency_name, st_name, year, month,assembly_no) %>%
  summarise(stb_mp0_count=sum(stable_mp0, na.rm=T),
            stb_mp1_count=sum(stable_mp1, na.rm=T),
            stb_mp2_count=sum(stable_mp2, na.rm=T),
            stb_mp3_count=sum(stable_mp3, na.rm=T),
            stb_mp4_count=sum(stable_mp4, na.rm=T)) %>%
  ungroup() 

stable_mp_lag <- pc_stable_mp %>% 
  dplyr::select(stb_mp0_count:stb_mp4_count, constituency_name,
                st_name, assembly_no) %>%
  distinct() %>% 
  group_by(st_name, constituency_name) %>% 
  arrange(st_name, constituency_name, assembly_no, .by_group = TRUE) %>%  # Ensure sorting within groups
  mutate(across(starts_with("stb_"),
                ~dplyr::lag(.x, order_by = assembly_no),
                .names = "lag_{.col}")) %>%
  ungroup()



ge_post_fill=ge_post_fill %>% 
  left_join(stable_mp_lag, 
            by=c("constituency_name","st_name", 
                 "assembly_no"))#

# 3. SAVING MERGED SAMPLE-----------------------------------------------------------

# This is GE-IAS Merged data containing votes, candidate X, IAS posting and IAS officer X
# at pc-district-year-month level.



date="2025-02-13"
qsave(ge_post_fill, paste0("data/3 MERGED SAMPLES/ge_ias_merged_",date,".qs"))




