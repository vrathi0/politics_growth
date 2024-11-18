rm(list=ls())
source("code/functions.R")

# NOTE
# READING PC ELECTION DATA FROM TCPD EXTRACT HERE. 

# THIS DATA HAS VOTE OUTCOME, PC CHARACTERISTICS,
# AND SOME CERTAIN DYNAMICS AROUND ELECTION TIME.THIS DOES NOT HAVE AFFEDAVIT DATA
# The data is at the level of PC-Parl_no/year-Position/Candidate
# That is candidate level outcome for the election (position and votes). 
# MOVING AWAY FROM WHAT I DID LAST TIME, I WONT BE SPENDING MUCH TIME ON VOTES ETC
# BUT THIS TIME, FOLLOWING MEASURES MIGHT BE GOOD TO LOOK AT

# Writing some data/variable notes in companion file: 1 PC ELECTION DATA NOTES.R

# 1 READING DATA ----------------------------------------------------------

ac_lvl_ge_raw=read_csv("../Data/TCPD/General Election/AC_LEVEL_All_States_2024-11-14.csv") %>% 
  clean_names()

ge_raw=read_csv("../Data/TCPD/General Election/GE_All_States_2024-11-14.csv") %>% 
  clean_names()

ac_pc_dist_delim_map=qread("data/2 CLEAN/ac_pc_dist_delim_map.qs")





# OKAY, so the difference between the PC level and AC level 
# is that even though they are both about GE elections, 
# the AC level data breaks down the votes at AC level, which is pretty cool
# and can potentially be useful. 
# Given that AC can directly be linked to Districts and they do not cross districts
# this effectively allows us to break the PC votes among districts if 
# there are more than one. 

# Lets directly work with AC level, 
# and then later on we can use the PC aggregate to construct some 
# characteristic/history variables



# 2 PROCESSING AND CONSTRUCTING METRICS -----------------------------------

pc_dt=ac_lvl_ge_raw



# by-poll is wrong recorded in the pc_data
# I can maybe fix this ( but later is its not super importnat)

dd=pc_dt %>% 
  dplyr::select(pid, assembly_no, 
                contested) %>% 
  group_by(pid) %>% mutate(cmax=max(contested)) %>% 
  dplyr::select(-contested) %>% 
  distinct()
# Even in the pooled sample, 73% of candidates only appear 
# in the same just 1 time. 

dd= pc_dt %>% filter(position!=1) %>% 
  dplyr::select(pid, recontest) %>% 
  group_by(pid) %>% mutate(tt=any(recontest)) %>% 
  ungroup() %>% dplyr::select(-recontest) %>% 
  distinct()



# 3 MERGING ELECTION DATA WITH DELIM MAP------------------------------


pc_ac_dt=ac_lvl_ge_raw %>% filter(assembly_no  %in% c(13,14,15))
pc_dt=ge_raw %>% filter(assembly_no  %in% c(13,14,15))
pc_dist_delim=ac_pc_dist_delim_map %>% 
  dplyr::select(-ac_name, -ac_type) %>% distinct()

pc_ac_dt=pc_ac_dt %>% 
    rename(st_name=state_name,
           ac_name=constituency_name) %>% 
    mutate(st_name=tolower(st_name),
           pc_name=tolower(pc_name),
           ac_name=tolower(ac_name)) 

pc_dt=pc_dt %>% 
  rename(st_name=state_name, 
         pc_name=constituency_name, 
         pc_type0=constituency_type) %>% 
  mutate(st_name=tolower(st_name),
         pc_name=tolower(pc_name))

el_pc_ac_delim= pc_ac_dt %>% left_join(ac_pc_dist_delim_map)
# Joining with `by = join_by(st_name, pc_name,
#                            ac_name, delim_id)`


el_pc_delim=pc_dt %>% inner_join(pc_dist_delim)
# Joining with `by = join_by(st_name, delim_id,
#                            pc_name)`



get_Rough=function(){ # ROUGH
# unit_delim=pc_dt %>% 
#   dplyr::select(state_name, pc_name, constituency_name, delim_id) %>% 
#   rename(st_name=state_name, 
#          ac_name=constituency_name) %>% distinct() %>% 
#   mutate(st_name=tolower(st_name), 
#          pc_name=tolower(pc_name), 
#          ac_name=tolower(ac_name)) %>% distinct()
# 
# unit_delim=unit_delim %>% 
#   mutate(
#     st_name = case_when(
#       st_name == "delhi" ~ "delhi_&_ncr",
#       st_name == "Arunachal Pradesh" ~ "Arunanchal Pradesh",
#       st_name == "Jammu & Kashmir" ~ "Jammu and Kashmir",
#       st_name == "Tamilnadu" ~ "Tamil Nadu",
#       TRUE ~ st_name  # Keep the state as-is if no changes are needed
#     )
#   )
# 
# only_pc=ac_pc_dist_delim_map %>% 
#   dplyr::select(delim_id, st_name, pc_name) %>% 
#   distinct()
# only_pc2=unit_delim %>% dplyr::select(-ac_name) %>% distinct()
# merge1=only_pc2 %>% inner_join(only_pc)

# Joining with `by = join_by(st_name, pc_name, ac_name,
#                            delim_id)`

# OVERALL THIS LOOKS GOOD 

# > unit_delim %>% count_fun(st_name, pc_name)
# [1] 730
# > merge1 %>% count_fun(st_name, pc_name)
# [1] 608
# > ac_pc_dist_delim_map %>% count_fun(st_name, pc_name)
# [1] 680
# > unit_delim %>% count_fun(st_name, pc_name, ac_name)
# [1] 6864
# > merge1 %>% count_fun(st_name, pc_name, ac_name)
# [1] 5150

# AT PC LEVEL:(slighly worse in post period)

# > table(merge1$delim_id)
# 
# 3   4 
# 518 465 
# > table(only_pc2$delim_id) # THIS IS FROM ELECTION DATA
# 
# 3   4 
# 568 543 

}


library(dplyr)

# Step 1: Classify Constituencies by Mobility Between Types
mobility_classification <- el_pc_delim %>%
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
  ) %>%
  filter(mob_cls != "Other")

table(mobility_classification$mob_cls)

# Step 2a: Compute % of Incumbents Re-Elected from Assembly 13 to 14
incumbent_re_election_13_14 <- el_pc_delim %>%
  filter(assembly_no %in% c(13, 14), position == 1) %>%
  group_by(pc_uid, assembly_no) %>%
  summarise(winner_pid = pid[1]) %>%
  spread(key = assembly_no, value = winner_pid) %>%
  mutate(re_elected = `13` == `14`) %>%
  left_join(el_pc_delim %>% filter(assembly_no == 13) %>% 
              dplyr::select(pc_uid, pc_type) %>% distinct(), 
            by = "pc_uid") %>%
  group_by(pc_type) %>%
  summarise(
    total_incumbents = n(),
    re_elected_count = sum(re_elected, na.rm = TRUE),
    re_election_rate = (re_elected_count / total_incumbents) * 100
  ) %>% ungroup()

View(incumbent_re_election_13_14)


# Step 2b: Compute % of Incumbents Re-Elected from 
#  Assembly 14 to 15 by Mobility Type
incumbent_re_election_14_15 <- el_pc_delim %>%
  filter(assembly_no %in% c(14, 15), position == 1) %>%
  group_by(pc_uid, assembly_no) %>%
  summarise(winner_pid = pid[1]) %>%
  spread(key = assembly_no, value = winner_pid) %>%
  mutate(re_elected = `14` == `15`) %>%
  left_join(mobility_classification, by = "pc_uid") %>%
  group_by(mob_cls) %>%
  summarise(
    total_incumbents = n(),
    re_elected_count = sum(re_elected, na.rm = TRUE),
    re_election_rate = (re_elected_count / total_incumbents) * 100
  ) %>% ungroup()




# ROUGH

mp_activity <- read_csv("~/Dropbox/BurkeLab Dropbox/Vaibhav Rathi/SU/Research/Data/TCPD/MPLADS/Vonter_india-representatives-activity__csv_Lok Sabha-csv__24_10_2024.csv")
mp_activity=mp_activity %>% clean_names()

