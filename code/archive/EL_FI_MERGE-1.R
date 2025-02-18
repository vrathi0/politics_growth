
rm(list=ls())
source("code/functions.R")


# NOTES -------------------------------------------------------------------

# There are two types of main data currently: election/pol data and Firm data
# Each specific dataset in these two broad categories share similar set of keys
# Therefore all the merge that is across the broad categories can be recorded here in this code file. 


# INPUT: Different kinds of ELection or Firm datasets
# PROCESS: Merging/mapping

# OUTPUT: Merged data that contains both ELection and FIrm variables





# MERGE 1: ELEC_CANDX with MCA FIRM DATA ----------------------------------


# ELection data is coming from: E1_readingIN.R
# MCA Firm flow data is coming from: 1_mca_ac_merge.R


election_df=read_rds("data/clean/elec_candX_inner.rds") %>% 
  mutate(state_name=tolower(state_name)) %>% 
  rename(el_year=year, state=state_name)

mca_ac_yr_df=qread("data/clean/MCA_ACYEAR_REG_COMP.qs") %>% 
  mutate(state=str_replace_all(state, " ", "_")) %>% 
  rename(fi_year=year)
mca_ac_yr_df$state[mca_ac_yr_df$state=="jammu_and_kashmir"]="Jammu_&_Kashmir"

# RIGHT NOW ITS MATCHING 26 STATE NAMES


election_firm_df=election_df %>% 
          inner_join(mca_ac_yr_df, by=c("state", "ac_no"))

# There are around 3482 ACs in the final merged data compared to 
# 3800 in the mca dt and 4117 in the election_df








# First challange rightaway is that there is little time overlap between the two data sets. 
# Need to collapse time dim in both

