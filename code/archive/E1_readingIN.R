
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

var_interest=c("pid", "assembly_no", "tcpd_year", 
               "pid_contest_count", "win_margin",
               "win", "constituency_name", 
               "state_name", "candidate","sex","tcpd_party",
               "candidate_type","ele_no_cand","pid_won_count",
               "turncoat","incumbent","pid_last_contest","margin_percentage",
               "ac_p_incwin","ac_avg_margin")


state_code=read_excel("../Data/TCPD/state_code.xlsx")
state_code$state_name[state_code$state_name=="Daman & Diu"]="Goa_Daman_&_Diu"
state_code$state_name[state_code$state_name=="NCT of Delhi"]="Delhi"
state_code$state_name[state_code$state_name=="Jammu and Kashmir"]="Jammu_&_Kashmir"


state_code=state_code %>% 
  mutate(state=str_replace_all(state_name, " ", "_")) %>% 
  dplyr::select(state, state_id) %>% 
  mutate(state=tolower(state)) %>% 
  mutate(state_id=padzero(state_id, 2))

# creating state+assembly_no+ac_id+poll_no+position level dt
# Creating sh_election_id to mimic SHRUG data. 
ac_election=rawdt %>% rename(state=state_name) %>% 
  mutate(state=tolower(state)) %>% 
  inner_join(state_code) %>% 
        mutate(assembly_no=padzero(assembly_no, 2), 
               constituency_no=padzero(constituency_no, 3),
               state_id=padzero(state_id, 2)) %>% 
        mutate(sh_election_id=paste(state_id, assembly_no, 
                                    constituency_no,poll_no, sep="-")) %>% 
        mutate(sh_election_id=str_replace(sh_election_id, "-0$","")) %>% 
        mutate(sh_cand_id=paste(sh_election_id, position, sep="-")) %>% 
        rename(tcpd_age=age) %>% 
        rename(tcpd_party=party) %>% 
        rename(tcpd_year=year, ac_no=constituency_no, 
               pid_contest_count=contested, pid_won_count=no_terms,
               pid_last_contest=recontest) %>% 
        mutate(win=(position==1)) %>% 
         mutate(state_const=paste(state, constituency_name, 
                                  sep="-")) %>% ungroup() #%>%

rj_st_party=data.frame(assembly_no=c(11:15), 
                       st_party=c("INC", "BJP","INC","BJP",
                                  "INC"))
rj_st_party=rj_st_party %>%  mutate(state="rajasthan") %>% 
  mutate(assembly_no=as.character(assembly_no))

up_st_party=data.frame(assembly_no=c(13:17), 
                       st_party=c("BJP", "SP","BSP","SP",
                                  "BJP"))
up_st_party=up_st_party %>%  mutate(state="uttar_pradesh")%>% 
  mutate(assembly_no=as.character(assembly_no))

st_party=rj_st_party %>%  bind_rows(up_st_party)

ac_election=ac_election %>%  left_join(st_party) 

ac_election=ac_election %>% 
  mutate(ac_st_align=tcpd_party==st_party)



  

  # mutate(winp=if_else(position==1, margin_percentage, NA_real_)) %>% 
  # group_by(sh_election_id) %>% arrange(sh_election_id, position) %>% 
  # fill(winp, .direction = "down") %>% 
  # mutate(win_marg=if_else(position==1, winp, if_else(position==2,
  #                                                          -winp, NA_real_))) %>% 
  # ungroup()
# RECONSIDER: the name: sh_election_id or 
# sh_cand_id ( this is not consistent across election for same individual)

# Fixing NOTA observation
ac_election=ac_election %>% 
  mutate(pid=if_else(is.na(pid), "NOTA", pid))



write_rds(ac_election, "data/clean/ac_election.rds", 
          compress = "gz")


# THe following panel would be of all st_aligned candidates along with their 
#. vote share percentage relative to the runnerup ( therefore this variable 
# can be negative zero or positive. Most cases, it would be zero or positive)

close_ele_stalign=ac_election %>% 
  group_by(sh_election_id) %>% 
  mutate(vs2=if_else(position==2,vote_share_percentage,NA_real_ )) %>% 
  fill(vs2,.direction = "downup") %>% 
  mutate(vs_dif=vote_share_percentage-vs2) %>% ungroup() %>% 
  filter(ac_st_align==T) %>% 
  dplyr::select(sh_election_id, ac_st_align, vs_dif,assembly_no,state, tcpd_year) %>% 
  distinct() %>% rename(year=tcpd_year)

close_ele_all=ac_election %>% 
  filter(win==TRUE) %>%
  filter(margin_percentage<=5) %>% 
  dplyr::select(sh_election_id, 
                margin_percentage, 
                incumbent, 
                ac_st_align, 
                assembly_no, 
                district_name, 
                tcpd_year ) %>% rename(year=tcpd_year) %>% distinct()
  

elec_iv=ac_election %>% 
  filter(win==TRUE) %>%
  mutate(election_iv=margin_percentage<5) %>%  
  rename(year=tcpd_year) %>% 
  distinct() %>% 
  dplyr::select(year, sh_election_id, 
                state, assembly_no, ac_no,
                 election_iv, margin_percentage) %>% 
  distinct() #"year", "sh_election_id", "state", "assembly_no", "state_id")

# ROUGH
dd=ac_election %>% group_by(state, assembly_no) %>% 
  summarise(ac_count=n_distinct(ac_no)) %>% ungroup()
dd2=elec_iv %>% group_by(state, assembly_no) %>% 
  summarise(ac_count=n_distinct(ac_no), 
            cls_ele=sum(election_iv, na.rm=T)) %>% ungroup()

# cand_lvl=ac_election %>% 
#   mutate(state_const=paste(state_name, constituency_name, sep="-")) %>% 
#   dplyr::select(pid, assembly_no, tcpd_year, contested, 
#                 vote_share_percentage,
#                 position, 
#                 win, state_const, constituency_name, 
#                 state_name, all_of(tcpd_varlist)) %>% distinct()
# This above is a candidate level dataset. 
# For every candidate( pid), it has election outcomes.

              


# CANDIDATE CHARACTERISTICS -----------------------------------------------

adr_cand_x=read_dta("../Data/SHRUG/ELECTION/shrug-adr-elections-cand-dta/affidavits_clean.dta")
adr_cand_x=adr_cand_x %>% rename(state_id=pc01_state_id) %>%
  inner_join(state_code) %>% 
  dplyr::select(-state_id)
# Data on political candidate characteristics 
# (chiefly, assets and criminal charges) from affidavits filed 
# with the Electoral Commission of India from 2003-2017. 
# Merges to election results data on sh_cand_id / sh_election_id, 
# and to SHRUG on ac07_id / ac08_id.
# NEED TO EXTEND IT TO 2022. 

# appending the new data

flist=list.files(path="../Data/TCPD/AC/MYNETA (2000-2021)/data", 
                  pattern = ".csv$", 
                 recursive = T, 
                 full.names = T)

rawdt=lapply(flist, 
             read_csv)
rawdf=rawdt %>% bind_rows() %>% clean_names() %>% 
  mutate(state=gsub(" ","_", state)) %>% 
  mutate_all(tolower) %>% mutate(year=as.numeric(year)) %>% 
  mutate(constituency=gsub("\\s*\\([^)]+\\)", "", as.character(constituency))) %>% 
  rename(const=constituency)

key1=rawdf %>% dplyr::select(state, year, const,  district) %>% 
   arrange(state, district, const, year) %>% 
  filter(as.numeric(year)>2010) %>% dplyr::select(-year) %>% distinct() 
       

key2=adr_cand_x %>% dplyr::select(ac_id,  state, 
                                  year, adr_district_name, adr_con_name) %>% 
  distinct() %>% mutate_all(tolower) %>% filter(adr_district_name!="") %>% 
    rename(district=adr_district_name, 
         const=adr_con_name) %>% 
  dplyr::select(state, district,ac_id, const, year) %>% 
  filter(as.numeric(year)>2010) %>% dplyr::select(-year) %>% distinct()

key_bridge=key1 %>% inner_join(key2)
#Joining, by = c("state", "const", "district")

# Now the idea is to bring key_bridge back to rawdf and just merge in the ac_id

cand_char_post2k17=rawdf %>%
  filter(year>=2017) %>% 
  left_join(key_bridge)

# This strategy is not 100% correct as even if ACs have not be delinieted after 2008
# states or districts have changed in some cases and the above method will break down for those. 

# BIG ISSUE: This is still a candidate level data but without 
# the position ( in election) variable. This means that it is just not possible 
# to merge this with ac_election as we dont know the vote ranking of 
# candidates within an AC-election. 
# The best we can do is to merge at AC-year level. 

# Renaming variables to match adr_cand_x
cand_char_post2k17=cand_char_post2k17 %>% 
  rename(adr_con_name=const,
         adr_district_name=district, 
         adr_cand_name=candidate_name, 
         self_profession=profession, 
         num_crim=criminal_cases, 
         crime_major=serious_criminal_cases, 
         ed_string=education, net_assets1=net_assets) %>% 
  dplyr::select(state, year, adr_con_name, winner,
                adr_district_name, 
                adr_cand_name, 
                self_profession, 
                num_crim, 
                crime_major, 
                ed_string, party, url, gender, age, 
                address, self_profession, 
                spouse_profession, 
                assets, liabilities, net_assets1, 
                ac_id) %>% distinct() %>% 
  mutate(liabilities=as.numeric(liabilities), 
         assets=as.numeric(assets), 
         num_crim=as.numeric(num_crim), 
         crime_major=as.numeric(as.logical(crime_major)), 
         age=as.numeric(age)) %>% 
  mutate(winner=as.numeric(as.logical(winner)))

# Now stacking both the data

adr_cand_x_tot=adr_cand_x %>%  bind_rows(cand_char_post2k17) %>% 
  distinct() %>% group_by(state, adr_district_name, 
                          adr_con_name) %>% 
  arrange(state, adr_district_name, adr_con_name, year) %>% 
  fill(ac_id) %>% ungroup()

# Now we have updated and expanded the year range


# Setting the missing liabilities to zero( see daily notes ADR section point 1)

adr_cand_x_tot=adr_cand_x_tot %>% 
  mutate(liabilities=if_else(is.na(liabilities), 0, liabilities)) %>% 
  filter(!is.na(assets))
         
adr_cand_x_tot=adr_cand_x_tot %>% 
  mutate(net_asset=assets-liabilities) 
   # this is a redundant variable to ac_election$win and causes confusion

# removing by-election
adr_cand_x_tot=adr_cand_x_tot %>%  filter(bye_election==0| is.na(bye_election))

# At this point, around 35% of obs are missing sh_election_id ( coming from post 17 data)

# Below I am fixing the problem, because I dont have AC level key to merge it with
# ac_election

# Constructing ac_id and sh_election_id
temp1=adr_cand_x_tot %>% 
  mutate(assembly_no=substr(sh_election_id, 4,5)) %>% 
  mutate(assembly_no=as.numeric(assembly_no)) %>% 
  ungroup()

assno_df=temp1 %>% 
  dplyr::select(state, assembly_no, year) %>% 
  distinct() %>%  arrange(state, year) %>% 
  filter(!(year==2017 & is.na(assembly_no))) %>% 
  group_by(state, year) %>% 
  fill(assembly_no) %>% ungroup() %>% distinct()

assno_df=assno_df %>% filter(state!="lok_sabha" & state!="telangana") %>% 
  group_by(state) %>%
  mutate(assembly_no = ifelse(is.na(assembly_no), 
                              max(assembly_no, na.rm = TRUE) + 1, 
                              assembly_no)) %>% 
  ungroup() %>% mutate(assembly_no=padzero(assembly_no,2))


adr_cand_x_tot=adr_cand_x_tot %>% 
        inner_join(assno_df) %>% 
          inner_join(state_code)

adr_cand_x_tot=adr_cand_x_tot %>% 
  mutate(acid=substr(ac_id, 9, 11)) %>% 
  mutate(sh_e_id2=paste0(state_id, "-",
                                assembly_no,"-",
                                acid)) %>% 
  mutate(sh_election_id=if_else(is.na(sh_election_id), sh_e_id2, 
                                sh_election_id)) %>%   ungroup()


# Fixing some district names
adr_cand_x_tot$adr_district_name[adr_cand_x_tot$adr_district_name %in% c("buland shahr","bulandshahar")]="bulandshahar"
adr_cand_x_tot$adr_district_name[adr_cand_x_tot$adr_district_name %in% c("chitrakoot","chitrakut")]="chitrakut"
adr_cand_x_tot$adr_district_name[adr_cand_x_tot$adr_district_name %in% c("chittorgargh","chittorgarh")]="chittorgarh"
adr_cand_x_tot$adr_district_name[adr_cand_x_tot$adr_district_name %in% c("gautam buddha nagar","gauttam budh nagar")]="gauttam budh nagar"
adr_cand_x_tot$adr_district_name[adr_cand_x_tot$adr_district_name %in% c("hanumangargh","hanumangarh")]="hanumangarh"
adr_cand_x_tot$adr_district_name[adr_cand_x_tot$adr_district_name %in% c("jyotiba pholey nagar","jyotiba phule nagar")]="jyotiba phule nagar"
adr_cand_x_tot$adr_district_name[adr_cand_x_tot$adr_district_name %in% c("muzaffar nagar","muzaffarnagar")]="muzaffarnagar"
adr_cand_x_tot$adr_district_name[adr_cand_x_tot$adr_district_name %in% c("pratapgargh","pratapgarh")]="pratapgarh"
adr_cand_x_tot$adr_district_name[adr_cand_x_tot$adr_district_name %in% c("siddharth nagar","siddharthnagar")]="siddharthnagar"



# CANDIDATE PROFESSION ----------------------------------------------------

# processing the profession for both self+spouse
# Currently only for RJ and UP
rj_up_cand_x=adr_cand_x_tot %>% 
  filter(state=="rajasthan"| state=="uttar_pradesh") %>% 
  filter(winner==1)

rj_up_cand_x=rj_up_cand_x %>% 
  mutate(prof=paste(self_profession, spouse_profession, sep=",")) %>% 
  mutate(prof=tolower(prof)) %>% 
  mutate(business_idx=grepl("business|agency|firm|partner|construction|consultancy|contractor", prof))

prof_list=rj_up_cand_x %>% 
  dplyr::select(self_profession,spouse_profession, business_idx) %>% distinct()

adr_cand_x_tot=adr_cand_x_tot %>% 
  mutate(prof=paste(self_profession, spouse_profession, sep=",")) %>% 
  mutate(prof=tolower(prof)) %>% 
  mutate(agr_idx=grepl("agriculture|farm|farmer", prof),
         business_idx=grepl("business|agency|firm|partner|construction|consultancy|contractor", prof))


adr_cand_x_tot=adr_cand_x_tot %>% 
  group_by(state, adr_district_name,assembly_no ) %>% 
  mutate(ac_perdist=n_distinct(acid)) %>%ungroup()

write_rds(adr_cand_x_tot, "data/daily_output/cand_X_total.rds", compress = "gz")

adr_cand_x_tot=read_rds("data/daily_output/cand_X_total.rds")
# Business, Agency, Firms, MLA, Agriculture,
# Partner, Construction,consultancy

# First need to subset ACs with close election and then we can take asset averages out of only 
# those ACs. 

cls_ele_cand_x=adr_cand_x_tot %>% filter(nchar(sh_election_id)!=0) %>% 
  filter(nchar(adr_district_name)!=0) %>% filter(!is.na(winner)) %>% 
  inner_join(close_ele_all)
#Joining, by = c("year", "sh_election_id", "assembly_no")

st_align_count=cls_ele_cand_x %>% filter(winner==1) %>% 
  group_by(state, adr_district_name, assembly_no) %>% 
  summarise(ac_st_align_ndist=sum(ac_st_align, na.rm=T)) %>% ungroup()

cls_ele_cand_x=cls_ele_cand_x %>% 
  group_by(state, adr_district_name,assembly_no, winner) %>% 
  mutate(w_net_asset=mean(net_asset, na.rm=T), 
         w_ed=mean(ed, na.rm=T), 
         w_totcrime_major=sum(crime_major, na.rm=T),
         bus_idx=sum(business_idx, na.rm=T)) %>% ungroup() %>%
  dplyr::select(sh_election_id, adr_district_name, ac_perdist, 
                w_net_asset, w_ed, w_totcrime_major,assembly_no,state, bus_idx,
                year, winner) %>% 
  distinct() %>% group_by(state,adr_district_name, assembly_no) %>% 
  mutate(w0_net_asset=if_else(winner==0, w_net_asset, NA_real_),
         w1_net_asset=if_else(winner==1, w_net_asset, NA_real_)) %>% 
  fill(w0_net_asset, .direction = "downup") %>% 
  fill(w1_net_asset, .direction = "downup") %>% 
  ungroup() %>% mutate(net_asset_diff=w1_net_asset-w0_net_asset) %>% 
  inner_join(st_align_count) %>% 
  mutate(acst_algn_dum=if_else(ac_st_align_ndist>1,1,0))



ele_iv_cand_x=adr_cand_x_tot %>% filter(nchar(sh_election_id)!=0) %>% 
  dplyr::select(-sh_cand_id) %>% 
  filter(nchar(adr_district_name)!=0) %>% filter(!is.na(winner)) %>% 
  inner_join(elec_iv) %>% 
  group_by(state, adr_district_name,assembly_no, winner) %>% 
  mutate(w_net_asset=mean(net_asset, na.rm=T), 
         w_ed=mean(ed, na.rm=T), 
         w_totcrime_major=sum(crime_major, na.rm=T),
         bus_idx=sum(business_idx, na.rm=T)) %>% ungroup() %>%
  distinct() %>% group_by(state,adr_district_name, assembly_no) %>% 
    mutate(ac_perdist=n_distinct(ac_no)) %>% 
  mutate(w0_net_asset=if_else(winner==0, w_net_asset, NA_real_),
         w1_net_asset=if_else(winner==1, w_net_asset, NA_real_)) %>% 
  fill(w0_net_asset, .direction = "downup") %>% 
  fill(w1_net_asset, .direction = "downup") %>% 
  ungroup() %>% mutate(net_asset_diff=w1_net_asset-w0_net_asset) 

eiv_df=ele_iv_cand_x %>% 
  dplyr::select(state,adr_district_name, assembly_no, ac_no, election_iv, 
                margin_percentage,ac_perdist) %>% 
  distinct() %>% group_by(state,adr_district_name, assembly_no) %>% 
  mutate(cls_ele_count_iv=sum(election_iv, na.rm=T), 
         cls_ele_shr_iv=cls_ele_count_iv/ac_perdist,
         margin_iv=mean(margin_percentage, na.rm=T)) %>% ungroup()

ele_iv_cand_x=ele_iv_cand_x %>% inner_join(eiv_df)

#Joining, by = c("year", "sh_election_id", "state", "assembly_no", "state_id")

#ROUGH
dd= ele_iv_cand_x %>% ungroup() %>% 
  dplyr::select(state, adr_district_name, assembly_no, winner,
                                    ac_no, w0_net_asset,w1_net_asset,
                w_net_asset) %>% distinct() %>% 
  arrange(state, adr_district_name, 
          assembly_no, ac_no)





# For each district-assembly, we get average cand X split along winning status

wcand_x_dist=adr_cand_x_tot %>% filter(nchar(sh_election_id)!=0) %>% 
  filter(nchar(adr_district_name)!=0) %>% filter(!is.na(winner)) %>% 
  group_by(adr_district_name,assembly_no, winner) %>% 
  mutate(w_net_asset=mean(net_asset, na.rm=T), 
         w_ed=mean(ed, na.rm=T), 
         w_totcrime_major=sum(crime_major, na.rm=T)) %>% ungroup() %>% 
  dplyr::select(sh_election_id, adr_district_name, ac_perdist, 
                w_net_asset, w_ed, w_totcrime_major,assembly_no,state, 
                year, winner) %>% 
  distinct() %>% group_by(adr_district_name, assembly_no) %>% 
  mutate(w0_net_asset=if_else(winner==0, w_net_asset, NA_real_),
         w1_net_asset=if_else(winner==1, w_net_asset, NA_real_)) %>% 
  fill(w0_net_asset, .direction = "downup") %>% 
  fill(w1_net_asset, .direction = "downup") %>% 
  ungroup() %>% mutate(net_asset_diff=w1_net_asset-w0_net_asset)

# THIS IS WRONG... FIRST NEED TO LIMIT IT TO CLOSE ELECTION
# AND THEN GET THE ASSET AVERAGES. 

# Now, the above panel is at district-assembly level
# Merging this with ac_election

close_ele_candX=close_ele_stalign %>%  inner_join(wcand_x_dist)
#Joining, by = c("sh_election_id", "assembly_no", "state", "year")

close_ele_candX2=close_ele_all %>% inner_join(wcand_x_dist)
#Joining, by = c("sh_election_id", "assembly_no", "year")

close_ele_candX=close_ele_candX %>%
  group_by(adr_district_name,assembly_no ) %>% 
  mutate(st_align_cwin=sum(vs_dif>0 & abs(vs_dif)<5, na.rm=T)) %>% 
  mutate(st_align_cwinshare=st_align_cwin/ac_perdist) %>% 
  ungroup() %>% 
  dplyr::select(adr_district_name, assembly_no,state,
                st_align_cwin,st_align_cwinshare,w_net_asset,
                w_ed,w_totcrime_major , year , winner) %>% 
  distinct()

write_rds(close_ele_candX, "data/daily_output/close_ele_candX_2004.rds", compress = "gz")
              
write_rds(close_ele_candX2, "data/daily_output/close_ele_candX2_0505.rds", compress = "gz")

write_rds(cls_ele_cand_x, "data/daily_output/close_elec_candX_1205.rds", compress = "gz")


write_rds(ele_iv_cand_x, "data/daily_output/ele_iv_cand_x.rds", compress = "gz")
  #mutate(st_align_win_dum=if_else(st_align_win>=median(st_align_win), 1,
                                 # 0))

# Unfortunately it seems like even the winner variable is bad for many states . 
# So, in the end I have to merge with voting data to get position ordering for candidates
# This merge would be at: state+constituency+candidate name
# It would not be a very good merge, but nothing I can do at this point. 
# Main target states: Rajasthan, Mahrashtra, 

state_eresult=ac_election %>%  dplyr::select(state, assembly_no, 
                                             constituency_name, 
                                             candidate, position, 
                                             incumbent,margin_percentage) %>% 
      distinct() %>% mutate_all(tolower) %>% 
  rename(adr_con_name=constituency_name, adr_cand_name=candidate)

adr_cand_x_tot1=adr_cand_x_tot %>% left_join(state_eresult)
#Joining, by = c("adr_con_name", "adr_cand_name", "state", "assembly_no")
adr_cand_x_tot1=adr_cand_x_tot1 %>% mutate(winner=if_else(position==1,1,winner))

adr_cand_x_tot=adr_cand_x_tot1



# Now I am going to do following:
# 1. For each sh_election_id, I am going to construct new metrics of asset distribution. 
# 2. This includes, winner_asset, winner_asset_cdf, rich, mean_asset. 

acyear_cand_asset=adr_cand_x_tot %>% filter(adr_district_name!="") %>% 
  filter(!is.na(assets)) %>% 
  group_by(sh_election_id) %>% 
  mutate(asset_cdf=ecdf(assets)(assets), 
         net_asset_cdf=ecdf(net_asset)(net_asset),
         rich=asset_cdf==1, 
         mean_ass=mean(assets, na.rm=T), 
         mean_net_asset=mean(net_asset, na.rm=T), 
         num_crim=mean(num_crim, na.rm=T)) %>% ungroup() %>% 
  dplyr::select(sh_election_id,ac_id, state, adr_district_name, assembly_no, 
                incumbent, party,margin_percentage,
                assets, year, winner,asset_cdf,rich,mean_ass,net_asset_cdf,
                mean_net_asset,net_asset,
                num_crim) %>%   filter(winner==1) %>% distinct() %>% 
        group_by(state, assembly_no) %>% 
  mutate(st_win_party=names((which.max(table(party))))) %>% ungroup() %>% 
  mutate(ac_st_align=party==st_win_party) 

acyear_cand_asset=acyear_cand_asset %>% 
  rename(winner_assets=assets, 
         winner_net_asset=net_asset,
         winner_asset_cdf=asset_cdf, 
         mean_all_asset=mean_ass, 
         winner_net_asset_cdf=net_asset_cdf,
         mean_all_net_asset=mean_net_asset, 
         mean_all_num_crim=num_crim, win_margin_p=margin_percentage)




# writing it 

write_rds(acyear_cand_asset, 
          file="data/clean/acyear_cand_asset.rds", 
          compress = "gz")


# Now here we have an ac-year panel of wealth distributional position of the winner at AC-year level. 
# For entire panel duration

# 
# %>% 
#    %>% 
#   rename( city=adr_district_name) %>% 
#   group_by(state, city, assembly_no, year) %>% 
#   summarise(assets=mean(assets, na.rm=T), 
#             asset_cdf=mean(asset_cdf, na.rm=T),
#             num_crim=sum(num_crim, na.rm=T), 
#             rich=mean(rich, na.rm=T), 
#             mean_ass=mean(mean_ass, na.rm=T)) %>% 
#   ungroup() 



# These two are not that useful 
adr_elec=read_dta("../Data/SHRUG/ELECTION/shrug-adr-elections-ac-dta/affidavits_ac.dta")
tr_cand=read_dta("../Data/SHRUG/ELECTION/shrug-triv-cand-dta/trivedi_candidates_clean.dta")


tr_codes= tr_cand %>% 
  dplyr::select( sh_election_id, sh_cand_id, 
                 pc01_state_id, pc01_district_id,ac_id,
                 tr_ac_id) %>%  distinct() %>% 
  mutate(tr_ac_id=as.numeric(tr_ac_id)) %>% 
  mutate(tr_ac_id=padzero(tr_ac_id, 3))
       
#tr_cand2= tr_cand %>% filter(sh_cand_id %in% c2_s)

# testdf=adr_cand_x %>%  inner_join(tr_cand)
# #Joining, by = c("ac07_id", "ac08_id", "ac_id", "sh_cand_id", "sh_election_id")
# shcand1=unique(adr_cand_x$sh_cand_id); shcan2=unique(tr_cand$sh_cand_id)
# sum(shcand1 %in% shcan2)
# 
# shelec1=unique(adr_cand_x$sh_election_id); shelec2=unique(ac_election$sh_election_id)
# sum(shelec1 %in% shelec2)

# MERGE SECTION -----------------------------------------------------------

## MERGE 1: ELECTION DATA WITH CANDIDATE X ( both inner and full)

elec_candX_full=ac_election %>% left_join(adr_cand_x) %>% 
  inner_join(tr_codes)
elec_candX_inner=ac_election %>% inner_join(adr_cand_x) %>%  inner_join(tr_codes)



write_rds(elec_candX_full, "data/clean/elec_candX_full.rds", 
          compress = "gz")


write_rds(elec_candX_inner, "data/clean/elec_candX_inner.rds", 
          compress = "gz")

#Joining, by = c("sh_election_id", "sh_cand_id")


# ROUGH- match stuff with firm data
# st_dt_con=elec_candX_inner %>%  dplyr::select(pc01_state_id, district_name,  ac_no) %>% distinct() %>% 
#   filter(!is.na(district_name)) %>% 
#   rename(st_code=pc01_state_id, 
#          dist_name=district_name) %>% 
#   mutate(dist_name=tolower(dist_name))
# 
# 
#   mutate(tr_ac_id= padzero(tr_ac_id, 3)) %>% 
#   rename(st_code=pc01_state_id, dt_code=pc01_district_id, ac_no=tr_ac_id) %>% distinct() %>% 
#   filter(dt_code!="") #%>% dplyr::select(-ac_no) %>% distinct()
# 
# st_dt_con2 <- st_read("../Data/MAPS- INDIA/AC/maps-master/assembly-constituencies/India_AC.shp") %>% 
#   st_make_valid() %>% as.data.frame() %>% 
#   dplyr::select(ST_CODE, DIST_NAME, AC_NO) %>% 
#   rename_all(tolower) %>% 
#   mutate( st_code=padzero(st_code, 2), 
#          ac_no=padzero(ac_no, 3)) %>% 
#    mutate_all(tolower) %>% distinct()
# 
# st_dt_merge=st_dt_con %>% inner_join(st_dt_con2)




# Constituency Char -------------------------------------------------------

# MOVE THIS SECTION TO ANOTHER DONWSTREAM CODE FILE

# So we have now for each election, all the standing candidates
# Further we also have some basic characteristics ( detailed forthcoming from scrapped data)
# that we can use to generate some stats that can be used to characterize constituencies

# unit: state_id; time: year/election_no
# unit-time: sh_election_id
# 
# dt=elec_candX_inner # Initializing the working dt
# 
# # Need to filter based on NA asset values already as most are NA 
# # only winner and runner up are non NA
# 
# dt= dt %>% ungroup() %>% #group_by(sh_election_id) %>% 
#   mutate(lg_asset=log(assets+1), 
#          lg_net_asset=log(net_asset+1),
#          lg_winner_asset=if_else(win==T, lg_asset, NA_real_))  %>% 
#   group_by(sh_election_id) %>% 
#          mutate(won_count=mean(pid_won_count), 
#                 winner_won_count=if_else(win==T, pid_won_count, NA_real_)) %>% 
#   ungroup()
# 
# subdt= dt %>% 
#   filter(position<=2) %>% 
#   group_by(sh_election_id) %>% 
#   arrange(-position) %>% 
#   mutate(lg_asset_margin=lg_asset-Lag(lg_asset),
#          lg_net_asset_margin=lg_net_asset-Lag(lg_net_asset), 
#          cc=n_distinct(win)) %>% 
#   ungroup() %>% 
#   dplyr::select(sh_election_id, position, lg_asset_margin, 
#                 lg_net_asset_margin,cc) %>% 
#   distinct()
# 
# ac_x=dt %>% 
#  filter(win==T) %>% 
#   group_by(state_const) %>% 
#   arrange(state_const, assembly_no) %>% 
#   mutate(ac_won_count=rollapply(pid_won_count, 
#                                3, mean, fill = NA, align = "right"),
#          ac_avg_margin=rollapply(margin_percentage, 
#                                  3, mean, fill = NA, align = "right")) %>% 
#   ungroup() %>% 
#   dplyr::select(sh_election_id, assembly_no,state_const,
#                 ac_won_count, ac_avg_margin) %>% distinct()
# 
# ac_bl = dt %>% 
#   group_by(state_id) %>% 
#   filter(win==T & assembly_no==min(assembly_no)) %>% 
#   mutate(lg_win_asset_qtile=cut(lg_asset, breaks=4, label=F)) %>% 
#   ungroup() %>% 
#   dplyr::select(state_id, state_const, lg_win_asset_qtile) %>% 
#   distinct()
#   
#   
#   
# 
# ac_char_dt=dt %>% inner_join(ac_x) %>% 
#   inner_join(subdt) %>% inner_join(ac_bl)
# # This is AC level characteristics that includes election outcome details. 
# 
# 
# 
# # SUM STATS ---------------------------------------------------------------
# 
# # 1. Trend ( statewise?) in lg wealth of winner and lg wealth margin
# 
# dt=ac_char_dt %>% 
#   filter(!is.na(lg_asset)) # Initializing. Just taking the non NA asset ( post 2003)
# 
# 
# # dt = dt %>%
# #   dplyr::select(state_id, ac_no, assembly_no, incumbent,win,
# #                 lg_asset,lg_net_asset,lg_asset_margin,
# #                 lg_net_asset_margin,
# #                 ac_p_incwin,
# #                 ac_avg_margin) %>%
# #   distinct() %>%
# #   group_by(state_id, ac_no) %>% # ERROR: ac_no is not consistent across years
# #   mutate(ecount=n_distinct(assembly_no)) %>%
# #   ungroup() %>%
# #   filter(ecount>=3) # filtering only AC with atleast 3 elections ( only keeping 2264/4096 AC)
# 
# # Now getting stats at AC and state level
# 
# state_lv_asset =dt %>% ungroup() %>% 
#   group_by(state_id, assembly_no,lg_win_asset_qtile) %>%
#   summarise(sd_lg_asset=sd(lg_asset, na.rm=T),
#          sd_asset_marg=sd(lg_asset_margin, na.rm=T),
#          lg_asset=mean(lg_asset, na.rm=T),
#          lg_winner_asset=mean(lg_winner_asset, na.rm=T),
#          lg_asset_margin=mean(lg_asset_margin, na.rm=T),
#          lg_net_asset=mean(lg_net_asset, na.rm=T), 
#          win_count=mean(won_count, na.rm=T), 
#          winner_win_count=mean(winner_won_count, na.rm=T)) %>% 
#   ungroup() %>% 
#   group_by(state_id) %>% arrange(assembly_no) %>% 
#   mutate(e_no=row_number()) %>%  ungroup()
# 
# winner_dt=dt %>%  
#   filter(win==T)
# 
# fml=as.formula(paste0("lg_asset~e_no"))
# reg=felm(fml, state_lv_asset); summary(reg)
# 
# # Interesting reg for cv_winner_asset and lg_asset
# # > summary(state_lv_asset$sd_lg_asset[state_lv_asset$e_no==3])/summary(state_lv_asset$sd_lg_asset[state_lv_asset$e_no==1])
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 1.0011  0.9053  0.8775  0.7675  0.7327  0.5943 
# 
# # Some states have seen higher decrease in CV (3rd q) than others ( lower q like median)
# 
# dt= elec_candX_full
# 
# inc_dt=dt %>% filter(incumbent==1 & win==1) %>% filter(!is.na(assets)) %>% 
#   group_by(sh_election_id) %>% 
#   mutate(lg_asset=log(assets+1)) %>% 
#   group_by(state_id, ac_no) %>% 
#   mutate(ecount=n_distinct(assembly_no)) %>% 
#   ungroup() %>% 
#   filter(ecount>=3)
#   
# 
# state_inc_asset=inc_dt %>% 
#   group_by(state_id, assembly_no) %>%
#   summarise(cv_winner_asset=sd(lg_asset)/mean(lg_asset),
#            # cv_asset_marg=sd(lg_asset_margin)/mean(lg_asset_margin),
#             lg_asset=mean(lg_asset)) %>% 
#             #lg_asset_margin=mean(lg_asset_margin),
#            # lg_net_asset=mean(lg_net_asset)) %>% 
#   group_by(state_id) %>% arrange(assembly_no) %>% 
#    mutate(e_no=row_number()) %>%  ungroup() 
# 
# 
# fml=as.formula(paste0("cv_winner_asset~e_no"))
# reg=felm(fml, state_inc_asset); summary(reg)
# 
# # CHECK JUST SD INSTEAD OF CV
# 
# 
# 
# dt$avg_asset_res=as.numeric(getRes(dt, var1="avg_asset",
#                          var2=c("state_id", "assembly_no")))
# 
# 
# dt$avg_net_asset_res=as.numeric(getRes(dt, var1="avg_net_asset",
#                         var2=c("state_id", "assembly_no")))
# 
# 
# ggplot(state_lv_asset, aes(x = e_no, y = cv_winner_asset, 
#                group = state_id, color = state_id)) +
#   geom_line() +
#   labs(
#     title = "Trend Lines of Residual Values by State",
#     x = "Year",
#     y = "Residual Value",
#     color = "State"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   scale_color_brewer(palette = "Set1")
# 
# 
# ##
# 
# # -----  MERGE 2: Merging with AC level yearly firm flow from MCA ---------------
# 
# 
# 
# zip_ac_map=read_rds("data/interm/zip_ac_map_intx.rds") %>% as.data.frame() %>% 
#   dplyr::select(pincode, ac_intersects, district_ac, 
#                 state_id, ac_no, pc_no) %>% 
#   rename(zip=pincode) %>% 
#   mutate(state_id=padzero(state_id, 2), 
#          ac_no=padzero(ac_no, 3)) %>% 
#   distinct()
# 
# reg_comp=read_rds("data/clean/MCA_registered_company.rds")
# 
# # zip has already been used to merge in district
# 
# df=reg_comp %>%  filter(!is.na(zip)) %>% 
#   dplyr::select(cin, state, district, category, zip,
#                 activity_description, activity_code, year,
#                 authorized_capital, paidup_capital) %>% 
#   rename(district_zip=district) %>% 
#   distinct()
# 
# 
# ac_firms=df %>% left_join(zip_ac_map) # This is at zip-year level of firm flow
# # Joining with `by = join_by(zip)`
# ac_firms= ac_firms %>% 
#   group_by(state_id, ac_no) %>% 
#   mutate(boundary_idx=n_distinct(district_zip)) %>% ungroup()
# # This above is AC being pulled in by zipcode that are on the boundary. 
# # These AC are not actually split, rather these AC are at the boundary of district
# # For eg, 4 or 5 dist_count mean there is a 4 or 5 zip codes, each belonging to 
# #  separate districts that intersects with this one AC. Therefore, this AC is super at the boundary ( of amy districts)
# 
# 
# ac_year_flow=ac_firms %>% group_by(state_id, ac_no,year) %>% 
#   mutate(firm_flow=n_distinct(cin)) %>% 
#   dplyr::select(ac_intersects, state_id, ac_no, year, 
#                 district_ac, pc_no, firm_flow, boundary_idx) %>% ungroup() %>% 
#   distinct()
# 
# ac_avg_flow=ac_year_flow %>% group_by(state_id, ac_no) %>% 
#   mutate(firm_flow=mean(firm_flow, na.rm=T)) %>% 
#   dplyr::select(ac_intersects, state_id, ac_no, 
#                 district_ac, pc_no, firm_flow, boundary_idx) %>% 
#   distinct()
# 
# ac_avg_winmargin=ac_election %>% group_by(ac_no, state_id) %>% 
#   filter(position==1) %>% 
#   mutate(avg_win_margin=mean(win_margin, na.rm=T)) %>% 
#               dplyr::select(sh_election_id,state_id, ac_no, 
#                   constituency_name, state_name,
#                   avg_win_margin) %>%
#     distinct()
# 
# ac_ele_firm=ac_avg_winmargin %>%  inner_join(ac_avg_flow, 
#                                             by=c( "state_id", "ac_no"))
# 
# # Stats for inner_join: MOSTLY JUST YEAR MISMATCH
# # > (ac_ele_firm_flow %>% dplyr::select(ac_year, state_id, ac_no) %>% distinct() %>% nrow())/(ac_election %>% dplyr::select(ac_year, state_id, ac_no) %>%  distinct() %>%  nrow())
# # [1] 0.1014211
# # > (ac_ele_firm %>% dplyr::select( state_id, ac_no) %>% distinct() %>% nrow())/(ac_avg_winmargin %>% dplyr::select(state_id, ac_no) %>%  distinct() %>%  nrow())
# # [1] 0.8635846
# 
# # > (ac_ele_firm %>% dplyr::select( state_id, ac_no) %>% distinct() %>% nrow())/(ac_avg_winmargin %>% dplyr::select(state_id, ac_no) %>%  distinct() %>%  nrow())
# # [1] 0.9300843
# 
# 
# # CAUTION: AC_NO might mean different AC in different dataset. CHECK
# 
# 
# fml=as.formula(paste0("avg_win_margin~firm_flow|district_ac"))
# reg=lfe::felm(fml, ac_ele_firm); summary(reg)


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
 # mutate(state_party=names(which.max(table(ac_party)))) %>% 
  dplyr::filter(win==1) %>% ungroup() %>% 
  dplyr::select(sh_election_id, assembly_no,state_const,
                inc_win, margin_percentage) %>% 
  distinct() %>% 
  group_by(state_const) %>% 
  arrange(state_const, assembly_no) %>% 
  mutate(p_inc_win=rollapply(inc_win, 
                             3, mean, fill = NA, align = "right"),
         avg_margin=rollapply(margin_percentage, 
                              3, mean, fill = NA, align = "right")) 


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
winner_crime_df= elec_candX_inner %>% filter(winner==1) %>% 
      #filter(!is.na(ac08_id)) %>%
      #filter(ac08_id!="") %>% # picking only elections post 08 delination
          dplyr::select(ac_id, year, num_crim, state_name, 
                        constituency_name,assembly_no) %>% 
  distinct() %>% mutate(assembly_no=as.numeric(assembly_no)) %>% 
  group_by(state_name, constituency_name) %>% 
  mutate(e_count=n_distinct(assembly_no)) %>% 
  ungroup()

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



