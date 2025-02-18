rm(list=ls())
source("code/functions.R")


# PENDING: 
# It seems like I should look into details related to this graduation from 
#. state CS to IAS separately. Look at the notes on github.  (bad_id1)

# TEST FILE TO PLAY AROUND WITH TCPD- IAS
# ias_exp <- read_csv("../Data/IAS-tcpd/ias-release-main/ias-experience.csv") %>% 
#   clean_names()

here::i_am("politics_growth.Rproj")

exp_df <- read_csv(here("../Data/TCPD/IAS/ias_experience.csv"))
exp_df = exp_df %>% clean_names()

profile_df= read_csv(here("../Data/TCPD/IAS/ias-profile.csv"))
profile_df=profile_df %>% clean_names()

edu_df=read_csv(here("../Data/TCPD/IAS/ias_education.csv")) %>% 
  clean_names()

exp_df=exp_df %>% mutate(start_date=as.Date(start_date), 
                             end_date=as.Date(end_date), 
                             start_year=year(start_date), 
                             end_year=year(end_date), 
                         start_month=month(start_date), 
                         end_month=month(end_date)) %>% 
                mutate(duration=end_date-start_date) %>% 
  mutate(level_eq=str_replace(level,"Equivalent","")) %>% 
  mutate(level_eq=str_trim(level_eq)) 

# Removing the "Not Available" level_eq, these are most concentrated in
# reference_value 0/1, meaning last/top positions. Maybe ghost rows at the top (?)

exp_df=exp_df %>% filter(level_eq!="Not Available") %>% 
  filter(level_eq!="Other Scales")

exp_df=exp_df %>% group_by(id) %>% 
  mutate(max_lvl=max(reference_value, na.rm=T), 
         min_lvl=min(reference_value, na.rm=T)) %>% 
  mutate(post_no=abs(max_lvl-reference_value)+1) %>% 
  ungroup() 

# DOING THIS NOW: 
# 1. Merging date of allotment and dob from ias_prof
# 2. Computing the no. of months of each post start date from date of allotment
# 3. This can give us no. of months in service at each jump. 
# 4. This is exactly what I need, aggregating this should be the average time taken
#.  that is in the plot 1 in Guo's paper. 

ias_det=profile_df %>% dplyr::select(id, allotment_year, date_of_birth, 
                                   date_of_joining, source_of_recruitment,
                                   gender, place_of_domicile, 
                                   last_education_qualification, 
                                   last_education_subject, 
                                   last_education_division, retired) %>% 
  distinct() %>%   rename(batch_yr=allotment_year) %>% 
  mutate(retired=if_else(retired=="1","1","0")) %>% 
  group_by(id) %>% mutate(id_dup=row_number()) %>% 
  filter(id_dup==1) %>% dplyr::select(-id_dup)


ias_df=exp_df %>%  inner_join(ias_det) # Actually perfect match from exp_df
# Joining, by = c("id")

ias_df=ias_df %>% 
  mutate(date_of_joining=as.Date(date_of_joining,format = "%Y-%m-%d"),
         date_of_birth=as.Date(date_of_birth, format = "%Y-%m-%d"),
         start_date=as.Date(start_date, format = "%Y-%m-%d")) %>% 
  group_by(id) %>%
  mutate(first_start_date = if_else(reference_value==max_lvl, 
                                    start_date, NA_Date_)) %>%
  fill(first_start_date, .direction = "up") %>% 
  ungroup() %>% 
  mutate(date_of_joining=if_else(is.na(date_of_joining), 
                                 first_start_date, date_of_joining)) %>% 
  mutate(month_in_service=as.numeric((start_date-date_of_joining)/30.44), 
         age_at_joining=as.numeric((date_of_joining-date_of_birth)/30.44)) %>% 
   mutate(year_diff=year(date_of_joining)-batch_yr)


# Now there are some weird entries that have negative mis or abnormal values
# they may be because of two (or maybe more) reasons: 
# 1. Values are recorded weirdly for candidates promoted from State CS. 
#.    They will need to be looked separately, but at the moment removing them for ease
# 2. Just bad data entry. Also removing those id

bad_id1=ias_df %>% mutate(bad_rec=if_else(month_in_service<0, 1, 0)) %>% 
  group_by(id) %>% mutate(bad_id=max(bad_rec)) %>% ungroup() %>%
  filter(bad_id==1) %>% dplyr::select(id) %>% distinct()
# PENDING: 
# It seems like I should look into details related to this graduation from 
#. state CS to IAS separately. Look at the notes on github. 

bad_id2=ias_df %>% mutate(bad_rec=if_else(age_at_joining<0, 1, 0)) %>% 
  group_by(id) %>% mutate(bad_id=max(bad_rec)) %>% ungroup() %>% 
  filter(bad_id==1) %>% dplyr::select(id) %>% distinct()

# Entries that do not have have valid level info (not a problem anymore)
# bad_id3=ias_df %>% mutate(bad_rec=if_else(is.na(lvl), 1, 0)) %>% 
#   group_by(id) %>% mutate(bad_id=max(bad_rec)) %>% ungroup() %>% 
#   filter(bad_id==1)# %>% dplyr::select(id) %>% distinct()

# REMOVING THE ABOVE TWO FOR NOW

ias_dr_df=ias_df %>%  filter(!(id %in% bad_id1$id)) %>% 
  filter(!(id %in% bad_id2$id))
# Also entirely removing state CS promotee for time being (NEED TO CHANGE THIS)
# Keeping only Direct Recruitment
ias_dr_df =ias_dr_df %>% filter(source_of_recruitment=="Direct Recruitment")

# There are also some bad entries that only have one posting info, 
# even when they are old timies with batch_yr of 1997 or something and 
# only their 11th posting is in the data. 

bad_id4=ias_dr_df %>% group_by(id) %>% 
   summarise(rec_per_id=n(), 
             max_lvl=unique(max_lvl), 
             batch_yr=unique(batch_yr)) %>% ungroup() %>% 
 mutate(mismatch=rec_per_id-max_lvl) %>% 
  filter(!(mismatch %in% c(0,1)))  # 1 and 0 is correct and not mistmatch
  
# So the obs with rec_per_id==1 are more or less bad, 
# but there are others with rec_per_id >20 that maybe good and 
# and only has a few bad entries that fucks up their counters. 
# But they are few in numbers so easier to drop everything for now. 

ias_dr_df=ias_dr_df %>% filter(!(id %in% bad_id4$id ))





# NOW FIRST I AM JUST MARKING THE post_no (that is the posting number 
# at which centere first appears)

ias_dr_df=ias_dr_df %>% 
  group_by(id) %>%
  mutate(lvl_centre = min(post_no[grepl("Centre", organisation, ignore.case = TRUE)], 
                          na.rm = T)) %>% 
  ungroup()

deput_time_chrt=ias_dr_df %>%
  filter(lvl_centre==post_no) %>% 
  group_by(batch_yr) %>% 
  mutate(deput_time_5p= quantile(month_in_service, 0.05, na.rm=T),
         deput_time_50p= quantile(month_in_service, 0.5, na.rm=T),
         deput_time_95p= quantile(month_in_service, 0.95, na.rm=T)) %>% 
  ungroup() %>% dplyr::select(batch_yr, deput_time_5p, deput_time_50p,
                              deput_time_95p) %>% distinct()
  
ggplot(deput_time_chrt, aes(x = batch_yr)) + 
  geom_line(aes(y = deput_time_5p, color = "deput_time_5p")) +
  geom_line(aes(y = deput_time_50p, color = "deput_time_50p")) +
  geom_line(aes(y = deput_time_95p, color = "deput_time_95p")) +
  labs(x = "Allotment Year", y = "Time", color = "Variables") +
  theme_minimal()


## ASSINGING SENIORITY LEVEL NUMBER
lev10=c("Junior Scale")
lev10b=c("Under Secretary")
lev11=c("Senior Time Scale","Deputy Secretary")
lev12_13=c("Junior Administrative Grade (Ordinary Grade)","Director",
       "Junior Administrative Grade (Selection Grade)")
lev14=c("Senior Administrative Grade","Joint Secretary")
lev15=c("Additional Secretary","Higher Administrative Grade","Above Secretary Level")
lev16=c("HAG +","Secretary")
lev17=c("Apex Scale")
lev18=c("Cabinet Secretary")

centre="Centre"
state= "Cadre (AIS)"

ias_exp=ias_dr_df
ias_exp$grade=NA
ias_exp$grade[(ias_exp$level_eq %in% lev10)]="10"
ias_exp$grade[(ias_exp$level_eq %in% lev10b) & 
              ias_exp$organisation %in% state]="10"
ias_exp$grade[(ias_exp$level_eq %in% lev10b) & 
              ias_exp$organisation %in% centre]="11"

ias_exp$grade[(ias_exp$level_eq %in% lev11) & 
              ias_exp$organisation %in% state]="11"
ias_exp$grade[(ias_exp$level_eq %in% lev11) & 
              ias_exp$organisation %in% centre]="12"

ias_exp$grade[(ias_exp$level_eq %in% lev12_13) & 
              ias_exp$organisation %in% state]="12"
ias_exp$grade[(ias_exp$level_eq %in% lev12_13) & 
              ias_exp$organisation %in% centre]="13"



ias_exp$grade[ias_exp$level_eq %in% lev14]="14"
ias_exp$grade[ias_exp$level_eq %in% lev15]="15"
ias_exp$grade[ias_exp$level_eq %in% lev16]="16"
ias_exp$grade[ias_exp$level_eq %in% lev17]="17"
ias_exp$grade[ias_exp$level_eq %in% lev18]="18"

ias_exp$mis_threshold=NA
ias_exp$mis_threshold[ias_exp$grade=="10" ]=1*12
ias_exp$mis_threshold[ias_exp$grade=="11"]=5*12
ias_exp$mis_threshold[ias_exp$grade=="12"]=9*12
ias_exp$mis_threshold[ias_exp$grade=="13"]=13*12
ias_exp$mis_threshold[ias_exp$grade=="14"]=16*12
ias_exp$mis_threshold[ias_exp$grade=="15"]=25*12
ias_exp$mis_threshold[ias_exp$grade=="16"]=30*12
ias_exp$mis_threshold[ias_exp$grade=="17"]=30*12
ias_exp$mis_threshold[ias_exp$grade=="18"]=30*12


ias_exp=ias_exp %>%  mutate(grade=as.numeric(grade))

field_exp_cum_df=ias_exp %>%
  mutate(N=n()) %>% 
  group_by(field_of_experience) %>% 
  mutate(field_share=n()*100) %>% ungroup() %>% 
  mutate(field_share=field_share/N) %>% 
  dplyr::select(field_of_experience, field_share) %>% distinct() %>% 
  arrange(desc(field_share)) %>% 
  mutate(cum_field_share = cumsum(field_share))

cat_exp_cum_df=ias_exp %>%
  mutate(N=n()) %>% 
  group_by(category_of_experience) %>% 
  mutate(cat_share=n()*100) %>% ungroup() %>% 
  mutate(cat_share=cat_share/N) %>% 
  dplyr::select(category_of_experience, cat_share) %>% distinct() %>% 
  arrange(desc(cat_share)) %>% 
  mutate(cum_cat_share = cumsum(cat_share))


# Saving a copy here for sector-tag
#qsave(ias_exp, "data/interm/ias_exp.qs")


# CENTRE DEPUT DUMMY
# - The issue here is that an officer can go on centre deputation, 
#.  and can come back again. But when it happens upwards of Director level, 
# only then it should count so as to exclude junior going on centre rotation

# So the way to do it would be to tag centre dummy 1 when it happens after director level
#.  and then fill it down. The level identification still might be tricky

level_centre_df=ias_dr_df %>% 
  mutate(centre_deput=if_else(grepl("Centre", organisation),1,0)) %>%
  group_by(level_eq) %>% 
  summarise(level_centre=mean(centre_deput, na.rm=T), 
            level_no=quantile(post_no,0.5, na.rm=T), 
            count=n()) %>% filter(count>=100) %>% 
  arrange(level_no)

# So here are some mean values :
# For Dept Sec: 0.175
# For Director: 0.247
# For JS: 0.164

# I think I am still going to go for JS (grade=="14") as the final marker of 
# at which centre deputation is non-reseverable. But you should still keep in 
# in mind of these earlier visits, can certainly be helpful in identification later on
centre_deput_df <- ias_exp %>%
  mutate(grade = as.numeric(grade)) %>%
  mutate(centre_dum = if_else(grepl("Centre", organisation), 1, 0)) %>%
  # Initialize `centre_deput` with existing `centre_dum` if grade >= 14, else keep as is
  mutate(centre_deput = if_else(grade >= 14 & centre_dum == 1, 1, NA_real_)) %>%
  group_by(id) %>%
  arrange(id, post_no, grade) %>%
  # Fill down within the group, only where it makes sense
  fill(centre_deput, .direction = "down") %>%
  # Replace NA where not affected by the above condition (i.e., grade < 14 or centre_dum != 1)
  mutate(centre_deput = if_else(is.na(centre_deput), 0, centre_deput)) %>%
  ungroup() %>%
  dplyr::select(id, grade, post_no, organisation, centre_deput,centre_dum) %>%
  distinct() %>% 
  group_by(id) %>% 
  mutate(id_centre_deput=max(centre_deput, na.rm=T)) %>% ungroup()

# At an aggregate, around 40% of officers are deputized:
dd=centre_deput_df %>% dplyr::select(id, id_centre_deput) %>%  distinct()
 nrow(dd)
#[1] 5703
mean(dd$id_centre_deput)
# [1] 0.4083816


# Merging back in:

ias_exp=ias_exp %>%  left_join(centre_deput_df)
#Joining with `by = join_by(id, organisation, post_no, grade)`


## CLEANING
# 1. Clipping obs using duration

ias_exp=ias_exp %>% 
  mutate(duration=clipp(duration, 0.01, 0.99)) %>% 
  filter(!is.na(duration)) %>% # CAN CHANGE THIS IF YOU WANT CURRENT POSTINGS TOO
  filter(year(end_date)<=2021) %>% 
  mutate(post_count=max_lvl+1) %>% 
  rename(first_centre=lvl_centre) %>% 
  rename(centre_deput_grd14=centre_deput)


ias_exp=ias_exp %>% 
  rename(cohort=batch_yr)

qsave(ias_exp, "data/tcpd_ias_exp.qs") # USE THIS FOR INPUT 



# ANALYSIS ----------------------------------------------------------------

# I think mostly from here on is descriptive, should be moved ideally. 

# Ranking by time to reach a grade ------------------------------------------

ias_id_rnkin=ias_exp %>% filter(!is.na(grade)) %>% 
  mutate(month_in_service=round(month_in_service),
         month_over_thrs=month_in_service-mis_threshold,
         age_at_joining=round(age_at_joining)) %>% 
  group_by(id, grade) %>% 
  #mutate(month_over_thrs=clipp(month_over_thrs, 0.05,0.95)) %>% 
  mutate(penalty_min=min(month_over_thrs, na.rm=T), 
         penalty_max=max(month_over_thrs, na.rm=T)) %>% ungroup()



ias_ladder=ias_id_rnkin %>% 
  dplyr::select(id, name, cadre, designation, office, organisation, 
                field_of_experience, category_of_experience, batch_yr,
                post_no, grade, level_eq, date_of_birth, date_of_joining,
                start_date, end_date, month_in_service,month_over_thrs,
                age_at_joining,penalty_min,penalty_max,source_of_recruitment) %>% 
  distinct()



min_pen_df=ias_ladder %>% dplyr::select(id, grade, penalty_min, 
                                        penalty_max) %>% distinct() %>% 
  filter(!(penalty_min==Inf)) %>% filter(grade %in% c("11","12","13","14")) %>% 
  group_by(grade) %>%  # THIS SHOULD BE WITHIN COHORT. CHANGE 
  mutate(off_count=n(), mot_cdf = rank(penalty_min, ties.method = "average") / n()) %>% 
  mutate(mot_q=as.numeric(cut(mot_cdf, breaks=4, labels=F))) %>% 
  ungroup()


id_minmob_df=min_pen_df %>% dplyr::select(id, grade, mot_q) %>% distinct()

id_minmob_df <- id_minmob_df %>%
  arrange(id, grade)

transitions <- id_minmob_df %>%
  group_by(id) %>%
  mutate(next_mot_q = Lag(mot_q, -1)) %>%
  filter(!is.na(next_mot_q)) %>%
  ungroup()

transition_matrix <- table(transitions$mot_q, transitions$next_mot_q)
mobility_matrix1 <- prop.table(transition_matrix, 1)


max_pen_df=ias_ladder %>% dplyr::select(id, grade, penalty_min, 
                                        penalty_max) %>% distinct() %>% 
  filter(!(penalty_max==-Inf)) %>%  filter(grade %in% c("11","12","13","14")) %>% 
  group_by(grade) %>% 
  mutate(mot_cdf = rank(penalty_max, ties.method = "average") / n()) %>% 
  mutate(mot_q=as.numeric(cut(mot_cdf, breaks=4, labels=F))) %>% 
  ungroup()

id_maxmob_df=max_pen_df %>% dplyr::select(id, grade, mot_q) %>% distinct()

id_maxmob_df <- id_maxmob_df %>%
  arrange(id, grade)

transitions <- id_maxmob_df %>%
  group_by(id) %>%
  mutate(next_mot_q = Lag(mot_q, -1)) %>%
  filter(!is.na(next_mot_q)) %>%
  ungroup()

transition_matrix <- table(transitions$mot_q, transitions$next_mot_q)
mobility_matrix2 <- prop.table(transition_matrix, 1)




# TAGGING THE CENTER REACHING STAGE ----------------------------

# The idea here is that, centre deputation can happen in three diff stages:

# A. Lets first start with % of center by batch_yr-level_eq

# limiting to batch_yr after 1988

centre_ratio=ias_exp %>% filter(batch_yr>=1988) %>% 
  mutate(centre_deput=if_else(grepl("Centre", organisation),1,0)) %>% 
  group_by(batch_yr, level_eq) %>% 
  mutate(batch_grade_count=n(), 
         batch_post_no_q10=quantile(post_no, 0.1, na.rm=T), 
         batch_post_no_q50=quantile(post_no, 0.5, na.rm=T),
         batch_post_no_q90=quantile(post_no, 0.9, na.rm=T),
         batch_c_post_no_q50=quantile(post_no[centre_deput==1], 
                                      0.5, na.rm=T),
         mis_q50=quantile(month_in_service, 0.5, na.rm=T), 
         mis_q90=quantile(month_in_service, 0.9, na.rm=T),
         batch_centre_count=sum(centre_deput==1, na.rm=T)) %>% 
  ungroup() %>% #filter(batch_grade_count>=10) %>% 
  mutate(batch_centre_ratio=batch_centre_count/batch_grade_count) %>% 
  dplyr::select(batch_yr, level_eq, batch_grade_count,batch_centre_count,
                batch_centre_ratio,
                batch_post_no_q10,batch_post_no_q50,
               batch_post_no_q90,batch_c_post_no_q50,
                mis_q50,mis_q90) %>% distinct()
  


p <- ggplot(centre_ratio, aes(x = batch_yr, y = batch_centre_ratio)) +
  geom_point() +  # Add dots for each point
  geom_line() +   # Connect points with lines
  facet_wrap(~ level_eq, scales = "free_y") +  # Create a separate plot for each level_eq
  theme_minimal() +
  labs(title = "Batch Centre Ratio by Year and Level",
       x = "Batch Year",
       y = "Batch Centre Ratio") +
  theme(strip.text.x = element_text(size = 10, angle = 0, hjust = 0.5))  # Improve facet labels appearance





ias_exp_csv=ias_exp %>% 
  dplyr::select(id,post_no,  cadre, designation, 
                level_eq, office, organisation, 
                field_of_experience, category_of_experience, grade,batch_yr,
                start_date, end_date, duration,month_in_service, retired) %>% 
  distinct() %>% 
  arrange(id, post_no) %>% rename(grade=grade) %>% filter(retired=="0") %>% 
  dplyr::select(-retired)


# write_excel_csv(ias_exp_csv,
#                 "data/ias_exp_export.csv")

centre_deputation <- filter(ias_exp_csv, organisation == 'Centre')

# Step 2 and 3: Identify the first instance of central deputation for each officer
first_centre_deputation <- centre_deputation %>%
  group_by(id) %>%
  slice_min(order_by = post_no, n = 1) %>%
  ungroup()  # Ensuring that the data is ungrouped for further operations


mis_center_summary=first_centre_deputation %>% 
  group_by(batch_yr, grade) %>% 
  summarise(count=n(), 
            p10_mis=as.numeric(quantile(month_in_service, 0.1, na.rm=T)), 
          p50_mis=as.numeric(quantile(month_in_service, 0.5, na.rm=T)),
          p90_mis=as.numeric(quantile(month_in_service, 0.9, na.rm=T)))


ggplot(first_centre_deputation, aes(x=month_in_service)) +
  geom_histogram(binwidth=12, fill="coral", color="black") +  # Binwidth set to 12 for monthly bins
  labs(title="First Central Deputation by Months in Service",
       x="Months in Service",
       y="Frequency of Officers") +
  theme_minimal()

# ROUGH
# here I am looking at batch 1997 and their current/last posting distribution
# for current/last posting use filter max_grade=post_no-1

# batch 1997-99
# # batch 1998
# b98_last=ias_exp %>% filter(batch_yr==1998) %>% filter(retired=="0") %>% 
#   group_by(id) %>% mutate(last_post=max(post_no)) %>% ungroup() %>% 
#   dplyr::select(id, last_post, post_no,max_grade, everything()) %>% distinct() %>% 
#   arrange(id) %>% 
#   filter(last_post==post_no) %>% 
#   arrange(cadre, id) %>% distinct()
# 
# # batch 1999
# b99_last=ias_exp %>% filter(batch_yr==1999) %>% filter(retired=="0") %>% 
#   group_by(id) %>% mutate(last_post=max(post_no)) %>% ungroup() %>% 
#   dplyr::select(id, last_post, post_no,max_grade, everything()) %>% distinct() %>% 
#   arrange(id) %>% 
#   filter(last_post==post_no) %>% 
#   arrange(cadre, id) %>% distinct()


# NOW AT grade 10-11, I WILL IDENTIFY COVETED POSTINGS AND TAG THE RECORD

# FOR JUNIOR SCALE
jnr_good_desig=c("Assistant Secy","Assistant Commissioner","Under Secy",
                 "Deputy Secretary","C E O","Chief Executive Officer",
                 "Director","Project Officer","Deputy Commissioner",
                 "Chief Dev Officer","Additional Deputy Commissioner",
                 "Secy","Commissioner","Director","Secretary")
jnr_good_desig_words=c("Secy","Commissioner","Director","Secretary")

pattern <- paste(jnr_good_desig, collapse = "|")

ias_exp=ias_exp %>% 
  mutate(jnr_good_desig=if_else(level=="Junior Scale" & str_detect(designation, pattern),1,0))

# Good departments or category_of_experience
# This is taken from Iyer and Mani paper

jnr_good_categ=c("Commerce", "Finance", "Home","Industries",
                 "Consumer Affairs, Food & PD", "Urban Development",
                 "Public Works","Corporate Management (New)",
                 "Mines & Minerals","Agriculture & Cooperation",
                 "Health")  
pattern <- paste(jnr_good_categ, collapse = "|")
# Assuming Commerce is excise and sales tax,
# Assuming Agriculture & Cooperation is irrigation, 

ias_exp=ias_exp %>% 
  mutate(jnr_good_categ=if_else(level=="Junior Scale" & str_detect(category_of_experience, pattern),1,0)) %>% 
  group_by(id) %>% 
  mutate(jnr_good_des=max(jnr_good_desig, na.rm=T),
         jnr_good_cat=max(jnr_good_categ, na.rm=T)) %>% 
  ungroup()


# Constructing other variables
ias_exp=ias_exp %>% group_by(id,level) %>% 
  mutate(mean_duration=mean(duration, na.rm=T), 
         posting_count=n(), 
         cat_count=n_distinct(category_of_experience)) %>% 
  ungroup()
  


# FOR UNDER SECRETARY LEVEL
us_good_desig=c("C E O","Chief Executive Officer",
                 "Deputy Commissioner",
                 "Additional Deputy Commissioner",
                 "Commissioner")
pattern <- paste(us_good_desig, collapse = "|")

ias_exp=ias_exp %>% 
  mutate(us_good_desig=if_else(level=="Under Secretary" & str_detect(designation, pattern),1,0))

# Good departments or category_of_experience
# This is taken from Iyer and Mani paper

us_good_categ=c("Commerce", "Finance", "Home","Industries",
                 "Consumer Affairs, Food & PD", "Urban Development",
                 "Public Works","Corporate Management (New)",
                 "Mines & Minerals","Agriculture & Cooperation",
                 "Health")  
pattern <- paste(us_good_categ, collapse = "|")
# Assuming Commerce is excise and sales tax,
# Assuming Agriculture & Cooperation is irrigation, 

ias_exp=ias_exp %>% 
  mutate(us_good_categ=if_else(level=="Under Secretary" & str_detect(category_of_experience, pattern),1,0)) %>% 
  group_by(id) %>% 
  mutate(us_good_des=max(us_good_desig, na.rm=T),
         us_good_cat=max(us_good_categ, na.rm=T)) %>% 
  ungroup()



dd=ias_exp %>% 
  dplyr::select(id, jnr_good_des, jnr_good_cat, 
                us_good_des, us_good_cat) %>% distinct()

tt=table(dd$jnr_good_cat, dd$us_good_cat)
mat=prop.table(tt, 1)

# This is not good, as it means that the classication has 
# kinda low correlation across jnr and us
# Maybe it is good as it means that only around 10% id
# are consistently good at both levels. 

dd=ias_expa %>% group_by(id) %>% 
  mutate(jnr_good_des=max(jnr_good_desig, na.rm=T),
         jnr_good_cat=max(jnr_good_categ, na.rm=T)) %>% 
  ungroup() %>% dplyr::select(id, jnr_good_des, jnr_good_cat) %>% 

#   > table(dd$jnr_good_cat)/nrow(dd)
# 
# -Inf          0          1 
# 0.03039539 0.78926425 0.18034036 
# > table(dd$jnr_good_des)/nrow(dd)
# 
# 0         1 
# 0.6885107 0.3114893 














ias_df1=ias_df1 %>% filter(!is.na(grade)) %>% 
  mutate(month_in_service=round(month_in_service),
         month_over_thrs=month_in_service-mis_threshold,
         age_at_joining=round(age_at_joining)) %>% 
  group_by(id, grade) %>% 
  #mutate(month_over_thrs=clipp(month_over_thrs, 0.05,0.95)) %>% 
  mutate(penalty_min=min(month_over_thrs, na.rm=T), 
         penalty_max=max(month_over_thrs, na.rm=T)) %>% ungroup()




ias_ladder=ias_df1 %>% 
  dplyr::select(id, name, cadre, designation, office, organisation, 
                  field_of_experience, category_of_experience, 
              post_no, grade, level_eq, date_of_birth, date_of_joining,
              start_date, end_date, month_in_service,month_over_thrs,
              age_at_joining,penalty_min,penalty_max,source_of_recruitment)



ladder_df=ias_ladder %>% group_by(grade) %>% 
  summarise(mis_mean=mean(month_in_service, na.rm=T), 
         mis_5p=quantile(month_in_service, 0.05, na.rm=T), 
         mis_50p=quantile(month_in_service, 0.5, na.rm=T), 
         mis_95p=quantile(month_in_service, 0.95, na.rm=T)) %>% 
  ungroup() 


min_pen_df=ias_ladder %>% dplyr::select(id, grade, penalty_min, 
                                       penalty_max) %>% distinct() %>% 
  filter(!(penalty_min==Inf)) %>% 
  group_by(grade) %>%  # THIS SHOULD BE WITHIN COHORT. CHANGE 
  mutate(mot_cdf = rank(penalty_min, ties.method = "average") / n()) %>% 
  mutate(mot_q=as.numeric(cut(mot_cdf, breaks=4, labels=F))) %>% 
  ungroup()

id_minmob_df=min_pen_df %>% dplyr::select(id, grade, mot_q) %>% distinct()

id_minmob_df <- id_minmob_df %>%
  arrange(id, grade)

transitions <- id_minmob_df %>%
  group_by(id) %>%
  mutate(next_mot_q = Lag(mot_q, -1)) %>%
  filter(!is.na(next_mot_q)) %>%
  ungroup()

transition_matrix <- table(transitions$mot_q, transitions$next_mot_q)
mobility_matrix1 <- prop.table(transition_matrix, 1)


max_pen_df=ias_ladder %>% dplyr::select(id, grade, penalty_min, 
                                        penalty_max) %>% distinct() %>% 
  filter(!(penalty_max==-Inf)) %>% 
  group_by(grade) %>% 
  mutate(mot_cdf = rank(penalty_max, ties.method = "average") / n()) %>% 
  mutate(mot_q=as.numeric(cut(mot_cdf, breaks=4, labels=F))) %>% 
  ungroup()

id_maxmob_df=max_pen_df %>% dplyr::select(id, grade, mot_q) %>% distinct()

id_maxmob_df <- id_maxmob_df %>%
  arrange(id, grade)

transitions <- id_maxmob_df %>%
  group_by(id) %>%
  mutate(next_mot_q = Lag(mot_q, -1)) %>%
  filter(!is.na(next_mot_q)) %>%
  ungroup()

transition_matrix <- table(transitions$mot_q, transitions$next_mot_q)
mobility_matrix2 <- prop.table(transition_matrix, 1)



# Now Trying out predicted reg also. 
# How much does early performance predict later success. 
# The idea is the construct a bunch of measures 
# and then see how much is the R2. 
# Random Forest type thing might work, but lets start simple. 

max_pen_wide=max_pen_df %>% dplyr::select(id, grade, mot_cdf) %>% distinct() %>% 
  #group_by(id, grade) %>% mutate(lc=n()) %>% ungroup() %>% 
  arrange(id, grade) %>% 
  pivot_wider(names_from = grade, values_from = mot_cdf, names_prefix = "mot_cdf_")


# Rough Reg 
# [1] "id"            "mot_cdf_10"    "mot_cdf_11"   
# [4] "mot_cdf_12_13" "mot_cdf_14"    "mot_cdf_15"   
# [7] "mot_cdf_16"    "mot_cdf_NA"    "mot_cdf_18"   
# [10] "mot_cdf_17"  

fml=as.formula(paste0("mot_cdf_15~mot_cdf_10"))
df=ias_mot_wide
reg=felm(fml, df);summary(reg)




# %>% 
#   dplyr::select(id, name, organisation, 
#                 grade, mis_mean, mis_5p, mis_50p, 
#                 mis_95p) %>% distinct()
        


# ROUGH: PLAYING AROUND
# Looking for ALOK RANJAN in the TCPD Supremo data. 

alok_exp=ias_exp %>% mutate(name=toupper(name)) %>% 
  mutate(alok_dum=str_detect(name, "ALOK RANJAN")) %>% 
  filter(alok_dum==1)

alok_prof

ashutosh_exp=ias_exp %>% mutate(name=toupper(name)) %>% 
  mutate(can_dum=str_detect(name, "ASHUTOSH GUPTA")) %>% 
  filter(can_dum==1)

ash_prof=ias_prof %>% mutate(name=toupper(name)) %>% 
  mutate(can_dum=str_detect(name, "ASHUTOSH GUPTA")) %>% 
  filter(can_dum==1)

# There are some errors in end date or start date making the date values out of range. 

#1. if duration <0, make end year=start year, and duration =NA

id=ias_exp$duration<0 & !is.na(ias_exp$duration)
ias_exp$end_year[id]=ias_exp$start_year[id]
ias_exp$duration[id]=NA

# 2. Some duration might be unncessary long ( end year is 2041 eg), Clipping them at 0.95p

id=ias_exp$duration>quantile(ias_exp$duration, 0.95, na.rm=T) & !is.na(ias_exp$duration)
ias_exp$duration[id]=quantile(ias_exp$duration, 0.95, na.rm=T)
ias_exp$end_year[id]=ias_exp$start_year[id]+round(as.numeric(quantile(ias_exp$duration, 0.95, na.rm=T)/365))


# DATA CHECK
# the most important thing I need to check is what % of data has district/location marker. 

dt=ias_exp #%>% filter(start_year>=1990) %>% filter(level=="Junior Scale")

# process: split along state lines > standardized the state names in district list as well as ias df
# match district names within each state > Tag district name to the ias exp df. 
# always use master_places for state-district info. Loaded in function.R itself

slist=unique(dt$cadre)
slist=slist[slist %in% unique(master_places$state)]
office_dist=list()

for(d in 1:length(slist)){
  
  dt_sub=dt %>% filter(cadre==slist[d]) %>% 
            mutate(office=str_squish(tolower(office)))
  
  place_sub=master_places %>% filter(state==slist[d])
  
  office_text=unique(dt_sub$office)
  dist_list=str_squish(tolower(unique(place_sub$district_name)))
  
  # matching
  
  contains_district <- sapply(dist_list, function(district) grepl(paste0("\\b", district, "\\b"), 
                                                                  office_text, ignore.case = TRUE), 
                              simplify = T)
  
  contains_district=(apply(contains_district, 1, function(x) colnames(contains_district)[x]))
  contains_district=lapply(contains_district, function(x) {if(length(x)==0) x=NA;x})
  contains_district=t(rbind(contains_district))# %>% bind_rows()
  
  if(length(contains_district)!=0){
      df=data.frame(office=office_text, 
                       district=as.character(contains_district))
      
      
      dt_sub=dt_sub %>% inner_join(df)
      
      
      office_dist[[d]]=dt_sub
  
      print(d)
  }
  
  if(length(contains_district)==0){print(paste0("No match for districts in ",slist[d]))}
  
  
  
}


# At this point we have data for 20 states where looking for districts is possible. 
# matching rate would differ state by state


# Combining all the data:

# This is the data that is a subset of the full ias_exp data

ias_exp_sub=office_dist %>% bind_rows()# %>% filter(start_year>=1990) %>% filter(start_year<=2021)

qsave(ias_exp_sub, 
          file="data/clean/IAS_EXP.qs")


dd= ias_exp_sub %>% filter(district!="NA")



# > sum(as.numeric(ias_exp$duration)/365>3, na.rm=T)/nrow(ias_exp)
# [1] 0.103762

# GETTING AVG TIME AT EACH LEVEL. 

## inverting the reference_value to get a index of seniority, 0-lowest. 
## 


level_df=ias_exp %>% group_by(id) %>% 
        mutate(max_lvl=max(reference_value)) %>% ungroup() %>% 
      mutate(grade=max_lvl-reference_value) %>% 
  group_by(id) %>% 
  arrange(id, grade) %>% 
  mutate(lvl_fct=(as.factor(level))) %>% ungroup()



lvl_order_df=level_df %>% filter(level!="Not Available") %>% 
  group_by(level) %>% 
          mutate(lvl_order=mean(grade, na.rm=T), lvl_count=n()) %>% 
  filter(lvl_count>=1000) %>% dplyr::select(level, lvl_order) %>%  distinct() %>% 
   arrange(lvl_order)
lvl_order_df$lvl_order=as.numeric(as.factor(unique(lvl_order_df$lvl_order)))

level_df= level_df %>% left_join(lvl_order_df)
#Joining, by = "level"


level_year_dur_df=level_df %>%  group_by(level, end_year) %>% 
  filter(!is.na(lvl_order)) %>% 
          mutate(lvl_dur=mean(duration,na.rm=T)) %>% 
          ungroup() %>% 
          dplyr::select(level, lvl_order,end_year,lvl_dur) %>% 
  distinct() %>% mutate(lvl_dur=as.numeric(lvl_dur))

# 
fml=as.formula(paste0("lvl_dur~as.factor(lvl_order)"))
reg=felm(fml, level_year_dur_df); summary(reg)






# Getting Avg time to reach every level
## Getting No of job posting at each level ( low number means frequent promotion)


mobility_df=level_df %>% group_by(id) %>% arrange(id, grade) %>% 
              mutate(days_to_post=start_date-Lag(start_date)) %>% 
              mutate(days_to_post=if_else(grade==0 & is.na(days_to_post), 0, 
                                          as.numeric(days_to_post))) %>% 
              mutate(total_days=cumsum(days_to_post)) %>% group_by(id, level) %>% 
              arrange(id, level, grade) %>% 
              mutate(days_to_level=min(total_days)) %>% ungroup() %>% 
  mutate(days_to_level=tcode(days_to_level, 0.05, 0.95)) %>% 
  group_by(level) %>% 
  mutate( level_sd=sd(days_to_level, na.rm=T), 
          level_p25=as.numeric(days_to_level, 0.25, na.rm=T), 
          level_p75=as.numeric(days_to_level, 0.75, na.rm=T)) %>%
  mutate(level_iqr= level_p75-level_p25) %>% 
  ungroup()
# FIX THE IQR

getcdf=function(x){
  
  fn=ecdf(x)
  
  p=fn(x)
  p
  
}

level_mobility_df=mobility_df %>% 
  dplyr::select(level, level_sd) %>% 
  distinct() %>% inner_join(lvl_order_df)


id_mobility_df=mobility_df %>% group_by(level) %>% 
      mutate(days_to_level_q=getcdf(days_to_level)) %>% ungroup() %>% 
      mutate(days_to_level_q=if_else(days_to_level==0, NA_real_, days_to_level_q)) %>% 
      dplyr::select(id, level, days_to_level, days_to_level_q) %>% 
      ungroup() %>%  distinct() %>% group_by(id) %>% 
      mutate(id_days_to_level_q=mean(days_to_level_q, na.rm=T)) %>% ungroup()



# READING IAS PROFILE -----------------------------------------------------

ias_profile=read_csv("../Data/IAS-tcpd/ias-release-main/ias-profile.csv") %>% 
  clean_names()
