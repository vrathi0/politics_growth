rm(list=ls())
source("code/functions.R")


# NOTE: Here I am trying to make a plot that has election time on x-axis ( going from -2,-1,0,1,2, etc)
# and some Y variable, where Y: Firm entry rate, firm total cost of prod, employment , etc

# Event Study



# First gathering all the datasets. 



# 1. ASI dataset ----------------------------------------------------------

asi_df=qread("data/clean/1_ASI.qs") %>% 
  rename(state_code=state)

asi_st_yr_lvl=asi_df %>% group_by(year, state_code, rural_urban) %>% 
  summarise(firm_count=sum(mult, na.rm=T),
            tot_cost_prd=sum(tot_cost_prd, na.rm=T),
            tot_wrk_days=sum(tot_wrk_days, na.rm=T),
            tot_wrk_days_manuf=sum(tot_wrk_days_manuf, na.rm=T),
            tot_wrk_days_nonmanuf=sum(tot_wrk_days_nonmanuf, na.rm=T)) %>% ungroup() %>% 
  distinct() %>% 
  mutate(year=as.numeric(year)) %>% rename(rural1_urban2=rural_urban)






# 2. MCA DATA

mca_total=read_rds("data/clean/MCA_registered_company_total.rds")

zip_ac_map=read_rds("data/interm/zip_ac_map4.rds") %>% 
  mutate(st_code_ac=padzero(st_code_ac,2)) %>% 
  dplyr::select(zip, st_code_ac, ac_no, district) %>% distinct()

mca_st_yr_flow=mca_total %>% filter(year>=1990) %>% 
  group_by(state_code, year) %>% 
  summarise(firm_flow=n_distinct(cin)) %>% 
  ungroup() %>% 
  arrange(state_code, year)


mca_ac=mca_total %>% filter(year>=1990) %>% 
  inner_join(zip_ac_map, by="zip")

mca_ac_yr_flow=mca_ac %>% group_by(st_code_ac, ac_no, year) %>% 
  summarise(firm_flow=n_distinct(cin)) %>%  ungroup() %>% 
  arrange(st_code_ac, ac_no, year) %>% 
  mutate(ac_no=padzero(ac_no, 3)) %>% 
  rename(state_code=st_code_ac)


# mca_dist_yr_flow=mca_ac %>% group_by(st_code_ac, district, year) %>% 
#   summarise(firm_flow=n_distinct(cin)) %>%  ungroup() %>% 
#   arrange(st_code_ac, district, year) %>% 
#   rename(state_code=st_code_ac) %>% group_by(state_code, district) %>% 
#   mutate(ff_dist=mean(firm_flow, na.rm=T)) %>% ungroup() %>% 
#   mutate(i80=if_else(ff_dist>=50,2,0),
#          i20=if_else(ff_dist<=5,1,0))
# 
# mca_dist_yr_flow$firm_flow_clp=clipp(mca_dist_yr_flow$firm_flow, 0.2,0.9)
# 
# fml=as.formula(paste0("firm_flow~as.numeric(as.factor(year))*i80 | state_code"))
# reg=felm(fml, mca_dist_yr_flow);summary(reg)

# 3. ELECTION DATA

ac_election=read_rds("data/clean/ac_election.rds") %>% 
  filter(poll_no==0)

st_party=ac_election %>% filter(win==T) %>%  
  group_by(state_id, assembly_no) %>% 
  mutate(st_win_party=names((which.max(table(tcpd_party))))) %>% 
  dplyr::select(state_id, assembly_no, st_win_party) %>% distinct() %>% 
  ungroup() %>% arrange(state_id, assembly_no) %>% group_by(state_id) %>% 
  mutate(party_lag=Lag(st_win_party)) %>% ungroup() %>% 
  mutate(st_incumbant=st_win_party==party_lag) %>% dplyr::select(-party_lag)

st_party2=ac_election %>% filter(win==T) %>%  
  group_by(state_id, assembly_no) %>% 
  mutate(st_win_party=names((which.max(table(tcpd_party))))) %>% 
  dplyr::select(state_id, assembly_no,ac_no, tcpd_party, st_win_party) %>% distinct() %>% 
  mutate(ac_st_align=tcpd_party==st_win_party)
  


ac_election=ac_election %>% inner_join(st_party2)

ac_el0=ac_election %>% 
  dplyr::select(state_name, assembly_no, month, tcpd_year, state_id) %>% 
  distinct() %>% filter(tcpd_year>=1990) %>% filter(!is.na(month)) %>% 
  group_by(state_id) %>% 
  mutate(ecount=n_distinct(assembly_no)) %>% ungroup() %>% 
  arrange(state_id, assembly_no, tcpd_year) %>% 
  dplyr::select(-month) %>% distinct() %>% 
  rename(state_code=state_id)


ac_el1= ac_election %>% filter(win==1) %>% 
  dplyr::select(state_id, assembly_no, tcpd_year,state_name, ac_no,  ac_st_align) %>% 
  distinct() %>% filter(tcpd_year>=1990) %>% 
  group_by(state_id) %>% 
  mutate(ecount=n_distinct(assembly_no)) %>%  ungroup() %>% 
  arrange(state_id,state_name, assembly_no, tcpd_year,ac_st_align, ac_no) %>% 
  distinct() %>% 
  rename(state_code=state_id, 
         year=tcpd_year)
                                     
# MERGING MCA-AC WITH AC-----------------------------------------------------------------

mca_ac_el=mca_ac_yr_flow %>% full_join(ac_el1, 
                                          by=c("state_code","ac_no",
                                          "year"))

mca_ac_el=mca_ac_el %>% arrange(state_code, year)


mca_ac_el=mca_ac_el %>%
  group_by(state_code) %>%  fill(assembly_no) %>% 
  group_by(state_code, assembly_no) %>% 
  arrange(state_code, assembly_no, ac_no, year) %>% 
  tidyr::fill(ac_st_align) %>% 
  ungroup()


mca_st_el=mca_ac_el %>% group_by(state_code, assembly_no, ac_st_align, year) %>% 
  mutate(firm_flow=sum(firm_flow, na.rm=T)) %>% ungroup() %>% 
  dplyr::select(state_name, firm_flow, state_code, assembly_no, ac_st_align, year) %>% 
  distinct() %>% 
  arrange(state_code, assembly_no, year)

mca_st_el$time_marker=NA
mca_st_el$time_marker[!is.na(mca_st_el$state_name)]=0
  

# mca_ac_el=mca_ac_el %>% 
#   mutate(st_ac=paste(state_code, ac_no, sep="-"))
# 
# 
# st_ac=unique(mca_ac_el$st_ac)
# yr_l=unique(mca_ac_el$year)
# 
# master_ac_year=expand.grid(st_ac=st_ac, year=yr_l)
# 
# mca_ac_el2=master_ac_year %>% left_join(mca_ac_el) %>% 
#   arrange(st_ac, year)
# 
# mca_ac_el2$time_marker=NA
# mca_ac_el2$time_marker[!is.na(mca_ac_el2$state_name)]=0

mca_st_el2=mca_st_el %>% arrange(state_code, ac_st_align, year) %>% 
      group_by(state_code, ac_st_align) %>% 
  mutate(lg1=Lag(time_marker, 1), 
         lg2=Lag(time_marker, 2),
         ld1=Lag(time_marker,-1),
         ld2=Lag(time_marker, -2))

mca_st_el2$time_marker[mca_st_el2$lg1==0]=1
mca_st_el2$time_marker[mca_st_el2$lg2==0]=1
mca_st_el2$time_marker[mca_st_el2$ld1==0]=-1
mca_st_el2$time_marker[mca_st_el2$ld2==0]=-2


mca_st_etime1=mca_st_el2 %>% 
  mutate(time_marker=as.character(time_marker)) %>% 
  group_by(state_code,ac_st_align, time_marker) %>% 
  mutate(firm_flow1=mean(firm_flow, na.rm=T)) %>% 
  ungroup()

mca_st_etime2=mca_st_el2 %>% 
  mutate(time_marker=as.character(time_marker)) %>% 
  group_by(state_code, time_marker) %>% 
  summarise(firm_flow1=mean(firm_flow, na.rm=T)) %>% 
  ungroup()


fml=as.formula(paste0("firm_flow1~as.factor(time_marker)*ac_st_align +year|state_code  "))
reg=lfe::felm(fml, mca_st_etime1);summary(reg)


# Plotting
vcov_matrix <- vcov(reg)

# Extract model coefficients
model_coefs <- coef(reg)

# Assuming 'time_marker' is a numerical variable. If it's a factor, you'll need to adjust the code accordingly.
time_markers <- c("-2","0","1")

# Initialize a data frame to store composite coefficients and their SE
results <- data.frame(time_marker = numeric(), 
                      coef_with_ac_st_align_0 = numeric(), 
                      coef_with_ac_st_align_1 = numeric(),
                      SE_with_ac_st_align_0 = numeric(),
                      SE_with_ac_st_align_1 = numeric())

for (time in time_markers) {
  # Calculate composite coefficients for ac_st_align = 0 and ac_st_align = 1
  coef_0 <- model_coefs[paste0('as.factor(time_marker)', time)]
  coef_1 <- coef_0 + model_coefs['ac_st_alignTRUE'] + 
  model_coefs[paste0('as.factor(time_marker)', time,":ac_st_alignTRUE")]
  
  # Calculate standard errors for composite coefficients using delta method
  SE_0 <- sqrt(vcov_matrix[paste0('as.factor(time_marker)', time), paste0('as.factor(time_marker)', time)])
  
  SE_1 <- sqrt(SE_0^2 + vcov_matrix[paste0('as.factor(time_marker)', time, ':ac_st_alignTRUE'),
                                    paste0('as.factor(time_marker)', time, ':ac_st_alignTRUE')] + #vcov_matrix['ac_st_alignTRUE', 'ac_st_alignTRUE'] + 
                 2 * vcov_matrix[paste0('as.factor(time_marker)', time, ':ac_st_alignTRUE'),
                                 paste0('as.factor(time_marker)', time)])
  
  # Store results
  results <- rbind(results, data.frame(time_marker = time, 
                                       coef_with_ac_st_align_0 = coef_0, 
                                       coef_with_ac_st_align_1 = coef_1,
                                       SE_with_ac_st_align_0 = SE_0,
                                       SE_with_ac_st_align_1 = SE_1))
}

ref_row=c("-1",
          0,model_coefs['ac_st_alignTRUE'],
          0,sqrt(vcov_matrix['ac_st_alignTRUE', 'ac_st_alignTRUE']))

results1=rbind(results, ref_row)
results1=results1 %>% mutate_all(as.numeric)




ggplot(results1, aes(x = time_marker)) + 
  geom_point(aes(y = coef_with_ac_st_align_0, color = "Non-aligned"), shape = 21, size = 4, fill = "darkorange") +
  geom_point(aes(y = coef_with_ac_st_align_1, color = "Aligned"), shape = 21, size = 4, fill = "darkolivegreen4") +
  geom_errorbar(aes(ymin = coef_with_ac_st_align_0 - 1.96*SE_with_ac_st_align_0, 
                    ymax = coef_with_ac_st_align_0 + 1.96*SE_with_ac_st_align_0, color = "Non-aligned"), 
                width = 0.2, linewidth = 1.5) +
  geom_errorbar(aes(ymin = coef_with_ac_st_align_1 - 1.96*SE_with_ac_st_align_1, 
                    ymax = coef_with_ac_st_align_1 + 1.96*SE_with_ac_st_align_1, color = "Aligned"), 
                width = 0.2, linewidth = 1.5) +
  labs(title = "Firm Flow ~ Election",
       x = "Years to Election",
       y = "Avg Firm Flow") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.7) +
  scale_color_manual(name = "", values = c("Non-aligned" = "darkorange", "Aligned" = "darkolivegreen4")) +
  theme_classic()

# MORE TO DO: FILTER THE SAME ONLY FOR CLOSE ELECTION AC AND SEE HOW DOES THAT LOOK? 


# MERGING MCA-ST WITH AC-----------------------------------------------------------------

mca_election=mca_st_yr_flow %>% full_join(ac_el0, 
                                          by=c("state_code", "year"="tcpd_year"))

mca_election=mca_election %>%
  group_by(state_code) %>% 
  fill(assembly_no) %>% 
  group_by(state_code, assembly_no) %>% 
  arrange(state_code,year, assembly_no) %>% 
 # tidyr::fill(st_incumbant) %>% 
  ungroup()

mca_election$time_marker=NA
mca_election$time_marker[!is.na(mca_election$state_name)]=0

mca_election=mca_election %>% group_by(state_code) %>% 
  mutate(lg1=Lag(time_marker, 1), 
         lg2=Lag(time_marker, 2),
         ld1=Lag(time_marker,-1),
         ld2=Lag(time_marker, -2))

mca_election$time_marker[mca_election$lg1==0]=1
mca_election$time_marker[mca_election$lg2==0]=2
mca_election$time_marker[mca_election$ld1==0]=-1
mca_election$time_marker[mca_election$ld2==0]=-2


mca_el_etime=mca_election %>% mutate(time_marker=as.character(time_marker)) %>% 
  group_by(state_code, time_marker) %>% 
  mutate(firm_flow1=mean(firm_flow, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(st_yr=paste(state_code, year, sep="-"))

dd=mca_el_etime %>% filter(state_code %in% c("10","06",
                                             "32","23",
                                             "27","03",
                                             "08", "09",
                                             "33"))

fml=as.formula(paste0("firm_flow1~(time_marker) |state_code +year"))
reg=lfe::felm(fml, mca_el_etime);summary(reg)




# Plotting a event time plot
event_df=data.frame(time=c(-2,-1,0,1),
                    beta=c(reg$coefficients[1,1],
                           0,
                           reg$coefficients[2,1],
                           reg$coefficients[3,1]),
                    se=c(reg$se[1],
                         0,
                         reg$se[2],
                         reg$se[3]))


event_df$lower_bound <- event_df$beta - 1.96 * event_df$se
event_df$upper_bound <- event_df$beta + 1.96 * event_df$se

# Create the event time plot
ggplot(event_df, aes(x = time, y = beta, 
                     ymin = lower_bound, ymax = upper_bound)) +
  geom_point(shape = 21, size = 4, fill="black") +  # Adjust shape, size, and color as needed
  geom_errorbar(width = 0.2, linewidth = 1.5) +  # Adjust width and size as needed
  labs(title = "Firm Flow ~ Election",
       x = "Year to Election",
       y = "Avg Firm Flow") +
  geom_hline(yintercept = 0, linetype = 
               "dashed", color = "red", linewidth=0.7) +  # Adjust line type, color, and size
  theme_classic()


# Running state wise regression
# coef=c()
# mca_election_sub=mca_election %>% filter(state_code!="04") %>% 
#   filter(state_code!="11") %>% filter(state_code!="36") %>% 
#   filter(state_code!="01")
# 
# for(s in unique(mca_election_sub$state_code)){
#   
#   print(s)
#   dt=mca_election %>%  filter(state_code==s)
#   
#   fml=as.formula(paste0("firm_flow1~as.factor(time_marker)"))
#   reg_sub=lfe::felm(fml, dt)#;summary(reg)
#   coef[s]=reg_sub$coefficients[3,1]
#   
# }


# MERGING ASI WITH AC-----------------------------------------------------------------


asi_election=asi_st_yr_lvl %>% full_join(ac_el0, 
                                          by=c("state_code", "year"="tcpd_year"))


asi_election$time_marker=NA
asi_election$time_marker[!is.na(asi_election$state_name)]=0

asi_election=asi_election %>% 
  mutate(lg1=Lag(time_marker, 1), 
         lg2=Lag(time_marker, 2),
         ld1=Lag(time_marker,-1),
         ld2=Lag(time_marker, -2))
asi_election$time_marker[asi_election$lg1==0]=1
asi_election$time_marker[asi_election$lg2==0]=2
asi_election$time_marker[asi_election$ld1==0]=-1
asi_election$time_marker[asi_election$ld2==0]=2


asi_election1=asi_election %>% mutate(time_marker=as.character(time_marker)) %>% 
  group_by(state_code, time_marker) %>% 
  mutate(val=mean(tot_cost_prd, na.rm=T)) %>% 
  ungroup()


fml=as.formula(paste0("val~as.factor(time_marker)|state_code"))
reg=lfe::felm(fml, asi_election1);summary(reg)

