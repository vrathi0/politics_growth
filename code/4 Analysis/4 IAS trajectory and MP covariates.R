
rm(list=ls())
source("code/functions.R")





# 1 READING THE MERGED PANEL ----------------------------------------------


ge_ias=qread("data/3 MERGED SAMPLES/ge_ias_merged.qs")

ias_exp=qread("data/tcpd_ias_exp.qs")
ias_exp=ias_exp %>% 
  mutate(cadre_st=tolower(cadre)) %>% 
  mutate(cadre_st=str_replace(cadre_st," ","_"))#

# 2 IAS TRAJECTORY --------------------------------------------------------
# Setting up structure to compute a running Prob(success) for IAS officers. 


# Rough IAS level exposure 

ias_exposure=ge_ias %>% 
  group_by(id,cadre_st) %>%
  mutate(n_gen=pc_type=="GEN",
         n_sc=pc_type=="SC",
         n_st=pc_type=="ST",
         avg_no_terms=mean(no_terms, na.rm = T),
         avg_n_question=mean(n_question, na.rm = T)) %>% 
  ungroup()

ias_pid_match=ge_ias %>% ungroup() %>% 
  filter(year>=2000) %>% 
  filter(!is.na(pid)) %>%
  filter(!is.na(id)) %>%
  mutate(portfolio_count=if_else(is.na(portfolio_count),0,portfolio_count),
         mos_count=if_else(is.na(mos_count),0,mos_count),
         mos_ic_count=if_else(is.na(mos_ic_count),0,mos_ic_count),
         cm_count=if_else(is.na(cm_count),0,cm_count)) %>%
  group_by(id, pid) %>% 
  mutate(pair_duration=n_distinct(year,month), 
         pair_assembly=n_distinct(assembly_no), 
         pair_district=n_distinct(district),
         pair_posting=n_distinct(post_no), 
         bizz=sum(bizz, na.rm=T), 
         no_terms=max(no_terms, na.rm=T), 
         politics=sum(politics, na.rm=T), 
         ex_gov=sum(ex_gov, na.rm=T), 
         id_centre_deput=max(id_centre_deput, na.rm=T)) %>%
  ungroup() %>% 
  dplyr::select(id, pid, starts_with("pair_") , no_terms, bizz, 
politics, ex_gov, portfolio_count, mos_count,mos_ic_count,cm_count,
id_centre_deput) %>% 
  distinct()


fml=as.formula("id_centre_deput ~  no_terms +pair_assembly|0")
reg=felm(fml, ias_pid_match);summary(reg)                           



# 3 RAW IAS CHURN --------------------------------------------------------
# Getting a sense of scale of monthly raw churn at various levels of IAS
# can also line it up with LS elections

grade_st_month_churn=ias_exp %>% group_by(cadre_st,grade, start_year, 
                                       start_month) %>% 
  summarise(monthly_churn=n()) %>% 
  ungroup() %>% 
  arrange(grade, start_year, start_month) %>% 
  filter(start_year>=2000) %>% 
  group_by(start_year, start_month) %>%  # Group by year and month
  mutate(time = cur_group_id()) %>%  # Assign a sequential ID for each group
  ungroup()
  
grade_yr_mth_churn=ias_exp %>% group_by(grade, start_year, 
                                          start_month) %>% 
  summarise(monthly_churn=n()) %>% 
  ungroup() %>% 
  arrange(grade, start_year, start_month) %>% 
  filter(start_year>=2000 & start_year<=2020) %>% 
  group_by(start_year, start_month) %>%  # Group by year and month
  mutate(time = cur_group_id()) %>%  # Assign a sequential ID for each group
  ungroup() %>% 
  mutate(quarter=ceil(time/3)) %>% 
  filter(grade<17) %>% 
  group_by(grade) %>% 
  mutate(monthly_churn_q=ntile(monthly_churn,4)) %>% ungroup() %>% 
  mutate(election=case_when(start_year==2004 & start_month==5 ~ 1,
                            start_year==2009 & start_month==5 ~ 1,
                            start_year==2014 & start_month==5 ~ 1,
                            start_year==2019 & start_month==5 ~ 1,
                            TRUE ~ NA_real_)) %>% 
  group_by(grade, quarter) %>%
  mutate(quarter_churn=sum(monthly_churn)) %>% ungroup() %>% 
  group_by(grade) %>% 
  mutate(quarter_churn_q=ntile(quarter_churn,4)) %>% ungroup() %>% 
  group_by(quarter) %>% 
  arrange(quarter,time) %>% 
  tidyr::fill(election, .direction = "updown") %>% ungroup() %>% 
  mutate(election=ifelse(is.na(election),0,election)) 

grade_yr_quarterly=grade_yr_mth_churn %>% 
  dplyr::select(-start_month,-monthly_churn,-time, -monthly_churn_q) %>% 
  distinct()  

grade_qtly_duration=ias_exp %>% 
  filter(start_year>=2000 & start_year<=2020) %>% 
  group_by(cadre_st,grade, start_year) %>%
  mutate(grade_st_duration=mean(as.numeric(duration), na.rm=T)) %>% 
  group_by(grade, start_year) %>%
  mutate(grade_duration=mean(as.numeric(duration), na.rm=T)) %>%
  dplyr::select(grade, cadre_st, grade_st_duration,
                grade_duration, start_year) %>% distinct()#
  
                
fml=as.formula("grade_st_duration ~  grade + start_year + grade:start_year|cadre_st")
fml2=as.formula("grade_duration ~  grade + start_year +grade:start_year|0")

reg1=felm(fml, grade_yr_duration);summary(reg1)
reg2=felm(fml2, grade_yr_duration);summary(reg2)

grade_mth_churn=ias_exp %>% 
  filter(start_year>=2000 & start_year<=2020) %>% 
  group_by(grade, start_month) %>% 
  summarise(monthly_churn=n()) %>% 
  ungroup() 


library(ggplot2)
library(dplyr)

# Create a list of unique grades
grades <- unique(grade_yr_quarterly$grade)

# Initialize an empty list to store plots
plots_list <- list()

# Loop over each grade and create a plot
for (g in grades) {
  # Filter data for the current grade
  grade_data <- grade_yr_quarterly %>% filter(grade == g)
  
  # Create the plot
  p <- ggplot(grade_data, aes(x = quarter, y = quarter_churn_q)) +
    #geom_line(color = "cyan4", size = 1) +   # Add a line for the trend
    geom_point(size = 2, color = "firebrick1") +   # Add points for each quarter
    scale_x_continuous(breaks = seq(min(grade_data$quarter), 
                                    max(grade_data$quarter), by = 4)) + # Adjust x-axis
    labs(
      title = paste("Grade:", g),
      x = "Quarter",
      y = "Churn quantile by grade"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 12),        # Adjust text size for readability
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
    )
  
  # Add the plot to the list
  plots_list[[g]] <- p
}

# Define the output PDF file
#pdf("fig/ias_grade_quaterly_churn_q.pdf", width = 10, height = 7)

# Loop through the list of plots
for (g in 10:16) {
  # Print the plot for each grade
  print(
    plots_list[[g]] +
      theme(
        plot.margin = margin(0.5, 0.5, 1.5, 0.5, "cm")  # Adjust space for footnote
      ) +
      annotate("text", x = Inf, y = -Inf, label = "quarterly churn q for each grade", 
               hjust = 1.1, vjust = -0.5, size = 4, color = "black")
  )
}

# Close the PDF device
#dev.off()



# 4 DISTRICT CHARACTERISTICS --------------------------------------------------------

# here I am trying to put together some district level covars that
# might be indicator of district economic potential (ideally at baseline)

### MINERAL MINING DATA FROM ASHER ET AL

dist_mine_df=read_dta("../Data/Replication/Asher_mining_India/mining_replication_data/con_mine_shocks.dta")

dist_mine_df=dist_mine_df %>% 
  dplyr::select(con_id_joint, year, value, value_wt, num_deps,
                num_deps_wt, num_deps_lg, num_deps_lg_wt,
                any_coal, any_iron, mineral_herf, cgroup,
                election_number, con_id, pc01_state_id ,
                pc01_district_id ,con_id08) %>% distinct()

con_dist_state=dist_mine_df %>% 
  dplyr::select(con_id_joint, pc01_state_id, pc01_district_id) %>% 
  distinct()

dd=con_dist_state %>% inner_join(pc_id)


dist_mine_df2=read_dta("../Data/Replication/Asher_mining_India/mining_replication_data/mining_con_adr.dta")

dist_mine_df2=dist_mine_df2 %>% 
  dplyr::select(con_id_joint, year, sdgroup,
                base_value) %>% distinct() %>% 
  inner_join(con_dist_state, by="con_id_joint") %>% 
  inner_join(ac_pc_dist_delim_map)
  
#Jan 20: Next Steps
# 1. We have a measure of stronghold AC and PC. Do districts With
# stronghold AC/PC have some differential churn or some other metric 
# in IAS data

# 2. Does time spent in these districts have predictive power 
# for future promotion (remember the LASSO exercise)

# 3. Does presense of mining or industrial area predict strong hold?

# All these are with the intent of establishing/understanding
# baseline behaviour pre-delim. Then we can show how does that change
# with the delim-shock. 



# 5 MP-x and churn --------------------------------------------------------

# Does stronghold constituencies have differential churn

df=ge_ias

ias_count=df %>% 
  filter(!is.na(id)) %>% 
  group_by(state_name,constituency_name, year) %>% 
  mutate(pc_year_count=n_distinct(id)) %>% ungroup() %>% 
  group_by(state_name, constituency_name, assembly_no) %>% 
  mutate(syear=min(year, na.rm=T), 
         election_time=year-syear) %>% 
  ungroup() %>% 
  filter(election_time<=5)

ias_churn_extensive=df %>% 
  filter(!is.na(id)) %>% 
  dplyr::select(id, state_name, constituency_name, 
                assembly_no, year) %>% distinct() %>% 
  group_by(state_name, constituency_name, year) %>% 
  arrange(state_name, constituency_name, year) %>% 
  mutate(pc_year_count=n_distinct(id)) %>% ungroup() %>%
  mutate(pc_year_delta=pc_year_count-dplyr::lag(pc_year_count)) %>% 
  dplyr::select(state_name, constituency_name, year, 
                pc_year_count, pc_year_delta) %>% 
  distinct()#
# Seems like on extensive margin there is no churn.
# for more than 80% of the distribution, wild swings at the
# extreme tail


ias_churn_intensive=df %>% 
  filter(!is.na(id)) %>% 
  filter(assembly_no >=13) %>%
  group_by(id, post_no) %>%
  mutate(post_start_year=min(year, na.rm=T), 
         post_end_year=max(year, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(state_name, constituency_name, year) %>%
  mutate(id_incoming=sum(year==post_start_year), 
         id_outgoing=sum(year==post_end_year)) %>%
  ungroup() %>% 
  group_by(state_name, constituency_name, assembly_no) %>% 
  mutate(syear=min(year, na.rm=T), 
         election_time=year-syear) %>% 
  ungroup() %>% 
  filter(election_time<=5) %>% 
  dplyr::select(pid, no_terms, election_time, 
                assembly_no, state_name, constituency_name, 
                id_incoming, id_outgoing) %>% distinct()#

fml=as.formula(paste0("pc_year_count~no_terms + election_time|assembly_no+state_name"))

reg=felm(fml, ias_count);summary(reg)


# YES, 1SD INCREASE IN no_terms has 0.1SD Effect 
# 22% effect at the mean. 

# The story is that stable MPs are able to easily influence 
# the bureaucracy. 


# trying the above in an event-time framework

library(dplyr)

# Step 1: Identify high no_terms candidates and their last year in office
events <- ias_churn %>%
  filter(no_terms >= 2) %>%
  group_by(pid, state_name,constituency_name) %>%
  summarise(event_year = max(year, na.rm=T), 
            .groups = "drop") %>% 
  filter(event_year %in% c(2009)) %>% 
  group_by(state_name, constituency_name) %>% 
  mutate(event_count=n_distinct(event_year)) %>% 
  ungroup() %>% filter(event_count==1)



# Step 2: Merge event year back to the main data
data_with_events <- ias_churn %>%
  left_join(events, by = c("pid", "state_name",
                           "constituency_name"))

# Step 3: Create event-time variable
data_with_events <- data_with_events %>%
  group_by(state_name, constituency_name) %>% 
  arrange(state_name, constituency_name, year) %>% 
  fill(event_year, .direction = "down") %>% 
  ungroup() %>% 
  mutate(
    event_time = year - event_year,  # Calculate time relative to the event year
    is_event = if_else(year == event_year, 1, 0)  # Flag the event year
  )

# Step 4: Filter for relevant time window (e.g., -5 to +5 years around the event)
event_study_data <- data_with_events %>%
  filter(event_time >= -5 & event_time <= 5)

# Step 5: Create summary for visualization or further analysis
event_study_summary <- event_study_data %>%
  group_by(event_time) %>%
  summarise(
    et_pc_year_off_count = mean(pc_year_off_count, 
                                na.rm = TRUE),  # Average pc_year_off_count
    n = n()  # Count of observations
  ) %>% ungroup()


# One possible comparison:
# Look at the above event_study_summary. 
# The idea is that strong MP go through more IAS
# but towards the end they settle down and have found their match
# so there are a set of IAS who are with the strong MPs. 
# but then due to election shock (random only for 2009)
# the strong MP goes. But you still have the same officer
# So it is perhaps possible to observe the same officer with differnet
# MPs. So you are comparing 2/3 years both pre and post. 


# 22 Jan
# The task is to think of strong Pol + Bcrat relationship
# This is one state of the world, compared to the same bcrat 
# but without the strong MP/Pol. 
# A few ways we can get it: 
# 1. Strong Pol loosing election at margin
# 2. Strong Pol not even running (good foresight)
# 3. Strong Pol getting the 2008 shock.(remember the treatment
#      there last one year of 2008 as the desicion was public in Jan only)
# Then you can look at Y var before and after. 
# Then you will still need to worry about time variant stuff or selection 
# but thats for later. 



# 

