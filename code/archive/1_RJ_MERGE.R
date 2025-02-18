rm(list=ls())
source("code/functions.R")


# THIS CODE IS TO MERGE ALL THREE:
# 1. POST LEVEL CS DATA, TAGGED WITH BUDGET SECTORs 
# 2. SECTOR LEVEL BUDGET DATA (FROM RBI)
# 3. LIST OF GOVT SECTORS AND DEPARTMENT WITHIN (NOT NEEDED)
# 4. List of budget controlling officers. 

standardize_hyphens <- function(x) {
  # Replace various dash types with the standard hyphen-minus
  str_replace_all(x, "[\u002D\u2010\u2012\u2013\u2014]", "-")
}



# 1. READING DATA  --------------------------------------------------------

# reading the bud contr officers

bg_post <- read_excel("data/budget_control_post.xlsx") %>% 
  rename(raw_post=post)

bg_post=bg_post %>% 
  mutate(
    parts = str_split(raw_post, ",\\s*"), # Split by comma and spaces
    desig = sapply(parts, `[`, 1),         # First part
    state_dist = sapply(parts, function(x) x[length(x)]), # Last part
    rest = sapply(parts, function(x) paste(x[2:(length(x)-1)], collapse = ", ")) # Middle parts
  ) %>% mutate(rest=str_trim(str_replace(rest, "Rajasthan", ""))) %>% 
  mutate(rest=str_remove(rest,",$")) %>% mutate(rest=str_trim(rest)) %>% 
  dplyr::select(-parts) %>% 
  filter(!is.na(raw_post)) %>% distinct() %>% 
  rename(department=rest) %>% 
  mutate(department=str_remove(department, "Department")) %>% 
  mutate(department=str_trim(department)) %>% 
  mutate(department=toupper(department)) %>% 
  mutate(desig=toupper(desig)) %>% 
  mutate(department=str_trim(str_remove(department, "^,"))) %>% 
  dplyr::select(department, desig, everything()) %>% 
  arrange(department)


cs_dept_desig=source("code/cs_dept_desig_bgp.R") # THIS HAS designation+dept+bg_yn
cs_dept_desig=cs_dept_desig$value # THIS HAS designation+dept+bg_yn

qreadm("data/clean/state_budget.qs")
qreadm("data/clean/RJ_CS_wpar3.qs")

s1=sort(toupper(unique(post_sector_rj$sector1)))
s2=sort(toupper(unique(st_budget3$line_item)))

dept_vec1=toupper(unique(bg_post$department))
desig_vec1=toupper(unique(bg_post$desig))

dept_vec2=unique(post_sector_rj$department)
desig_vec2=toupper(unique(post_sector_rj$designation))

off_df=off_rj_clean



# 2. MERGING WITH CROSS SECTIONAL BUDGET ----------------------------------

# st_budget3 is cross-sectional budget, meaning covar at budget line item
# This can be used just to seggregate various departments (and designation along with bg_yn)
# Remember, that we see that for "R", the time series is smooth and separated. 
# "C" has unexpected shocks. Both "R" and "C" can be used for different purposes. 


post_df=post_sector_rj %>% rename(sector_li=sector1)
budget3_df=st_budget3 %>%  
  filter(state=="RJ") %>% dplyr::select(-state) %>% 
  mutate(line_item=toupper(line_item)) %>% 
  rename(sector_li=line_item) # Only keeping RJ
bgp_df=cs_dept_desig


merged_df=post_df %>% left_join(bgp_df)
#Joining, by = c("department", "designation")

merged_df=merged_df %>% left_join(budget3_df, by="sector_li")









# OLD



# 2. Merging BG posts with CS Post data -----------------------------------

# Doing it using following way:
# 1. From Dept+Desig in post CS data, filtering high level posts that 
#.  are present in the BG posts.
# 2. Creating a new dataframe which has just depat + filtered design
# 3. Then identifying BG post in that data manually. 


# First figuring out what vector of keywords
# will cover the whole bg_post designations

# dd=bg_post %>% filter(!grepl("REGISTRAR|DIRECTOR|COMMISSIONER|SECRETARY|PRINCIPAL|CHIEF|GENERAL",
#                              desig))
# 
# 
# bg_desig_vec=c("REGISTRAR|DIRECTOR|COMMISSIONER|SECRETARY|PRINCIPAL|CHIEF|GENERAL")
# 
# cs_dept_desig=post_sector_rj %>% 
#   filter(grepl(bg_desig_vec, designation)) %>% 
#   dplyr::select(designation, department) %>% distinct() %>% 
#   arrange(department)

cs_dept_desig=source("code/cs_dept_desig_bgp.R") # THIS HAS 
cs_dept_desig=cs_dept_desig$value
# cs_dept_desig =cs_dept_desig %>% arrange(department, designation)
# data_edit(cs_dept_desig,
#           code= "code/cs_dept_desig_bgp.R",
#           viewer = "browser",
#           #col_bind = c("bg_yn"),
#           col_options = list(bg_yn = c("","yes","maybe","no")))
                             

# NOTES:
# 1. FOREST has a LOTTT of sub category and they all have similar designation titles
#.   Need to look at it separately. Very confusing.
# 2. PUBLIC HEALTH ENGINEERING (PHED): CHIEF ENGINEER
# 3. Motor Garage: Controller (absent in CS data)
# 4. Water REsource PLanning Dept: Chief Engineer

# now we can merge this with the original data

bg_post_sector_rj=post_sector_rj %>% inner_join(cs_dept_desig)
#Joining, by = c("department", "designation")

# This right now has dept+design+bgpost+sector

post_dept_sector=source("code/post_dept_sector_tag.R")
post_dept_sector=post_dept_sector$value

# 3 MERGING WITH BUDGET -TIME SERIES ---------------------------------------------------

# This is merging sector level budget with post data that also has sector. 

year_month_df <- expand.grid(
  yr = 2003:2020,
  month = 1:12
)
year_month_df <- year_month_df[order(year_month_df$yr, year_month_df$month), ]



df_budget=state_budget %>% ungroup() %>% 
  mutate(sm=4, syr=year_be-1, em=3, eyr=year_be) %>% 
  mutate(sdate=as.Date(paste(syr, sm, "01", sep="-")), 
         edate=as.Date(paste(eyr, em, "31", sep="-")))


df_posting=bg_post_sector_rj %>% 
  mutate(start_month=padzero(start_month, 2), 
         end_month=padzero(end_month, 2)) %>% 
  mutate(sdate=as.Date(paste(as.character(start_year), 
                             as.character(start_month), "01", sep="-")), 
         edate=as.Date(paste(as.character(end_year), end_month, "31", sep="-")))





result <- fuzzy_left_join(
  df_posting,
  df_budget,
  by = c("sdate" = "sdate", "edate" = "edate"),
  match_fun = list(`<=`, `>=`)
)


as.Date(paste(df_postings$end_year, df_postings$end_month, "01", sep = "-"))

st_budget=year_month_df %>% left_join(st_budget, by=c("month","yr"))
st_budget=st_budget %>%  arrange(state, type, yr, month, match_word) %>% 
  dplyr::select(state, type, match_word, yr, month, everything())





st_budget_l=split(st_budget, st_budget$match_word)

li=unique(st_budget$match_word)
stb_list=list()
for(l in 1:length(li)){
  
  df=st_budget %>% filter(match_word==li[l]) %>% arrange(yr)
  df=year_month_df %>% left_join(df, by=c("month","yr")) 
  df=df %>% fill(everything(), .direction = "down")
  
  
}

st_budget=year_month_df %>% left_join(st_budget, by=c("month","yr"))





