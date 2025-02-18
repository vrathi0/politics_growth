rm(list=ls())
source("code/functions.R")


# FUNCTIONS

drop_subsets <- function(titles) {
  # Sort titles by length in descending order
  titles_sorted <- titles[order(nchar(titles), decreasing = TRUE)]
  
  # Initialize a vector to hold titles that are not subsets
  non_subset_titles <- c()
  
  # Loop through sorted titles and add to non_subset_titles if not a subset
  for (title in titles_sorted) {
    if (!any(sapply(non_subset_titles, function(t) grepl(title, t)))) {
      non_subset_titles <- c(non_subset_titles, title)
    }
  }
  
  return(non_subset_titles)
}


find_designations <- function(job_desc, designations) {
  # This will hold the matched designations
  matched_designations <- sapply(designations, function(desig) {
    if(grepl(desig, job_desc)) {
      return(desig)
    }
  })
  
  # Remove NULL values and collapse into a single string separated by commas
  matched_designations <- paste(na.omit(matched_designations), collapse = ", ")
  
  # Return the matched designations
  return(matched_designations)
}



# THIS CODE IS TO GET THE UP IAS POSTING DATA. 

# The DS seems straight forward, one page for all the current postings, with 
#hyperlink
# to all their previous posting.

# According to gradataion list 2023, total strength of UP cadre 
#is supposed to be 652


# Level 1 : base URL ------------------------------------------------------



# url0 <- "https://niyuktionline.upsdc.gov.in/civil-list-ias.htm"
# doc <- read_html(url0)
# current_posting <- html_table(doc, fill = TRUE)[[1]] %>% clean_names()
# 
# 
# 
# 
# 
# 
# # Level 2 : List of URLs ------------------------------------------------------
# 
# 
# # Extract the first table on the page
# t1 <- html_nodes(doc, "table")[[1]]
# 
# # Extract the links from the last column of the table
# links <- html_nodes(t1, "td:last-child a") %>% html_attr("href")
# 
# 
# # Now to extract the posting details from individual specific URLs
# p_details_list=list();posting_details_list=list()
# for(l in 1:length(links)){
# 
#  if(round(l/50)==l/50){
#    print(l)
#  }
#   # Specify the url
#   url <- links[l]
# 
# # Read HTML content from the URL
#   webpage <- read_html(url)
# 
#   # Extract Personal Details part using CSS selectors
#   personal_details1 <- webpage %>%
#     html_nodes(css = "#contentArea span") %>% 
#     html_text()
# 
#   personal_details2=webpage %>% 
#     html_nodes(css=".formRight") %>% 
#     html_text()
# 
#   
#   personal_details_df=t(cbind(personal_details1, personal_details2))
#   colnames(personal_details_df)=personal_details_df[1,]
#   personal_details_df=personal_details_df %>% as.data.frame() 
#   personal_details_df=personal_details_df[-1,] %>% 
#       clean_names() %>% mutate_all(str_trim)
#   
#   
#   p_details_list[[l]]=personal_details_df
#   
#   
# 
#     # Extract Posting Details table using CSS selectors (modify this according to actual structure)
#     posting_details <- webpage %>%
#       html_nodes(css = ".tableOut") %>% 
#       html_table()
#     
#     posting_details=posting_details[[1]]
#     posting_details$officers_name=personal_details_df$officers_name
#     posting_details$id_no=personal_details_df$id_no
#     
#     
#     
#     posting_details_list[[l]]=posting_details
#     
#     
# }
# 
# 
# write_rds(p_details_list, "../Data/Civil Service Dir/UP/personal_details.rds", compress = "gz")
# write_rds(posting_details_list, "../Data/Civil Service Dir/UP/posting_details.rds", compress = "gz")
# write_rds(current_posting, "../Data/Civil Service Dir/UP/current_posting.rds", compress = "gz")

# # Load required libraries
# library(magick)
# #library(tesseract)
# 
# # Read the image (replace with the actual path to your image)
# input <- image_read("data/interm/up_post.jpeg") %>%
#   # Preprocess the image
#   image_convert(type = 'Grayscale') %>%
#   image_deskew() %>%
#   image_resize("2000x") %>%
#   ocr()
# 
# # Convert the OCR result to a data frame
# df <- data.frame(text = input$text)
# 
# # Extract the text as a character vector
# text_vector <- df$text
# 
# # Print the character vector (you can save it or process it further)
# print(text_vector)


# CLEANING ----------------------------------------------------------------

person_det=read_rds("../Data/Civil Service Dir/UP/personal_details.rds")
posting=read_rds("../Data/Civil Service Dir/UP/posting_details.rds")
current_posting=read_rds("../Data/Civil Service Dir/UP/current_posting.rds")
# up_clist_df=read_excel("data/interm/1715120383901=up_post.xlsx", col_names = c("A","B","C"))
# up_clist=c(as.character(up_clist_df$A), as.character(up_clist_df$C))
# up_clist=toupper(str_squish(up_clist))
# up_clist[up_clist=="SPECIAL SECRETARY TO GOVERMENT"]="SPECIAL SECRETARY TO GOVERNMENT"

up_desig_dept=read_excel("data/interm/desig_dept_UP.xlsx")
up_desig_dept=up_desig_dept %>% 
  mutate(designation=str_remove(designation, ","), 
         designation=str_trim(designation), 
         department=str_trim(department),
         department=str_remove(department, ", Uttar Pradesh$"),
         department=str_remove(department, "DEPARTMENT"),
         department=str_trim(str_remove(department, ",")), 
         department=str_remove(department, "\\d+"))

up_desig=toupper(unique(up_desig_dept$designation))
up_desig <- up_desig[!is.na(up_desig)]
up_dept=toupper(unique(up_desig_dept$department))
up_dept <- up_dept[!is.na(up_dept)]
up_dept=up_dept[!(up_dept=="")]
up_dept=str_trim(str_remove(up_dept, "DEPARTMENT"))


d_list=read_excel("data/interm/desig_list.xlsx")

additional_desig=c("JOINT MAGISTRATE","JOINT. MAGISTRATE","DISTRICT MAGISTRATE",
                   "ON WAITING","LBSNAA",
                   "C.E.O")

desig_list=unique(c(up_desig, d_list$designation, additional_desig))


# PERSONAL DF
person_det_df=person_det %>%  bind_rows() 
rownames(person_det_df)=NULL

person_det_df=person_det_df %>% 
  mutate(ias_date1=as.Date(date_of_appointment_to_ias, format = "%d/%m/%Y"), 
         govt_date=as.Date(date_of_entry_to_govt_service, format = "%d/%m/%Y"),
         ias_date2=as.Date(date_of_confirmation_to_ias, format = "%d/%m/%Y"),
         empnl_date=as.Date(officiating_in_senior_scale, format = "%d/%m/%Y")) %>% 
  mutate(ias_year1=year(ias_date1), govt_year=year(govt_date),
         ias_year2=year(ias_date2), empnl_year=year(empnl_date)) %>% 
  mutate(batch_year=as.numeric(year)) %>% 
  dplyr::select( id_no, ias_year1, ias_date1, 
                ias_date2, ias_year2, govt_date, govt_year, 
                empnl_date, empnl_year, batch_year, pay_scale, 
                qualifications) %>% distinct()

# Removing waiting posting (NOT REMOVING, IT CAN BE AN OUTCOME VA
# JUST LIKE UNDER SUSPENSION IN RJ)

#posting_df1 =posting_df %>% filter(grepl("Waiting", posts_held))
up_posting=posting_df %>% mutate(st_name="UTTAR PRADESH") %>% 
  rename(district_name=dist_name)

up_posting$district_name[up_posting$district_name=="GAUTAMBUDHNAGAR"]="GAUTTAM BUDH NAGAR"




# POSTING DATA

posting_df=posting %>%  bind_rows() %>% clean_names() %>% 
  mutate(oid=cumsum(sr_no==1))

posting_df=posting_df %>% 
  mutate(start_date=as.Date(join_date, format = "%d/%m/%Y"), 
           end_date=as.Date(rel_date, format = "%d/%m/%Y")) %>% 
  mutate(start_year=year(start_date), end_year=year(end_date)) %>% 
  mutate(duration=end_date-start_date) %>% group_by(oid) %>% 
  mutate(off_start_year=min(start_year, na.rm=T), 
         off_end_year=max(end_year, na.rm = T)) %>% ungroup()

# Removing waiting posting (NOT REMOVING, IT CAN BE AN OUTCOME VA
# JUST LIKE UNDER SUSPENSION IN RJ)

#posting_df1 =posting_df %>% filter(grepl("Waiting", posts_held))
up_posting=posting_df %>% mutate(st_name="UTTAR PRADESH") %>% 
  rename(district_name=dist_name)

up_posting$district_name[up_posting$district_name=="GAUTAMBUDHNAGAR"]="GAUTTAM BUDH NAGAR"


# DATA SANITY
check_df=up_posting %>% group_by(officers_name, start_date) %>% 
  mutate(start_dup=row_number()) %>% ungroup() %>% 
  group_by(officers_name, end_date) %>% 
  mutate(end_dup=row_number()) %>%  ungroup()
# STOPGAP: Need better solution
# I am limiting one job posting per officer at one time. 
# This is not true sometimes and one officer can hold multiple postings
# Need a better way to deal with it ( does it matter?)

#
# up_posting1=up_posting %>% group_by(officers_name, start_date) %>% 
#   mutate(start_dup=row_number()) %>% ungroup() %>% 
#   group_by(officers_name, end_date) %>% 
#   mutate(end_dup=row_number()) %>%  ungroup() %>% 
#   filter(start_dup==1) %>% filter(end_dup==1) %>% 
#   dplyr::select(-start_dup, -end_dup, -sr_no, 
#                 -join_date, -rel_date)

up_posting=up_posting %>% dplyr::select(id_no, officers_name, 
                                        start_date, end_date, 
                                        posts_held, everything()) %>% 
  arrange(id_no, officers_name, start_date) %>% 
  mutate(posts_held=toupper(posts_held))

up_posting=up_posting %>%  inner_join(person_det_df)


# CATEGORIZING POST INFORMATION

# DESIGNATION PROC --------------------------------------------------------

up_posting=up_posting %>% 
  mutate(posts_held=str_replace(posts_held, "SECY.", 
                                "SECRETARY"),
         posts_held=str_replace(posts_held, "GOVT|GOVT.", 
                                "GOVERNMENT"),
         posts_held=str_replace(posts_held, "SPL.", 
                                "SPECIAL "),
         posts_held=str_replace(posts_held, "JT.", 
                                "JOINT "),
         posts_held=if_else(grepl("COLLECTOR|D.M.", posts_held), 
                              "DISTRICT MAGISTRATES", posts_held),
         posts_held=str_replace(posts_held, "MAGISTRATE ", 
                                "MAGISTRATES "),
         posts_held=str_replace(posts_held, "DISTRIC ", 
                                "DISTRICT "), 
         posts_held=str_replace_all(posts_held, "\\(|\\)",""),
         posts_held=str_replace(posts_held, "ADDL.COMMR.", 
                                "ADDITIONAL COMMISSIONER"), 
         posts_held=str_replace(posts_held, "ASST.|ASSTT.|ASSTTT.", 
                                "ASSISTANT ")
         )


pinfo=str_squish(unique(up_posting$posts_held))

# Apply the function over the job descriptions vector
designation_info <- sapply(pinfo, find_designations, designations = desig_list)
designation_info=gsub("NULL", "", designation_info)
designation_info <- gsub("(?![a-zA-Z])\\s,", "", designation_info, perl = TRUE)
designation_info <- gsub("^,\\s*|\\s*,\\s$", "", designation_info)

# Combine the job descriptions with their corresponding designation information
post_desig_df <- data.frame(posts_held = pinfo, desig = as.character(designation_info))
post_desig_df=post_desig_df %>% mutate(post_count=str_count(posts_held, "\\+")+1, 
                                       post_count2=str_count(posts_held, "\\+")+1+
                                         str_count(posts_held, "\\&"))


# Use the function to drop subset strings
post_desig_df$non_subset_titles <- sapply(strsplit(as.character(post_desig_df$desig), ", "),
                                          function(titles) {
  paste(drop_subsets(titles), collapse = ", ")
})


post_desig_df=post_desig_df %>% dplyr::select(-desig) %>% 
  rename(desig_vec=non_subset_titles)



# NOW MERGING THEM BACK to the main dataset via posts_held

up_posting=up_posting %>% inner_join(post_desig_df)



# CATEGORIZING DEPARTMENT INFO --------------------------------------------

pinfo=str_squish(unique(up_posting$posts_held))

# Apply the function over the job descriptions vector
dept_info <- sapply(pinfo, find_designations, designations = up_dept)
dept_info=gsub("NULL", "", dept_info)
dept_info <- gsub("(?![a-zA-Z])\\s,", "", dept_info, perl = TRUE)
dept_info <- gsub("^,\\s*|\\s*,\\s$", "", dept_info)

post_dept_df <- data.frame(posts_held = pinfo, dept = as.character(dept_info))

up_posting=up_posting %>% inner_join(post_dept_df)


# RANKING DESIGNATION BASED ON TENURE LENGTH ------------------------------

up_posting=up_posting %>% 
  mutate(tenure_length=start_year-batch_year) %>% distinct()

up_posting=up_posting %>% 
  separate(desig_vec, 
           into = c("desig1", "desig2", "desig3",
                    "desig4", "desig5"), 
           sep = ",\\s*") %>% 
 group_by(id_no, start_date) %>% 
  pivot_longer(cols = starts_with("desig"), 
               names_to = "tno", 
               values_to = "designation") %>% 
  ungroup() %>% filter(!is.na(designation)) %>% 
  mutate(designation=str_trim(designation)) %>% 
  mutate(under_training=if_else(grepl("UNDER TRAINING|TRAINING|LBSNAA", designation),1,0)) %>% 
  mutate(tno=sub("desig","", tno)) %>% group_by(oid, start_date) %>% 
  mutate(under_training=max(under_training, na.rm=T)) %>% ungroup()

desig_lvl_df=up_posting %>% 
   group_by(designation,  under_training) %>% 
  summarise(desig_order_mean=mean(tenure_length, na.rm=T), 
            desig_order_sd=sd(tenure_length, na.rm=T), 
            count=n()) %>% 
  mutate(desig_order_cov=desig_order_sd/desig_order_mean) %>% ungroup() %>% 
  dplyr::select(designation,  desig_order_mean,under_training) %>% 
  distinct() %>% 
  mutate(desig_lvl=floor(desig_order_mean/2)+1) %>% 
  dplyr::select(-desig_order_mean) %>% distinct()


up_posting=up_posting %>% inner_join(desig_lvl_df)

# Now categorizing shifts

up_posting=up_posting %>% group_by(id_no, start_date) %>%
  mutate(desig_lvl=min(desig_lvl, na.rm = T)) %>% 
  group_by(id_no) %>%  arrange(start_date) %>% 
  mutate(next_desig_lvl=Lag(desig_lvl, -1), 
         prev_desig_lvl=Lag(desig_lvl, 1)) %>% ungroup() %>% 
  group_by(id_no, start_date) %>% 
  mutate(next_desig_lvl=max(next_desig_lvl, na.rm=T), 
         prev_desig_lvl=min(prev_desig_lvl, na.rm=T)) %>% ungroup()


# Now we can measure each shift category per unit time (year)

up_posting=up_posting %>% 
  mutate(shift_indc=if_else(desig_lvl==prev_desig_lvl,0, 
                            if_else(desig_lvl>prev_desig_lvl,1,-1))) %>% 
  mutate(shift_indc=if_else(prev_desig_lvl==Inf| prev_desig_lvl==-Inf,NA_real_, shift_indc))





qsavem(up_posting,  file="data/clean/UP_CS.qs")


## END END @@@####

# posting_df=up_posting
# # Counting the people joining at district-year level ( this is a measure of churn)
# 
# dist_year_den=posting_df %>% group_by(st_name,district, end_year) %>% 
#           summarise(dist_year_churn=n(), 
#                     mean_duration=mean(as.numeric(duration)))
# 
# 
# ac_shp <- st_read("../Data/MAPS- INDIA/AC/maps-master/assembly-constituencies/India_AC.shp") %>% 
#   st_make_valid()
# 
# ac_df=ac_shp %>%  as.data.frame() %>%  clean_names() %>% rename(district=dist_name)
# 
# ac_df=ac_df %>%  filter(st_name=="UTTAR PRADESH") %>% 
#   mutate(district=str_extract(district,"\\w+")) %>% 
#   dplyr::select(st_name, district, ac_no, ac_name, pc_no, pc_name) %>% distinct() %>% 
#   mutate(ac_no=padzero(ac_no, 3), ac_name=toupper(ac_name)) %>% 
#   mutate(ac_name=gsub("\\s*\\([^)]+\\)", "", ac_name))
# 
# dist_year_den$district[dist_year_den$district=="GAUTAMBUDHNAGAR"]="GAUTTAM BUDH NAGAR"
# dist_ac_year_den=dist_year_den #%>% inner_join(ac_df)
# #Joining, by = c("st_name", "district")
# 
# election_df=read_rds(here("data/clean/elec_candX_inner.rds"))
# 
# up_election_winner=election_df %>% filter(position==1 & state_name=="Uttar_Pradesh") %>% 
#   dplyr::select(state_name, ac_no, tcpd_year, assembly_no,
#                 month, sex, tcpd_age, constituency_name, 
#                 district_name, win_margin, adr_district_name,
#                 incumbent,assets, num_crim, crime_major, ed ) %>% distinct() %>% 
#   mutate(district=if_else(is.na(district_name), toupper(adr_district_name), district_name)) %>% 
#   group_by(district, tcpd_year) %>% 
#   summarise(assets=sum(assets), 
#             num_crim=sum(num_crim), 
#             ed=mean(ed)) %>% ungroup() %>% 
#   rename(year=tcpd_year) %>% ungroup()
# 
# 
# 
# 
# ##FIXX: in up_election data for year 2007 district name are missing, might need to pull from somewhere using AC_NAME
# 
# dist_el_den=dist_ac_year_den %>% filter(end_year>=2007)
# dist_el_den$year=NA
# dist_el_den$year[dist_el_den$end_year %in% c(2007:2011)]=2007
# dist_el_den$year[dist_el_den$end_year %in% c(2012:2016)]=2012
# dist_el_den$year[dist_el_den$end_year %in% c(2017:2021)]=2017
# 
# # Constructing y vars: average and lagged
# 
# dist_el_den = dist_el_den %>% group_by(district, year) %>% 
#   mutate(y_total=sum(dist_year_churn), 
#          mean_duration=mean(mean_duration, na.rm=T)) 
# 
# dist_el_den=dist_el_den %>%  filter(end_year==year)
# 
# 
# dt=dist_el_den %>% inner_join(up_election_winner) %>% mutate(assets=assets/1e6) %>% 
#   mutate(asset2=assets^2) %>% filter(!is.na(district))
# #Joining, by = c("district", "year")
# 
# # Clipping away the outlier assets
# dt=dt %>% mutate(assets=clipp(assets, 0.0, 0.95)) %>% filter(!is.na(assets)) %>% 
#   filter(district!="LUCKNOW")
# 
# 
# 
# fml=as.formula(paste0("y_total~ num_crim | district"))
# reg_total=felm(fml, dt); summary(reg_total)






