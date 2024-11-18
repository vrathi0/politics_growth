rm(list=ls())
source("code/functions.R")






# READING OFFICER LIST ----------------------------------------------------

# this list has details of all the post help by each listed officer in RJ cadre. 

raw_dt=read_excel("../Data/IAS-tcpd/RJ/Inservice_Officer_Details1.xlsx") %>% 
  clean_names()

raw_dt=raw_dt %>% fill(unique_id)
raw_dt=raw_dt %>% group_by(unique_id) %>% 
  fill(service_type, name_of_officer, date_of_birth,home_town,
       qualification) %>%  ungroup()

raw_dt1=raw_dt %>% mutate(order_date=as.Date(substr(order_joining, 1, 10), 
                                            format = "%d/%m/%Y"), 
                         join_date=as.Date(substr(order_joining, 11, 20), 
                                           format = "%d/%m/%Y")) %>% 
  mutate(end_date=as.Date(date_to,format = "%d/%m/%Y")) %>% 
  mutate(term_duration=end_date-join_date) %>% group_by(unique_id) %>% 
  mutate(post_count=n()) %>% ungroup() %>% 
  mutate(end_year=year(end_date), 
         order_year=year(order_date), 
         join_year=year(join_date))







# READING POST LEVEL DATA


post_dt=read_csv("../Data/IAS-tcpd/RJ/PostHistroy_Services1.csv") %>% 
  clean_names()

post_dt1=post_dt %>% 
  mutate(order_date=as.Date(order_date,format = "%d/%m/%Y") ,
         join_date=as.Date(order_date,format = "%d/%m/%Y"),
         end_date=as.Date(relieving_date,format = "%d/%m/%Y")) %>% 
  mutate(sno= gsub("[\r\n\t.]", "", sno)) %>% 
  mutate(sno=gsub("[[:space:].]", "", sno)) %>% 
  mutate(district=sub(".+?,\\s*(.+)$", "\\1", post_description)) %>% 
  mutate(post_id= cumsum(sno == 1)) %>% 
  group_by(post_id) %>% 
  mutate(post_start_year=year(min(order_date, na.rm=T))) %>% 
  mutate(term_duration=end_date-join_date)
