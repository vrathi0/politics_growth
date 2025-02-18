rm(list=ls())
source("code/functions.R")

# FUNCTION 

# TO STANDARDIZE HYPHENS
standardize_hyphens <- function(x) {
  # Replace various dash types with the standard hyphen-minus
  str_replace_all(x, "[\u002D\u2010\u2012\u2013\u2014]", "-")
}


# EDIT NOTES (Stuff to add)
# 1. There is a lot of admin cate officers in CS data. So need to add:
#    - "Organs of state"
#.   - "Fiscal Services"

# 2. Need to add standalone "Communication"


# RBI LINK

#. https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=State%20Finances%20:%20A%20Study%20of%20Budgets




# url_list=c("https://prsindia.org/budgets/states/rajasthan-budget-analysis-2020-21",
#            "https://prsindia.org/budgets/states/rajasthan-budget-analysis-2021-22")
# 
# 
# page <- read_html(url_list[1])
# 
# # Extract all tables from the page
# all_tables <- page %>% html_table()
# 
# # Find the table with "Table 3" in the heading
# table_3 <- all_tables[[5]]  # Assuming "Table 3" is the third table on the page
# 

# Lets first read the excel files 2005 onwards that are present in the data/budget folder

# BUDGET READ NOTES:
# 1. In the C files, at present all the Non-development Capital expenses
#.   are either interests or some other kind of transfers. Excluding them for now. 

root="../Data/Budget"
flist=list.files(root, pattern = ".xls$|.xlsx", full.names = T)


# Revised: 
# Now the strategy is to read the first/anyone file, 
# get all the categories in a vector and then read using those categories 
# as a filter, read in lines that matches those categories. 

snames=excel_sheets(flist[4])
dread <- lapply(snames, function(sheet) {
  read_excel(flist[4], sheet = sheet)
})

dread=bind_rows(dread)
cname=unique(unlist(dread[,c(1:4)]))

# Read the .rtf file
vname <- readtext("../Data/Budget/line_items.rtf")
text=vname$text

# Remove all the leading numbers, standalone numbers, line breakers, and "|"
clean_text <- str_replace_all(text, "\\|", "")  # Remove "|"
clean_text <- str_replace_all(clean_text, "\\n", " ")  # Replace line breakers with space
clean_text <- str_replace_all(clean_text, 
                              "^\\d+\\s|\\s\\d+\\s|\\s\\d+$", " ")  # Remove leading or standalone numbers


# Split the cleaned text into a vector of words
words_vector <- str_split(clean_text, "\\*")[[1]]
words_vector=str_trim(str_squish(words_vector))
words_vector <- str_replace_all(words_vector, 
                                "^\\b(?:i{1,3}|^iv|^v(?:i{0,3})|^ix|^x|^xi|^xii|^III)\\)\\s", "")
words_vector <- str_replace_all(words_vector, 
                                "\\s*\\(.*?\\)\\s*", " ")
words_vector=unique(words_vector)
words_vector[words_vector==""]=NA
words_vector=words_vector[!is.na(words_vector)]

words_vector <- gsub("^\\d+\\.?\\s*", "", words_vector) # Remove leading numbers and potential dots followed by space
words_vector <- gsub("\\\\n$", "", words_vector) # Remove trailing newline escape sequence
words_vector=str_trim(words_vector)
words_vector=gsub("\\.", "", words_vector)
#words_vector=gsub("[[:punct:]]+$", "", words_vector)
#words_vector[words_vector=="III. Local Bodies and Panchayati Raj Institutions Grants"]="Local Bodies and Panchayati Raj Institutions Grants"
words_vector=unique(words_vector)
words_vector=standardize_hyphens(words_vector)
# Initialize an empty data frame to store the final result
final_data <- list()
nonmatch_rvec=list()

for (f in 1:length(flist)) { # looping over file list
  # Read the Excel file
  
  snames=excel_sheets(flist[f])
  dr <- lapply(snames, function(sheet) {
    read_excel(flist[f], sheet = sheet)
  })
  
  # Extract the "C" or "R" value
  cap_rev <- str_extract(flist[f], "(?<=/)[CR]")
  
  # extracting state 
  st=str_extract(flist[f], "(?<=_)[A-Z]+(?=_)")
  
  # Extract the year value
  yr <- str_extract(flist[f], "\\d{4}")
  
  cdt=list()
  
  for(l in 1:length(dr)){
  
  # Apply a rowwise function to first break the row into a character vector
    
    data=dr[[l]]    
    
    data$rvec<- apply(data, 1, function(x) return(paste(unlist(x), collapse=",")))
    data$rvec=standardize_hyphens(data$rvec)
        
        # Then check if any element of the row character vector matches with the input character vector
        df <- data %>% rowwise() %>% 
          mutate(match = any(sapply(words_vector, 
                                    function(x) any(grepl(x, rvec, ignore.case = TRUE))))) %>% 
          mutate(match_word = paste(words_vector[sapply(words_vector, 
                                                        function(x) any(grepl(x, rvec, ignore.case = TRUE)))], collapse=", "))
        # qq=sapply(words_vector, 
        #                function(x) regexpr(paste0("\\b", x, "\\b"), data$rvec, perl = F))
        
        # If there is a match then retain the row else discard it
        #df <- df %>% filter(match == TRUE)
        
        df=df[,seq(ncol(df)-15+1,ncol(df))]
        
        cn=names(df);cn=cn[!(cn%in% c("match", "match_word","rvec"))]

        #df=df %>% mutate_at(vars(all_of(cn)), as.numeric)
        # 
        # df2=janitor::remove_empty(df1, which = "cols")
        # df=df[,-1]
        
        
        
        cdt[[l]]=df
        
  }
  
  rdt=cdt %>% bind_rows()
  
  colnames(rdt)=c(paste0("y",1:12), "rvec", "match", "match_word")
  
  rdt$type=cap_rev;rdt$year=yr;rdt$state=st
  
  rdt=rdt %>% ungroup()
 
  # Concatenate all observations into one string
  combined_string <- paste(rdt$rvec, collapse = " ")
  
  # Determine the multiplier based on the presence of "Million" or "Lakh"
  multiplier <- if_else(str_detect(combined_string, "Million|million|Milion"), 10^6,
                        if_else(str_detect(combined_string, "Lakh|lakh"), 10^5, NA_real_))
  
  # Create the new variable in the dataframe
  rdt <- rdt %>%
    mutate(mult = multiplier)
  
  
  # FOR CAPITAL EXP FILE, ONLY TAKIGN DEVELOPMENT EXPENSES (as rest are interest and transfers)
  rno=min(which(grepl("Non-Development",rdt$rvec))) # ONLY FOR "C"
  
        # First lets filter from rno onwards
        if(unique(rdt$type)=="C"){
          
          rdt=rdt[1:rno,]
          rdt$match_word[rdt$match_word=="Village and Small Industries, Industries" &
                           rdt$type=="C"]="Village and Small Industries" 
          rdt$match_word[rdt$match_word=="Iron and Steel Industries, Industries" &
                           rdt$type=="C"]="Iron and Steel Industries" 
          
          }
       
  
  
        
        
  # FOR REVENUE EXP
        # 1. Removing "Communication" as its only a standalong cat in "C"
  rdt$match[rdt$match_word=="Communications" & rdt$type=="R"]=FALSE
  rdt$match_word[rdt$match_word=="Village and Small Industries, Industries" &
                   rdt$type=="R"]="Village and Small Industries"   
  rdt$match_word[rdt$match_word=="Flood Control, Flood Control and Drainage" &
                   rdt$type=="R"]="Flood Control and Drainage"   
  
  


  
  
  # if(rdt$type=="R"){
  #   
  #   rdt$sector1[rdt$match_word =rdt %>% 
  #     mutate(sector1=if_else(match_word %in% 
  #                              c("Education, Sports, Art and Culture",                                      
  #                            "Medical and Public Health",                                               
  #                           "Family Welfare",                                                          
  #                            "Water Supply and Sanitation",
  #                              "Housing",                                                                 
  #                            "Urban Development",                                                       
  #                             "Welfare of Scheduled Castes, Scheduled Tribes and Other Backward Classes",
  #                            "Labour and Labour Welfare",                                           
  #                             "Social Security and Welfare",                                             
  #                             "Nutrition",                                                               
  #                             "Relief on account of Natural Calamities",                                 
  #                            "Others"  )
  #   
  # }
        
  
  
  # recording all non-match rvecs in a list/vector so taht I can inspect it
  # Just to make sure that no budget line item was not left out (absent in words_vector)
  nonmatch_rvec[[f]]=rdt %>% filter(match==F) %>% dplyr::select(rvec)
  
  
  # Now filtering for only matched line items/budget heads 
  rdt=rdt %>% filter(match==T)
  
  # GENERATING ROW NUMBERS THAT CAN HELP ME IN IDENTIFYING DIFFERENT OTHERS, AND ELSE
  
  rdt=rdt %>% mutate(rno=row_number())
  rdt$sector=NA
  
  # Now I am trying to tag the sectors here only. 
  #. the way is to get the range dynamically for each sheet (relative position)
  # FOR C:
  if(unique(rdt$type)=="C"){  
          c_ss_range=seq(which(rdt$match_word=="Education, Sports, Art and Culture"),
                         which(rdt$match_word=="Crop Husbandry")-1)
          c_es_ag_range=seq(which(rdt$match_word=="Crop Husbandry"),
                            (which(rdt$match_word=="Rural Development")-1))
          c_es_irr_range=seq(which(rdt$match_word=="Special Area Programmes")+1,
                             (which(rdt$match_word=="Energy")-1))
          c_indus_range=seq(which(rdt$match_word=="Energy")+1, 
                          which(rdt$match_word=="Roads and Bridges")-1)
          c_trnspt_range=seq(which(rdt$match_word=="Roads and Bridges"), 
                           which(rdt$match_word=="Roads and Bridges")+1)
          c_es_gen=seq(which(rdt$match_word=="Tourism"), 
                       which(rdt$match_word=="Tourism")+1)
    
    ## FILL
          rdt$sector[rdt$rno %in% c_ss_range ]="Social Services"
          rdt$sector[rdt$rno %in% c_es_ag_range ]="Agriculture and Allied Activities"
          rdt$sector[rdt$rno %in% c_es_irr_range ]="Irrigation and Flood Control"
          rdt$sector[rdt$rno %in% c_indus_range ]="Industry and Minerals"
          rdt$sector[rdt$rno %in% c_trnspt_range ]="Transport"
          rdt$sector[rdt$rno %in% c_es_gen ]="General Economic Services"
          #rdt$sector[rdt$rno %in% r_as ]="Administrative Services"
          rdt=rdt %>% mutate(sector=if_else(is.na(sector), match_word, 
                                            sector))
  }
  
    
    
    # FOR R: 
    
    if(unique(rdt$type)=="R"){
      
        r_ss_range=seq(which(rdt$match_word=="Education, Sports, Art and Culture"),
                       which(rdt$match_word=="Crop Husbandry")-1)
        
        r_es_ag_range=seq(which(rdt$match_word=="Crop Husbandry"),
                          (which(rdt$match_word=="Other Agricultural Programmes")))
        r_es_irr_range=seq(which(rdt$match_word=="Flood Control"),
                          (which(rdt$match_word=="Flood Control and Drainage")))
        r_indus_range=seq(which(rdt$match_word=="Village and Small Industries"),
                           (which(rdt$match_word=="Roads and Bridges")-1))
        r_trnspt_range=seq(which(rdt$match_word=="Roads and Bridges"), 
                           which(rdt$match_word=="Roads and Bridges")+1)
        r_es_gen=seq(which(rdt$match_word=="Secretariat - Economic Services"), 
                           which(rdt$match_word=="Secretariat - Economic Services")+3)
        r_as=seq(which(rdt$match_word=="Secretariat - General Services"), 
                     nrow(rdt))
    
    
      
         rdt$sector[rdt$rno %in% r_ss_range ]="Social Services"
         rdt$sector[rdt$rno %in% r_es_ag_range ]="Agriculture and Allied Activities"
         rdt$sector[rdt$rno %in% r_es_irr_range ]="Irrigation and Flood Control"
         rdt$sector[rdt$rno %in% r_indus_range ]="Industry and Minerals"
         rdt$sector[rdt$rno %in% r_trnspt_range ]="Transport and Communications"
         rdt$sector[rdt$rno %in% r_es_gen ]="General Economic Services"
         rdt$sector[rdt$rno %in% r_as ]="Administrative Services"
         rdt=rdt %>% mutate(sector=if_else(is.na(sector), match_word, 
                                           sector))
         
         
    }
    
    
        
  # Append the filtered data to the final data frame
  final_data[[f]] <- rdt
  
  print(f)
}


# Merging all the sheets:
# Now we have all the sheets in all the files in the following rbind

final_df=final_data %>% bind_rows()



#final_df=final_df %>% mutate_at(vars(starts_with("y")), as.numeric)

# Trying to fix some line break glitch that has been 
# carried from the raw files

final_df=final_df %>% 
  mutate(match_word=if_else(match_word=="Major and Medium Irrigation and Flood Control, Flood Control, Major and Medium Irrigation", 
                            "Major and Medium Irrigation and Flood Control", match_word)) %>% 
  mutate(match_word=if_else(match_word=="Special Area Programmes"|match_word=="of which: Hill Areas", 
                            "Special Area Programmes of which: Hill Areas", match_word)) %>% 
  mutate(match_word=if_else(match_word=="Tribes and Other Backward Classes", 
                            "Welfare of Scheduled Castes, Scheduled Tribes and Other Backward Classes",
                            match_word)) %>% 
  mutate(match_word=if_else(match_word=="Non-Ferrous Mining and Metallurgical Industries, Industries", 
                            "Non-Ferrous Mining and Metallurgical Industries",
                            match_word)) %>% 
  mutate(match_word=if_else(match_word=="Welfare of Scheduled Castes, Scheduled Tribes and Other Backward Classes, Tribes and Other Backward Classes", 
                            "Welfare of Scheduled Castes, Scheduled Tribes and Other Backward Classes",
                            match_word)) #%>% 
  #group_by(match_word, type, year, state) %>% 
 # fill((starts_with("y")), .direction = "downup") %>% ungroup()


chk_df=final_df %>% group_by(match_word, type) %>% 
  summarise(count=n()) %>% ungroup() %>% arrange(type)


  
final_df=final_df %>% dplyr::select( -match) %>% 
  distinct()

# Multiplying in the multiplyer
final_df <- final_df %>%
  mutate(across(matches("^y[1-9]$|^y1[0-2]$"), ~ as.numeric(.) * mult))


final_df=final_df %>% 
  rename(plan_acc=y1,nonplan_acc=y2, total_acc=y3,
         plan_be=y4, nonplan_be=y5, total_be=y6, 
         plan_re=y7, nonplan_re=y8, total_re=y9) %>% 
  mutate(year=as.numeric(year))


# Now we have to assign year of record which is different than year from the filename. 
# the file format jumps around 2008-10, so a bit tricky. 

pre2k2=final_df %>% filter(year<=2008)
pre2k2=pre2k2 %>% mutate(year_acc=year-1, 
                         year_be=year, year_re=year)


post2k2=final_df %>% filter(year>=2010)
post2k2=post2k2 %>% mutate(year_acc=year-2, 
                         year_be=year-1, year_re=year-1)


st_budget=post2k2 %>% bind_rows(pre2k2)


# Post PROC

st_budget=st_budget %>%  rename(line_item=match_word)

accounts_df=st_budget %>% ungroup() %>% 
  dplyr::select(total_acc, line_item, type, state, sector, year_acc) %>% 
  distinct() %>% mutate(total_acc=total_acc/1e6) %>% 
  rename(year=year_acc) %>% arrange(state, type, line_item, year)

be_df=st_budget %>% ungroup() %>% 
  dplyr::select(total_be, line_item, type, state, sector, year_be) %>% 
  distinct() %>%  mutate(total_be=total_be/1e6) %>% 
  rename(year=year_be)%>% arrange(state, type, line_item, year)

re_df=st_budget %>% ungroup() %>% 
  dplyr::select(total_re, line_item, type, state, sector, year_re) %>% 
  distinct() %>% mutate(total_re=total_re/1e6) %>% 
  rename(year=year_re)%>% arrange(state, type, line_item, year)

st_budget2=accounts_df %>% inner_join(be_df) %>% inner_join(re_df) %>% 
  mutate(re_be_ratio=total_re/total_be, 
         acc_be_ratio=total_acc/total_be) %>%
  filter(!is.na(re_be_ratio)) %>% filter(!is.na(acc_be_ratio)) %>% 
  group_by(state, type, line_item) %>% 
  mutate(nobs=n()) %>% ungroup() %>% filter(nobs>2)

# Tagging Jumps (yoy abnormal changes)

st_budget2a <- st_budget2 %>% ungroup() %>% 
  mutate(total_acc=clipp(total_acc, 0.01, 0.99), 
         total_be=clipp(total_be, 0.01, 0.99), 
         total_re=clipp(total_re, 0.01, 0.99)) %>% 
  #mutate_at(c("total_acc", "total_be", "total_re"), log)
  group_by(state, type, line_item) %>%
  arrange(year) %>%
  mutate(total_acc_delta = total_acc - Lag(total_acc), 
         total_be_delta= total_be - Lag(total_be),
         total_re_delta= total_re - Lag(total_re)) %>%
  group_by(state, type, line_item) %>%
  mutate(total_acc_mean=mean(total_acc, na.rm=T), 
         total_be_mean=mean(total_be, na.rm=T),
         total_re_mean=mean(total_re, na.rm=T)) %>% 
  group_by(state, type) %>% 
  mutate(total_acc_q=cut(total_acc_mean, 4, labels=F),
         total_be_q=cut(total_be_mean, 4, labels=F),
         total_re_q=cut(total_re_mean, 4, labels=F)) %>% 
  ungroup()

dd=st_budget3 %>% dplyr::select(state, type, line_item, total_acc_q, 
                                total_be_q, total_re_q) %>% distinct()

# Calculate the standard deviation and mean of the YoY changes for each sector
st_budget2b <- st_budget2a %>%
  group_by(state, type, line_item) %>%
  mutate_at(vars(c("total_acc_delta", "total_be_delta", "total_re_delta")),
            list(mean = ~mean(., na.rm=T), sd = ~sd(., na.rm=T))) %>% 
  ungroup() %>% 
  mutate(z_acc = (total_acc_delta - total_acc_delta_mean) / total_acc_delta_sd) %>%
  mutate(abnormal_pos=z_acc>2) %>% 
  mutate(abnormal_neg= z_acc <= -1.5) %>% 
  group_by(state, type, line_item) %>% 
  mutate(abnormal_pos_count=sum(abnormal_pos, na.rm=T),
         abnormal_neg_count=sum(abnormal_neg, na.rm=T)) %>% 
   ungroup() #%>% 
  # dplyr::select(state, type, line_item, year, total_acc, 
  #               total_be, total_re, sector, abnormal_line, abnormal_change) %>% 
  # distinct()


# st_budget2 is a time series version
# st_budget3 is going to be a cross sectional version. Meaning classyfying line_items

st_budget3=st_budget2b %>% 
  dplyr::select(state, type, line_item, abnormal_pos_count,abnormal_neg_count,
                sector, 
                total_acc_q, total_be_q, total_re_q) %>% distinct()


# IN THE END, "year" in st_budget2 is the end of the respective financial year. 
# that is if year is 2010, then the year in question is 2009-2010. 

write.xlsx(final_df, "data/interm/state_budget.xlsx")

qsavem(st_budget2,st_budget3, file="data/clean/state_budget.qs")



# 
# # Get the names of all sheets in the Excel file
# #sheet_names <- excel_sheets(flist[1])
# vec1 <- c("y1a", "y2be", "y2re", "y3be")
# vec2 <- c("plan", "non-plan", "total")
# 
# # Generate all combinations
# colnames <- c("cat1","cat2",paste(rep(vec1[1],3), vec2, sep="_"),
#               paste(rep(vec1[2],3), vec2, sep="_"),
#               paste(rep(vec1[3],3), vec2, sep="_"),
#               paste(rep(vec1[4],3), vec2, sep="_"))
# 
# 
# # Read all sheets and store them in a list of data frames
# dat_list=list()
# for(f in 1:length(flist)){
#   
#   sheet_names <- excel_sheets(flist[f])
#   ll=list()
#   for(s in 1:length(sheet_names)){
#     df=read_excel(flist[f], sheet = sheet_names[s])
#     total_row_index <- which(grepl("^TOTAL|^\\d", df[[1]]))[1]
#     df=df[total_row_index:nrow(df), ]
#     ll[[s]]=df
#   }
#   ll=bind_rows(ll)
#   colnames(ll)=colnames
#   
#   dat_list[[f]]=ll
#   print(f)
#   
# }
# 
# 
# # Combine all data frames by row binding them
# combined_df <- bind_rows(dat_list)
# colnames(combined_df)=colnames
# 
# combined_df=combined_df %>% 
#   mutate(cat1=if_else(grepl("^\\d{1,2}$", cat1),NA_character_, cat1)) %>% 
#   fill(cat1, .direction = "down") %>% 
#   filter(grepl("Social Services|Economic Services", cat1)) %>% 
#   mutate(cat1=if_else(grepl("Social Services",cat1), "Social Services", cat1), 
#          cat1=if_else(grepl("Economic Services",cat1), "Economic Services", cat1))
# 
# 


## ALL STATES e-state data

estate <- read_excel("C:/Users/vara4206/Downloads/ESTATES202324_04012023DF93C7AB334049D5977C59F001B5B41F.XLSX", 
                     sheet = "DATA")

estate=estate %>% clean_names()

# Appendix-1 Seems to be about taxes and revenue

bihar_a2=estate %>% filter(state_ut=="Bihar") %>% 
  filter(appendix=="Appendix-2")
# A-2 seems to be expenditure 

bihar_a3=estate %>% filter(state_ut=="Bihar") %>% 
  filter(appendix=="Appendix-3")
# A-3 is something called capital receipts


bihar_a4=estate %>% filter(state_ut=="Bihar") %>% 
  filter(appendix=="Appendix-4")
# A-4 this seems to be capital disbursement (is it capital expenditure?)

# okay from this page: https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=State%20Finances%20:%20A%20Study%20of%20Budgets

# This is the classification:

# Appendix I: Revenue Receipts of States and Union Territories with Legislature
# Appendix II: Revenue Expenditure of States and Union Territories with Legislature
# Appendix III: Capital Receipts of States and Union Territories with Legislature
# Appendix IV: Capital Expenditure of States and Union Territories with Legislature


tab1=estate %>% 
  filter(appendix=="Appendix-2") %>% 
  dplyr::select(state_ut, budget_head, fiscal_year) %>% distinct() %>% 
  filter(state_ut!="All States/UT")






