# Load necessary libraries
rm(list=ls())
source("code/functions.R")


library(rvest)
library(dplyr)
library(openxlsx)

# MOTIVE:
# 1. TO READ THE DEPT DATA FROM THE SAVED WEBPAGE
# 2. TAG THE SECTOR IN THIS DATA WITH THE SECTOR FROM BUDGET DATA (from RBI )


# READING DATA ------------------------------------------------------------


qreadm(here("data/clean/RJ_CS_wpar3.qs")) # post level 
rj_budget=qread(here("data/clean/state_budget.qs")) # sector level
rj_budget=rj_budget %>% 
  mutate(match_word=toupper(str_trim(match_word))) %>% 
  rename(sector=match_word)



#  [NOT DOING DEPT MATCHING ] FUNCTIONS ---------------------------------------------------------------
# idea with the following function is 
# just to count the number of matching words across two dept/sector
# then there is some kind of normalization that accounts for 
# just the total count of words. Its a bit hacky and 
# should be stress tested later on

min_match_ratio <- function(str1, str2) {
  
  # removing () from str2, ie from CS post data
  
  vec2 <- gsub("\\s*\\(.*\\)", "", str2)
  
  vec1 <- strsplit(toupper(str1), " ")[[1]]
  vec2 <- strsplit(toupper(vec2), " ")[[1]]
  
  # Convert clip_list to uppercase
  clip_list <- toupper(clip_list)
  
  # Remove words in clip_list from vec1 and vec2
  vec1 <- vec1[!vec1 %in% clip_list]
  vec2 <- vec2[!vec2 %in% clip_list]
  
  
  common_words <- intersect(vec1, vec2)
  uncommon_words <- setdiff(vec1, vec2)
  
  return((length(common_words)*length(common_words)) / min(length(vec1), length(vec2)))
}

max_match_ratio <- function(str1, str2) {
  
  # removing () from str2, ie from CS post data
  
  vec2 <- gsub("\\s*\\(.*\\)", "", str2)
  
  vec1 <- strsplit(toupper(str1), " ")[[1]]
  vec2 <- strsplit(toupper(vec2), " ")[[1]]
  
  
  # Convert clip_list to uppercase
  clip_list <- toupper(clip_list)
  
  # Remove words in clip_list from vec1 and vec2
  vec1 <- vec1[!vec1 %in% clip_list]
  vec2 <- vec2[!vec2 %in% clip_list]
  
  
  common_words <- intersect(vec1, vec2)
  uncommon_words <- setdiff(vec1, vec2)
  
  return((length(common_words)*length(common_words)) / max(length(vec1), length(vec2)))
}


# Function to split the string
split_string <- function(string) {
  # Extract the part inside the parentheses
  inside_parentheses <- str_extract(string, "\\(([^)]+)\\)")
  inside_parentheses <- gsub("[()]", "", inside_parentheses)
  
  # Remove the part inside the parentheses from the original string
  string_no_parentheses <- gsub("\\s*\\([^)]*\\)", "", string)
  
  # Extract the part after the comma
  after_comma <- str_extract(string_no_parentheses, ",\\s*(.*)")
  after_comma <- gsub("^,\\s*", "", after_comma)
  
  # Extract the part before the comma
  before_comma <- str_extract(string_no_parentheses, "^[^,]+")
  
  # Handle cases where there is no comma or parentheses
  before_comma <- ifelse(is.na(before_comma), string_no_parentheses, before_comma)
  
  return(data.frame(
    dept1 = ifelse(is.na(before_comma), string, before_comma),
    dept2 = ifelse(is.na(inside_parentheses), "", inside_parentheses),
    district = ifelse(is.na(after_comma), "", after_comma)
  ))
}



# 1. READING THE GOVT SECTOR-DEPT LIST ------------------------------------



# HERE I AM READING IN THE HTML FILEE FOR THIS PAGE:
# https://rajasthan.gov.in/pages/sm/sector-department/web-directory/146111

# Load the HTML file
html_content <- read_html("/Users/vaibhavrathi/Downloads/dump/Sector & Departments _ State Portal, Rajasthan Government.html")
# Initialize lists to store the extracted data
h2_list <- list()
a_list <- list()
url_list <- list()

# Extract all h2 tags
h2_tags <- html_content %>%
  html_nodes("h2")

# Iterate over each h2 tag and extract corresponding a tags and URLs
for (h2_tag in h2_tags) {
  # Get the text of the h2 tag
  h2_text <- h2_tag %>% html_text(trim = TRUE)
  
  # Find the next sibling that contains the a tags
  a_tags <- h2_tag %>%
    html_nodes(xpath = 'following-sibling::div//a')
  
  # Extract the text and URLs from the a tags
  a_texts <- a_tags %>% html_text(trim = TRUE)
  a_urls <- a_tags %>% html_attr("href")
  
  # Append the h2 text, a texts, and URLs to the lists
  h2_list <- append(h2_list, rep(h2_text, length(a_texts)))
  a_list <- append(a_list, a_texts)
  url_list <- append(url_list, a_urls)
}

# Create a data frame from the lists
data <- data.frame(
  H2_Tag = unlist(h2_list),
  A_Tag = unlist(a_list),
  URL = unlist(url_list),
  stringsAsFactors = FALSE
)

data=data %>% clean_names()
names(data)=c("sector", "dept","url")

data=data %>% mutate(sector=gsub("^Sector -\\s*|\\s*Portal$", "", sector)) %>% 
  mutate(sector=str_trim(sector), 
         dept=str_trim(dept))



# 2. TAGGING THE SECTOR WITH BUDGET SECTORS -------------------------------

data$bsector=NA
data$bsector[data$sector=="Agriculture"]="Agriculture and Allied Activities"
data$bsector[data$sector=="Urban"]="Urban Development"
data$bsector[data$sector=="Animal Husbandry"]="Animal Husbandry"

data$bsector[data$sector=="Social Justice & Empowerment"]="Welfare of Scheduled Castes, Scheduled, Tribes and Other Backward Classes"
data$bsector[data$sector=="Social Justice & Empowerment"]="SOCIAL SECURITY AND WELFARE"
data$bsector[data$sector=="Social Justice & Empowerment"]="SOCIAL SERVICES"
data$bsector[data$sector=="Social Justice & Empowerment"]="WELFARE OF SCHEDULED CASTES, SCHEDULED"

data$bsector[data$sector=="Education"]="Education, Sports, Art and Culture"
data$bsector[data$sector=="Medical & Health"]="Medical and Public Health"
data$bsector[data$sector=="Medical Education"]="Education, Sports, Art and Culture"

data$bsector[data$sector=="Energy"]="Energy"

data$bsector[data$sector=="Industrial"]="Industry and Minerals"
data$bsector[data$sector=="Industrial"]="IRON AND STEEL INDUSTRIES"
data$bsector[data$sector=="Industrial"]="METALLURGICAL INDUSTRIES"
data$bsector[data$sector=="Industrial"]="NON-FERROUS MINING AND"
data$bsector[data$sector=="Industrial"]="NON-FERROUS MINING AND, METALLURGICAL INDUSTRIES"



data$bsector[data$sector=="Information Technology & Communication"]="Communications"

data$bsector[data$sector=="Planning & Statistics"]=""
data$bsector[data$sector=="Rural Development & Panchayati Raj"]="Rural Development"

data$bsector[data$sector=="Skill & Livelihood"]=""
data$bsector[data$sector=="Rajasthan Police"]=""
data$bsector[data$sector=="Water"]="Water Supply and Sanitation"

data$bsector[data$sector=="Department of Environment and Climate Change"]="Science, Technology and Environment"
data$bsector[data$sector=="Department of Environment and Climate Change"]="SOIL AND WATER CONSERVATION"

data$bsector[data$sector=="Land Revenue"]=""
data$bsector[data$sector=="Other Sectors(DOP/ AR/ DIPR /GAD)"]=""
data$bsector[data$sector=="Law & Justice"]=""
data$bsector[data$sector=="Higher & Technical Education"]="Education, Sports, Art and Culture"
data$bsector[data$sector=="Art & Culture"]="Education, Sports, Art and Culture"
data$bsector[data$sector=="Home"]=""
data$bsector[data$sector=="Finance"]=""
data$bsector[data$sector=="Food & Civil Supply"]="Housing, Food Storage and Warehousing"
data$bsector[data$sector=="Tourism"]="Tourism"
data$bsector[data$sector=="DIETS of Rajasthan"]="Education, Sports, Art and Culture"
data$bsector[data$sector=="Collectorate"]=""
data$bsector[data$sector=="Sector- Excise"]=""
data$bsector[data$sector=="Forest"]="Forestry and Wild Life"
data$bsector[data$sector=="ITI Colleges of Rajasthan"]="Education, Sports, Art and Culture"
data$bsector[data$sector=="Sector-Women & Child Development"]="Family Welfare"
data$bsector[data$sector=="Transport"]="Transport"
data$bsector[data$sector=="LSG"]=""
data$bsector[data$sector=="Tribal Area Development"]="Tribes and Other Backward Classes"
data$bsector[data$sector=="Nagar Nigam Rajasthan"]="Urban Development"
data$bsector[data$sector=="Nagar Palika Rajasthan"]="Urban Development"
data$bsector[data$sector=="Nagar Parishad Rajasthan"]="Urban Development"
data$bsector[data$sector=="Public Works"]="Urban Development"
data$bsector[data$sector=="Sports & Youth Affairs"]="Education, Sports, Art and Culture"
data$bsector[data$sector=="UIT (Urban Improvement Trust)"]="Urban Development"
data$bsector[data$sector=="Zila Parishad Rajasthan"]="Urban Development"

rj_dept=data
# Write the data to an Excel file
#write.xlsx(data, file = "sector_department_rajasthan_complete.xlsx", rowNames = FALSE)

# SPLITTING THE DEPT STRING

# Apply the function to the data
df <- rj_dept %>% rowwise() %>% mutate(split = list(split_string(dept))) %>% unnest(split)

df=df %>% mutate(dept1=toupper(dept1), 
                 dept2=toupper(dept2), 
                 district=toupper(district))

df=df %>% mutate(dept1=gsub("&", "AND", dept1))
rj_dept=df



# I ALSO WROTE THE CODE TO MATCH THIS WITH POST DATA USING DEPT NAMES
# BUT NOT DOING THAT. 


# (ALT VERSION) TAGGING POST DATA WITH SECTORS ------------------------------------------
# The logic here is that perhaps its easier to directly tag sectors to departments in CS data
# as the budget variation is anyway at the sector level. 

# I have done this and the results are 
# recorded here: code/post_dept_sector_tag.R
# and this is now already included in the post_rj_clean data

# This can be done using data_edit() and manually tagging, will generate a .R code
# rj_post=post_rj_clean
# post_dept=((rj_post %>% dplyr::select(department) %>% distinct()))
# 
# sector_list=unique(rj_budget$sector)# rj_budget %>% dplyr::select(sector) %>% distinct()
# # sector_list=as.character(sector_list)
# # 
# # elements <- strsplit(gsub('(^c\\(|\\)$)', '', sector_list), 
# #                      '", "', fixed = TRUE)[[1]]
# 
# # Remove the enclosing double quotes from each element
# #elements <- gsub('^"|"$', '', elements)
# elements=c("","INDUSTRIES", sort(sector_list))
# elements=sort(elements)
# 
# post_dept=source("code/post_dept_sector_tag.R")
# post_dept=post_dept$value
# data_edit(post_dept, 
#           code= "code/post_dept_sector_tag.R", 
#           viewer = "browser",
#           #col_bind = c("sector1","sector2","apex"),
#           col_options = list(sector1 = elements, 
#                              sector2=elements, 
#                              apex=c("","apex", "psu")))


# NOTES
# 1. FOOD, CIVIL SUPPLIES AND CONSUMER AFFAIRS: EMPTY WEBSITE. INvolved in PDS
# 2. INDUSTRIES AND COMMERCE : needs to be tagged with "INDUSTRY"
# 3. SETTLEMENT AND BOARD OF REVENUE and COLONISATION, both should be tagged as " LAND REVENUE"
# 4. "REGISTRATION AND STAMPS" should be tagged as "LAND REGISTERATION"


# # (NOT ACTIVE ) MERGING WITH CS DIRECTORY DATA- RJ_POST ---------------------------------
# # I have the CS data at post and officer level. 
# # Now I need to merge the dept here(dept-sect) with the dept in that (CS) data. 
# rj_post=post_rj_clean
# 
# v0=unique(sort(rj_dept$dept)); v1=unique(sort(rj_dept$dept1));v2=unique(sort(rj_post$department))
# # Initialize an empty matrix to store the results
# match_matrix1 <- matrix(nrow = length(v1), ncol = length(v2))
# match_matrix2 <- matrix(nrow = length(v1), ncol = length(v2))
# 
# clip_list=c("OF", "AND", "DEPARTMENT","BOARD","COMMISSION","AUTHORITY",
#             "RAJASTHAN", "ACADEMY","FOR","RAJASTHAN", "STATE")
# 
# # Apply the function to every pair of strings and store the result in the matrix
# for (i in 1:nrow(match_matrix1)) {
#   for (j in 1:ncol(match_matrix1)) {
#     match_matrix1[i, j] <- min_match_ratio(v1[i], v2[j])
#   }
# }
# 
# for (i in 1:nrow(match_matrix2)) {
#   for (j in 1:ncol(match_matrix2)) {
#     match_matrix2[i, j] <- max_match_ratio(v1[i], v2[j])
#   }
# }
# 
# 
# # Name the columns of the match_matrix with the elements of vec2
# colnames(match_matrix1) <- v2;colnames(match_matrix2) <- v2
# 
# # Find the column name with the highest match ratio for each row
# match_indices1 <- apply(match_matrix1, 1, function(x) {
#   if (all(x == 0)) {
#     return(NA)
#   } else {
#     max_val <- max(x)
#     max_cols <- colnames(match_matrix1)[x == max_val]
#     return(paste(max_cols, collapse = ","))
#   }
# })
# 
# match_indices2 <- apply(match_matrix2, 1, function(x) {
#   if (all(x == 0)) {
#     return(NA)
#   } else {
#     max_val <- max(x)
#     max_cols <- colnames(match_matrix2)[x == max_val]
#     return(paste(max_cols, collapse = ","))
#   }
# })
# 
# 
# match_ratios1 <- apply(match_matrix1, 1, max)
# match_ratios2 <- apply(match_matrix2, 1, max)
# 
# 
# matched_df3=as.data.frame(cbind(v1, match_indices1,match_indices2,match_ratios1,
#                                match_ratios2))
# 
# matched_df3=matched_df3%>% mutate(total_ratio=as.numeric(match_ratios1)+
#                                    as.numeric(match_ratios2)) %>% 
#   arrange(-total_ratio) %>% 
#   mutate(mcount=str_count(match_indices1, ",")) %>% 
#   dplyr::select(v1,match_indices1, match_ratios1, mcount ) %>% 
#   separate_rows(match_indices1, sep = ",") %>% 
#   arrange(-mcount, v1) %>% 
#   rename(sec_dept=v1, 
#          cs_dept=match_indices1)
# 
# # NOW AT THIS POINT WE NEED TO DO MANUAL CLEAING UP 
# # OF THE BAD MATCHES. 
# # IN THE FIRST TRY, I AM USING THE FOLLOWING FUNCTION, 
# # IT WILL ALSO SAVE A CODE FILE WHICH I CAN JUST SOURCE LATER ON. 
# 
# 
# 
# sec_cs_match_df=matched_df3 %>% 
#   inner_join(rj_dept, by=c("sec_dept"="dept1")) %>% 
#   dplyr::select(sec_dept, cs_dept, match_ratios1, mcount, 
#                 sector, dept, url, bsector, dept2, district) %>% 
#   rename(sec_cs_match=match_ratios1, 
#          mul_matchcount=mcount) %>% 
#   mutate(sector=toupper(sector))
# 
# 
# 
# 
# # NOW WE CAN MERGE THIS WITH THE BUDGET DATA ALSO
# # This merge would use bsector (from sec_cs_match_df) and sector (rj_budget)
# 
# final_match_df= sec_cs_match_df %>% left_join(rj_budget, 
#                                               by=c("bsector"="sector"))









