

source("code/functions.R")


##  Using this file to collate all the data_edit() code
#.  where I manually edit some data, mostly to add columns to match better



# ADDING BUDGET SECTOR DETAILS IN IAS DATA --------------------------------

ias_exp=qread("data/interm/ias_exp.qs")
all_budget=qread("data/clean/all_budget.qs")


field_cat=ias_exp %>% filter(!(grepl("Centre",organisation))) %>% 
  dplyr::select(field_of_experience,office, 
                category_of_experience) %>% filter(!is.na(office)) %>% 
  arrange(category_of_experience, field_of_experience) %>%
  distinct()

field_office=field_cat %>% filter(!is.na(office)) %>% 
  arrange(field_of_experience, office)

# Getting a list of budget line items that would form a 
  # list for merge. THey will come from 
# section2="Total Capital Outlay" + exp_type=="capital"
# section2= "DEVELOPMENTAL EXPENDITURE" + exp_type="revenue"
# section2= "NON-DEVELOPMENTAL EXPENDITURE (General Services)" + exp_type==revenue
# section2="Grants-in-Aid and Contributions" + exp_type==revenue

line_item=all_budget %>%
  filter(
    (section2 == "Total Capital Outlay" & 
       exp_type == "capital") |
      (section2 == "DEVELOPMENTAL EXPENDITURE" & 
         exp_type == "revenue") |
      (section2 == "NON-DEVELOPMENTAL EXPENDITURE (General Services)" & 
         exp_type == "revenue") |
      (section2 == "Grants-in-Aid and Contributions" & 
         exp_type == "revenue")
  ) %>% dplyr::select(budget_head_clean, sector, level, exp_type) %>% 
  distinct() %>% 
  filter(!(exp_type=="capital"& level<=3) & 
           !(exp_type=="revenue" & level<3)) %>% 
  rename(li=budget_head_clean) %>% 
  mutate(sector=sub(".*-\\s*", "", sector))


li_list=as.character(sort(unique(line_item$li)))
li_list=c("", li_list)
bs_list=as.character(sort(unique(line_item$sector)))
bs_list=c("",bs_list)



# This can be done using data_edit() and manually tagging, will generate a .R code
# 
field_cat_sector=source("code/0 CW/field_cat_sector_tag.R")
field_cat_sector=field_cat_sector$value
#field_cat$recheck=NA
# data_edit(field_cat_sector,
#           code= "code/field_cat_sector_tag.R",
#           viewer = "browser",
#           #col_bind = c("line_item","sector"),
#           col_options = list(line_item = li_list,
#                              sector=bs_list,
#                              recheck=c("",
#                                        "yes")))



# NOW field_cat_sector has field and categ of experience from supremo IAS data.
# combined with line_item and sector from the RJ and UP state budget docs. 

# NOTES: 19 Oct
# 1. Industries & Mineral > Industries(R): THis should also include Others (C)
# 2. All the State PSU type bodies are being put in "Organs of State"
# 3. Food Storage and Warehousing (Agri) and Civil Supplies (GES) should be combined
# 4. All these heads likely can not be individually distinguished:
  #   GES, Fiscal Services. 
# 5. Look specially to "Heavy Industry" field_of_exp. I have put it 
  #  under "Organs of state" as most belong to come Corporations or PSU type
# 6. I am putting Agro INdustry under Agri Research + Agri and allied. 
#   as from a budget pov, it is likely to draw budget from both Industry as well 
# as agri. JOINT CAT
#  7. "Industries" in field is really wide in the data and all encompassing
#   can be PSU/Organ of the state. JOINT CAT
# 8. "PLanning and Prog IMpl" putting in SEct-ES + GES. No particular reason, 
#     could be under Sect-AS + AS also. 
# 9. Handloom /Handicraft is also being put into Organs of the state
# 10. Not sure if its a good idea to put such disperate entities in OAS. Need to 
#  maybe look again into LI tagged as Organs of the state. 
# 11. Water REsources should mostly come under Urban Dev as its most entity like Jal Board 
#  water supply and sanitations, etc



