

## DOING INITIAL ANALYSIS AND RELEVENT CORRELATION HERE 


rm(list=ls())


# Reading the imported files
library(here)

source(here("code/functions.R"))

my_custom_theme <- function() {
  theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.position = "top",
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold", siz=8),
      axis.text.y = element_text(size = 10)
      # Add any other theme settings you want to reuse
    )
}




# IMPORTING THE DATA ------------------------------------------------------

elec_cand_2k17=read_rds(here("data/clean/elec_candX_inner.rds"))
acyear_winner_asset=read_rds(here("data/clean/acyear_cand_asset.rds"))
close_ele_candX=read_rds(here("data/daily_output/close_ele_candX_2004.rds"))

qreadm(here("data/clean/RJ_CS.qs"))
qreadm(here("data/clean/UP_CS.qs"))

cci_monthly=read_rds(here("data/clean/cci_total.rds"))
cci_monthly=cci_monthly  %>%  arrange(city, year, month) %>% 
  mutate(date=as.Date(paste(year, month, "01", sep = "-"))) %>% 
  mutate_at(vars(building:maintenance), as.numeric)




# TRANSFER TRENDS : v2 ----------------------------------------------------
# (Redoing the transfer patterns here again to check, 
# this should be the final version of this)



# USE THISFILE FOR SOMETHING ELSE
# WE ALREADY HAVE civil_services_notes.qmd














