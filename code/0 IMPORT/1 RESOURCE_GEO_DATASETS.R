

# NOTE:
# 1. KEEPING RECORD OF DIFFERENT RESOURCE GEO DATASETS (SUCH AS MINING)





# 0 HEADER ----------------------------------------------------------------



rm(list=ls())
source("code/functions.R")


# 1 COAL MINES --------------------------------------------------

coal_mines <- read_excel("../Data/Mining/Pande_Sudarshan_data/Indian Coal Mines Dataset_January 2021-1.xlsx", 
                              sheet = "Mines Datasheet") %>% 
  clean_names() 

# above folder also has data on EC clearances/approval for mining project
# codebook: Data/Mining/Pande_Sudarshan_data/TW8R2. 1017/EC_data_codebook_03052018.pdf
# Also see Asher_mining data here: /Data/Replication/Asher_mining_India



# 2 INDIA INDUSTRIAL LAND DATABASE --------------------------------------------------
industrial_land <- read_excel("../Data/India Firm/India Industrial Land Bank.xlsx", 
                                              skip = 17) %>% clean_names()




# 3 SEZ DATA --------------------------------------------------

# pdfs here: /Data/India Indus Pol/SEZ/Notified_SEZs_7_12_356.pdf




