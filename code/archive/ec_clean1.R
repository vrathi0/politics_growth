rm(list=ls())
source("code/functions.R")


#  Compiling 2013 EC data -------------------------------------------------------------------



# Compiling 2013 EC data

fpath=list.files(path="../Data/Economics Census/EC2012/", pattern = ".dta", 
                 full.names = T)

# No. of lines to read ( set to 1000 for coding, then Inf)
rline=Inf

readf=function(x, n){
  
  t=read_dta(x, n_max = n)
  lab=labelled::var_label(t)
  lab=(unlist(lab));names(lab)=NULL; lab=clean_strings(lab)
  
  t=t %>% mutate_all(as.character) %>% clean_names()
  
  return(list(t, lab))

}

list_dump=lapply(fpath, readf, rline) 

dt_2013=lapply(list_dump, function(x) x[[1]]) %>% bind_rows() %>% 
  mutate(year="2013")

varlab=unique(unlist(lapply(list_dump, function(x) x[[2]]))) # This contains variable labs from stata .dat
# write_rds(varlab, file="../Data/Economics Census/clean/ec2013_varlab.rds", 
#           compress = "gz")

write_rds(dt_2013, file="../Data/Economics Census/clean/ec2013.rds", 
          compress = "gz")

indices <- sample(nrow(ec2013), nrow(ec2013)*0.25, replace = FALSE)
subsample <- ec2013[indices, ]

write_rds(subsample, file="../Data/Economics Census/clean/ec2013_ran.rds", 
          compress = "gz")

# Compiling 2005 EC data

# Compiling 2005 EC data --------------------------------------------------

getstop=function(s,l){return(s+l-1)}

# This data is a list of .rar files in a folder. 
getdt05=function(path){
  
  read_con=file(path, open="r")
  
  
  # Get the total number of lines in the file
  # num_total_lines <- length(readLines(read_con))
  # 
  # # Define the number of lines to read
  # num_lines_to_read <- min(50000, 0.25*num_total_lines )  # Adjust this number as needed
  # #num_lines_to_read <-  0.25*num_total_lines   # Adjust this number as needed
  # 
  # # Generate a random sample of line indices
  # line_indices <- sample(1:num_total_lines, num_lines_to_read)
  
  
  
  lines=as.list(readLines(read_con))
  #lines=lines[line_indices]
  
  rid=unlist(lapply(lines, function(x) {return(substring(x,1,getstop(1,2)))}))
  sector_code=unlist(lapply(lines, function(x) {return(substring(x,3,getstop(3,1)))}))
  
  v_stateut_code=unlist(lapply(lines, function(x) {return(substring(x,4,getstop(4,2)))}))
  v_dist_code=unlist(lapply(lines, function(x) {return(substring(x,6,getstop(6,2)))}))
  tehsil_code=unlist(lapply(lines, function(x) {return(substring(x,8,getstop(8,4)))}))
  village_code=unlist(lapply(lines, function(x) {return(substring(x,12,getstop(12,8)))}))
  enb_no=unlist(lapply(lines, function(x) {return(substring(x,20,getstop(20,4)))}))
  
  c_stateut_code=unlist(lapply(lines, function(x) {return(substring(x,4,getstop(4,2)))}))
  c_dist_code=unlist(lapply(lines, function(x) {return(substring(x,6,getstop(6,2)))}))
  town_code=unlist(lapply(lines, function(x) {return(substring(x,8,getstop(8,2)))}))
  frame_code=unlist(lapply(lines, function(x) {return(substring(x,10,getstop(10,2)))}))
  ward_no=unlist(lapply(lines, function(x) {return(substring(x,12,getstop(12,5)))}))
  investigator_no=unlist(lapply(lines, function(x) {return(substring(x,17,getstop(17,5)))}))
  ufs_block_no=unlist(lapply(lines, function(x) {return(substring(x,22,getstop(22,2)))}))
  
  
  page_no=unlist(lapply(lines, function(x) {return(substring(x,24,getstop(24,2)))}))
  
  line_no=unlist(lapply(lines, function(x) {return(substring(x,26,getstop(26,3)))}))
  premise_status=unlist(lapply(lines, function(x) {return(substring(x,29,getstop(29,1)))}))
  activity_code=unlist(lapply(lines, function(x) {return(substring(x,32,getstop(32,1)))}))
  nic_2004=unlist(lapply(lines, function(x) {return(substring(x,33,getstop(33,4)))}))
  classf_code=unlist(lapply(lines, function(x) {return(substring(x,37,getstop(37,1)))}))
  oper_code=unlist(lapply(lines, function(x) {return(substring(x,38,getstop(38,1)))}))
  owner_code=unlist(lapply(lines, function(x) {return(substring(x,39,getstop(39,1)))}))
  social_code=unlist(lapply(lines, function(x) {return(substring(x,40,getstop(40,1)))}))
  fuel_code=unlist(lapply(lines, function(x) {return(substring(x,41,getstop(41,1)))}))
  registr1_code=unlist(lapply(lines, function(x) {return(substring(x,42,getstop(42,1)))}))
  registr2_code=unlist(lapply(lines, function(x) {return(substring(x,43,getstop(43,1)))}))
  amale_no=unlist(lapply(lines, function(x) {return(substring(x,44,getstop(44,5)))}))
  afemale_no=unlist(lapply(lines, function(x) {return(substring(x,49,getstop(49,3)))}))
  cmale_no=unlist(lapply(lines, function(x) {return(substring(x,52,getstop(52,2)))}))
  cfemale_no=unlist(lapply(lines, function(x) {return(substring(x,54,getstop(54,2)))}))
  extra_no=unlist(lapply(lines, function(x) {return(substring(x,69,getstop(69,3)))}))
  finance=unlist(lapply(lines, function(x) {return(substring(x,72,getstop(72,1)))}))
  
  
  close(read_con)
  df=as.data.frame(cbind(rid,sector_code,v_stateut_code, v_dist_code,
                         tehsil_code,village_code, enb_no,
                         c_stateut_code, c_dist_code,
                         town_code, frame_code,  ward_no ,investigator_no,
                         ufs_block_no , page_no ,line_no,
                         premise_status, activity_code, nic_2004,
                         classf_code, oper_code, owner_code,
                         social_code, fuel_code, registr1_code, 
                         registr2_code, amale_no, afemale_no, cmale_no, cfemale_no,
                         extra_no, finance))
  
  df
  
}


# Set the path to the folder containing the .rar files
folder_path <- "../Data/Economics Census/EC2005/exdir/"

# Get the list of .txt files in the folder
flist <- list.files(path = folder_path, 
                        full.names = TRUE)

# # Running a parallel loop

#Setup backend to use many processors
totalCores = detectCores()

#Leave one core to avoid overload your computer
cluster <- makeCluster(9)
registerDoParallel(cluster)

library(foreach)
#Run forloop in Parallel
dt_list <- foreach(i = 1:length(flist),  .verbose = TRUE) %dopar% {
  
  
 getdt05(flist[i])
}


#Stop cluster
stopCluster(cluster)

# tic()
# dt_list=lapply(flist, getdt05)
# toc()
ec_data05=dt_list %>% bind_rows() %>%  mutate_all(str_trim) %>% 
  mutate(year="2005")


write_rds(ec_data05, file="../Data/Economics Census/clean/ec2005.rds", 
          compress = "gz")

ss=ec_data05 %>% sample_frac(size=0.25)
write_rds(ss, file="../Data/Economics Census/clean/ec2005_ran.rds", 
          compress = "gz")

# Compiling 1998 EC data --------------------------------------------------

# This data is a list of .rar files in a folder. 
getdt98=function(path){
  
  read_con=file(path, open="r")
  
  
  # Get the total number of lines in the file
  # num_total_lines <- length(readLines(read_con))
  # 
  # # Define the number of lines to read
  # num_lines_to_read <-  0.25*num_total_lines   # Adjust this number as needed
  # 
  # # Generate a random sample of line indices
  # line_indices <- sample(1:num_total_lines, num_lines_to_read)
  
  
  
  lines=as.list(readLines(read_con))
  
  file_id=unlist(lapply(lines, function(x) {return(substr(x,1,6))}))
  id_no=unlist(lapply(lines, function(x) {return(substr(x,7,18))}))
  state_code=unlist(lapply(lines, function(x) {return(substr(x,19,20))}))
  dist_code=unlist(lapply(lines, function(x) {return(substr(x,21,22))}))
  tehsil_code=unlist(lapply(lines, function(x) {return(substr(x,23,25))}))
  cd_block=unlist(lapply(lines, function(x) {return(substr(x,26,27))}))
  sector_code=unlist(lapply(lines, function(x) {return(substr(x,28,28))}))
  village_code=unlist(lapply(lines, function(x) {return(substr(x,29,31))}))
  ward_code=unlist(lapply(lines, function(x) {return(substr(x,32,34))}))
  iv_unit=unlist(lapply(lines, function(x) {return(substr(x,35,37))}))
  ufs_block_no=unlist(lapply(lines, function(x) {return(substr(x,38,39))}))
  page_no=unlist(lapply(lines, function(x) {return(substr(x,40,42))}))
  line_no=unlist(lapply(lines, function(x) {return(substr(x,43,47))}))
  premise_status=unlist(lapply(lines, function(x) {return(substr(x,48,48))}))
  nic=unlist(lapply(lines, function(x) {return(substr(x,54,57))}))
  classf_code=unlist(lapply(lines, function(x) {return(substr(x,58,58))}))
  oper_code=unlist(lapply(lines, function(x) {return(substr(x,59,59))}))
  owner_code=unlist(lapply(lines, function(x) {return(substr(x,60,60))}))
  social_code=unlist(lapply(lines, function(x) {return(substr(x,61,61))}))
  fuel_code=unlist(lapply(lines, function(x) {return(substr(x,62,62))}))
  oper_year=unlist(lapply(lines, function(x) {return(substr(x,63,63))}))
  registr_code=unlist(lapply(lines, function(x) {return(substr(x,64,65))}))
  finance=unlist(lapply(lines, function(x) {return(substr(x,66,66))}))
  total_empl=unlist(lapply(lines, function(x) {return(substr(x,87,91))}))
  total_hired=unlist(lapply(lines, function(x) {return(substr(x,112,116))}))
  enterprise_type=unlist(lapply(lines, function(x) {return(substr(x,120,120))}))
  activity_code=unlist(lapply(lines, function(x) {return(substr(x,121,122))}))
  
  
  close(read_con)
  df=as.data.frame(cbind(id_no,state_code, dist_code, tehsil_code, cd_block, 
                          sector_code, village_code, ward_code, iv_unit, 
                         ufs_block_no, page_no, line_no, premise_status, 
                        nic, classf_code, oper_code, owner_code, social_code, 
                      fuel_code, oper_year, registr_code, finance, total_empl, 
                      total_hired, enterprise_type, activity_code ))
  
  df
  
}


# Set the path to the folder containing the .rar files
folder_path <- "../Data/Economics Census/EC1998/exdir/"

# Get the list of .txt files in the folder
flist <- list.files(path = folder_path, 
                    full.names = TRUE)


# # Running a parallel loop

#Setup backend to use many processors
totalCores = detectCores()

#Leave one core to avoid overload your computer
cluster <- makeCluster(9)
registerDoParallel(cluster)

library(foreach)
#Run forloop in Parallel
dt_list <- foreach(i = 1:length(flist),  .verbose = TRUE) %dopar% {
  
  
  getdt98(flist[i])
}


#Stop cluster
stopCluster(cluster)

ec_data98=dt_list %>% bind_rows() %>%  mutate_all(str_trim) %>% 
  mutate(year="1998")

write_rds(ec_data98, file="../Data/Economics Census/clean/ec1998.rds", 
          compress = "gz")

ss=ec_data98 %>% sample_frac(size=0.25)
write_rds(ss, file="../Data/Economics Census/clean/ec1998_ran.rds", 
          compress = "gz")
