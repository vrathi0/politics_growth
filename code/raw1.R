

source("code/functions.R")


dt=read_dta("../Data/SHRUG/dta_shrug-v1.5.samosa-pop-econ-census-dta/shrug-v1.5.samosa-pop-econ-census-dta/shrug_ec.dta")


names(dt) <- gsub("^(.{8}).(.*)$", "\\1\\2", names(dt))

dt_long=dt %>% 
  pivot_longer(cols = -shrid, 
               names_to = c("year", "var_type"), 
               names_sep = "_",
               values_to = "value")


dt_long=dt_long %>% mutate(year=gsub("ec", "", year)) %>% 
            mutate(year=as.numeric(year))
# "empall"      "empmanuf"    "empservices"

dt_all=dt_long %>% filter(var_type=="empall") %>% arrange(shrid, year) %>% 
                  group_by(shrid) %>% mutate(obs_count=sum(!is.na(value))) %>% 
                  filter(obs_count>=3)
dt_manuf=dt_long %>% filter(var_type=="empmanuf") %>% arrange(shrid, year) %>% 
  group_by(shrid) %>% mutate(obs_count=sum(!is.na(value))) %>% 
  filter(obs_count>=3)

dt_service=dt_long %>% filter(var_type=="empservices") %>% arrange(shrid, year) %>% 
  group_by(shrid) %>% mutate(obs_count=sum(!is.na(value))) %>% 
  filter(obs_count>=3)


eq1=as.formula(paste0("value~1|shrid + year"))
reg1=felm(eq1, dt_all)
res1=reg1$residuals
res1=clipp(res1, 0.01,0.99)
res_all=as.data.frame(res1) %>% filter(!is.na(res1))
ggplot(res_all, aes(x = value)) +
  geom_density() +
  labs(title = "Total Empl- Residual")

resall_tot=dt_all$value[!is.na(dt_all$value)]
resall_tot=clipp(resall_tot, 0.01,0.95)
resall_tot=resall_tot-mean(resall_tot, na.rm=T)
resall_tot=as.data.frame(resall_tot) %>% filter(!is.na(resall_tot))
ggplot(resall_tot, aes(x = resall_tot)) +
  geom_density() +
  labs(title = "Total Empl")


# Manuf

eq1=as.formula(paste0("value~1|shrid + year"))
reg1=felm(eq1, dt_manuf)
res_manuf=reg1$residuals
res_manuf=clipp(res_manuf, 0.01,0.99)
res_manuf=as.data.frame(res1) %>% filter(!is.na(res1))
ggplot(res_manuf, aes(x = value)) +
  geom_density() +
  labs(title = "Manuf Empl- Residual")

resmanuf_tot=dt_manuf$value[!is.na(dt_manuf$value)]
resmanuf_tot=clipp(resmanuf_tot, 0.01,0.95)
resmanuf_tot=resmanuf_tot-mean(resmanuf_tot, na.rm=T)
resmanuf_tot=as.data.frame(resmanuf_tot) %>% filter(!is.na(resmanuf_tot))
ggplot(resmanuf_tot, aes(x = resmanuf_tot)) +
  geom_density() +
  labs(title = "Manuf Empl")

# Service

eq1=as.formula(paste0("value~1|shrid + year"))
reg1=felm(eq1, dt_service)
res_service=reg1$residuals
res_service=clipp(res_service, 0.01,0.99)
res_service=as.data.frame(res_service) %>% filter(!is.na(res_service))
ggplot(res_service, aes(x = value)) +
  geom_density() +
  labs(title = "Service Empl- Residual")

resservice_tot=dt_service$value[!is.na(dt_service$value)]
resservice_tot=clipp(resservice_tot, 0.01,0.95)
resservice_tot=resservice_tot-mean(resservice_tot, na.rm=T)
resservice_tot=as.data.frame(resservice_tot) %>% filter(!is.na(resservice_tot))
ggplot(resservice_tot, aes(x = resservice_tot)) +
  geom_density() +
  labs(title = "Service Empl")


dt_manuf=dt_long %>% filter(var_type=="empmanuf") %>% arrange(shrid, year)
dt_service=dt_long %>% filter(var_type=="empservices") %>% arrange(shrid, year)
