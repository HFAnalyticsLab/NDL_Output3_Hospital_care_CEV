#Import libraries
library(dplyr)
library(data.table)

`%nin%` <- Negate(`%in%`)

data <- data.frame(fread("Output3data.csv"))

#Set variables order
data$gender <- factor(data$gender,levels = c("Female","Male","Unknown"))
data$ageband <- factor(data$ageband,levels = c("<30","30 to 49","50 to 69","70 or older","Unknown"))
data$IMD_Quintile <- factor(data$IMD_Quintile,levels = c("1","2","3","4","5","Unknown"))
data$Year <- factor(data$Year, levels = c("2018","2019","2020"))
data$Month <- factor(data$Month, levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                            "Jul","Aug","Sep","Oct","Nov","Dec"))

#Main function

makeO3table <- function(dt,mainvar,groupvar = c("Month","Year"),label,type){
  
  dt$target <- dt[,mainvar]
  
  if(type =='t1'){
    dt <- dt %>%
      filter(target>0) %>%
      group_by(.dots = groupvar,.drop=F) %>%
      summarise(number_events = sum(target),
                number_patients = length(unique(pseudoid))) %>%
      mutate(type = label,
             number_events = ifelse(number_events<=7, "<7",as.character(number_events)),
             number_patients = ifelse(number_patients<=7,"<7",as.character(number_patients))) %>%
      arrange(desc(Year),desc(Month)) 
  } else if(type == 't2'){
    dt <- dt %>%
      filter(target>0) %>%
      group_by(.dots = groupvar,.drop=F) %>%
      summarise(number_patients = length(unique(pseudoid))) %>%
      mutate(type = label,
             number_patients = ifelse(number_patients<=7,"<7",as.character(number_patients))) %>%
      arrange(desc(Year),desc(Month)) 
  } else if (type == 't3'){
    dt <- dt %>%
      filter(target>0) %>%
      group_by(.dots = groupvar,.drop=F) %>%
      summarise(number_deaths = length(unique(pseudoid))) %>%
      mutate(type = label,
             number_deaths = ifelse(number_deaths<=7,"<7",as.character(number_deaths))) %>%
      arrange(desc(Year),desc(Month))
  }
    
    return(dt)
} 

#Table 3-1 Overall####

t31_ov_all <- makeO3table(dt = data,mainvar ="total_admissions",label = "admissions_all",type = 't1')
t31_ov_elec <- makeO3table(dt = data,mainvar ="total_elective_admissions",label = "admissions_electives",type = 't1')
t31_ov_emer <- makeO3table(dt = data,mainvar ="total_emergency_admissions",label = "admissions_emergency",type = 't1')
t31_ov_othe <- makeO3table(dt = data,mainvar ="total_other_admissions",label = "admissions_other",type = 't1')
t31_ov_ae <- makeO3table(dt = data,mainvar ="ae_attendances",label = "AE_attendance",type = 't1')
t31_ov_out <- makeO3table(dt = data,mainvar ="outpatient_attendances",label = "outpatient_attendance",type = 't1')

t31_overall <- bind_rows(t31_ov_all,
                         t31_ov_elec,
                         t31_ov_emer,
                         t31_ov_othe,
                         t31_ov_ae,
                         t31_ov_out)

rm(t31_ov_all,
   t31_ov_elec,
   t31_ov_emer,
   t31_ov_othe,
   t31_ov_ae,
   t31_ov_out)

type_order_t1 <- c("admissions_all","admissions_electives","admissions_emergency","admissions_other","AE_attendance","outpatient_attendance")
t31_overall$type <- factor(t31_overall$type , levels = type_order_t1)

t31_overall <- t31_overall %>% select(Month,Year,type,number_events,number_patients) %>%
  arrange(type,desc(Year),desc(Month))

#Table 3-2 Overall####

t32_ov_any <- makeO3table(dt = data,mainvar ="covid19_admissions",
                          label = "COVID19_admissions_any_diagnosis",type = 't2')
t32_ov_fir <- makeO3table(dt = data,mainvar ="covid19_admissions_first_episode", 
                          label = "COVID19_admissions_first_episode",type = 't2')

t32_overall <- bind_rows(t32_ov_any,t32_ov_fir)
rm(t32_ov_any,t32_ov_fir)

t32_overall <- t32_overall %>% filter(Year == "2020" & Month %in% c("Jul","Jun","May","Apr","Mar")) %>%
  select(Month,Year,type,number_patients)

#Table 3-3 Overall####

t33_ov_any <- makeO3table(dt = data, mainvar = "death_any_setting", label = "deaths_anycause_anywhere", type = 't3')
t33_ov_hos <- makeO3table(dt = data, mainvar = "death_in_hospital", label = "deaths_anycause_hospital", type = 't3')

t33_overall <- bind_rows(t33_ov_any,t33_ov_hos)

rm(t33_ov_any,t33_ov_hos)

t33_overall <- t33_overall %>% 
  filter(Year == "2020" & Month %in% c("Jul","Jun","May","Apr","Mar")) %>%
  select(Month,Year,type,number_deaths) %>%
  arrange(type,Year,Month)

#Table 3-1 Age####

t31_ag_all <- makeO3table(dt = data,mainvar ="total_admissions", groupvar = c("Month","Year","ageband"),
                          label = "admissions_all",type = 't1')
t31_ag_elec <- makeO3table(dt = data,mainvar ="total_elective_admissions", groupvar = c("Month","Year","ageband"),
                           label = "admissions_electives",type = 't1')
t31_ag_emer <- makeO3table(dt = data,mainvar ="total_emergency_admissions", groupvar = c("Month","Year","ageband"),
                           label = "admissions_emergency",type = 't1')
t31_ag_othe <- makeO3table(dt = data,mainvar ="total_other_admissions", groupvar = c("Month","Year","ageband"),
                           label = "admissions_other",type = 't1')
t31_ag_ae <- makeO3table(dt = data,mainvar ="ae_attendances",groupvar = c("Month","Year","ageband"),
                         label = "AE_attendance",type = 't1')
t31_ag_out <- makeO3table(dt = data,mainvar ="outpatient_attendances", groupvar = c("Month","Year","ageband"),
                          label = "outpatient_attendance",type = 't1')

t31_age <- bind_rows(t31_ag_all,
                     t31_ag_elec,
                     t31_ag_emer,
                     t31_ag_othe,
                     t31_ag_ae,
                     t31_ag_out)

rm(t31_ag_all,
   t31_ag_elec,
   t31_ag_emer,
   t31_ag_othe,
   t31_ag_ae,
   t31_ag_out)


t31_age$type <- factor(t31_age$type , levels = type_order_t1)

t31_age <- t31_age %>% select(Month,Year,type,ageband,number_events,number_patients) %>%
  arrange(type,ageband,desc(Year),desc(Month))

#Table 3-2 Age####

t32_ag_any <- makeO3table(dt = data,mainvar ="covid19_admissions", groupvar = c("Month","Year","ageband"),
                          label = "COVID19_admissions_any_diagnosis",type = 't2')
t32_ag_fir <- makeO3table(dt = data,mainvar ="covid19_admissions_first_episode", groupvar = c("Month","Year","ageband"),
                          label = "COVID19_admissions_first_episode",type = 't2')

t32_age <- bind_rows(t32_ag_any,t32_ag_fir)
rm(t32_ag_any,t32_ag_fir)

t32_age <- t32_age %>% filter(Year == "2020" & Month %in% c("Jul","Jun","May","Apr","Mar")) %>%
  select(Month,Year,type,ageband,number_patients) %>%
  arrange(type,ageband,desc(Year),desc(Month))

#Table 3-3 Age####

t33_ag_any <- makeO3table(dt = data, mainvar = "death_any_setting", groupvar = c("Month","Year","ageband"),
                          label = "deaths_anycause_anywhere", type = 't3')
t33_ag_hos <- makeO3table(dt = data, mainvar = "death_in_hospital", groupvar = c("Month","Year","ageband"),
                          label = "deaths_anycause_hospital", type = 't3')

t33_age <- bind_rows(t33_ag_any,t33_ag_hos)

rm(t33_ag_any,t33_ag_hos)

t33_age <- t33_age %>% 
  filter(Year == "2020" & Month %in% c("Jul","Jun","May","Apr","Mar")) %>%
  select(Month,Year,type,ageband,number_deaths) %>%
  arrange(type,ageband,Year,desc(Month))

#Table 3-1 Sex####
t31_se_all <- makeO3table(dt = data,mainvar ="total_admissions", groupvar = c("Month","Year","gender"),
                          label = "admissions_all",type = 't1')
t31_se_elec <- makeO3table(dt = data,mainvar ="total_elective_admissions", groupvar = c("Month","Year","gender"),
                           label = "admissions_electives",type = 't1')
t31_se_emer <- makeO3table(dt = data,mainvar ="total_emergency_admissions", groupvar = c("Month","Year","gender"),
                           label = "admissions_emergency",type = 't1')
t31_se_othe <- makeO3table(dt = data,mainvar ="total_other_admissions", groupvar = c("Month","Year","gender"),
                           label = "admissions_other",type = 't1')
t31_ag_ae <- makeO3table(dt = data,mainvar ="ae_attendances", groupvar = c("Month","Year","gender"),
                         label = "AE_attendance",type = 't1')
t31_se_out <- makeO3table(dt = data,mainvar ="outpatient_attendances", groupvar = c("Month","Year","gender"),
                          label = "outpatient_attendance",type = 't1')

t31_sex <- bind_rows(t31_se_all,
                     t31_se_elec,
                     t31_se_emer,
                     t31_se_othe,
                     t31_ag_ae,
                     t31_se_out)

rm(t31_se_all,
   t31_se_elec,
   t31_se_emer,
   t31_se_othe,
   t31_ag_ae,
   t31_se_out)

t31_sex$type <- factor(t31_sex$type , levels = type_order_t1)

t31_sex <- t31_sex %>% select(Month,Year,type,gender,number_events,number_patients) %>%
  arrange(type,gender,desc(Year),desc(Month))




#Table 3-2 Sex####
t32_se_any <- makeO3table(dt = data,mainvar ="covid19_admissions", groupvar = c("Month","Year","gender"),
                          label = "COVID19_admissions_any_diagnosis",type = 't2')
t32_se_fir <- makeO3table(dt = data,mainvar ="covid19_admissions_first_episode", groupvar = c("Month","Year","gender"),
                          label = "COVID19_admissions_first_episode",type = 't2')

t32_sex <- bind_rows(t32_se_any,t32_se_fir)
rm(t32_se_any,t32_se_fir)

t32_sex <- t32_sex %>% filter(Year == "2020" & Month %in% c("Jul","Jun","May","Apr","Mar"))  %>%
  select(Month,Year,type,gender,number_patients) %>%
  arrange(type,gender,desc(Year),desc(Month))

#Table 3-3 Sex####

t33_se_any <- makeO3table(dt = data, mainvar = "death_any_setting", groupvar = c("Month","Year","gender"),
                          label = "deaths_anycause_anywhere", type = 't3')
t33_se_hos <- makeO3table(dt = data, mainvar = "death_in_hospital", groupvar = c("Month","Year","gender"),
                          label = "deaths_anycause_hospital", type = 't3')

t33_sex <- bind_rows(t33_se_any,t33_se_hos)

rm(t33_se_any,t33_se_hos)

t33_sex <- t33_sex %>% 
  filter(Year == "2020" & Month %in% c("Jul","Jun","May","Apr","Mar")) %>%
  select(Month,Year,type,gender,number_deaths) %>%
  arrange(type,gender,Year,desc(Month))

#Table 3-1 IMD####

t31_im_all <- makeO3table(dt = data,mainvar ="total_admissions", groupvar = c("Month","Year","IMD_Quintile"),
                          label = "admissions_all",type = 't1')
t31_im_elec <- makeO3table(dt = data,mainvar ="total_elective_admissions", groupvar = c("Month","Year","IMD_Quintile"),
                           label = "admissions_electives",type = 't1')
t31_im_emer <- makeO3table(dt = data,mainvar ="total_emergency_admissions", groupvar = c("Month","Year","IMD_Quintile"),
                           label = "admissions_emergency",type = 't1')
t31_im_othe <- makeO3table(dt = data,mainvar ="total_other_admissions", groupvar = c("Month","Year","IMD_Quintile"),
                           label = "admissions_other",type = 't1')
t31_ag_ae <- makeO3table(dt = data,mainvar ="ae_attendances", groupvar = c("Month","Year","IMD_Quintile"),
                         label = "AE_attendance",type = 't1')
t31_im_out <- makeO3table(dt = data,mainvar ="outpatient_attendances", groupvar = c("Month","Year","IMD_Quintile"),
                          label = "outpatient_attendance",type = 't1')

t31_imd <- bind_rows(t31_im_all,
                     t31_im_elec,
                     t31_im_emer,
                     t31_im_othe,
                     t31_ag_ae,
                     t31_im_out)

rm(t31_im_all,
   t31_im_elec,
   t31_im_emer,
   t31_im_othe,
   t31_ag_ae,
   t31_im_out)


t31_imd$type <- factor(t31_imd$type , levels = type_order_t1)
t31_imd <- t31_imd %>% select(Month,Year,type,IMD_Quintile,number_events,number_patients) %>%
  arrange(type,IMD_Quintile,desc(Year),desc(Month))


#Table 3-2 IMD####

t32_im_any <- makeO3table(dt = data,mainvar ="covid19_admissions", groupvar = c("Month","Year","IMD_Quintile"),
                          label = "COVID19_admissions_any_diagnosis",type = 't2')
t32_im_fir <- makeO3table(dt = data,mainvar ="covid19_admissions_first_episode", groupvar = c("Month","Year","IMD_Quintile"),
                          label = "COVID19_admissions_first_episode",type = 't2')

t32_imd <- bind_rows(t32_im_any,t32_im_fir)
rm(t32_im_any,t32_im_fir)

t32_imd <- t32_imd %>% filter(Year == "2020" & Month %in% c("Jul","Jun","May","Apr","Mar")) %>%
  select(Month,Year,type,IMD_Quintile,number_patients) %>%
  arrange(type,IMD_Quintile,desc(Year),desc(Month))

#Table 3-3 IMD####

t33_im_any <- makeO3table(dt = data, mainvar = "death_any_setting", groupvar = c("Month","Year","IMD_Quintile"),
                          label = "deaths_anycause_anywhere", type = 't3')
t33_im_hos <- makeO3table(dt = data, mainvar = "death_in_hospital", groupvar = c("Month","Year","IMD_Quintile"),
                          label = "deaths_anycause_hospital", type = 't3')

t33_imd <- bind_rows(t33_im_any,t33_im_hos)

rm(t33_im_any,t33_im_hos)

t33_imd <- t33_imd %>% 
  filter(Year == "2020" & Month %in% c("Jul","Jun","May","Apr","Mar")) %>%
  select(Month,Year,type,IMD_Quintile,number_deaths) %>%
  arrange(Year,Month,type,IMD_Quintile)

#OUTPUT####

fwrite(t31_overall,"Table3_1_Overall.csv")
fwrite(t32_overall,"Table3_2_Overall.csv")
fwrite(t33_overall,"Table3_3_Overall.csv")
fwrite(t31_age,"Table3_1_age.csv")
fwrite(t32_age,"Table3_2_age.csv")
fwrite(t33_age,"Table3_3_age.csv")
fwrite(t31_sex,"Table3_1_sex.csv")
fwrite(t32_sex,"Table3_2_sex.csv")
fwrite(t33_sex,"Table3_3_sex.csv")
fwrite(t31_imd,"Table3_1_IMD.csv")
fwrite(t32_imd,"Table3_2_IMD.csv")
fwrite(t33_imd,"Table3_3_IMD.csv")

