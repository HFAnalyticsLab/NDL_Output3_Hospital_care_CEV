##############################################
################### TO-DO ####################
##############################################

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,readODS,
               gmodels,DescTools,data.table,
               tibble,pbapply,pbmcapply,here,
               tidyverse,readxl)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Partner outputs/Output 3 HC usage/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#############################################################
##################### Table 1: HC usage #####################
#############################################################

#month	year	time_start	time_end	type	number.events	number.patients	total.patients
#breakdown breakdown.level
#admissions_emergency (OK)
#admissions_electives
#admissions_other
#AE_attendance
#all_admissions
#outpatient_attendance
# month	year	time_start	time_end	type	number.events	number.patients	total.patients

##############################
####### Table 1, Overall #####
##############################

###### Overall - all admissions

table1.overall.grampian.all <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","hospital_admissions_total.csv"),
                                 header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",strata="overall",breakdown.level="admissions_all") %>%
  select(.,year_month,breakdown,strata,breakdown.level,people_admitted,hospital_admissions) %>%
  dplyr::rename(.,number.patients=people_admitted,number.events=hospital_admissions)

###### Overall - emergency and routine

table1.overall.grampian.emrout <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","hospital_admissions_by_type.csv"),
                                            header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",strata="overall",breakdown.level=admission_type,
         number.events=hospital_admissions,number.patients=people_admitted) %>%
  mutate(.,breakdown.level=ifelse(breakdown.level=="emergency","admissions_emergency",breakdown.level)) %>%
  mutate(.,breakdown.level=ifelse(breakdown.level=="routine","admissions_electives",breakdown.level)) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

###### Overall - outpatients

table1.overall.grampian.outpatient <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","outpatient_attendances_total.csv"),
                                        header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",strata="overall",breakdown.level="outpatient_attendance",
         number.events=outpatient_attendances,number.patients=people_attending) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

###### Overall - A&E

table1.overall.grampian.ae <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","ae_attendances_total.csv"),
                                            header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",strata="overall",breakdown.level="AE_attendance",
         number.events=ae_attendances,number.patients=people_attending) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

table1.overall.grampian <- plyr::rbind.fill(table1.overall.grampian.all,table1.overall.grampian.emrout,table1.overall.grampian.outpatient,table1.overall.grampian.ae)
rm(table1.overall.grampian.all,table1.overall.grampian.emrout,table1.overall.grampian.outpatient,table1.overall.grampian.ae)

##############################
####### Table 1, by age ######
##############################

###### Age - all admissions

table1.age.grampian.all <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","hospital_admissions_by_age.csv"),
                                     header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="age",breakdown.level="admissions_all",strata=age_band) %>%
  select(.,year_month,breakdown,breakdown.level,strata,people_admitted,hospital_admissions) %>%
  dplyr::rename(.,number.patients=people_admitted,number.events=hospital_admissions)

###### Age - emergency and routine

table1.age.grampian.emrout <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","hospital_admissions_by_type_by_age.csv"),
                                 header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="age",breakdown.level=admission_type,strata=age_band) %>%
  mutate(.,breakdown.level=ifelse(breakdown.level=="emergency","admissions_emergency",breakdown.level)) %>%
  mutate(.,breakdown.level=ifelse(breakdown.level=="routine","admissions_electives",breakdown.level)) %>%
  select(.,year_month,breakdown,breakdown.level,strata,people_admitted,hospital_admissions) %>%
  dplyr::rename(.,number.patients=people_admitted,number.events=hospital_admissions)

###### Age - outpatients

table1.age.grampian.outpatient <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","outpatient_attendances_by_age.csv"),
                                            header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="age",strata=age_band,breakdown.level="outpatient_attendance",
         number.events=outpatient_attendances,number.patients=people_attending) %>%
  select(.,year_month,breakdown,breakdown.level,strata,number.patients,number.events)

###### Age - A&E

table1.age.grampian.ae <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","ae_attendances_by_age.csv"),
                                    header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="age",strata=age_band,breakdown.level="AE_attendance",
         number.events=ae_attendances,number.patients=people_attending) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

table1.age.grampian <- plyr::rbind.fill(table1.age.grampian.all,table1.age.grampian.emrout,table1.age.grampian.outpatient,table1.age.grampian.ae)
rm(table1.age.grampian.all,table1.age.grampian.emrout,table1.age.grampian.outpatient,table1.age.grampian.ae)

##############################
####### Table 1, by sex ######
##############################

###### Sex - all admissions

table1.sex.grampian.all <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","hospital_admissions_by_sex.csv"),
                                 header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="sex",strata=sex,breakdown.level="admissions_all") %>%
  select(.,year_month,breakdown,breakdown.level,strata,people_admitted,hospital_admissions) %>%
  dplyr::rename(.,number.patients=people_admitted,number.events=hospital_admissions)

###### Sex - emergency and routine

table1.sex.grampian.emrout <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","hospital_admissions_by_type_by_sex.csv"),
                                    header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="sex",breakdown.level=admission_type,strata=sex) %>%
  mutate(.,breakdown.level=ifelse(breakdown.level=="emergency","admissions_emergency",breakdown.level)) %>%
  mutate(.,breakdown.level=ifelse(breakdown.level=="routine","admissions_electives",breakdown.level)) %>%
  select(.,year_month,breakdown,breakdown.level,strata,people_admitted,hospital_admissions) %>%
  dplyr::rename(.,number.patients=people_admitted,number.events=hospital_admissions)

###### Sex - outpatients

table1.sex.grampian.outpatient <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","outpatient_attendances_by_sex.csv"),
                                        header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="sex",breakdown.level="outpatient_attendance",strata=sex,
         number.events=outpatient_attendances,number.patients=people_attending) %>%
  select(.,year_month,breakdown,breakdown.level,strata,number.patients,number.events)

###### Sex - A&E

table1.sex.grampian.ae <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","ae_attendances_by_sex.csv"),
                                    header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="sex",strata=sex,breakdown.level="AE_attendance",
         number.events=ae_attendances,number.patients=people_attending) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

table1.sex.grampian <- plyr::rbind.fill(table1.sex.grampian.all,table1.sex.grampian.emrout,table1.sex.grampian.outpatient,table1.sex.grampian.ae)
rm(table1.sex.grampian.all,table1.sex.grampian.emrout,table1.sex.grampian.outpatient,table1.sex.grampian.ae)

##############################
####### Table 1, by IMD ######
##############################

###### IMD - all admissions

table1.imd.grampian.all <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","hospital_admissions_by_deprivation.csv"),
                                 header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",strata=simd_quintile,breakdown.level="admissions_all") %>%
  select(.,year_month,breakdown,breakdown.level,strata,people_admitted,hospital_admissions) %>%
  dplyr::rename(.,number.patients=people_admitted,number.events=hospital_admissions)

###### IMD - emergency and routine

table1.imd.grampian.emrout <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","hospital_admissions_by_type_by_deprivation.csv"),
                                    header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",strata=simd_quintile,breakdown.level=admission_type) %>%
  mutate(.,breakdown.level=ifelse(breakdown.level=="emergency","admissions_emergency",breakdown.level)) %>%
  mutate(.,breakdown.level=ifelse(breakdown.level=="routine","admissions_electives",breakdown.level)) %>%
  select(.,year_month,breakdown,breakdown.level,strata,people_admitted,hospital_admissions) %>%
  dplyr::rename(.,number.patients=people_admitted,number.events=hospital_admissions)

###### IMD - outpatients

table1.imd.grampian.outpatient <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","outpatient_attendances_by_deprivation.csv"),
                                        header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",strata=simd_quintile,breakdown.level="outpatient_attendance",
         number.events=outpatient_attendances,number.patients=people_attending) %>%
  select(.,year_month,breakdown,breakdown.level,strata,number.patients,number.events)

###### IMD - A&E

table1.imd.grampian.ae <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","ae_attendances_by_deprivation.csv"),
                                header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",strata=simd_quintile,breakdown.level="AE_attendance",
         number.events=ae_attendances,number.patients=people_attending) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

table1.imd.grampian <- plyr::rbind.fill(table1.imd.grampian.all,table1.imd.grampian.emrout,table1.imd.grampian.outpatient,table1.imd.grampian.ae)
rm(table1.imd.grampian.all,table1.imd.grampian.emrout,table1.imd.grampian.outpatient,table1.imd.grampian.ae)

######################
####### Table 1 ######
######################

table1.grampian <- plyr::rbind.fill(table1.overall.grampian,table1.age.grampian,table1.sex.grampian,table1.imd.grampian)
rm(table1.overall.grampian,table1.age.grampian,table1.sex.grampian,table1.imd.grampian)
table1.grampian$partner <- "Grampian"

####################################################################
##################### Table 2: COVID admissions ####################
####################################################################

table2.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","covid_diagnoses.csv"),
                         header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(date_month,format="%d/%m/%Y"),"%Y-%m"),
         breakdown="overall",breakdown.level="overall") %>%
  select(.,year_month,breakdown,breakdown.level,people_with_any_covid_diagnosis,people_with_covid_diagnosis_first_spell) %>%
  reshape2::melt(data = ., id.vars = c("year_month","breakdown","breakdown.level"),
                 measure.vars = c("people_with_any_covid_diagnosis",
                                  "people_with_covid_diagnosis_first_spell"),
                 value.name="number.patients",variable.name="type") %>%
  mutate(.,type=str_replace_all(type,"people_with_any_covid_diagnosis","COVID19_admissions_any_diagnosis")) %>%
  mutate(.,type=str_replace_all(type,"people_with_covid_diagnosis_first_spell","COVID19_admissions_first_episode"))
table2.grampian$partner <- "Grampian"

###########################################################
##################### Table 3: Deaths #####################
###########################################################

###### Overall

table3.grampian.overall <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","deaths_total.csv"),
                                 header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(month_death_ndl,format="%d/%m/%Y"),"%Y-%m"),
         breakdown="overall",breakdown.level="overall") %>%
  dplyr::rename(.,anywhere=n_deaths,`in hospital`=died_in_hospital) %>%
  select(.,year_month,breakdown,breakdown.level,`in hospital`,anywhere) %>%
  reshape2::melt(data = ., id.vars = c("year_month","breakdown","breakdown.level"),
                 measure.vars = c("anywhere","in hospital"),
                 value.name="number.patients",variable.name="location")

###### Age

table3.grampian.age <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","deaths_by_age.csv"),
                                 header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(month_death_ndl,format="%d/%m/%Y"),"%Y-%m"),
         breakdown="age",breakdown.level=age_band) %>%
  dplyr::rename(.,anywhere=n_deaths,`in hospital`=died_in_hospital) %>%
  select(.,year_month,breakdown,breakdown.level,`in hospital`,anywhere) %>%
  reshape2::melt(data = ., id.vars = c("year_month","breakdown","breakdown.level"),
                 measure.vars = c("anywhere","in hospital"),
                 value.name="number.patients",variable.name="location")

###### Sex

table3.grampian.sex <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","deaths_by_sex.csv"),
                             header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(month_death_ndl,format="%d/%m/%Y"),"%Y-%m"),
         breakdown="sex",breakdown.level=sex) %>%
  dplyr::rename(.,anywhere=n_deaths,`in hospital`=died_in_hospital) %>%
  select(.,year_month,breakdown,breakdown.level,`in hospital`,anywhere) %>%
  reshape2::melt(data = ., id.vars = c("year_month","breakdown","breakdown.level"),
                 measure.vars = c("anywhere","in hospital"),
                 value.name="number.patients",variable.name="location")

###### IMD

table3.grampian.imd <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/grampian_health_care_usage/","deaths_by_deprivation.csv"),
                             header=TRUE, sep=",", check.names=T) %>%
  mutate(.,year_month=format(as.Date(month_death_ndl,format="%d/%m/%Y"),"%Y-%m"),
         breakdown="imd",breakdown.level=simd_quintile) %>%
  dplyr::rename(.,anywhere=n_deaths,`in hospital`=died_in_hospital) %>%
  select(.,year_month,breakdown,breakdown.level,`in hospital`,anywhere) %>%
  reshape2::melt(data = ., id.vars = c("year_month","breakdown","breakdown.level"),
                 measure.vars = c("anywhere","in hospital"),
                 value.name="number.patients",variable.name="location")

######################
####### Table 3 ######
######################

table3.grampian <- plyr::rbind.fill(table3.grampian.overall,table3.grampian.age,table3.grampian.sex,table3.grampian.imd)
rm(table3.grampian.overall,table3.grampian.age,table3.grampian.sex,table3.grampian.imd)
table3.grampian$partner <- "Grampian"

######################################################
##################### SAVE FILES #####################
######################################################

fwrite(table1.grampian, file = paste0(rawdatadir,"Grampian-Aberdeen/table1.grampian.csv"), sep = ",")
fwrite(table2.grampian, file = paste0(rawdatadir,"Grampian-Aberdeen/table2.grampian.csv"), sep = ",")
fwrite(table3.grampian, file = paste0(rawdatadir,"Grampian-Aberdeen/table3.grampian.csv"), sep = ",")