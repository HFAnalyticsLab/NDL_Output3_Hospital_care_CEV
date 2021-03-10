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

#Don't import totals
#FINAL COL NAMES
#year_month	breakdown	breakdown.level	strata	number.patients	number.events

#month	year	time_start	time_end	type	number.events	number.patients	total.patients
#breakdown breakdown.level
#admissions_emergency (OK)
#admissions_electives
#admissions_other
#AE_attendance
#all_admissions
#outpatient_attendance
# month	year	time_start	time_end	type	number.events	number.patients	total.patients

#######################
####### Table 1 ####### 
#######################

###### Overall

table1.overall.wales <- read_excel(paste0(rawdatadir,"Wales/NDL Wales Output 3.xlsx"),
                                          sheet = "Table 3-1 Overall") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",strata="overall",breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

##### Age

table1.age.wales <- read_excel(paste0(rawdatadir,"Wales/NDL Wales Output 3.xlsx"),
                                      sheet = "Table 3-1 Age") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="age",strata=age_group,breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events) %>%
  mutate(.,strata=str_replace_all(strata,"<30","0-29")) %>%
  mutate(.,strata=str_replace_all(strata,".to.","-")) %>%
  mutate(.,strata=str_replace_all(strata,".or.older","+"))
  
##### Sex

table1.sex.wales <- read_excel(paste0(rawdatadir,"Wales/NDL Wales Output 3.xlsx"),
                                      sheet = "Table 3-1 Sex") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="sex",strata=sex,breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events) %>%
  mutate(.,strata=str_replace_all(strata,"female","F")) %>%
  mutate(.,strata=str_replace_all(strata,"male","M"))

##### IMD

table1.imd.wales <- read_excel(paste0(rawdatadir,"Wales/NDL Wales Output 3.xlsx"),
                                  sheet = "Table 3-1 Deprivation") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",strata=deprivation,breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

##### Aggregate

table1.wales <- plyr::rbind.fill(table1.overall.wales,table1.age.wales,table1.sex.wales,table1.imd.wales)
rm(table1.overall.wales,table1.age.wales,table1.sex.wales,table1.imd.wales)
table1.wales$partner <- "Wales"

#######################
####### Table 2 ####### 
#######################

###### Overall

table2.overall.wales <- read_excel(paste0(rawdatadir,"Wales/NDL Wales Output 3.xlsx"),
                                      sheet = "Table 3-2 Overall") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",breakdown.level="overall") %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

###### Age

table2.age.wales <- read_excel(paste0(rawdatadir,"Wales/NDL Wales Output 3.xlsx"),
                                      sheet = "Table 3-2 Age") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="age",breakdown.level=age_group) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".or.older","+")) %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

###### Sex

table2.sex.wales <- read_excel(paste0(rawdatadir,"Wales/NDL Wales Output 3.xlsx"),
                                  sheet = "Table 3-2 Sex") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="sex",breakdown.level=sex) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M")) %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

###### IMD

table2.imd.wales <- read_excel(paste0(rawdatadir,"Wales/NDL Wales Output 3.xlsx"),
                                  sheet = "Table 3-2 Deprivation") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",breakdown.level=deprivation) %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

##### Aggregate

table2.wales <- plyr::rbind.fill(table2.overall.wales,table2.age.wales,table2.sex.wales,table2.imd.wales)
rm(table2.overall.wales,table2.age.wales,table2.sex.wales,table2.imd.wales)
table2.wales$partner <- "Wales"

#######################
####### Table 3 ####### 
#######################

###### Overall

table3.overall.wales <- read_excel(paste0(rawdatadir,"Wales/NDL Wales Output 3.xlsx"),
                                      sheet = "Table 3-3 Overall") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",breakdown.level="overall",
         number.patients=number.deaths,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital"))

###### Age

table3.age.wales <- read_excel(paste0(rawdatadir,"Wales/NDL Wales Output 3.xlsx"),
                                      sheet = "Table 3-3 Age") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="age",breakdown.level=age_group,
         number.patients=number.deaths,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".or.older","+"))

###### Sex

table3.sex.wales <- read_excel(paste0(rawdatadir,"Wales/NDL Wales Output 3.xlsx"),
                                  sheet = "Table 3-3 Sex") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="sex",breakdown.level=sex,
         number.patients=number.deaths,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

###### IMD

table3.dep.wales <- read_excel(paste0(rawdatadir,"Wales/NDL Wales Output 3.xlsx"),
                                  sheet = "Table 3-3 Deprivation") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",breakdown.level=deprivation,
         number.patients=number.deaths,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital"))

##### Aggregate

table3.wales <- plyr::rbind.fill(table3.overall.wales,table3.age.wales,table3.sex.wales,table3.dep.wales)
rm(table3.overall.wales,table3.age.wales,table3.sex.wales,table3.dep.wales)
table3.wales$partner <- "Wales"

######################################################
##################### SAVE FILES #####################
######################################################

fwrite(table1.wales, file = paste0(rawdatadir,"Wales/table1.wales.csv"), sep = ",")
fwrite(table2.wales, file = paste0(rawdatadir,"Wales/table2.wales.csv"), sep = ",")
fwrite(table3.wales, file = paste0(rawdatadir,"Wales/table3.wales.csv"), sep = ",")