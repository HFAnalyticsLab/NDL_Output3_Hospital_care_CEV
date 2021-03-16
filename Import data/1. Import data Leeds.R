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

###### Overall

table1.overall.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_3B_Tables_Leeds4.xlsx"),
                                          sheet = "Table 3-1 Overall") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",strata="overall",breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

##### Age

table1.age.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_3B_Tables_Leeds4.xlsx"),
                                      sheet = "Table 3-1 Age") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="age",strata=age_group,breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events) %>%
  mutate(.,strata=str_replace_all(strata,"<30","0-29")) %>%
  mutate(.,strata=str_replace_all(strata,".to.","-")) %>%
  mutate(.,strata=str_replace_all(strata,".or.older","+"))
  
##### Sex

table1.sex.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_3B_Tables_Leeds4.xlsx"),
                                      sheet = "Table 3-1 Sex") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="sex",strata=sex,breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events) %>%
  mutate(.,strata=str_replace_all(strata,"female","F")) %>%
  mutate(.,strata=str_replace_all(strata,"male","M"))

##### IMD

table1.imd.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_3B_Tables_Leeds4.xlsx"),
                                  sheet = "Table 3-1 Deprivation") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",strata=deprivation,breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

##### Aggregate

table1.leeds <- plyr::rbind.fill(table1.overall.leeds,table1.age.leeds,table1.sex.leeds,table1.imd.leeds)
rm(table1.overall.leeds,table1.age.leeds,table1.sex.leeds,table1.imd.leeds)
table1.leeds$partner <- "Leeds"

##### Remove all_admissions

table1.leeds <- table1.leeds %>%
  mutate(.,breakdown.level=ifelse(breakdown.level=="admissions_elective","admissions_electives",breakdown.level))

#######################
####### Table 2 ####### 
#######################

###### Overall

table2.overall.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_3B_Tables_Leeds4.xlsx"),
                                      sheet = "Table 3-2 Overall") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",breakdown.level="overall") %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

###### Age

table2.age.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_3B_Tables_Leeds4.xlsx"),
                                      sheet = "Table 3-2 Age") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="age",breakdown.level=age_group) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".or.older","+")) %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

###### Sex

table2.sex.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_3B_Tables_Leeds4.xlsx"),
                                  sheet = "Table 3-2 Sex") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="sex",breakdown.level=sex) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M")) %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

###### IMD

table2.imd.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_3B_Tables_Leeds4.xlsx"),
                                  sheet = "Table 3-2 Deprivation") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",breakdown.level=deprivation) %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

##### Aggregate

table2.leeds <- plyr::rbind.fill(table2.overall.leeds,table2.age.leeds,table2.sex.leeds,table2.imd.leeds)
rm(table2.overall.leeds,table2.age.leeds,table2.sex.leeds,table2.imd.leeds)
table2.leeds$partner <- "Leeds"

#######################
####### Table 3 ####### 
#######################

###### Overall

table3.overall.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_3B_Tables_Leeds4.xlsx"),
                                      sheet = "Table 3-3 Overall") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",breakdown.level="overall",
         number.patients=number.patients,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital"))

###### Age

table3.age.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_3B_Tables_Leeds4.xlsx"),
                                      sheet = "Table 3-3 Age") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="age",breakdown.level=age_group,
         number.patients=number.patients,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".or.older","+"))

###### Sex

table3.sex.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_3B_Tables_Leeds4.xlsx"),
                                  sheet = "Table 3-3 Sex") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="sex",breakdown.level=sex,
         number.patients=number.patients,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

###### IMD

table3.dep.leeds <- read_excel(paste0(rawdatadir,"Leeds/","Output_3B_Tables_Leeds4.xlsx"),
                                  sheet = "Table 3-3 Deprivation") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",breakdown.level=deprivation,
         number.patients=number.patients,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital"))

##### Aggregate

table3.leeds <- plyr::rbind.fill(table3.overall.leeds,table3.age.leeds,table3.sex.leeds,table3.dep.leeds)
rm(table3.overall.leeds,table3.age.leeds,table3.sex.leeds,table3.dep.leeds)
table3.leeds$partner <- "Leeds"

######################################################
##################### SAVE FILES #####################
######################################################

fwrite(table1.leeds, file = paste0(rawdatadir,"Leeds/table1.leeds.csv"), sep = ",")
fwrite(table2.leeds, file = paste0(rawdatadir,"Leeds/table2.leeds.csv"), sep = ",")
fwrite(table3.leeds, file = paste0(rawdatadir,"Leeds/table3.leeds.csv"), sep = ",")