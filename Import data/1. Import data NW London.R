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

table1.overall.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 3 Tables - machine readableV2 - NWL.xlsx"),
                                          sheet = "Table 3-1 Overall") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",strata="overall",breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

##### Age

table1.age.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 3 Tables - machine readableV2 - NWL.xlsx"),
                                      sheet = "Table 3-1 Age") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="age",strata=age_group,breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events) %>%
  mutate(.,strata=str_replace_all(strata,"<30","0-29")) %>%
  mutate(.,strata=str_replace_all(strata,".to.","-")) %>%
  mutate(.,strata=str_replace_all(strata,".or.older","+"))
  
##### Sex

table1.sex.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 3 Tables - machine readableV2 - NWL.xlsx"),
                                      sheet = "Table 3-1 Sex") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="sex",strata=sex,breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events) %>%
  mutate(.,strata=str_replace_all(strata,"female","F")) %>%
  mutate(.,strata=str_replace_all(strata,"male","M"))

##### IMD

table1.imd.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 3 Tables - machine readableV2 - NWL.xlsx"),
                                  sheet = "Table 3-1 Deprivation") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",strata=deprivation,breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

##### Aggregate

table1.nwlondon <- plyr::rbind.fill(table1.overall.nwlondon,table1.age.nwlondon,table1.sex.nwlondon,table1.imd.nwlondon)
rm(table1.overall.nwlondon,table1.age.nwlondon,table1.sex.nwlondon,table1.imd.nwlondon)
table1.nwlondon$partner <- "NW London"

#######################
####### Table 2 ####### 
#######################

###### Overall

table2.overall.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 3 Tables - machine readableV2 - NWL.xlsx"),
                                      sheet = "Table 3-2 Overall") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",breakdown.level="overall") %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

###### Age

table2.age.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 3 Tables - machine readableV2 - NWL.xlsx"),
                                      sheet = "Table 3-2 Age") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="age",breakdown.level=age_group) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".or.older","+")) %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

###### Sex

table2.sex.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 3 Tables - machine readableV2 - NWL.xlsx"),
                                  sheet = "Table 3-2 Sex") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="sex",breakdown.level=sex) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M")) %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

###### IMD

table2.imd.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 3 Tables - machine readableV2 - NWL.xlsx"),
                                  sheet = "Table 3-2 Deprivation") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",breakdown.level=deprivation) %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

##### Aggregate

table2.nwlondon <- plyr::rbind.fill(table2.overall.nwlondon,table2.age.nwlondon,table2.sex.nwlondon,table2.imd.nwlondon)
rm(table2.overall.nwlondon,table2.age.nwlondon,table2.sex.nwlondon,table2.imd.nwlondon)
table2.nwlondon$partner <- "NW London"

#######################
####### Table 3 ####### 
#######################

###### Overall

table3.overall.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 3 Tables - machine readableV2 - NWL.xlsx"),
                                      sheet = "Table 3-3 Overall") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="overall",breakdown.level="overall",
         number.patients=number.deaths,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital"))

###### Age

table3.age.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 3 Tables - machine readableV2 - NWL.xlsx"),
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

table3.sex.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 3 Tables - machine readableV2 - NWL.xlsx"),
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

table3.dep.nwlondon <- read_excel(paste0(rawdatadir,"NW London/","Output 3 Tables - machine readableV2 - NWL.xlsx"),
                                  sheet = "Table 3-3 Deprivation") %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m"),
         breakdown="imd",breakdown.level=deprivation,
         number.patients=number.deaths,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital"))

##### Aggregate

table3.nwlondon <- plyr::rbind.fill(table3.overall.nwlondon,table3.age.nwlondon,table3.sex.nwlondon,table3.dep.nwlondon)
rm(table3.overall.nwlondon,table3.age.nwlondon,table3.sex.nwlondon,table3.dep.nwlondon)
table3.nwlondon$partner <- "NW London"

######################################################
##################### SAVE FILES #####################
######################################################

fwrite(table1.nwlondon, file = paste0(rawdatadir,"NW London/table1.nwlondon.csv"), sep = ",")
fwrite(table2.nwlondon, file = paste0(rawdatadir,"NW London/table2.nwlondon.csv"), sep = ",")
fwrite(table3.nwlondon, file = paste0(rawdatadir,"NW London/table3.nwlondon.csv"), sep = ",")