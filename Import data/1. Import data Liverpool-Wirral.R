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

###### Date formats

month.lookup <- data.frame(month.number=c("01","02","03","04","05","06","07","08","09","10","11","12"),
                           month.abbr=month.abb)

###### Overall

table1.overall.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/NDL_Output 3 Tables_Combined1.xlsx"),
                                          sheet = "Table 3-1 Overall") %>%
  left_join(.,month.lookup,by=c("month"="month.abbr")) %>%
  mutate(.,year_month=paste(year,month.number,sep="-"),
         breakdown="overall",strata="overall",breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

##### Age

table1.age.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/NDL_Output 3 Tables_Combined1.xlsx"),
                                      sheet = "Table 3-1 Age") %>%
  left_join(.,month.lookup,by=c("month"="month.abbr")) %>%
  mutate(.,year_month=paste(year,month.number,sep="-"),
         breakdown="age",strata=age_group,breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events) %>%
  mutate(.,strata=str_replace_all(strata,"<30","0-29")) %>%
  mutate(.,strata=str_replace_all(strata,".to.","-")) %>%
  mutate(.,strata=str_replace_all(strata,".or.older","+"))
  
##### Sex

table1.sex.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/NDL_Output 3 Tables_Combined1.xlsx"),
                                      sheet = "Table 3-1 Sex") %>%
  left_join(.,month.lookup,by=c("month"="month.abbr")) %>%
  mutate(.,year_month=paste(year,month.number,sep="-"),
         breakdown="sex",strata=sex,breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events) %>%
  mutate(.,strata=str_replace_all(strata,"female","F")) %>%
  mutate(.,strata=str_replace_all(strata,"male","M"))

##### IMD

table1.imd.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/NDL_Output 3 Tables_Combined1.xlsx"),
                                  sheet = "Table 3-1 Deprivation") %>%
  left_join(.,month.lookup,by=c("month"="month.abbr")) %>%
  mutate(.,year_month=paste(year,month.number,sep="-"),
         breakdown="imd",strata=deprivation,breakdown.level=type) %>%
  select(.,year_month,breakdown,strata,breakdown.level,number.patients,number.events)

##### Aggregate

table1.lpoolwirral <- plyr::rbind.fill(table1.overall.lpoolwirral,table1.age.lpoolwirral,table1.sex.lpoolwirral,table1.imd.lpoolwirral)
rm(table1.overall.lpoolwirral,table1.age.lpoolwirral,table1.sex.lpoolwirral,table1.imd.lpoolwirral)
table1.lpoolwirral$partner <- "Liverpool-Wirral"

#######################
####### Table 2 ####### 
#######################

###### Overall

table2.overall.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/NDL_Output 3 Tables_Combined1.xlsx"),
                                      sheet = "Table 3-2 Overall") %>%
  left_join(.,month.lookup,by=c("month"="month.abbr")) %>%
  mutate(.,year_month=paste(year,month.number,sep="-"),
         breakdown="overall",breakdown.level="overall") %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

###### Age

table2.age.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/NDL_Output 3 Tables_Combined1.xlsx"),
                                      sheet = "Table 3-2 Age") %>%
  left_join(.,month.lookup,by=c("month"="month.abbr")) %>%
  mutate(.,year_month=paste(year,month.number,sep="-"),
         breakdown="age",breakdown.level=age_group) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".or.older","+")) %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

###### Sex

table2.sex.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/NDL_Output 3 Tables_Combined1.xlsx"),
                                  sheet = "Table 3-2 Sex") %>%
  left_join(.,month.lookup,by=c("month"="month.abbr")) %>%
  mutate(.,year_month=paste(year,month.number,sep="-"),
         breakdown="sex",breakdown.level=sex) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M")) %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

###### IMD

table2.imd.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/NDL_Output 3 Tables_Combined1.xlsx"),
                                  sheet = "Table 3-2 Deprivation") %>%
  left_join(.,month.lookup,by=c("month"="month.abbr")) %>%
  mutate(.,year_month=paste(year,month.number,sep="-"),
         breakdown="imd",breakdown.level=deprivation) %>%
  select(.,year_month,breakdown,breakdown.level,type,number.patients)

##### Aggregate

table2.lpoolwirral <- plyr::rbind.fill(table2.overall.lpoolwirral,table2.age.lpoolwirral,table2.sex.lpoolwirral,table2.imd.lpoolwirral)
rm(table2.overall.lpoolwirral,table2.age.lpoolwirral,table2.sex.lpoolwirral,table2.imd.lpoolwirral)
table2.lpoolwirral$partner <- "Liverpool-Wirral"

#######################
####### Table 3 ####### 
#######################

###### Overall

table3.overall.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/NDL_Output 3 Tables_Combined1.xlsx"),
                                      sheet = "Table 3-3 Overall") %>%
  left_join(.,month.lookup,by=c("month"="month.abbr")) %>%
  mutate(.,year_month=paste(year,month.number,sep="-"),
         breakdown="overall",breakdown.level="overall",
         number.patients=number.deaths,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital"))

###### Age

table3.age.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/NDL_Output 3 Tables_Combined1.xlsx"),
                                      sheet = "Table 3-3 Age") %>%
  left_join(.,month.lookup,by=c("month"="month.abbr")) %>%
  mutate(.,year_month=paste(year,month.number,sep="-"),
         breakdown="age",breakdown.level=age_group,
         number.patients=number.deaths,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"<30","0-29")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".to.","-")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,".or.older","+"))

###### Sex

table3.sex.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/NDL_Output 3 Tables_Combined1.xlsx"),
                                  sheet = "Table 3-3 Sex") %>%
  left_join(.,month.lookup,by=c("month"="month.abbr")) %>%
  mutate(.,year_month=paste(year,month.number,sep="-"),
         breakdown="sex",breakdown.level=sex,
         number.patients=number.deaths,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"female","F")) %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"male","M"))

###### IMD

table3.dep.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/NDL_Output 3 Tables_Combined1.xlsx"),
                                  sheet = "Table 3-3 Deprivation") %>%
  left_join(.,month.lookup,by=c("month"="month.abbr")) %>%
  mutate(.,year_month=paste(year,month.number,sep="-"),
         breakdown="imd",breakdown.level=deprivation,
         number.patients=number.deaths,location=type) %>%
  select(.,year_month,breakdown,breakdown.level,location,number.patients) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_anywhere","anywhere")) %>%
  mutate(.,location=str_replace_all(location,"deaths_anycause_hospital","in hospital"))

##### Aggregate

table3.lpoolwirral <- plyr::rbind.fill(table3.overall.lpoolwirral,table3.age.lpoolwirral,table3.sex.lpoolwirral,table3.dep.lpoolwirral)
rm(table3.overall.lpoolwirral,table3.age.lpoolwirral,table3.sex.lpoolwirral,table3.dep.lpoolwirral)
table3.lpoolwirral$partner <- "Liverpool-Wirral"

######################################################
##################### SAVE FILES #####################
######################################################

fwrite(table1.lpoolwirral, file = paste0(rawdatadir,"Liverpool-Wirral/table1.lpoolwirral.csv"), sep = ",")
fwrite(table2.lpoolwirral, file = paste0(rawdatadir,"Liverpool-Wirral/table2.lpoolwirral.csv"), sep = ",")
fwrite(table3.lpoolwirral, file = paste0(rawdatadir,"Liverpool-Wirral/table3.lpoolwirral.csv"), sep = ",")