##############################################
################### TO-DO ####################
##############################################

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,
               gmodels,data.table,
               tibble,pbapply,pbmcapply,here,
               tidyverse,readxl)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Partner outputs/Output 3 HC usage/"

#Git directory
gitdir <- rstudioapi::getSourceEditorContext()$path %>%
  dirname(.) %>%
  dirname(.)

###########################################################
##################### Import Table 1  #####################
###########################################################

#Grampian
table1.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/table1.grampian.csv"),
                               header=TRUE, sep=",", check.names=T)
#Leeds
table1.leeds <- fread(paste0(rawdatadir,"Leeds/table1.leeds.csv"),
                         header=TRUE, sep=",", check.names=T)

#Liverpool Wirral
table1.liverpoolwirral <- fread(paste0(rawdatadir,"Liverpool-Wirral/table1.lpoolwirral.csv"),
                      header=TRUE, sep=",", check.names=T)

#Wales
table1.wales <- fread(paste0(rawdatadir,"Wales/table1.wales.csv"),
                                header=TRUE, sep=",", check.names=T)

#NW London
table1.nwlondon <- fread(paste0(rawdatadir,"NW London/table1.nwlondon.csv"),
                      header=TRUE, sep=",", check.names=T)

#Merge all
table1 <- plyr::rbind.fill(table1.grampian,table1.wales,table1.nwlondon,table1.liverpoolwirral,table1.leeds)
table1 <- filter(table1,!(is.na(number.patients)&is.na(number.events)))
rm(table1.grampian,table1.wales,table1.nwlondon,table1.liverpoolwirral,table1.leeds)

###########################################################
##################### Import Table 2  #####################
###########################################################

#Grampian
table2.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/table2.grampian.csv"),
                         header=TRUE, sep=",", check.names=T)

#Leeds
table2.leeds <- fread(paste0(rawdatadir,"Leeds/table2.leeds.csv"),
                      header=TRUE, sep=",", check.names=T)

#Liverpool Wirral
table2.liverpoolwirral <- fread(paste0(rawdatadir,"Liverpool-Wirral/table2.lpoolwirral.csv"),
                                header=TRUE, sep=",", check.names=T)

#Wales
table2.wales <- fread(paste0(rawdatadir,"Wales/table2.wales.csv"),
                                header=TRUE, sep=",", check.names=T)

#NW London
table2.nwlondon <- fread(paste0(rawdatadir,"NW London/table2.nwlondon.csv"),
                      header=TRUE, sep=",", check.names=T)

#Merge all
table2 <- plyr::rbind.fill(table2.grampian,table2.wales,table2.nwlondon,table2.liverpoolwirral,table2.leeds)
table2 <- filter(table2,!(is.na(number.patients)))
rm(table2.grampian,table2.wales,table2.nwlondon,table2.liverpoolwirral,table2.leeds)

###########################################################
##################### Import Table 3  #####################
###########################################################

#Grampian
table3.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/table3.grampian.csv"),
                         header=TRUE, sep=",", check.names=T)

#Leeds
table3.leeds <- fread(paste0(rawdatadir,"Leeds/table3.leeds.csv"),
                      header=TRUE, sep=",", check.names=T)

#Liverpool Wirral
table3.liverpoolwirral <- fread(paste0(rawdatadir,"Liverpool-Wirral/table3.lpoolwirral.csv"),
                      header=TRUE, sep=",", check.names=T)

#Wales
table3.wales <- fread(paste0(rawdatadir,"Wales/table3.wales.csv"),
                                header=TRUE, sep=",", check.names=T)

#NW London
table3.nwlondon <- fread(paste0(rawdatadir,"NW London/table3.nwlondon.csv"),
                      header=TRUE, sep=",", check.names=T)

#Merge all
table3 <- plyr::rbind.fill(table3.grampian,table3.wales,table3.nwlondon,table3.liverpoolwirral,table3.leeds)
table3 <- filter(table3,!(is.na(number.patients)))
rm(table3.grampian,table3.wales,table3.nwlondon,table3.liverpoolwirral,table3.leeds)

###########################################################
##################### Merge in totals #####################
###########################################################

totals <- fread(paste0(rawdatadir,"totals.csv"),
                               header=TRUE, sep=",", check.names=T) %>%
  mutate(.,total.patients=as.numeric(total.patients))

#Table 1

table1 <- left_join(table1,select(totals,partner,breakdown,strata,total.patients),
                             by=c("partner","breakdown","strata"))

#Table 2
table2 <- left_join(table2,select(totals,partner,breakdown,breakdown.level,total.patients),
                             by=c("partner","breakdown","breakdown.level"))

#Table 3
table3 <- left_join(table3,select(totals,partner,breakdown,breakdown.level,total.patients),
                             by=c("partner","breakdown","breakdown.level"))

#Remove totals data
rm(totals)

######################################################################
##################### Values changed due to SDC  #####################
######################################################################

numbers_only <- function(x) !grepl("\\D", x)

#Table 1
table1 <- table1 %>%
  mutate(.,number.patients.sdc=ifelse(numbers_only(number.patients),number.patients,NA),
         number.events.sdc=ifelse(numbers_only(number.events),number.events,NA)) %>%
  mutate(.,number.patients.sdc=as.numeric(number.patients.sdc),
         number.events.sdc=as.numeric(number.events.sdc))
  
#Table 2
table2 <- table2 %>%
  mutate(.,number.patients.sdc=ifelse(numbers_only(number.patients),number.patients,NA)) %>%
  mutate(.,number.patients.sdc=as.numeric(number.patients.sdc))

#Table 3
table3 <- table3 %>%
  mutate(.,number.patients.sdc=ifelse(numbers_only(number.patients),number.patients,NA)) %>%
  mutate(.,number.patients.sdc=as.numeric(number.patients.sdc))

##############################################################################
##################### Time-varying totals due to deaths  #####################
##############################################################################

#Add cumulative deaths by partner and death location

detach(package:plyr)
table3 <- table3 %>%
  mutate(.,year_month_day=lubridate::ymd(paste0(year_month,"-01"))) %>%
  arrange(.,partner,breakdown,breakdown.level,location,year_month_day) %>%
  group_by(partner,breakdown,breakdown.level,location) %>%
  summarise(.,number.patients=number.patients,
            number.patients.sdc=number.patients.sdc,
            year_month=year_month,
            year_month_day=year_month_day,
            total.patients=total.patients,
            cumdeaths=cumsum(replace_na(number.patients.sdc,0))) %>%
  ungroup() %>%
  mutate(.,death_cum_pct=(cumdeaths/total.patients)*100)

#Compute time-varying denominator (from 2018)

# skeleton_months <- table1 %>%
#   filter(.,breakdown=="overall",strata=="overall",breakdown.level=="admissions_all") %>%
#   select(.,partner,breakdown,year_month)
# 
# detach(package:plyr)
# total.patients.rem <- table3 %>%
#   filter(.,breakdown=="overall",location=="anywhere") %>%
#   select(.,partner,breakdown,breakdown.level,year_month,year_month_day,cumdeaths,total.patients) %>%
#   arrange(.,partner,breakdown,breakdown.level,year_month_day) %>%
#   group_by(partner) %>%
#   mutate(cumdeaths_lm = lag(cumdeaths, order_by=partner)) %>%
#   mutate(cumdeaths_lm = ifelse(is.na(cumdeaths_lm),0,cumdeaths_lm)) %>%
#   mutate(total.patients.rem=(total.patients-cumdeaths_lm)) %>%
#   select(.,partner,breakdown,year_month,total.patients.rem) %>%
#   left_join(skeleton_months,.,by=c("partner","breakdown","year_month")) %>%
#   arrange(.,partner,breakdown,desc(year_month))
# 
# rm(skeleton_months)
# total.patients.rem <- total.patients.rem %>%
#   group_by(partner) %>%
#   fill(total.patients.rem) %>%
#   arrange(partner,breakdown,year_month) %>%
#   ungroup()
# 
# #Merge time-varying total into other tables
# 
# table1 <- left_join(table1,total.patients.rem,by=c("partner","breakdown","year_month"))
# table2 <- left_join(table2,total.patients.rem,by=c("partner","breakdown","year_month"))
# table3 <- left_join(table3,total.patients.rem,by=c("partner","breakdown","year_month"))
# rm(total.patients.rem)

###########################################################################
##################### Rolling difference in activity  #####################
###########################################################################

# table1 <- table1 %>%
#   group_by(partner,breakdown,strata,breakdown.level) %>%
#   arrange(.,partner,breakdown,strata,breakdown.level,year_month) %>%
#   mutate(number.events_lm = dplyr::lag(number.events.sdc, n = 1, default = NA)) %>%
#   mutate(pct_change_activity=(number.events.sdc-number.events_lm)/number.events_lm*100) %>%
#   ungroup()

# filter(table1,partner=="NW London",strata=="overall",
#        breakdown.level=="admissions_all",year_month>="2019-01") %>%
#   select(.,year_month,breakdown.level,number.events.sdc,number.events_lm,pct_change_activity)

###################################################################################
##################### How many admissions were COVID-related  #####################
###################################################################################

table2.covid <- table2 %>%
  filter(.,type=="COVID19_admissions_any_diagnosis") %>%
  select(.,partner,year_month,breakdown,breakdown.level,number.patients.sdc) %>%
  dplyr::rename(.,number.patients.covid=number.patients.sdc,
                strata=breakdown.level) %>%
  mutate(.,breakdown.level="admissions_all")

table1 <- left_join(table1,table2.covid,by=c("year_month","partner","breakdown","breakdown.level","strata")) %>%
  mutate(.,pct.patients.adm.covid=number.patients.covid/number.patients.sdc*100,
         pct.patients.covid=number.patients.covid/total.patients*100)

#################################################
##################### Rates #####################
#################################################

#Table 1
table1 <- table1 %>%
  mutate(.,pct.people=(number.patients.sdc/total.patients)*100,
         cases.per.100=(number.events.sdc/total.patients)*100)

#Table 2
table2 <- table2 %>%
  mutate(.,pct.people=(number.patients.sdc/total.patients)*100)

#########################################################
##################### Save results  #####################
#########################################################

fwrite(table1, file = paste0(gitdir,"/Visualize data/Data/table1.all.partners.csv"), sep = ",")
fwrite(table2, file = paste0(gitdir,"/Visualize data/Data/table2.all.partners.csv"), sep = ",")
fwrite(table3, file = paste0(gitdir,"/Visualize data/Data/table3.all.partners.csv"), sep = ",")