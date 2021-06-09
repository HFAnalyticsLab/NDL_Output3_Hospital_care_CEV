##############################################
################### TO-DO ####################
##############################################

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,ggplot2,easycsv,readxl,
               gmodels,DescTools,data.table,
               tibble,plotly,lubridate,
               here,RColorBrewer,ggthemes,hrbrthemes,
               tidyverse,showtext,tidytext,
               knitr,kableExtra)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Partner outputs/Output 3 HC usage/"

#Git directory
gitdir <- rstudioapi::getSourceEditorContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  dirname(.)

#Auxiliary function
numbers_only <- function(x) !grepl("\\D", x)

##########################################################################
##################### Import central analysis table  #####################
##########################################################################

#Service use statistics
table1.all.partners <- fread("Visualize data/Data/table1.all.partners.csv",
                             header=TRUE, sep=",", check.names=T) %>%
  filter(.,strata=="overall",partner %in% c("NW London","Liverpool-Wirral")) %>%
  mutate(.,year=word(year_month, 1, sep = fixed('-')),month=word(year_month, 2, sep = fixed('-'))) %>%
  mutate(.,year_month_day=paste(year_month,"01",sep="-")) %>%
  select(.,-c("number.patients.covid","pct.patients.adm.covid","pct.patients.covid","breakdown","strata")) %>%
  mutate(.,cohort="SPL")

############################################################
##################### Import Sup. Table  ###################
############################################################

######################## NW London

### Demographics

tableS2.demo.nwlondon <- read_excel(paste0(rawdatadir,"NW London/NW London -NDL - Non Shielding Patients Combined 27042021.xlsx"),sheet="Demographics",skip=3)
total.patients.nwlondon <- tableS2.demo.nwlondon %>%
  filter(.,Metric=="Total Patients") %>%
  pull(Value)

### Service use

tableS2.nwlondon <- read_excel(paste0(rawdatadir,"NW London/NW London -NDL - Non Shielding Patients Combined 27042021.xlsx"),sheet="Hospital activity",skip=2) %>%
  rename(.,number.patients=`Total Patients`,number.events=`Total Events`,breakdown.level=`Activity Type`) %>%
  mutate(.,year_month=format(as.Date(Period,format="%Y-%m-%d"),"%Y-%m")) %>%
  select(.,-c("Period")) %>%
  mutate(.,total.patients=total.patients.nwlondon,cohort="Non SPL",partner="NW London") %>%
  mutate(.,breakdown.level=str_replace_all(breakdown.level,"A&E - Attendances","AE_attendance"),
         breakdown.level=str_replace_all(breakdown.level,"Admissions - All","admissions_all"),
         breakdown.level=str_replace_all(breakdown.level,"Admissions - Emergency","admissions_emergency"),
         breakdown.level=str_replace_all(breakdown.level,"Admissions - Elective","admissions_electives"),
         breakdown.level=str_replace_all(breakdown.level,"Admissions - Other","admissions_other"),
         breakdown.level=str_replace_all(breakdown.level,"Outpatient - Attendances","outpatient_attendance")) %>%
  mutate(.,number.patients.sdc=ifelse(numbers_only(number.patients),number.patients,NA),
         number.events.sdc=ifelse(numbers_only(number.events),number.events,NA)) %>%
  mutate(.,
         number.patients.sdc=as.numeric(number.patients.sdc),
         number.events.sdc=as.numeric(number.events.sdc)) %>%
  mutate(.,cases.per.100=number.events.sdc/total.patients*100,
         pct.people=number.patients.sdc/total.patients*100,
         year_month_day=paste(year_month,"01",sep="-"),
         year=word(year_month, 1, sep = fixed('-')),
         month=word(year_month, 2, sep = fixed('-')))
rm(total.patients.nwlondon)

### Append with non-SPL

tableS2.nwlondon.cev <- table1.all.partners %>%
  filter(.,partner=="NW London") %>%
  select(.,names(tableS2.nwlondon))

tableS2.nwlondon <- plyr::rbind.fill(tableS2.nwlondon,tableS2.nwlondon.cev)
rm(tableS2.nwlondon.cev)

######################## Liverpool-Wirral

### Demographics

tableS2.demo.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/Liverpool Wirral - Output Table Additional - cev vs gen pop Liverpool and Wirral.xlsx"),sheet="table 1",skip=1)

### Service use

tableS2.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/Liverpool Wirral - Output Table Additional - cev vs gen pop Liverpool and Wirral.xlsx"),sheet="table 2") %>%
  select(.,time_start,type,cev.number.events,cev.total.patients,gen.pop.number.events,gen.pop.total.patients) %>%
  reshape2::melt(.,id.vars=c("time_start", "type","cev.total.patients","gen.pop.total.patients"),
       measure.vars=c("cev.number.events", "gen.pop.number.events"),
       variable.name="cohort",
       value.name="number.events"
  ) %>%
  mutate(.,cohort=str_replace_all(cohort,"cev.number.events","SPL"),
         cohort=str_replace_all(cohort,"gen.pop.number.events","Non SPL"),
         total.patients=ifelse(cohort=="SPL",cev.total.patients,gen.pop.total.patients),
         partner="Liverpool-Wirral") %>%
  select(.,-c("cev.total.patients","gen.pop.total.patients")) %>%
  rename(.,breakdown.level=type) %>%
  mutate(.,year_month=format(as.Date(time_start,format="%Y-%m-%d"),"%Y-%m")) %>%
  select(.,-c("time_start")) %>%
  mutate(.,number.events.sdc=ifelse(numbers_only(number.events),number.events,NA)) %>%
  mutate(.,number.events.sdc=as.numeric(number.events.sdc)) %>%
  mutate(.,cases.per.100=number.events.sdc/total.patients*100,
         year_month_day=paste(year_month,"01",sep="-"),
         year=word(year_month, 1, sep = fixed('-')),
         month=word(year_month, 2, sep = fixed('-')))

######################## Merge all

tableS2 <- plyr::rbind.fill(tableS2.nwlondon,tableS2.lpoolwirral)
rm(tableS2.nwlondon,tableS2.lpoolwirral,table1.all.partners)

######################## % change data

tableS2.all.partners.pcts <-  tableS2 %>%
  mutate(.,year_month_day=lubridate::ymd(year_month_day),
         year_l1=(as.numeric(year)-1)) %>%
  mutate(.,year_month_day_l1=lubridate::ymd(paste(year_l1,month,"01",sep="-")),
         keyone=paste(partner,cohort,breakdown.level,sep="-"))

tableS2.all.partners.pcts.l1 <- tableS2.all.partners.pcts %>%
  select(.,keyone,year_month_day,number.events.sdc) %>%
  dplyr::rename(.,number.events.sdc_l1=number.events.sdc,
                year_month_day_l1=year_month_day)

tableS2.all.partners.pcts <- left_join(tableS2.all.partners.pcts,
                                       tableS2.all.partners.pcts.l1,
                                      by=c("keyone","year_month_day_l1")) %>%
  select(.,-c("keyone","year_l1")) %>%
  mutate(.,pct_change_20to19=(number.events.sdc-number.events.sdc_l1)/number.events.sdc_l1*100,
         abs_change_12m=(number.events.sdc-number.events.sdc_l1)) %>%
  mutate(.,year_month_day=lubridate::ymd(year_month_day)) %>%
  filter(.,year_month>="2019-07") %>%
  arrange(.,partner,cohort,breakdown.level,year_month)
rm(tableS2.all.partners.pcts.l1)

# #NW London, SPL, Outpatient, Apr 20
# #49,933 in 2019, 26,363 in 2020 (delta -23570 and % -47.20325)
# 
# tableS2.all.partners.pcts %>%
#   filter(.,partner=="NW London",cohort=="SPL",
#          breakdown.level=="outpatient_attendance",year_month=="2020-04")
# tableS2 %>%
#   filter(.,partner=="NW London",cohort=="SPL",
#          breakdown.level=="outpatient_attendance",year_month=="2019-04")
# 
# #Liverpool-Wirral, Non SPL, A&E, May 20
# #22,929 in 2019, 12,983 in 2020 (delta  and % )
# 
# tableS2.all.partners.pcts %>%
#   filter(.,partner=="Liverpool-Wirral",cohort=="Non SPL",
#          breakdown.level=="AE_attendance",year_month=="2020-05")
# tableS2 %>%
#   filter(.,partner=="Liverpool-Wirral",cohort=="Non SPL",
#          breakdown.level=="AE_attendance",year_month=="2019-05")

######################## Pooled results

# detach("package:plyr", unload=TRUE)
# tableS1_pooled <- tableS1 %>%
#   group_by(type,ICD10.chapter) %>%
#   summarise(.,number.events.2019.sdc=sum(number.events.2019.sdc,na.rm=TRUE),
#             number.events.2020.sdc=sum(number.events.2020.sdc,na.rm=TRUE)) %>%
#   ungroup() %>%
#   mutate(.,partner="Average",
#          pct.change.1920=(number.events.2020.sdc-number.events.2019.sdc)/number.events.2019.sdc*100)
# 
# tableS1 <- plyr::rbind.fill(tableS1,tableS1_pooled)
# rm(tableS1_pooled)

######################## Any SDC violations

filter(tableS2,number.events.sdc<5)
filter(tableS2,number.patients.sdc<5)

######################## Save

fwrite(tableS2, file = paste0(gitdir,"/Visualize data/Data/tableS2.all.partners.csv"), sep = ",")

#####################################################
##################### Load data #####################
#####################################################

tableS2.all.partners <- tableS2

##############################################################################
##################### Absolute - Contacts per 100 people #####################
##############################################################################

### Outpatient attendance in NW London

abs.outpatient.london.data <- tableS2.all.partners %>%
  filter(.,breakdown.level=="outpatient_attendance")

abs.outpatient.london.chart <- abs.outpatient.london.data %>%
  ggplot(., aes(x=year_month, y=cases.per.100, group=cohort)) +
  geom_line(aes(color=cohort)) +
  facet_wrap(~ partner, scales = "free") +
  ggtitle("Contacts per 100 people") +
  theme_ipsum() +
  xlab("Year month") +
  ylab("Contacts per 100 people") +
  labs(col="Cohort") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 8),
        legend.text=element_text(size=9),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, hjust = 1,size = 8),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(abs.outpatient.london.chart)

##############################################################################
##################### Relative - Contacts per 100 people #####################
##############################################################################

### Outpatient attendance in NW London

rel.outpatient.london.data <- tableS2.all.partners.pcts %>%
  filter(.,breakdown.level=="outpatient_attendance",partner=="Liverpool-Wirral")

rel.outpatient.london.chart <- rel.outpatient.london.data %>%
  ggplot(., aes(x=year_month, y=pct_change_20to19, group=cohort)) +
  geom_line(aes(color=cohort)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey") +
  ggtitle("% change relative to the previous year") +
  theme_ipsum() +
  xlab("Year month") +
  ylab("% change") +
  labs(col="Cohort") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 8),
        legend.text=element_text(size=9),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, hjust = 1,size = 8),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(rel.outpatient.london.chart)

# #Save for Flourish
# 
# data.both.flourish <- data.both %>%
#   filter(.,partner=="Average") %>%
#   select(.,partner,type,ICD10.chapter,ICD10.number,ICD10.label,max_change,abs_diff_1920) %>%
#   pivot_wider(
#     names_from = "type",
#     values_from = c("abs_diff_1920")
#   ) %>%
#   arrange(.,partner,-max_change)
# 
# onedrivedir <- "C:/Users/SebastienP/OneDrive - The Health Foundation/Output 3/Charts/"
# fwrite(data.both.flourish, file = paste0(onedrivedir,"top10.changes.admissions.both.csv"), sep = ",")

##################################################################################################
##################### Relative changes in NW London- Contacts per 100 people #####################
##################################################################################################

cases.100.nwlondon.flourish <- tableS2.all.partners %>%
  filter(.,partner=="NW London",breakdown.level!="admissions_all",
         breakdown.level!="admissions_other") %>%
  select(.,breakdown.level,year_month,year_month_day,cohort,cases.per.100) %>%
  pivot_wider(
    names_from = 'cohort',
    names_sep = ".",
    values_from = c('cases.per.100')
  ) %>%
  rename(.,Type=breakdown.level,CEV=SPL,`Adult population (non-CEV)`=`Non SPL`) %>%
  mutate(.,Type=ifelse(Type=="admissions_electives","Elective admissions",Type)) %>%
  mutate(.,Type=ifelse(Type=="admissions_emergency","Emergency admissions",Type)) %>%
  mutate(.,Type=ifelse(Type=="AE_attendance","A&E attendance",Type)) %>%
  mutate(.,Type=ifelse(Type=="outpatient_attendance","Outpatient attendance",Type))

onedrivedir <- "C:/Users/SebastienP/OneDrive - The Health Foundation/Output 3/Charts/"
fwrite(cases.100.nwlondon.flourish, file = paste0(onedrivedir,"cases.100.nwlondon.flourish.csv"), sep = ",")