##############################################
################### TO-DO ####################
##############################################

#Remove pregnancy from charts
#Narrow down to a relevant list of specialites
#Add source of info online on chapters
#Add some lines of interpretation (after having more data)

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

#Return top 10
return_top_10 <- function(typefun,partnerfun){
  out <- filter(tableS1.all.partners,partner==partnerfun,type==typefun) %>%
    filter(.,!(ICD10.chapter %in% c("No match with ICD-10 chapters","22 - Codes for special purposes"))) %>%
    mutate(.,abs_diff_1920=(number.events.2020.sdc-number.events.2019.sdc),
           abs_sign_diff_1920=abs(number.events.2020.sdc-number.events.2019.sdc)) %>%
    slice_max(.,abs_sign_diff_1920,n=10) %>%
    arrange(.,type,partner,abs_diff_1920) %>%
    select(.,type,partner,ICD10.chapter,abs_diff_1920,number.events.2019.sdc,number.events.2020.sdc,pct.change.1920) %>%
    mutate(.,ICD10.number=as.numeric(string::word(ICD10.chapter,sep='-')))
  return(out)
}

#Return top 5
return_top_5 <- function(typefun,partnerfun){
  out <- filter(tableS1.all.partners,partner==partnerfun,type==typefun) %>%
    filter(.,!(ICD10.chapter %in% c("No match with ICD-10 chapters","22 - Codes for special purposes"))) %>%
    mutate(.,abs_diff_1920=(number.events.2020.sdc-number.events.2019.sdc),
           abs_sign_diff_1920=abs(number.events.2020.sdc-number.events.2019.sdc)) %>%
    slice_max(.,abs_sign_diff_1920,n=5) %>%
    arrange(.,type,partner,abs_diff_1920) %>%
    select(.,type,partner,ICD10.chapter,abs_diff_1920,number.events.2019.sdc,number.events.2020.sdc,pct.change.1920) %>%
    mutate(.,ICD10.number=as.numeric(string::word(ICD10.chapter,sep='-'))) %>%
    mutate(.,ICD10.number=as.factor(ICD10.number))
  return(out)
}

#Return top 5 (both types)
return_both_top_5 <- function(partnerfun){
  #Top 5 elective chapters
  out_elective <- return_top_5(typefun="elective",partner=partnerfun) %>%
    select(.,ICD10.chapter)
  #Top 5 emergency chapters
  out_emergency <- return_top_5(typefun="emergency",partner=partnerfun) %>%
    select(.,ICD10.chapter)
  #Absolute changes for those chapters
  out.chap <- rbind(out_elective,out_emergency) %>% unique(.)
  out.results <- filter(tableS1.all.partners,partner==partnerfun) %>%
    filter(.,(ICD10.chapter %in% out.chap$ICD10.chapter)) %>%
    mutate(.,abs_diff_1920=(number.events.2020.sdc-number.events.2019.sdc),
           abs_sign_diff_1920=abs(number.events.2020.sdc-number.events.2019.sdc)) %>%
    arrange(.,type,partner,abs_diff_1920) %>%
    select(.,type,partner,ICD10.chapter,abs_diff_1920,number.events.2019.sdc,number.events.2020.sdc,pct.change.1920) %>%
    mutate(.,ICD10.number=as.numeric(stringr::word(ICD10.chapter,sep='-'))) %>%
    mutate(.,ICD10.number=as.factor(ICD10.number))
  #Return function
  return(out.results)
}

#Return top 10 (both types)
return_both_top_10 <- function(partnerfun){
  #Top 5 elective chapters
  out_elective <- return_top_10(typefun="elective",partner=partnerfun) %>%
    select(.,ICD10.chapter)
  #Top 5 emergency chapters
  out_emergency <- return_top_10(typefun="emergency",partner=partnerfun) %>%
    select(.,ICD10.chapter)
  #Absolute changes for those chapters
  out.chap <- rbind(out_elective,out_emergency) %>% unique(.)
  out.results <- filter(tableS1.all.partners,partner==partnerfun) %>%
    filter(.,(ICD10.chapter %in% out.chap$ICD10.chapter)) %>%
    mutate(.,abs_diff_1920=(number.events.2020.sdc-number.events.2019.sdc),
           abs_sign_diff_1920=abs(number.events.2020.sdc-number.events.2019.sdc)) %>%
    arrange(.,type,partner,abs_diff_1920) %>%
    select(.,type,partner,ICD10.chapter,abs_diff_1920,number.events.2019.sdc,number.events.2020.sdc,pct.change.1920) %>%
    mutate(.,ICD10.number=as.numeric(stringr::word(ICD10.chapter,sep='-'))) %>%
    mutate(.,ICD10.number=as.factor(ICD10.number))
  #Return function
  return(out.results)
}

############################################################
##################### Import Table S1  #####################
############################################################

######################## Leeds

tableS1.leeds <- read_excel(paste0(rawdatadir,"Leeds/Output3C_Supplementary_Leeds.xlsx"))

#Change levels
tableS1.leeds <- tableS1.leeds %>%
  mutate(.,time.period=str_replace_all(time.period,"march_to_july_","")) %>%
  mutate(.,type=str_replace_all(type,"admissions_",""))

#Long to wide
tableS1.leeds <- tableS1.leeds %>%
  pivot_wider(.,names_from = "time.period",names_sep = ".",values_from = c("number.events")) %>%
  dplyr::rename(.,number.events.2020="2020",number.events.2019="2019")

#Values suppressed due to SDC
tableS1.leeds <- tableS1.leeds %>%
  mutate(.,number.events.2020.sdc=ifelse(numbers_only(number.events.2020),number.events.2020,NA),
         number.events.2019.sdc=ifelse(numbers_only(number.events.2019),number.events.2019,NA)) %>%
  mutate(.,
         number.events.2020.sdc=as.numeric(number.events.2020.sdc),
         number.events.2019.sdc=as.numeric(number.events.2019.sdc))

#Percentage change 2019-2020
tableS1.leeds <- tableS1.leeds %>%
  mutate(.,pct.change.1920=(number.events.2020.sdc-number.events.2019.sdc)/number.events.2019.sdc*100)

#Partner name
tableS1.leeds$partner <- "Leeds"

######################## NW London

tableS1.nwlondon <- read_excel(paste0(rawdatadir,"NW London/Output 3 Supplementary Table SS final 1204.xlsx"),sheet="Supplementary Table S3-1")

#Rename variables
tableS1.nwlondon <- tableS1.nwlondon %>%
  dplyr::rename(.,type=type.admission)

#Change levels
tableS1.nwlondon <- tableS1.nwlondon %>%
  mutate(.,time.period=str_replace_all(time.period,"march_to_july_",""))

#Long to wide
tableS1.nwlondon <- tableS1.nwlondon %>%
  pivot_wider(.,names_from = "time.period",names_sep = ".",values_from = c("number.events")) %>%
  dplyr::rename(.,number.events.2020="2020",number.events.2019="2019")
  
#Vales suppressed due to SDC
tableS1.nwlondon <- tableS1.nwlondon %>%
  mutate(.,number.events.2020.sdc=ifelse(numbers_only(number.events.2020),number.events.2020,NA),
         number.events.2019.sdc=ifelse(numbers_only(number.events.2019),number.events.2019,NA)) %>%
  mutate(.,
         number.events.2020.sdc=as.numeric(number.events.2020.sdc),
         number.events.2019.sdc=as.numeric(number.events.2019.sdc))

#Percentage change 2019-2020
tableS1.nwlondon <- tableS1.nwlondon %>%
  mutate(.,pct.change.1920=(number.events.2020.sdc-number.events.2019.sdc)/number.events.2019.sdc*100)

#Partner name
tableS1.nwlondon$partner <- "NW London"

######################## Wales

tableS1.wales <- read_excel(paste0(rawdatadir,"Wales/Supplementary+Table+Wales.xlsx"),sheet="Supplementary Table S3-1")

#Rename variables
tableS1.wales <- tableS1.wales %>%
  dplyr::rename(.,type=type.admission)

#Change levels
tableS1.wales <- tableS1.wales %>%
  mutate(.,time.period=str_replace_all(time.period,"march_to_july_",""))

#Long to wide
tableS1.wales <- tableS1.wales %>%
  pivot_wider(.,names_from = "time.period",names_sep = ".",values_from = c("number.events")) %>%
  dplyr::rename(.,number.events.2020="2020",number.events.2019="2019")

#Values suppressed due to SDC
tableS1.wales <- tableS1.wales %>%
  mutate(.,number.events.2020.sdc=ifelse(numbers_only(number.events.2020),number.events.2020,NA),
         number.events.2019.sdc=ifelse(numbers_only(number.events.2019),number.events.2019,NA)) %>%
  mutate(.,
         number.events.2020.sdc=as.numeric(number.events.2020.sdc),
         number.events.2019.sdc=as.numeric(number.events.2019.sdc))

#Percentage change 2019-2020
tableS1.wales <- tableS1.wales %>%
  mutate(.,pct.change.1920=(number.events.2020.sdc-number.events.2019.sdc)/number.events.2019.sdc*100)

#Partner name
tableS1.wales$partner <- "Wales"

######################## Grampian

tableS1.grampian <- fread(paste0(rawdatadir,"Grampian-Aberdeen/grampian_health_care_usage/additional_analysis/icd_chapters.csv"),
      header=TRUE, sep=",", check.names=T)

#Reshape and change levels
tableS1.grampian <- tableS1.grampian %>%
  pivot_longer(!level, names_to = "longid", values_to = "number.events") %>%
  mutate(.,type=ifelse(longid %in% c("freq_emergency_covid","freq_emergency_precovid"),"emergency","elective"),
         year=ifelse(endsWith(longid, "_precovid"),"2019","2020")) %>%
  select(.,-"longid") %>%
  pivot_wider(.,names_from = "year",names_sep = ".",values_from = c("number.events")) %>%
  dplyr::rename(.,ICD10.chapter=level,number.events.2020="2020",number.events.2019="2019")

#Vales suppressed due to SDC
tableS1.grampian <- tableS1.grampian %>%
  mutate(.,number.events.2020.sdc=ifelse(numbers_only(number.events.2020),number.events.2020,NA),
         number.events.2019.sdc=ifelse(numbers_only(number.events.2019),number.events.2019,NA)) %>%
  mutate(.,
         number.events.2020.sdc=as.numeric(number.events.2020.sdc),
         number.events.2019.sdc=as.numeric(number.events.2019.sdc))

#Percentage change 2019-2020
tableS1.grampian <- tableS1.grampian %>%
  mutate(.,pct.change.1920=(number.events.2020.sdc-number.events.2019.sdc)/number.events.2019.sdc*100)

#Partner name
tableS1.grampian$partner <- "Grampian"

######################## Liverpool-Wirral

tableS1.lpoolwirral <- read_excel(paste0(rawdatadir,"Liverpool-Wirral/Output 3 Supplementary Table - Liv & Wirral supressed.xlsx"),sheet="Joint") %>%
  select(.,"time.period","type.admission","ICD10.chapter","number.events")

#Rename variables
tableS1.lpoolwirral <- tableS1.lpoolwirral %>%
  dplyr::rename(.,type=type.admission)

#Change levels
tableS1.lpoolwirral <- tableS1.lpoolwirral %>%
  mutate(.,time.period=str_replace_all(time.period,"march_to_july_",""))

#Long to wide
tableS1.lpoolwirral <- tableS1.lpoolwirral %>%
  pivot_wider(.,names_from = "time.period",names_sep = ".",values_from = c("number.events")) %>%
  dplyr::rename(.,number.events.2020="2020",number.events.2019="2019")

#Values suppressed due to SDC
tableS1.lpoolwirral <- tableS1.lpoolwirral %>%
  mutate(.,number.events.2020.sdc=ifelse(numbers_only(number.events.2020),number.events.2020,NA),
         number.events.2019.sdc=ifelse(numbers_only(number.events.2019),number.events.2019,NA)) %>%
  mutate(.,
         number.events.2020.sdc=as.numeric(number.events.2020.sdc),
         number.events.2019.sdc=as.numeric(number.events.2019.sdc))

#Percentage change 2019-2020
tableS1.lpoolwirral <- tableS1.lpoolwirral %>%
  mutate(.,pct.change.1920=(number.events.2020.sdc-number.events.2019.sdc)/number.events.2019.sdc*100)

#Partner name
tableS1.lpoolwirral$partner <- "Liverpool and Wirral"

######################## Merge all

tableS1 <- plyr::rbind.fill(tableS1.nwlondon,tableS1.leeds,tableS1.wales,tableS1.grampian,tableS1.lpoolwirral)
rm(tableS1.nwlondon,tableS1.leeds,tableS1.wales,tableS1.grampian,tableS1.lpoolwirral)

######################## Pooled results

detach("package:plyr", unload=TRUE)
tableS1_pooled <- tableS1 %>%
  group_by(type,ICD10.chapter) %>%
  summarise(.,number.events.2019.sdc=sum(number.events.2019.sdc,na.rm=TRUE),
            number.events.2020.sdc=sum(number.events.2020.sdc,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(.,partner="Average",
         pct.change.1920=(number.events.2020.sdc-number.events.2019.sdc)/number.events.2019.sdc*100)

tableS1 <- plyr::rbind.fill(tableS1,tableS1_pooled)
rm(tableS1_pooled)

######################## Any SDC violations

tableS1.sdc<- tableS1 %>%
  filter(.,!((number.events.2019.sdc<5)|(number.events.2020.sdc<5))) %>%
  arrange(.,ICD10.chapter,type,partner)

# anti_join(tableS1, tableS1.sdc, by = c('partner','type','ICD10.chapter')) %>%
#   arrange(.,ICD10.chapter,type,partner)

######################## Save

fwrite(tableS1.sdc, file = paste0(gitdir,"/Visualize data/Data/tableS1.all.partners.csv"), sep = ",")

############################################################
##################### Absolute changes #####################
############################################################

tableS1.all.partners <- tableS1.sdc

### Elective admissions

tableS1.data.elective <- tableS1.all.partners %>%
  filter(.,!(ICD10.chapter %in% c("No match with ICD-10 chapters","22 - Codes for special purposes"))) %>%
  filter(.,type=="elective") %>%
  select(.,partner,ICD10.chapter,number.events.2019.sdc,number.events.2020.sdc) %>%
  gather(., year, number.events, number.events.2019.sdc:number.events.2020.sdc, factor_key=TRUE) %>%
  mutate(.,year=str_replace_all(year,"number.events.2019.sdc","2019")) %>%
  mutate(.,year=str_replace_all(year,"number.events.2020.sdc","2020")) %>%
  mutate(.,year=as.numeric(year)) %>%
  arrange(.,partner,-year,-number.events)

chart.elective <- filter(tableS1.data.elective,partner=="Average") %>%
  ggplot(., aes(x=year, y=number.events, group=ICD10.chapter, color=ICD10.chapter)) +
  geom_line()+
  ggtitle("Number of elective admissions by ICD-10 chapter") +
  theme_ipsum() +
  xlab("Year") +
  ylab("Number of admissions") +
  labs(col="ICD-10 chapter") +
  theme(legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 8),
        legend.text=element_text(size=9),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, hjust = 1,size = 8),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(chart.elective)

### Emergency admissions

tableS1.data.emergency <- tableS1.all.partners %>%
  filter(.,!(ICD10.chapter %in% c("No match with ICD-10 chapters","22 - Codes for special purposes"))) %>%
  filter(.,type=="emergency") %>%
  select(.,partner,ICD10.chapter,number.events.2019.sdc,number.events.2020.sdc) %>%
  gather(., year, number.events, number.events.2019.sdc:number.events.2020.sdc, factor_key=TRUE) %>%
  mutate(.,year=str_replace_all(year,"number.events.2019.sdc","2019")) %>%
  mutate(.,year=str_replace_all(year,"number.events.2020.sdc","2020")) %>%
  mutate(.,year=as.numeric(year))

chart.emergency <- filter(tableS1.data.emergency,partner=="Average") %>%
  ggplot(., aes(x=year, y=number.events, group=ICD10.chapter, color=ICD10.chapter)) +
  geom_line() +
  ggtitle("Number of emergency admissions by ICD-10 chapter") +
  theme_ipsum() +
  xlab("Year") +
  ylab("Number of admissions") +
  labs(col="ICD-10 chapter") +
  theme(legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 8),
        legend.text=element_text(size=9),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, hjust = 1,size = 8),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(chart.emergency)

############################################################
##################### Relative changes #####################
############################################################

#Top 10 - elective - 2019
top10_a <- filter(tableS1.all.partners,type=="elective",partner=="Average") %>%
  slice_max(number.events.2019.sdc, n = 10) %>%
  select(.,ICD10.chapter) %>%
  unique(.)

#Top 10 - emergency - 2019
top10_b <- filter(tableS1.all.partners,type=="emergency",partner=="Average") %>%
  slice_max(number.events.2019.sdc, n = 10) %>%
  select(.,ICD10.chapter) %>%
  unique(.)

#Top 10 - elective - 2020
top10_c <- filter(tableS1.all.partners,type=="elective",partner=="Average") %>%
  slice_max(number.events.2020.sdc, n = 10) %>%
  select(.,ICD10.chapter) %>%
  unique(.)

#Top 10 - emergency - 2020
top10_d <- filter(tableS1.all.partners,type=="emergency",partner=="Average") %>%
  slice_max(number.events.2020.sdc, n = 10) %>%
  select(.,ICD10.chapter) %>%
  unique(.)

#Top 10 summary
top10 <- rbind(top10_a,top10_b,top10_c,top10_d) %>%
  unique(.)
rm(top10_a,top10_b,top10_c,top10_d)

#Order of chart
orderS1 <- tableS1.all.partners %>%
  filter(.,type=="elective",partner=="Average") %>%
  filter(.,(ICD10.chapter %in% top10$ICD10.chapter)) %>%
  arrange(.,-pct.change.1920) %>%
  mutate(.,order=1:nrow(.)) %>%
  select(.,ICD10.chapter,order)

#Chart data
tableS1.data <- tableS1.all.partners %>%
  filter(.,(ICD10.chapter %in% top10$ICD10.chapter)) %>%
  left_join(.,orderS1,by="ICD10.chapter")

#Chart
tableS1.chart <- tableS1.data %>%
  ggplot(., aes(y = reorder(ICD10.chapter, -order), x = pct.change.1920,col=partner)) +
  geom_point(size = 3) +  # Use a larger dot
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "black", size=1) +
  facet_wrap(~ type) +
  theme_ipsum() +
  labs(title="% change in number of events",subtitle="March to July period, 2019-2020",
       x ="% change", y = "ICD-10 chapter") +
  theme(legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(tableS1.chart) %>%
  layout(title = list(text = paste0('% change in number of events',
                                    '<br>',
                                    '<sup>',
                                    'March to July period, 2019-2020',
                                    '</sup>')))

##################################################################
##################### Elective vs. Emergency #####################
##################################################################

tableS1.type.comp <- tableS1.all.partners %>%
  filter(.,!(ICD10.chapter %in% c("No match with ICD-10 chapters","22 - Codes for special purposes"))) %>%
  select(.,partner,type,ICD10.chapter,number.events.2019.sdc,number.events.2020.sdc) %>%
  pivot_wider(.,names_from = type,
             names_sep = ".",
             values_from = c(number.events.2019.sdc, number.events.2020.sdc)) %>%
  mutate(.,pct_change_emergency=(number.events.2020.sdc.emergency-number.events.2019.sdc.emergency)/number.events.2019.sdc.emergency*100,
         pct_change_elective=(number.events.2020.sdc.elective-number.events.2019.sdc.elective)/number.events.2019.sdc.elective*100) %>%
  mutate(.,pct_points_diff=(pct_change_emergency-pct_change_elective))

filter(tableS1.type.comp,partner=="Average") %>%
  select(.,partner,ICD10.chapter,pct_change_emergency,pct_change_elective) %>%
  filter(.,pct_change_elective>pct_change_emergency)

filter(tableS1.type.comp,partner!="Average") %>%
  select(.,partner,ICD10.chapter,pct_change_emergency,pct_change_elective,pct_points_diff) %>%
  filter(.,pct_change_emergency<pct_change_elective) %>%
  arrange(.,ICD10.chapter,pct_points_diff)

###################################################################
##################### Differences in patterns #####################
###################################################################

##################### Elective

data.abs.elective.chart.data <- rbind(return_top_5(typefun="elective",partnerfun="Average"),
return_top_5(typefun="elective",partnerfun="NW London"),
return_top_5(typefun="elective",partnerfun="Leeds"),
return_top_5(typefun="elective",partnerfun="Liverpool and Wirral"),
return_top_5(typefun="elective",partnerfun="Wales"),
return_top_5(typefun="elective",partnerfun="Grampian"))

# %>%
#   mutate(.,ICD10.number=reorder_within(ICD10.number, abs_diff_1920, partner)) %>%
#   mutate(., ICD10.chapter=reorder_within(ICD10.chapter, abs_diff_1920, partner))

data.abs.elective.chart <- data.abs.elective.chart.data %>%
  ggplot(., aes(x=reorder_within(ICD10.number,abs_diff_1920,partner), y=abs_diff_1920)) +
  geom_bar(aes(fill=ICD10.chapter), stat="identity", width=0.5) +
  facet_wrap(~ partner, scales = "free") +
  ggtitle("Elective admissions: ICD-10 chapters with highest reductions") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reordered() +
  xlab("Chapter") +
  ylab("Absolute reduction") +
  labs(fill="ICD-10") +
  theme(legend.position = "bottom",
        legend.text=element_text(size=8))

ggplotly(data.abs.elective.chart)

##################### Emergency

data.abs.emergency.chart.data <- rbind(return_top_5(typefun="emergency",partnerfun="Average"),
                                      return_top_5(typefun="emergency",partnerfun="NW London"),
                                      return_top_5(typefun="emergency",partnerfun="Leeds"),
                                      return_top_5(typefun="emergency",partnerfun="Liverpool and Wirral"),
                                      return_top_5(typefun="emergency",partnerfun="Wales"),
                                      return_top_5(typefun="emergency",partnerfun="Grampian"))

# %>%
#   mutate(.,ICD10.number=reorder_within(ICD10.number, abs_diff_1920, partner)) %>%
#   mutate(., ICD10.chapter=reorder_within(ICD10.chapter, abs_diff_1920, partner))

data.abs.emergency.chart <- data.abs.elective.chart.data %>%
  ggplot(., aes(x=reorder_within(ICD10.number,abs_diff_1920,partner), y=abs_diff_1920)) +
  geom_bar(aes(fill=ICD10.chapter), stat="identity", width=0.5) +
  facet_wrap(~ partner, scales = "free") +
  ggtitle("Emergency admissions: ICD-10 chapters with highest reductions") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reordered() +
  xlab("Chapter") +
  ylab("Absolute reduction") +
  labs(fill="ICD-10") +
  theme(legend.position = "bottom",
        legend.text=element_text(size=8))

ggplotly(data.abs.emergency.chart)

################################################################################
##################### Absolute: top 5, combined admissions #####################
################################################################################

unique.chapters.labels <- fread(paste0(gitdir,"/Visualize data/Data/unique.chapters.labels.csv"),
                                header=TRUE, sep=",", check.names=T)

data.both <- rbind(return_both_top_5(partnerfun="Average"),
                   return_both_top_5(partnerfun="NW London"),
                   return_both_top_5(partnerfun="Leeds"),
                   return_both_top_5(partnerfun="Liverpool and Wirral"),
                   return_both_top_5(partnerfun="Wales"),
                   return_both_top_5(partnerfun="Grampian"))

data.both <- data.both %>%
  group_by(partner,ICD10.chapter) %>%
  mutate(.,max_change=max(abs(abs_diff_1920))) %>%
  ungroup() %>%
  arrange(.,partner,ICD10.chapter,type) %>%
  left_join(.,unique.chapters.labels,by="ICD10.chapter")

chart.abs.both <- data.both %>%
  ggplot(., aes(x=reorder_within(ICD10.number,-max_change,partner), y=abs_diff_1920)) +
  geom_bar(aes(fill = type), position = "dodge", stat="identity", width=0.5) +
  facet_wrap(~ partner, scales = "free") +
  ggtitle("Hospital admissions: ICD-10 chapters with largest changes between 2019-2020") +
  scale_y_continuous(labels = scales::comma) +
  tidytext::scale_x_reordered() +
  xlab("Chapter") +
  ylab(" ") +
  labs(fill="Type of admission") +
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "right",
        legend.text=element_text(size=8))

ggplotly(chart.abs.both) %>%
  layout(title = list(text = paste0('Hospital admissions: ICD-10 chapters with largest changes',
                                    '<br>',
                                    '<sup>',
                                    'March to July period, 2019-2020',
                                    '</sup>')))

#Save for Flourish

data.both.flourish <- data.both %>%
  select(.,partner,type,ICD10.chapter,ICD10.number,ICD10.label,max_change,abs_diff_1920) %>%
  pivot_wider(
    names_from = "type",
    values_from = c("abs_diff_1920")
  ) %>%
  arrange(.,partner,-max_change)

onedrivedir <- "C:/Users/SebastienP/OneDrive - The Health Foundation/Output 3/Charts/"
fwrite(data.both.flourish, file = paste0(onedrivedir,"top5.changes.admissions.both.csv"), sep = ",")

#########################################################################################
##################### Absolute: top 10, combined admissions, Pooled #####################
#########################################################################################

unique.chapters.labels <- fread(paste0(gitdir,"/Visualize data/Data/unique.chapters.labels.csv"),
                                header=TRUE, sep=",", check.names=T)

data.both <- rbind(return_both_top_10(partnerfun="Average"),
                   return_both_top_10(partnerfun="NW London"),
                   return_both_top_10(partnerfun="Leeds"),
                   return_both_top_10(partnerfun="Liverpool and Wirral"),
                   return_both_top_10(partnerfun="Wales"),
                   return_both_top_10(partnerfun="Grampian"))

data.both <- data.both %>%
  group_by(partner,ICD10.chapter) %>%
  mutate(.,max_change=max(abs(abs_diff_1920)),
         mean_pct_change=mean(pct.change.1920)) %>%
  ungroup() %>%
  arrange(.,partner,ICD10.chapter,type) %>%
  left_join(.,unique.chapters.labels,by="ICD10.chapter")

chart.abs.both <- data.both %>%
  ggplot(., aes(x=reorder_within(ICD10.number,-max_change,partner), y=abs_diff_1920)) +
  geom_bar(aes(fill = type), position = "dodge", stat="identity", width=0.5) +
  facet_wrap(~ partner, scales = "free") +
  ggtitle("Hospital admissions: ICD-10 chapters with largest changes between 2019-2020") +
  scale_y_continuous(labels = scales::comma) +
  tidytext::scale_x_reordered() +
  xlab("Chapter") +
  ylab(" ") +
  labs(fill="Type of admission") +
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "right",
        legend.text=element_text(size=8))

ggplotly(chart.abs.both) %>%
  layout(title = list(text = paste0('Hospital admissions: ICD-10 chapters with largest changes',
                                    '<br>',
                                    '<sup>',
                                    'March to July period, 2019-2020',
                                    '</sup>')))

#Save for Flourish

data.both.flourish <- data.both %>%
  filter(.,partner=="Average") %>%
  select(.,partner,type,ICD10.chapter,ICD10.number,ICD10.label,max_change,abs_diff_1920) %>%
  pivot_wider(
    names_from = "type",
    values_from = c("abs_diff_1920")
  ) %>%
  arrange(.,partner,-max_change)

onedrivedir <- "C:/Users/SebastienP/OneDrive - The Health Foundation/Output 3/Charts/"
fwrite(data.both.flourish, file = paste0(onedrivedir,"top10.changes.admissions.both.csv"), sep = ",")

#Save for Flourish (% change chart)

data.both.pct.flourish <- data.both %>%
  filter(.,partner=="Average") %>%
  select(.,partner,type,ICD10.chapter,ICD10.number,ICD10.label,pct.change.1920) %>%
  arrange(.,partner,type,-pct.change.1920)

onedrivedir <- "C:/Users/SebastienP/OneDrive - The Health Foundation/Output 3/Charts/"
fwrite(data.both.pct.flourish, file = paste0(onedrivedir,"top10.changes.admissions.both.pct.csv"), sep = ",")