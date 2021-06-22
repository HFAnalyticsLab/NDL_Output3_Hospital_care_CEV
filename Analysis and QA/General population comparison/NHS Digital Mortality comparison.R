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
rawdatadir <- "M:/Analytics/Networked Data Lab/Shielding/Tracking Healthcare Activity and Outcomes/Sep 2020/"

#Git directory
gitdir <- rstudioapi::getSourceEditorContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  dirname(.)

#One drive directory
onedrivedir <- "C:/Users/SebastienP/OneDrive - The Health Foundation/Output 3/Charts/"

#Auxiliary function
numbers_only <- function(x) !grepl("\\D", x)

####################################################################
##################### Import NHS Digital data  #####################
####################################################################

######################## Load NHS Digital data: Mortality

nhsdigital.mortality <- fread(paste0(rawdatadir,"Tracking Healthcare Activity and Outcomes for Shielded Patients, England, Open Data.csv"),
                             header=TRUE, sep=",", check.names=T) %>%
  filter(.,Activity_type=="Mortality",Breakdown=="All",Rate!="NULL") %>%
  mutate(.,Rate=as.numeric(Rate)) %>%
  select(.,Population_group,Breakdown,Date,Rate,Numerator,Denominator) %>%
  pivot_wider(
    names_from = Population_group,
    names_sep = ".",
    values_from = c(Rate,Numerator,Denominator)
  ) %>%
  mutate(.,Date=lubridate::dmy(Date)) %>%
  filter(.,Date<="2020-08-31")

nhsdigital.mortality %>%
  slice_max(.,SPL,n=1)

#Deaths between 23/03/20 and 31/7/30
nhsdigital.mortality %>%
  pull(.,Numerator.SPL) %>%
  as.numeric() %>%
  sum(.)

######################## Save

fwrite(nhsdigital.mortality, file = paste0(onedrivedir,"nhsdigital.mortality.csv"), sep = ",")

######################## Load NHS Digital data: COVID testing

nhsdigital.testing <- fread(paste0(rawdatadir,"Tracking Healthcare Activity and Outcomes for Shielded Patients, England, Open Data.csv"),
                              header=TRUE, sep=",", check.names=T) %>%
  filter(.,Activity_type=="Pillar 1 testing",Breakdown=="COVID-19",Rate!="NULL") %>%
  mutate(.,Rate=as.numeric(Rate)) %>%
  select(.,Population_group,Breakdown,Date,Rate) %>%
  pivot_wider(
    names_from = Population_group,
    names_sep = ".",
    values_from = c(Rate)
  ) %>%
  mutate(.,Date=lubridate::dmy(Date)) %>%
  filter(.,Date<="2020-07-31")

nhsdigital.testing %>%
  slice_max(.,SPL,n=1)

nhsdigital.testing %>%
  slice_max(.,`General population`,n=1)

######################## Save

fwrite(nhsdigital.testing, file = paste0(onedrivedir,"nhsdigital.testing.csv"), sep = ",")