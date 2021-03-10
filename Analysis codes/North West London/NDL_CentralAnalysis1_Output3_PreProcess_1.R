#Import libraries
library(data.table)
library(readxl)
library(dplyr)


data <- fread("../NDL - Shielding Patients 20210212.csv")
cols <- read_excel("../NDL - Shielding Patients COLUMN NAMES 20210209.xlsx")

colnames(data) <- colnames(cols)

data$total_admissions <- data$total_elective_admissions + data$total_emergency_admissions + data$total_other_admissions

#Data cleaning####
##Clean age variable
data <- data %>% 
  mutate(ageband = as.numeric(age)) %>%
  mutate(ageband = case_when(ageband < 30 ~ "<30",
                             ageband>=30 & ageband<50 ~ "30 to 49",
                             ageband>=50 & ageband<70 ~ "50 to 69",
                             ageband>=70 ~ "70 or older",
                             is.na(ageband) ~ "Unknown"))

##Clean gender variable
data$gender[data$gender == "NULL"] <- "Unknown"

##Clean IMD ranks
data <- data %>%
  mutate(imd_rank = as.numeric(imd_rank)) %>%
  mutate(IMD_Quintile = case_when(imd_rank <= 6568 ~ "1",
                                  imd_rank >= 6569  & imd_rank <= 13137 ~ "2",
                                  imd_rank >= 13138 & imd_rank <= 19706 ~ "3",
                                  imd_rank >= 19707 & imd_rank <= 26275 ~ "4",
                                  imd_rank >= 26276 ~ "5",
                                  is.na(imd_rank) ~ "Unknown")
         )

##Add month and year variables

data$Period <- as.Date(data$Period, format = "%Y-%m-%d")
data$Year <- format(data$Period, format="%Y")
data$Month <- format(data$Period, format = "%b")

#Filter data past jul 2020
data <- subset(data,data$Period < as.Date("2020-08-01"))

#Fix issue with death any cause not showing up in certain cases
data$death_any_setting[data$death_any_setting == 0 & data$death_in_hospital == 1] <- 1

#Write data
fwrite(data,"Output3data.csv")

