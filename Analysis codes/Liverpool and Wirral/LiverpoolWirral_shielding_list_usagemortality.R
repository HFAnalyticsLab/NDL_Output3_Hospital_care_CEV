#----------------------------
# code to produce Output 3 of NDL
# service use of shielded patients (Mar2018 -July 2020)
# covid admissions of shielded patients (Mar-July 2020)
# all cause mortality of shielded patients (Mar-July 2020)
#--------------------------
# Author: Roberta Piroddi
# Date: February 2021
#--------------------------
#-----------------------------


library(data.table)
library(stringr)
library(dtplyr)
library(dplyr)
library(tidyr)

source("setup3.R")
# this file defines the path to the data as datapath <- "..."
# and the input data file name as datafile <- "*.csv"

# spl shielded patient list uploaded in setup3
setnames(spl, tolower(names(spl)))

spdat <- spl[,c(2,97,99,113)] #these are the positions of the characteristic below
setnames(spdat,names(spdat),c("linkpseudo","age","sex","deprivation"))

spdat <- spdat %>% mutate(
                   age_group = case_when(
                    age<30 ~ "<30",
                    age>29 & age<50 ~ "30to49",
                    age>49 & age<70 ~ "50to69",
                    age>69 ~ "70orolder",
                    TRUE ~ "unknown"
                   )
)

# ep1 loaded in setup3 is the file containing the event based table of hospital first episodes
setnames(ep1, tolower(names(ep1)))

dat <- inner_join(ep1,spdat,by="linkpseudo")

dat <- dat %>% filter(start_date_hosp_pro_spell>"2020-02-29") %>% filter(start_date_hosp_pro_spell<"2020-08-01")

dat$month <- month.abb[month(as.POSIXlt(dat$start_date_hosp_pro_spell))]
dat$monthnum <- month(dat$start_date_hosp_pro_spell)


datcovid <- dat %>% filter(any(c("U071","U072") %in% unlist(strsplit(diagnosis_all,split="/"))))
# here select the diagnosis of covid in any position in the first episode
dat <- dat %>% mutate(
               covid_diag = case_when(
               "U071" %in% unlist(strsplit(diagnosis_all,split="/")) ~ 1,
               "U072" %in% unlist(strsplit(diagnosis_all,split="/")) ~ 2,
               TRUE ~ 0
               )
)


dat[, covid := any(c("U071","U072") %in% unlist(strsplit(diagnosis_all,split="/"))), by=1:nrow(dat)]

covdat <- dat[covid==TRUE,]

table32s <- as.data.table(table(covdat$linkpseudo, covdat$monthnum, covdat$sex.x))

table32s<- table32s %>% filter(N>0)

table32d <- as.data.table(table(covdat$linkpseudo, covdat$monthnum, covdat$deprivation))

table32d<- table32d %>% filter(N>0)


#-------------------------------------------------------------------------
# this is the code used to find any admission which is Covid-related

# this part of code refers to a data structure dfoTimeMonths that stores the paths and file
# locations for a number of outcomes of interest, subdivided by the month when they
# took place

oc <- 1   #here oc=1 relates to hospital admissions - outcome 
row <- 27 #here row=27 is the month of March 2020

colname = as.character(dfoutcomes$name[oc])
print(colname)
fname = dfoTimeMonths[row,colname]
outcomeTable=read.csv(fname,header = TRUE,sep=",",nrows=nrows)

setnames(outcomeTable, tolower(names(outcomeTable)))

dat <- outcomeTable

dat$diagnosis_all = as.character(dat$diagnosis_all)

dat[, covid := any(c("U071","U072") %in% unlist(strsplit(diagnosis_all,split="/"))), by=1:nrow(dat)]

dat$year <- 2020

dat$month <- month(dat$start_date_hosp_pro_spell)

for(row in 28:35) # these are months April 2020 to November 2020
{
  
  colname = as.character(dfoutcomes$name[oc])
  print(colname)
  fname = dfoTimeMonths[row,colname]
  outcomeTable=read.csv(fname,header = TRUE,sep=",",nrows=nrows)
  
  setnames(outcomeTable, tolower(names(outcomeTable)))
  
  dat2 <- outcomeTable
  
  dat2$diagnosis_all = as.character(dat2$diagnosis_all)
  
  dat2 <- data.table(dat2)
  
  dat2[, covid := any(c("U071","U072") %in% unlist(strsplit(diagnosis_all,split="/"))), by=1:nrow(dat2)]
  
  dat2$year <- 2020
  
  dat2$month <- month(dat2$start_date_hosp_pro_spell)
  
  dat <- rbind(dat,dat2)
   
  rm(dat2)
  
}

dat0 <- dat

dat <- dat[covid==TRUE,]

dat <- dat[month<8,]

dat1 <- dat

dat <- inner_join(dat,spdat,by="linkpseudo")

ttab <- data.table(table(dat$linkpseudo, dat$month))

t1 <- ttab[N>0,]

tt1 <- data.table(table(t1$V2))

write.table(tt1, "clipboard", sep="\t",row.names = FALSE)

ttaba <- data.table(table(dat$linkpseudo, dat$month, dat$age_group))

t1 <- ttaba[N>0,]

tt1 <- data.table(table(t1$V2, t1$V3))

write.table(tt1, "clipboard", sep="\t",row.names = FALSE)

ttabs <- data.table(table(dat$linkpseudo, dat$month, dat$sex.x))

t1 <- ttabs[N>0,]

tt1 <- data.table(table(t1$V2, t1$V3))

write.table(tt1, "clipboard", sep="\t",row.names = FALSE)

ttabd <- data.table(table(dat$linkpseudo, dat$month, dat$deprivation))

t1 <- ttabd[N>0,]

tt1 <- data.table(table(t1$V2, t1$V3))

write.table(tt1, "clipboard", sep="\t",row.names = FALSE)

#------------------------------------------------------------------------------------
# this is the code used to determine all-cause mortality in the shielded population


oc <-4 # these are the outcomes realted to mortality
row <-27 # the month is March 2020


colname = as.character(dfoutcomes$name[oc])
print(colname)
fname = dfoTimeMonths[row,colname]
outcomeTable=read.csv(fname,header = TRUE,sep=",",nrows=nrows)

setnames(outcomeTable, tolower(names(outcomeTable)))

dat <- outcomeTable

dat$year <- 2020

dat$month <- month(dat$reg_date_of_death)

for(row in 28:31) # for the Months April-July 2020 - as in the data requested
{

  colname = as.character(dfoutcomes$name[oc])
  print(colname)
  fname = dfoTimeMonths[row,colname]
  outcomeTable=read.csv(fname,header = TRUE,sep=",",nrows=nrows)
  
  setnames(outcomeTable, tolower(names(outcomeTable)))
  
  dat2 <- outcomeTable

  
  dat2$year <- 2020
  
  dat2$month <- month(dat2$reg_date_of_death)
  
  dat <- rbind(dat,dat2)
  rm(dat2)
  
  
}

dat <- data.table(dat)
setnames(dat, "dec_nhs_number", "linkpseudo")

dat <- inner_join(dat,spdat,by="linkpseudo")

tm <- data.table(table(dat$month))

tm <- data.table(table(dat$month))
write.table(tm, "clipboard", sep="\t",row.names = FALSE)

tm <- data.table(table(dat$month,dat$age_group))



tm <- data.table(table(dat$month,dat$sex))
write.table(tm, "clipboard", sep="\t",row.names = FALSE)

tm <- data.table(table(dat$month,dat$deprivation))
write.table(tm, "clipboard", sep="\t",row.names = FALSE)

hdat <- dat[pod_establishment_type==1 | pod_establishment_type==3 | pod_establishment_type==18 | pod_establishment_type==19 | pod_establishment_type==99 , ]

#------------------------------------------
# this is the code used to produce the counts of service usage in the shileded population
# for the months between Jan 2018 and July 2020

# spout is loaded from a file specified in setup3 - it contains all service outcomes
# of interest, monthly in the period from Jan 2018 to current

spout$timestamp <- as.numeric(as.character(spout$timestamp))

dat <- inner_join(spout,spdat,by="linkpseudo")

dat <- data.table(dat)

dat <- dat[timestamp>1802 & timestamp<2008,] # this selects only outcomes for the months between Jan 2018 and July 2020

#-----------------------------------------------------------------
# deprivation - counting events

admd <- dat[ , list(all_adm = sum(admissions_all)), by=c('timestamp','deprivation')]

admd <- admd[order(deprivation,-timestamp)]

write.table(admd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(admissions_electives)), by=c('timestamp','deprivation')]

eladmd <- eladmd[order(deprivation,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(admissions_emergency)), by=c('timestamp','deprivation')]

eladmd <- eladmd[order(deprivation,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(admissions_other)), by=c('timestamp','deprivation')]

eladmd <- eladmd[order(deprivation,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(AE_attendance)), by=c('timestamp','deprivation')]

eladmd <- eladmd[order(deprivation,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(outpatient_attendance)), by=c('timestamp','deprivation')]

eladmd <- eladmd[order(deprivation,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


#------------------------------------------------------------------------
# patients - deprivation

admd <- dat[ admissions_all>0 , list(all_pat = .N), by=c('timestamp','deprivation')]

admd <- admd[order(deprivation,-timestamp)]

write.table(admd, "clipboard", sep="\t",row.names = FALSE)

admd <- dat[ admissions_electives>0 , list(all_pat = .N), by=c('timestamp','deprivation')]

admd <- admd[order(deprivation,-timestamp)]

write.table(admd, "clipboard", sep="\t",row.names = FALSE)

ap <- dat[ admissions_electives>0 ,]

t <- data.table(table(ap$timestamp,ap$deprivation))

t <- t[order(V1,-V2)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ admissions_all>0 ,]

t <- data.table(table(ap$timestamp,ap$deprivation))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)



ap <- dat[ admissions_emergency>0 ,]

t <- data.table(table(ap$timestamp,ap$deprivation))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ admissions_other>0 ,]

t <- data.table(table(ap$timestamp,ap$deprivation))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ AE_attendance>0 ,]

t <- data.table(table(ap$timestamp,ap$deprivation))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ outpatient_attendance>0 ,]

t <- data.table(table(ap$timestamp,ap$deprivation))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)




#----------------------------------------------
# sex - counting events


admd <- dat[ , list(all_adm = sum(admissions_all)), by=c('timestamp','sex')]

admd <- admd[order(sex,-timestamp)]

write.table(admd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(admissions_electives)), by=c('timestamp','sex')]

eladmd <- eladmd[order(sex,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(admissions_emergency)), by=c('timestamp','sex')]

eladmd <- eladmd[order(sex,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(admissions_other)), by=c('timestamp','sex')]

eladmd <- eladmd[order(sex,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(AE_attendance)), by=c('timestamp','sex')]

eladmd <- eladmd[order(sex,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(outpatient_attendance)), by=c('timestamp','sex')]

eladmd <- eladmd[order(sex,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


#-------------------------------------------------
# patients - sex


ap <- dat[ admissions_all>0 ,]

t <- data.table(table(ap$timestamp,ap$sex))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ admissions_electives>0 ,]

t <- data.table(table(ap$timestamp,ap$sex))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)



ap <- dat[ admissions_emergency>0 ,]

t <- data.table(table(ap$timestamp,ap$sex))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ admissions_other>0 ,]

t <- data.table(table(ap$timestamp,ap$sex))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ AE_attendance>0 ,]

t <- data.table(table(ap$timestamp,ap$sex))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ outpatient_attendance>0 ,]

t <- data.table(table(ap$timestamp,ap$sex))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)



#--------------------------------------------------------------
# age - counting events

admd <- dat[ , list(all_adm = sum(admissions_all)), by=c('timestamp','age_group')]

admd <- admd[order(age_group,-timestamp)]

write.table(admd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(admissions_electives)), by=c('timestamp','age_group')]

eladmd <- eladmd[order(age_group,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(admissions_emergency)), by=c('timestamp','age_group')]

eladmd <- eladmd[order(age_group,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(admissions_other)), by=c('timestamp','age_group')]

eladmd <- eladmd[order(age_group,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(AE_attendance)), by=c('timestamp','age_group')]

eladmd <- eladmd[order(age_group,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


eladmd <- dat[ , list(all_adm = sum(outpatient_attendance)), by=c('timestamp','age_group')]

eladmd <- eladmd[order(age_group,-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)


#------------------------------------------------------------------------
# patients - age group

ap <- dat[ admissions_all>0 ,]

t <- data.table(table(ap$timestamp,ap$age_group))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ admissions_electives>0 ,]

t <- data.table(table(ap$timestamp,ap$age_group))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)



ap <- dat[ admissions_emergency>0 ,]

t <- data.table(table(ap$timestamp,ap$age_group))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ admissions_other>0 ,]

t <- data.table(table(ap$timestamp,ap$age_group))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ AE_attendance>0 ,]

t <- data.table(table(ap$timestamp,ap$age_group))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ outpatient_attendance>0 ,]

t <- data.table(table(ap$timestamp,ap$age_group))

t <- t[order(V2,-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)



#----------------------------------------------------------------------------------
# this is the code for the tables containing the *overall* counts
#-------------------------------------------------------------------
# events - overall


admd <- dat[ , list(all_adm = sum(admissions_all)), by=c('timestamp')]

admd <- admd[order(-timestamp)]

write.table(admd, "clipboard", sep="\t",row.names = FALSE)


admd <- dat[ , list(all_adm = sum(admissions_electives)), by=c('timestamp')]

admd <- admd[order(-timestamp)]

write.table(admd, "clipboard", sep="\t",row.names = FALSE)

admd <- dat[ , list(all_adm = sum(admissions_emergency)), by=c('timestamp')]

admd <- admd[order(-timestamp)]

write.table(admd, "clipboard", sep="\t",row.names = FALSE)


admd <- dat[ , list(all_adm = sum(admissions_other)), by=c('timestamp')]

admd <- admd[order(-timestamp)]

write.table(admd, "clipboard", sep="\t",row.names = FALSE)


admd <- dat[ , list(all_adm = sum(AE_attendance)), by=c('timestamp')]

admd <- admd[order(-timestamp)]

write.table(admd, "clipboard", sep="\t",row.names = FALSE)



eladmd <- dat[ , list(all_adm = sum(outpatient_attendance)), by=c('timestamp')]

eladmd <- eladmd[order(-timestamp)]

write.table(eladmd, "clipboard", sep="\t",row.names = FALSE)

#-------------------------------------------------------
# patients overall


ap <- dat[ admissions_all>0 ,]

t <- data.table(table(ap$timestamp))

t <- t[order(-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)


ap <- dat[ admissions_electives>0 ,]

t <- data.table(table(ap$timestamp))

t <- t[order(-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)

ap <- dat[ admissions_emergency>0 ,]

t <- data.table(table(ap$timestamp))

t <- t[order(-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)

ap <- dat[ admissions_other>0 ,]

t <- data.table(table(ap$timestamp))

t <- t[order(-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)

ap <- dat[ AE_attendance>0 ,]

t <- data.table(table(ap$timestamp))

t <- t[order(-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)

ap <- dat[ outpatient_attendance>0 ,]

t <- data.table(table(ap$timestamp))

t <- t[order(-V1)]

write.table(t, "clipboard", sep="\t",row.names = FALSE)

