#################################################### 
#### Insert this code before your analysis code #### 
#################################################### 

#### Load packages
library(stringr)
library(purrr)
library(dplyr)
library(DescTools)

### Create empty list
ICD10_chapter_codes <- vector(mode = "list", length = 22)

### Names of ICD-10 chapters
ICD10_chapter_names <- c("1 - Certain infectious and parasitic diseases",
"2 - Neoplasms",
"3 - Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
"4 - Endocrine, nutritional and metabolic diseases",
"5 - Mental, Behavioral and Neurodevelopmental disorders",
"6 - Diseases of the nervous system",
"7 - Diseases of the eye and adnexa",
"8 - Diseases of the ear and mastoid process",
"9 - Diseases of the circulatory system",
"10 - Diseases of the respiratory system",
"11 - Diseases of the digestive system",
"12 - Diseases of the skin and subcutaneous tissue",
"13 - Diseases of the musculoskeletal system and connective tissue",
"14 - Diseases of the genitourinary system",
"15 - Pregnancy, childbirth and the puerperium",
"16 - Certain conditions originating in the perinatal period",
"17 - Congenital malformations, deformations and chromosomal abnormalities",
"18 - Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
"19 - Injury, poisoning and certain other consequences of external causes",
"20 - External causes of morbidity",
"21 - Factors influencing health status and contact with health services",
"22 - COVID-19")

### Assign names to list
names(ICD10_chapter_codes) <- ICD10_chapter_names

### Assign ICD-10 code prefixes to list
ICD10_chapter_codes[[1]] <- c(paste0("A",sprintf("%02d", 0:99)),
                              paste0("B",sprintf("%02d", 0:99)))
ICD10_chapter_codes[[2]] <- c(paste0("C",sprintf("%02d", 0:99)),
                              paste0("D",sprintf("%02d", 0:49)),
                              "C7A","C7B","C4A","D3A")
ICD10_chapter_codes[[3]] <- paste0("D",sprintf("%02d", 50:89))
ICD10_chapter_codes[[4]] <- paste0("E",sprintf("%02d", 0:90))
ICD10_chapter_codes[[5]] <- paste0("F",sprintf("%02d", 0:99))
ICD10_chapter_codes[[6]] <- paste0("G",sprintf("%02d", 0:99))
ICD10_chapter_codes[[7]] <- paste0("H",sprintf("%02d", 0:59))
ICD10_chapter_codes[[8]] <- paste0("H",sprintf("%02d", 60:95))
ICD10_chapter_codes[[9]] <- paste0("I",sprintf("%02d", 0:99))
ICD10_chapter_codes[[10]] <- paste0("J",sprintf("%02d", 0:99))
ICD10_chapter_codes[[11]] <- paste0("K",sprintf("%02d", 0:95))
ICD10_chapter_codes[[12]] <- paste0("L",sprintf("%02d", 0:99))
ICD10_chapter_codes[[13]] <- c(paste0("M",sprintf("%02d", 0:99))
                               ,"M1A")
ICD10_chapter_codes[[14]] <- paste0("N",sprintf("%02d", 0:99))
ICD10_chapter_codes[[15]] <- c(paste0("O",sprintf("%02d", 0:99)),
                                    "O9A")
ICD10_chapter_codes[[16]] <- paste0("P",sprintf("%02d", 0:96))
ICD10_chapter_codes[[17]] <- paste0("Q",sprintf("%02d", 0:99))
ICD10_chapter_codes[[18]] <- paste0("R",sprintf("%02d", 0:99))
ICD10_chapter_codes[[19]] <- c(paste0("S",sprintf("%02d", 0:99)),
                               paste0("T",sprintf("%02d", 0:98)))
ICD10_chapter_codes[[20]] <- c(paste0("V",sprintf("%02d", 0:99)),
                               paste0("W",sprintf("%02d", 0:99)),
                               paste0("X",sprintf("%02d", 0:99)),
                               paste0("Y",sprintf("%02d", 0:99)))
ICD10_chapter_codes[[21]] <- c(paste0("Z",sprintf("%02d", 0:99)),
                                    "Z3A")
ICD10_chapter_codes[[22]] <- c("U071","U072")

### Function that returns ICD-10 chapter

icd10_to_chapter <- function(inputcode){
  
  #Remove all punctuation from ICD-10 code field, and convert to ASCII format and uppercase
  inputcode_nopunct <- stringr::str_replace_all(string = inputcode, pattern = "[^[:alnum:]]", replacement = "") %>%
    iconv(., from = "UTF-8", to = "ASCII") %>%
    toupper(.)
  
  #Get first three from ICD-10 code
  first_three_characters <- substr(inputcode_nopunct, 1, 3)
  
  #Check if first three characters match any of the chapters
  #Starting with COVID codes, which are a special case because it is based on four characters
  
  if(inputcode_nopunct %in% c("U071","U072")){
    chapter_result <- "22 - COVID-19"
  } else{
    
  #If there is no match with either COVID codes, look for matches among the 21 ICD-10 chapters
    output_vector <- rep(NA,length(ICD10_chapter_codes))
    for (j in 1:length(ICD10_chapter_codes)){
      output_vector[j] <- (first_three_characters %in% ICD10_chapter_codes[[j]])
    }
    
  #Return name of chapter that matches ICD-10 code or notify if there is no match
    chapter_result <- ifelse(sum(output_vector)==0,
           "No match with ICD-10 chapters",
           names(ICD10_chapter_codes)[which(output_vector==TRUE)[1]])
  }
  
return(chapter_result)
  
}

#################################################################
#### Example: applying the function to dummy admissions data #### 
#################################################################

### Dummy admissions data with 10 patients, and each patient has 2 admissions

elective_admissions_covid_period <- data.frame(
  patient_id=rep(c(1:10),2),
  admission_spell=c(rep(1,10),rep(2,10)),
  primary_diagnostic=c("U07.2","p70.8","abcd00","H15.0","h40.2",
                       "K35.8","K70.0","U07.1","asthma","J01.1",
                       "Q35.3","H15.0","S79.8","S00.4","K70.0",
                       "H40.2","P70.8","C80.0","C74.0","H15.0")) %>%
  arrange(.,patient_id,admission_spell)

### Apply function to each admission and produce summary statistics

summary_ICD_chapters <- elective_admissions_covid_period %>%
  select(.,primary_diagnostic) %>%
  unlist(.) %>%
  map(icd10_to_chapter) %>%
  flatten_chr(.) %>%
  DescTools::Freq(.) %>%
  select(.,level,freq)