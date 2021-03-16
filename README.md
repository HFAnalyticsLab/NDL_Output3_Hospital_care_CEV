<img src="ndlbanner.png" width="405" height="96">

# Networked Data Lab: Hospital care for the clinically extremely vulnerable before and during COVID-19

#### Project Status: In progess

## Project Description

- This Networked Data Lab analysis focusses on clinically extremely vulnerable (CEV) people, also known as the shielding population - the group of people most at risk of becoming seriously ill from COVID-19 . This group were asked to not leave their homes and to minimise all face-to-face contact up until the end of July 2020 in most of the UK. Whilst the shielding guidance was paused over Summer, people were once again asked to minimise their contact with others from November.

- We are using a federated approach to data analysis and each partner will be contributing the same descriptive analysis based on their local population. These results will then be analysed and aggregated where necessary.

- Our full [statistical analysis plan](https://www.health.org.uk/sites/default/files/2020-12/ndl_statistical_analysis_plan_-_descriptive_analysis_of_cev_people_during_covid-19_0.pdf) for this project is available on our website.

- This output describes the changes in secondary health care use for CEV people during the period of shielding between March and July 2020. We also examine the direct impacts of COVID-19 on this group across each local area. These findings are useful in building an understanding of the impacts from both a patient and a service perspective.

## Partners

The following partners have taken part in this analysis and contributed results:

- The Aberdeen Centre for Health Data Science (ACHDS) which includes NHS Grampian and the University of Aberdeen
- Public Health Wales, NHS Wales Informatics Service (NWIS), Swansea University (SAIL Databank) and Social Care Wales (SCW)
- Imperial College Health Partners (ICHP), Institute of Global Health Innovation (IGHI), Imperial College London (ICL), and North West London CCGs
- Liverpool CCG, Healthy Wirral Partnership and Citizens Advice Bureau
- Leeds CCG and Leeds City Council    

## Data sources

This analysis relies on the following data sources, which have been accessed by NDL partners.

- The Shielded Patient List (SPL).
- Secondary health care records: hospital admissions (elective, emergency and other), outpatient attendance and A&E attendances.
- Patient demographics databases.
- External Open data sources linked to a patients’ LSOA (or other geography) of residence. These are the 2019 [English](https://data-communities.opendata.arcgis.com/datasets/d4b79be994ac4820ad44e10ded313df3_0
)/[Welsh](https://gov.wales/sites/default/files/statistics-and-research/2019-11/welsh-index-multiple-deprivation-2019-index-and-domain-ranks-by-small-area.ods
)/[Scottish](https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-data-zone-look-up-file/documents/scottish-index-of-multiple-deprivation-data-zone-look-up/scottish-index-of-multiple-deprivation-data-zone-look-up/govscot%3Adocument/SIMD%2B2020v2%2B-%2Bdatazone%2Blookup.xlsx) Index of Multiple Deprivation quintiles and [English](https://data.gov.uk/dataset/b1165cea-2655-4cf7-bf22-dfbd3cdeb242/rural-urban-classification-2011-of-lower-layer-super-output-areas-in-england-and-wales)/[Welsh](https://data.gov.uk/dataset/b1165cea-2655-4cf7-bf22-dfbd3cdeb242/rural-urban-classification-2011-of-lower-layer-super-output-areas-in-england-and-wales)/[Scottish](https://www.opendata.nhs.scot/fa_IR/dataset/urban-rural-classification) urban/rural indicators based on the 2011 Census.

## How does it work?

The repository contains the following folders:

- **Import data:** this contains the R codes used to read in and process individuals outputs from each of the five Networked Data Lab partners, and includes the code used to create our main outcome variables.
- **Analysis and QA:** this contains the R codes used by each of the Networked Data Lab partners to produce their individual outputs and covers both data processing and analysis. The markdown file '3. QA markdown.Rmd' contains the [key statistics](https://htmlpreview.github.io/?https://github.com/HFAnalyticsLab/NDL_Output3_Hospital_care_CEV/blob/main/Analysis%20and%20QA/Results%20for%20publication%20and%20QA/3.-QA-markdown.html) which were then reported in our long chart.
- **Data release:** this contains the [open data release](https://github.com/HFAnalyticsLab/NDL_Output1_Demographics/blob/main/Outputs/Networked-Data-Lab-Characteristics-of-CEV-people-GitHub.xlsx) with the summary statistics produced by all partners (after applying statistical disclosure control methods).

### Requirements

These scripts were written in R version 4.0.2 and RStudio Version 1.1.383. 

## Authors

* Karen Hodgson, The Health Foundation - [Twitter](https://twitter.com/KarenHodgePodge) - [GitHub](https://github.com/KarenHodgson)
* Sebastien Peytrignet, The Health Foundation - [Twitter](https://twitter.com/SebastienPeytr2) - [GitHub](https://github.com/sg-peytrignet)
* Jessica Butler, Aberdeen Centre for Health Data Science (ACHDS) - [Twitter](https://twitter.com/jessbutler284)
* Alisha Davies, Public Health Wales - [Twitter](https://twitter.com/AlishaDavies1)
* Sara Sekelj, Imperial College Health Partners - [Twitter](https://twitter.com/sekeljsara)
* Simon Chambers, Wirral University Teaching Hospital NHS Foundation Trust
* Frank Wood, NHS Leeds CCG

## License

This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/NDL_Output3_Hospital_care_CEV/blob/main/LICENSE).
