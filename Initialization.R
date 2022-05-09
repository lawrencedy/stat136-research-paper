library(tidyverse)
library(psych)
library(olsrr)
library(readxl)

load("WVS_Cross-National_Wave7.RData") ## Loads WVS Data
View(WVS_Data)

## Converting all non-response codes to NA
WVS_Data[WVS_Data == -1] = NA
WVS_Data[WVS_Data == -2] = NA
WVS_Data[WVS_Data == -4] = NA
WVS_Data[WVS_Data == -5] = NA


## Can't figure out how to summarize into the variables we have, kasi pag % share how do we combine across questions??

CPI <- read_xlsx("EXCEL - CORRUPTION PERCEPTIONS INDEX 2021 (GLOBAL RESULTS AND TRENDS).xlsx", skip = 2) 
CPI <- CPI %>% select(2, 4)

urban <- read_csv("API_SP.URB.TOTL.IN.ZS_DS2_en_csv_v2_4030202.csv", skip = 4) %>% select(c(2, 65)) %>% rename(urban_2020 = '2020')
GDP_capita <- read_csv("API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_4022086.csv", skip = 4) %>% select(c(2, 64)) %>% rename(gdp_capita_2019 = '2019') ## Used 2019 data since 2020 is weird for GDP for obvious reasons
GDP_growth <- read_csv("API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_4019069.csv", skip = 4) %>% select(c(2, 64)) %>% rename(gdp_growth_2019 = '2019') ## Used 2019 data since 2020 is weird for GDP for obvious reasons
population_growth <- read_csv("API_SP.POP.GROW_DS2_en_csv_v2_4028432.csv", skip = 4) %>% select(c(2, 64))  %>% rename(pop_growth_2019 = '2019') ## Used 2019 data since 2020 is weird for population for obvious reasons
literacy <- read_excel("API_SE.ADT.LITR.ZS_DS2_en_excel_v2_4027578.xls", skip = 3) %>% select(c(2, 67)) %>% rename(literacy_latest = 'LATEST') ## I modified the data file to have a column for latest literacy rate, will merge with WVS Literacy for missing data

non_WVS_data <- urban %>% 
  full_join(GDP_capita, by ="Country Code") %>% 
  full_join(GDP_growth, by="Country Code") %>% 
  full_join(population_growth, by="Country Code") %>% 
  full_join(literacy, by="Country Code") %>%
  full_join(CPI, by=c("Country Code" = "ISO3"))
non_WVS_data[149,1] <- "MOR" # Morocco is coded as MAR sa non wvs data

dataset <- left_join(WVS_Data_By_Country, non_WVS_data, by=c("B_COUNTRY_ALPHA" = "Country Code"))
## modify WVS_Data_By_Country to actual form of WVS variables for analysis

View(dataset)
## dataset contains all necessary variables now BUT the WVS variables are not in dichotomous format yet, and have to deal with the NAs

## To do:
## 1. Decide how to recode WVS-based likert variables
## 2. Figure out a way to eliminate NAs for WVS Data, e.g. omit the variable in creation of recoded variables?
## 3. Write the code to recode WVS based likert variables, modifying the code above
## 4. Figure out a way to eliminate NAs from non-WVS data, e.g. use latest year data (even if not same year data), if we delete countries baka maubos na ang data
## 5. Write the code to correct NAs in non-WVS data
## 6. Do descriptive analysis code
## 7. Do initialization of full model using lm formula
## 8. Conduct variable selection
## 9. Do diagnostic checking (normality, linearity, heteroskedasticity, autocorrelation, correlation between indep var, outliers, influential observations)
## 10. Rerun final model for analysis using lm formula, interpret results

## Possible reference paper: https://drive.google.com/drive/u/0/folders/0BzMkFhdwCZrpbDBOSm5iRHBWeEk?resourcekey=0-h2EmqZujzhhsSmhaFkSQzg



