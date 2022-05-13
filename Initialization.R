library(tidyverse)
library(psych)
library(olsrr)
library(readxl)
library(dplyr)

load("WVS_Cross-National_Wave7.RData") ## Loads WVS Data
View(WVS_Data)

## Converting all non-response codes to NA
WVS_Data[WVS_Data == -1] = NA
WVS_Data[WVS_Data == -2] = NA
WVS_Data[WVS_Data == -4] = NA
WVS_Data[WVS_Data == -5] = NA

## Natirang likert variables:
## Q199 -- Political Interest and Participation (1 - very interested, 4-not at all interested)
## Q251 -- Perception of Democratic Governance (1-not at all, 10-completely democratic)
## Q176 -- Moral Absolutism (1-Relativism, 10-Absolutism)
## Q57 -- Social Trust (dichotomized na originally: 1- most people can be trusted, 2- need to be careful)
## Q131 -- Perception of Security (1-most secure, 4-not at all secure)
## Q198 -- Right of Government to Surveillance (1-agree, 4-disagree)

## Dichotomizing likert variables and getting their mean
WVS_Data_By_Country <- WVS_Data %>% mutate_at(vars(c(Q199, Q131, Q198)), funs(case_when( . == 1 | . == 2 ~ 1, . == 3 | . == 4 ~ 0))) %>% mutate_at(vars(c(Q251, Q176)),
    funs(case_when( . == 10 | . == 9 | . == 8 | . == 7 | . == 6 ~ 1, . == 5 | . == 4 | . == 3 | . == 2 | . == 1 ~ 0))) %>% mutate(Q57=case_when(Q57 == 1 ~ 1, Q57 == 2 ~ 0)) %>% group_by(B_COUNTRY_ALPHA) %>% summarize(across(c(Q199, Q251, Q176, Q57, Q131, Q198), ~ mean(.x, na.rm = TRUE)))

CPI <- read_xlsx("EXCEL - CORRUPTION PERCEPTIONS INDEX 2021 (GLOBAL RESULTS AND TRENDS).xlsx", skip = 2) 
CPI <- CPI %>% select(2, 4)

urban <- read_csv("API_SP.URB.TOTL.IN.ZS_DS2_en_csv_v2_4030202.csv", skip = 4) %>% select(c(2, 65)) %>% rename(urban_2020 = '2020')
GDP_capita <- read_csv("API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_4022086.csv", skip = 4) %>% select(c(2, 64)) %>% rename(gdp_capita_2019 = '2019') ## Used 2019 data since 2020 is weird for GDP for obvious reasons
GDP_growth <- read_csv("API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_4019069.csv", skip = 4) %>% select(c(2, 64)) %>% rename(gdp_growth_2019 = '2019') ## Used 2019 data since 2020 is weird for GDP for obvious reasons
population_growth <- read_csv("API_SP.POP.GROW_DS2_en_csv_v2_4028432.csv", skip = 4) %>% select(c(2, 64))  %>% rename(pop_growth_2019 = '2019') ## Used 2019 data since 2020 is weird for population for obvious reasons
education <- read_csv("Education index.csv") %>% select(c(3,4)) %>% rename(education_2019 = "2019")
## GDP per Capita is in PPP, current international dollars

literacy <- read_excel("API_SE.ADT.LITR.ZS_DS2_en_excel_v2_4027578.xls", skip = 3) %>% select(c(2, 67)) %>% rename(literacy_latest = 'LATEST') ## I modified the data file to have a column for latest literacy rate, will merge with WVS Literacy for missing data

non_WVS_data <- urban %>% 
  full_join(GDP_capita, by ="Country Code") %>% 
  full_join(GDP_growth, by="Country Code") %>% 
  full_join(population_growth, by="Country Code")
non_WVS_data[149,1] <- "MOR" # Morocco is coded as MAR sa non wvs data

non_WVS_data <- non_WVS_data %>% full_join(education, by = "Country Code") 

CPI[91, 1] <- "MOR"

dataset <- left_join(WVS_Data_By_Country, non_WVS_data, by=c("B_COUNTRY_ALPHA" = "Country Code")) %>%
  inner_join(CPI, by=c("B_COUNTRY_ALPHA" = "ISO3"))

dataset <- dataset %>%
  rename(political_interest = Q199,
         democratic_governance = Q251,
         moral_absolutism = Q176,
         social_trust = Q57,
         security_perception = Q131,
         government_surveillance = Q198)

dataset[49, 8] <- 78.9 # Taiwan urban population from https://www.worldometers.info/world-population/taiwan-population/
dataset[49, 9] <- 53424.38 # Taiwan GDP per Capita, 2019
dataset[49, 10] <- 2.964 # Taiwan GDP Growth from IMF Database, based on gov't data
dataset[49, 11] <- 1 - 23588932/23561236 # Taiwan population growth 2020 based on offical gov't data
dataset[49, 12] <- ((16.5/18) + (12.3/15))/2  # Taiwan education index 2019 calculated manually using formula in Wikipedia: https://en.wikipedia.org/wiki/Education_Index#cite_note-2, and data from here: https://link.springer.com/chapter/10.1007/978-3-030-93951-9_4
dataset[52, 9] <- 7342.59 # IMF Estimate of GDP Per Capita for Venezuela, 2019
dataset[52, 10] <- -35 # IMF Estimate for Venezuela, 2019

dataset <- dataset[-14, ] # Remove Egypt due to missing data

View(dataset)

## To do:
## DONE -> 1. Decide how to recode WVS-based likert variables
## DONE -> 2. Figure out a way to eliminate NAs for WVS Data, e.g. omit the variable in creation of recoded variables?
## DONE -> 3. Write the code to recode WVS based likert variables, modifying the code above
## DONE -> 4. Figure out a way to eliminate NAs from non-WVS data, e.g. use latest year data (even if not same year data), if we delete countries baka maubos na ang data
## DONE -> 5. Write the code to correct NAs in non-WVS data
## 6. Do descriptive analysis code
## 7. Do initialization of full model using lm formula
## 8. Conduct variable selection
## 9. Do diagnostic checking (normality, linearity, heteroskedasticity, autocorrelation, correlation between indep var, outliers, influential observations)
## 10. Rerun final model for analysis using lm formula, interpret results
## Check zeros, moved deadline to June 8 or 10?

## Possible reference paper: https://drive.google.com/drive/u/0/folders/0BzMkFhdwCZrpbDBOSm5iRHBWeEk?resourcekey=0-h2EmqZujzhhsSmhaFkSQzg
