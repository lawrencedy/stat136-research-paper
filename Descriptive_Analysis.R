library(tidyverse)
library(psych)
library(sjstats)

## You may use WVS_Data_Individual dataset to get summaries for individual questions

## To do:
## 1. Mean, SD, Variance, SE, trimmed mean, quantiles, skew, kurtosis etc. of all variables (response and independent)
## 2. Proportion of responses for each option for WVS Data
## 3. Boxplots for non-WVS independent variables and the CPI response variable, note outliers
## 4. Correlation Matrix of All Variables
## 5. Scatterplot of All Independent Variables vs. CPI

## Mean, SD, Variance, SE, ...
wvs_desc_stat <- data.frame(summary(WVS_Data_Individual))
wvs_desc_stat2 <- describe(WVS_Data_Individual)
non_wvs_desc_stat <- data.frame(summary(dataset[, c(1, 8:13)]))
non_wvs_desc_stat2 <- describe(dataset[, c(1, 8:13)])

quantiles <-
  data.frame(matrix(
    c(2, 2.662, 5, 8, 1, 2, 2, 2, 1, 3, 2, 4,
      54.19, 81.42, 9019, 29967, 1.145, 4.601, 0.3547, 1.4698, 0.63, 0.827, 28, 49),
    nrow = 12,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      c("political_interest", "democratic_governance", "moral_absolutism",
        "social_trust", "security_perception", "government_surveillance",
        "urban_2020", "gdp_capita_2019", "gdp_growth_2019", "pop_growth_2019",
        "education_2019", "CPI score 2021"), c("1st Qu", "3rd Qu"))))

partial_desc_stat <- rbind(wvs_desc_stat2[-1,], non_wvs_desc_stat2[-1,])
rownames(partial_desc_stat)[c(1:6, 12)] <- c("political_interest", "democratic_governance",
                                             "moral_absolutism", "social_trust", "security_perception",
                                             "government_surveillance", "CPI score 2021")

desc_stat <- cbind(partial_desc_stat, quantiles)
colnames(desc_stat)[14:15] <- c("1st Qu", "3rd Qu")

# Proportion of responses for each option for WVS Data
political_interest <- WVS_Data_Individual %>% group_by(B_COUNTRY_ALPHA) %>% 
  prop(Q199 == 1, Q199 == 2, Q199 == 3, Q199 == 4)
democratic_governance <- WVS_Data_Individual %>% group_by(B_COUNTRY_ALPHA) %>% 
  prop(Q251 == 1, Q251 == 2, Q251 == 3, Q251 == 4, Q251 == 5, Q251 == 6,
       Q251 == 7, Q251 == 8, Q251 == 9, Q251 == 10)
moral_absolutism <- WVS_Data_Individual %>% group_by(B_COUNTRY_ALPHA) %>%
  prop(Q175 == 1, Q175 == 2, Q175 == 3, Q175 == 4, Q175 == 5, Q175 == 6,
       Q175 == 7, Q175 == 8, Q175 == 9, Q175 == 10)
social_trust <- WVS_Data_Individual %>% group_by(B_COUNTRY_ALPHA) %>%
  prop(Q57 == 1, Q57 == 2)
security_perception <- WVS_Data_Individual %>% group_by(B_COUNTRY_ALPHA) %>%
  prop(Q131 == 1, Q131 == 2, Q131 == 3, Q131 == 4)
government_surveillance <- WVS_Data_Individual %>% group_by(B_COUNTRY_ALPHA) %>%
  prop(Q198 == 1, Q198 == 2, Q198 == 3, Q198 == 4)

## Boxplots for non-WVS independent variables and the CPI response variable, note outliers
# urban_2020
boxplot(dataset$urban_2020, main = "Boxplot of urban_2020")

# gdp_capita_2019
boxplot(dataset$gdp_capita_2019, main = "Boxplot of gdp_capita_2019")
outliers_gdp_capita_2019 <- boxplot.stats(dataset$gdp_capita_2019)$out
outliers_ind_gdp_capita_2019 <- which(dataset$gdp_capita_2019 %in% c(outliers_gdp_capita_2019)) # Hong Kong, Singapore, USA are outliers

# gdp_growth_2019
boxplot(dataset$gdp_growth_2019, main = "Boxplot of gdp_growth_2019")
outliers_gdp_growth_2019 <- boxplot.stats(dataset$gdp_growth_2019)$out
outliers_ind_gdp_growth_2019 <- which(dataset$gdp_growth_2019 %in% c(outliers_gdp_growth_2019)) # Iran, Lebanon, Venezuela, Zimbabwe

# pop_growth_2019
boxplot(dataset$pop_growth_2019, main = "Boxplot of pop_growth_2019")

# education_2019
boxplot(dataset$education_2019, main = "Boxplot of education_2019")

# CPI score 2021
boxplot(dataset$CPI, main = "Boxplot of CPI score 2021")
outliers_cpi_2021 <- boxplot.stats(dataset$CPI)$out
outliers_ind_cpi_2021 <- which(dataset$CPI %in% c(outliers_cpi_2021)) # New Zealand, Singapore

## Correlation Matrix of All Variables
cor(dataset[, c(3:5, 7:11, 13)], method = "pearson")

## Scatterplot of All Independent Variables vs CPI
par(mfrow = c(3,3))
plot(dataset$CPI ~ dataset$gdp_capita_2019, xlab = "GDP per Capita (2019)", ylab = "CPI")
plot(dataset$CPI ~ dataset$gdp_growth_2019, xlab = "GDP Growth (2019)", ylab = "CPI")
plot(dataset$CPI ~ dataset$pop_growth_2019, xlab = "Population Growth (2019)", ylab = "CPI")
plot(dataset$CPI ~ dataset$urban_2020, xlab = "Urban Dwellers (2019)", ylab = "CPI")
plot(dataset$CPI ~ dataset$democratic_governance, xlab = "Democratic Governance", ylab = "CPI")
plot(dataset$CPI ~ dataset$government_surveillance, xlab = "Government Surveillance", ylab = "CPI")
plot(dataset$CPI ~ dataset$moral_absolutism, xlab = "Moral Absolutism", ylab = "CPI")
plot(dataset$CPI ~ dataset$social_trust, xlab = "Social Trust", ylab = "CPI")
