library(olsrr)
library(car)
library(pls)
View(dataset)

## Initial Model
initial_model <- lm(CPI ~ factor(Region) + political_interest + democratic_governance + moral_absolutism + social_trust + security_perception + government_surveillance + urban_2020 + gdp_capita_2019 + gdp_growth_2019 + pop_growth_2019 + education_2019, data = dataset)
summary(initial_model)
anova(initial_model)
ols_regress(initial_model) 
confint(initial_model, level = 0.95)

ols_plot_resid_hist(initial_model)

par(mfrow=c(2,2))
plot(initial_model)

## Variable Selection
# Forward selection using p-values
forward_selection_p <- ols_step_forward_p(initial_model, penter=.05, details = TRUE)
forward_selection_p

# Forward selection using AIC
forward_selection_AIC <- ols_step_forward_aic(initial_model, details = TRUE)
forward_selection_AIC
plot(forward_selection_AIC)

# Backward selection using p-values
backward_selection_p <- ols_step_backward_p(initial_model, prem=.05, details = TRUE)
backward_selection_p

# Backward selection using AIC
backward_selection_AIC <- ols_step_backward_aic(initial_model, details = TRUE)
backward_selection_AIC
plot(backward_selection_AIC)

# Stepwise selection using p-values
stepwise_selection_p <- ols_step_both_p(initial_model, pent=.05, prem=.05, details = TRUE)
stepwise_selection_p

# Stepwise selection using AIC
stepwise_selection_AIC <- ols_step_both_aic(initial_model, details = TRUE)
stepwise_selection_AIC
plot(stepwise_selection_AIC)

# All possible subsets
all_subsets <- ols_step_all_possible(initial_model, details = TRUE)
View(all_subsets)
plot(all_subsets)

all_subsets_regression <- ols_step_best_subset(initial_model)
all_subsets_regression

## Candidate Model Selection
# Model 1
model1 <- lm(CPI ~ gdp_capita_2019 + democratic_governance + moral_absolutism + social_trust, data = dataset)
summary(model1)
anova(model1)
ols_regress(model1) 

par(mfrow=c(2,2))
plot(model1)

# Model 2
model2 <- lm(CPI ~ gdp_capita_2019 + democratic_governance + moral_absolutism + social_trust + urban_2020, data = dataset)
summary(model2)
anova(model2)
ols_regress(model2) 

par(mfrow=c(2,2))
plot(model2)

# Model 3
model3 <- lm(CPI ~ gdp_capita_2019 + democratic_governance + moral_absolutism + social_trust + urban_2020 + gdp_growth_2019, data = dataset)
summary(model3)
anova(model3)
ols_regress(model3) 

par(mfrow=c(2,2))
plot(model3)

# Model 4
model4 <- lm(CPI ~ gdp_capita_2019 + democratic_governance + moral_absolutism + social_trust + urban_2020 + gdp_growth_2019 + government_surveillance + pop_growth_2019, data = dataset)
summary(model4)
anova(model4)
ols_regress(model4) 

par(mfrow=c(2,2))
plot(model4)

# Model 5
model5 <- lm(CPI ~ gdp_capita_2019 + democratic_governance + moral_absolutism + social_trust + urban_2020 + gdp_growth_2019 + government_surveillance + pop_growth_2019 + education_2019, data = dataset)
summary(model5)
anova(model5)
ols_regress(model5) 

par(mfrow=c(2,2))
plot(model5)

## Comparing Models
F_test12 <- anova(model1, model2)$`Pr(>F)`[2]  ## p = 0.167241
F_test12

F_test13 <- anova(model1, model3)$`Pr(>F)`[2]  ## p = 0.03850383
F_test2

F_test14 <- anova(model1, model4)$`Pr(>F)`[2]  ## p = 0.03850383
F_test14

F_test15 <- anova(model1, model5)$`Pr(>F)`[2]  ## p = 0.02591362
F_test15

F_test23 <- anova(model2, model3)$`Pr(>F)`[2]  ## p = 0.03239284
F_test23

F_test24 <- anova(model2, model4)$`Pr(>F)`[2]  ## p = 0.01540809
F_test24

F_test25 <- anova(model2, model5)$`Pr(>F)`[2]  ## p = 0.02887449
F_test25

F_test34 <- anova(model3, model4)$`Pr(>F)`[2]  ## p = 0.05321044
F_test34

F_test35 <- anova(model3, model5)$`Pr(>F)`[2]  ## p = 0.09745208
F_test35

F_test45 <- anova(model4, model5)$`Pr(>F)`[2]  ## p = 0.479583
F_test45

# Let us choose model 4. (subject to change)
reduced_model <- model4
summary(reduced_model)
anova(reduced_model)
confint(reduced_model, level = 0.95)

## Diagnostic Analysis
ols_plot_resid_box(reduced_model)
## Not 100% normal but still roughly bell shaped

ols_plot_obs_fit(reduced_model)
## Close to expected fit

par(mfrow=c(1,1))
plot(reduced_model)
ols_plot_comp_plus_resid(reduced_model)
## No apparent deviation from Linearity
## Some minor deviation from diagonal line in QQ plot
## Scale-location plot seems somewhat problematic for heteroskedasticity
## Observations 9, 36, 42 appear to be influential

# Linearity
ols_plot_resid_fit(reduced_model)
residualPlots(reduced_model)
crPlots(reduced_model)
## None of the variables appear to be nonnormal, they roughly follow the dotted line

# Test for Heteroskedasticity
ols_plot_resid_fit(reduced_model)
ols_test_breusch_pagan(reduced_model) ## p = 0.144768 -> homoskedastic

par(mfrow=c(2,4))
plot(dataset$democratic_governance, resid(reduced_model))
plot(dataset$gdp_capita_2019, resid(reduced_model))
plot(dataset$moral_absolutism, resid(reduced_model))
plot(dataset$social_trust, resid(reduced_model))
plot(dataset$pop_growth_2019, resid(reduced_model))
plot(dataset$government_surveillance, resid(reduced_model))
plot(dataset$urban_2020, resid(reduced_model))
plot(dataset$gdp_growth_2019, resid(reduced_model))
## No obvious heteroskedasticity

# Test for Normality
ols_plot_resid_qq(reduced_model)
ols_plot_resid_hist(reduced_model)
ols_test_normality(reduced_model) ## 3 of 4 tests have p > 0.05, can assume normality

# Test for Autocorrelation
durbinWatsonTest(reduced_model)  ## p = 0.7, no significant autocorrelation

# Checks for Multicollinearity
ols_regress(gdp_capita_2019 ~ democratic_governance + moral_absolutism + social_trust + urban_2020 + gdp_growth_2019 + government_surveillance + pop_growth_2019, data = dataset) ## R^2 = 0.816 - bad
ols_regress(democratic_governance ~ gdp_capita_2019 + moral_absolutism + social_trust + urban_2020 + gdp_growth_2019 + government_surveillance + pop_growth_2019, data = dataset) ## R^2 = 0.653 - kinda bad
ols_regress(moral_absolutism ~ gdp_capita_2019 + democratic_governance + social_trust + urban_2020 + gdp_growth_2019 + government_surveillance + pop_growth_2019, data = dataset) ## R^2 = 0.120
ols_regress(social_trust ~ gdp_capita_2019 + democratic_governance + moral_absolutism + urban_2020 + gdp_growth_2019 + government_surveillance + pop_growth_2019, data = dataset) ## R^2 = 0.518 - kinda bad
ols_regress(urban_2020 ~ gdp_capita_2019 + democratic_governance + moral_absolutism + social_trust + gdp_growth_2019 + government_surveillance + pop_growth_2019, data = dataset) ## R^2 = 0.540 - kinda bad
ols_regress(gdp_growth_2019 ~ gdp_capita_2019 + democratic_governance + moral_absolutism + social_trust + urban_2020 + government_surveillance + pop_growth_2019, data = dataset) ## R^2 = 0.583 - kinda bad
ols_regress(government_surveillance ~ gdp_capita_2019 + democratic_governance + moral_absolutism + social_trust + urban_2020 + gdp_growth_2019 + pop_growth_2019, data = dataset) ## R^2 = 0.383
ols_regress(pop_growth_2019 ~ gdp_capita_2019 + democratic_governance + moral_absolutism + social_trust + urban_2020 + gdp_growth_2019 + government_surveillance, data = dataset) ## R^2 = 0.410

cor(dataset[, c(3:5, 7:11, 13)], method = "pearson")
pairs(dataset[, c(3:5, 7:11, 13)])

## High correlation between social trust and democratic governance, between GDP per capita and social trust, Urban 2020, gov't surveillance and pop growth

ols_coll_diag(reduced_model)
## Condition Index < 30 so OK (says old paper)

vif(reduced_model)
ols_vif_tol(reduced_model)
## GDP per capita has high VIF but not critical (<10), otherwise seems OK although there is moderate correlations

pcr_reduced_model <- pcr(CPI ~ gdp_capita_2019 + democratic_governance + moral_absolutism + social_trust + urban_2020 + gdp_growth_2019 + government_surveillance + pop_growth_2019, data = dataset, scale = FALSE, validation = "CV")
pcr_reduced_model$projection
pcr_reduced_model$loadings

par(mfrow=c(1,3))
validationplot(pcr_reduced_model, val.type = "RMSEP")
validationplot(pcr_reduced_model, val.type = "MSEP")
validationplot(pcr_reduced_model, val.type = "R2")

summary(pcr_reduced_model)
pcr_reduced_model$coefficients
# Mukhang hindi naman masyado nagbabago ang conclusions with PCR so straight with linear regression is ok, this is just for checking

pcr_mod <- lm(dataset$CPI~pcr_reduced_model$scores)
summary(pcr_mod)

# Outliers and Influential Observations
ols_test_outlier(reduced_model)
## 36 is most outlying but Bonferroni p-value not significant

ols_plot_added_variable(reduced_model)
## Partial regression plot

ols_plot_resid_stand(reduced_model)
## Outliers based on studentized residuals: 8, 14, 36, 41

ols_leverage(reduced_model)
ols_plot_resid_lev(reduced_model)
## Outliers: 8, 14, 36, 41
## High Leverage: 9, 42, 51

## Maybe (?) delete #51 - Venezuela due to excessively high leverage due to -35% GDP growth due to collapse of state
## Maybe (?) delete #42 - Singapore due to being very influential due to very high GDP per capita (PPP)

ols_plot_resid_stud(reduced_model) 
outlierTest(reduced_model)
## #36 - New Zealand has the highest studentized residual, but not significant, NO OUTLIERS!!

ols_plot_cooksd_bar(reduced_model)
## Influential observations based on Cook's Distance: 8, 9, 14, 36, 42

ols_plot_dffits(reduced_model)
## Influential observations based on DFFITS: 8, 9, 14, 21, 36, 42

ols_plot_dfbetas(reduced_model)
## Influential observations on pop. growth based on DFBETAS: 9, 14, 36, 41, 42
## Influential observations on social trust based on DFBETAS: 9, 36, 42
## Influential observations on GDP growth based on DFBETAS: 2, 42, 51
## Influential observations on urban 2020 based on DFBETAS: 8, 14, 21, 36, 42
## Influential observations on gov't surveillance based on DFBETAS: 8, 9, 21, 28, 42
## Influential observations on democratic governance based on DFBETAS: 4, 8, 14, 21, 28
## Influential observations on GDP per capita based on DFBETAS: 8, 9, 21, 42 (!!!)
## Influential observations on moral absolutism based on DFBETAS: 41, 46, 52
## Influential observations on Intercept based on DFBETAS: 2, 8, 14, 21, 30, 36
## Influential observations: 2, 4, 8, 9, 14, 21, 28, 30, 36, 41, 42, 46, 51, 52

## Outliers based on partial residual plot, studentized residuals
## Influential Obs. based on Cook D, DFFITS, DFBETAS, Leverage
## Outliers AND Influential Observations: 8, 14, 36, 9, 42, 51 <- TO CHECK THESE
## Outlier BUT NOT Influential Observation: 41
## Influential Observations but NOT Outliers: 2, 4, 21, 28, 30, 46, 52

## Will do test for those that are Outliers AND Influential Observations only
reduced_model_exoutliers <- lm(CPI ~ gdp_capita_2019 + democratic_governance + moral_absolutism + social_trust + urban_2020 + gdp_growth_2019 + government_surveillance + pop_growth_2019, data = dataset[-c(8, 9, 14, 36, 42, 51), ])
summary(reduced_model_exoutliers)

# For New Zealand
predict(reduced_model_exoutliers, dataset[36, -c(1, 13)], interval="predict")
## PI: [52.28, 80.62] does not contain 88, may need restatement of model

# For Ethiopia
predict(reduced_model_exoutliers, dataset[14, -c(1, 13)], interval="predict")
## PI: [9.95, 37.84] does not contain 39, may need restatement of model

# For Chile
predict(reduced_model_exoutliers, dataset[8, -c(1, 13)], interval="predict")
## PI: [34.59, 61.10] does not contain 67, may need restatement of model

# For China
predict(reduced_model_exoutliers, dataset[9, -c(1, 13)], interval="predict")
## PI: [33.79, 67.60] contains 45, OK

# For Singapore
predict(reduced_model_exoutliers, dataset[42, -c(1, 13)], interval="predict")
## PI: [83.18, 118.34] contains 85, OK

# For Venezuela
predict(reduced_model_exoutliers, dataset[51, -c(1, 13)], interval="predict")
## PI: [-15.41, 36.23] does not contain 14, OK

## To do:
## 1. Figure out what to do when outliers are detected, as above
## 2. Figure out if we are going to do transformations of the data
## 3. Figure out how to know if we need interaction variables
## 4. After figuring out the above, define final model
