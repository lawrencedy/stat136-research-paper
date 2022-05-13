library(olsrr)
View(dataset)

## Initial Model
initial_model <- lm(CPI ~ political_interest + democratic_governance + moral_absolutism + social_trust + security_perception + government_surveillance + urban_2020 + gdp_capita_2019 + gdp_growth_2019 + pop_growth_2019 + education_2019, data = dataset)
summary(initial_model)
anova(initial_model)
ols_regress(initial_model) 

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
all_subsets_regression <- ols_step_all_possible(initial_model, details = TRUE)
View(all_subsets_regression)

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
plot(model4)



F_test1 <- anova(model1, model2)$`Pr(>F)`[2]  ## F = 115.8408

