#####################
### DATA RECODING ###
#####################

## Set working directory
setwd("~/Universiteit Utrecht/2020-2021/Semester 2/ML SEM/MLM Test")

## Load the data 
library(foreign)
dat <- read.spss(file = "dataset.sav", 
                 to.data.frame = TRUE, use.value.labels = FALSE)

## Convert data to long format
library(tidyr) #just copy this code
dat_long <- pivot_longer(data = dat, cols = c(2:9), #columns containing time-varying covariates                
                         names_to = c(".value", "time"), #indicates that component of the name defines the name of the column containing the cell values                   
                         names_pattern = "(anti|read)(.)") #the names of the time-varying covariate without the number

## Make dummies
dat <- dat %>% mutate(male = if_else(male == "female", 0, 1))
dat <- dat %>% mutate(pped = if_else(pped == "no", 0, 1))

## Recode time to include 0
dat_long$time <- as.numeric(dat_long$time) - 1

## Center dependent variable to cluster means to check normality predictor at second level
dat_long$dependent.clusterMean  <- ave(dat_long$dependent, dat_long$id) #use this one in the plot
## Add this for cluster mean centering
dat_long$dependent_clusterMeanC <- dat_long$dependent - dat_long$dependent.clusterMean

## Grand mean centering
mean_var <- mean(dat_long$var)
dat_long$var.grandMC <- dat_long$var - mean_var

## Check normality
library(ggplot2)
ggplot(dat_long,
       aes(x = time, y = anti)) + #fill in what you want to check
  geom_point() +
  geom_smooth(method = "lm",
              aes(color = "linear"),
              se = FALSE) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              aes(color = "quadratic"),
              se = FALSE) #linear and do not see outliers

###########################
### PART I - Normal MLM ###
###########################

## Intercept-only model without random intercept 
M0 <- lm(formula = dependent ~ 1, data = dat)
summary(M0)
summ(M0)
#intercept is the average of the dependent variable when x = 0 if you did not center, of the average subject if you did

## Intercept-only model with random intercept 
M1 <- lmer(formula = dependent ~ 1 + (1 | cluster), data = dat, REML = F)
summary(M1)
summ(M1)
#intercept is the average of the dependent variable when x = 0 if you did not center, of the average subject if you did
#the variance around the intercept reflects the variance of the slopes (name is cluster variable)
#residual variance is the variance of the lowest level

###########
### ICC ###
###########

library(sjstats) #old
library(performance)
performance::icc(model = M1)

## Anova to test is the overall fit of the  model is improved
anova(M1, M0)
#significance means that the overall fit of the model is improved (X^2 (df) = , p = )

## Add level 1 predictors to the model
M2 <- lmer(formula = dependent ~ 1 + level1 + level1+ (1 | cluster), data = dat, REML = F)
summary(M2)
summ(M2)
anova(M2, M1)
#the interpretation of the intercept does not change
## Interpretation of the slopes
#if you centered: the average predicted increase in the dependent variable is slope units, 
#given that the independent variable increases with one unit, controlling for the other variables in the model
#interpretation variance remains the same 

####################
### Calculate R2 ###
####################

#class level(2): (var cluster baseline - var cluster new) / var cluster baseline 
#individual level(1): (var residual baseline - var residual new) / var residual baseline 

## Add level 2 predictors to the model
M3 <- lmer(formula = dependent ~ 1 + level1 + level1 + level2+ (1 | cluster), data = dat, REML = F)
summary(M3)
summ(M3)
anova(M3, M2)
#interpretation remains the same

## Add random slope and covariance
M4 <- lmer(formula = dependent ~ 1 + level1 + level1 + level2 +((level1 + level1) | cluster), data = dat, REML = F)
summary(M4)
summ(M4)
anova(M4, M3)

## Add interaction term
M5 <- lmer(formula = dependent ~ 1 + level1 + level1 + level2 + level1 * level2 (orsomethingelse) + (level1 (+ level1) | cluster), data = dat, REML = F)
summary(M5)
summ(M5)
anova(M5, M4)
## Cross-level explained variance
#use model with random slopes as baseline

## All models in one
library(texreg)
screenreg(list(M0, M1, M2, M3, M4, M5))

###################################
### PART II - Longitudinal MLM ###
##################################

## Model 0 and 1 are the same
#model 1 is still used for the ICC

## Add time to the model
M2 <- lmer(formula = dependent ~ 1 + time +(1 | cluster), data = dat, REML = F)
summary(M2)
summ(M2)
anova(M2, M1)
#baseline model for R2

## Subsequent models are the same

##############################
### PART III - Ordinal MLM ###
##############################

## Intercept-only model without random intercept 
M0 <- glm(dependent ~ 1, data = dat, family = "binomial")
summary(M0)

## Intercept-only model with random intercept 
M1 <- glmer(dependent ~ 1 + (1 | cluster), data = dat, family = 'binomial', nAGQ = 10)
summary(M1)
summ(M1, exp = T)

## Add level 1 predictors to the model
M2 <- glmer(dependent ~ 1 + level1 + level1 + (1 | cluster), data = dat, family = 'binomial', nAGQ = 10)
summary(M2)
summ(M2, exp = T)

## Variance linear predictor
linear_pred <- intercept + slope1*dat$dummy(ofcontinuous) - slope2*dat$dummy(ofcontinuous) 
varLP <- var(linear_pred)

## R2 not calculated in accordance with baseline model, as the scale differs in every model
## Calculate R2 for M2 and M3
#L2 class level: var cluster current model / (var residual + var cluster + var linear predictor current model)
#L1 individual level: var residual current model (3.29) / (var residual + var cluster + var linear predictor current model)
#overall explained variance: var linear predictor current model / (var residual + var cluster + var linear predictor current model)

## Add level 2 predictors to the model
M3 <- glmer(dependent ~ 1 + level1 + level1 + level2 + (1 | cluster), data = dat, family = 'binomial', nAGQ = 10)
summary(M3)
summ(M3, exp = T)

## Add random slope and covariance
M4 <- glmer(dependent ~ 1 + level1 + level1 (+ level2) + (1 + level1 | cluster), data = dat, family = 'binomial', nAGQ = 1) #nb nAGQ now 1
summ(M4, exp = T)

## We did not add an interaction in the lab, but you could do this in theory (see book p. 121)
M5 <- glmer(dependent ~ 1 + level1 + level1 + level1 * level2 + (1 + level1 | cluster), data = dat, family = 'binomial', nAGQ = 1) #not sure about the value of nAGQ
summ(M5, exp = T)

## All models in one
library(texreg)
screenreg(list(M0, M1, M2, M3, M4))