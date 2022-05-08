library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(foreign)
library(jtools)
library(texreg)

library(readr)
math_08 <- read_csv("math_08.csv")
View(math_08)

### from model 0 

model_0 <- lm(Multi ~ 1, data  = math_08)
summary(model_0) ## not included in the excel file

model_1 <- lmer(Multi ~ 1 + (1|student), REML = F, data = math_08)
summary(model_1) 
summ(model_1) 
anova(model_1, model_0)


## time included 

model_2 <- lmer(Multi ~ 1 + time + (1|student), REML = F, data = math_08)

summary(model_2)  
summ(model_2)

anova(model_2, model_1)


## include time and Math( level 1 predictors )

model_3 <- lmer(Multi ~ 1 + time + Math + (1|student), REML = F, data = math_08)

summary(model_3)  
summ(model_3)

anova(model_3, model_2)


## include time, Math, Gender, Origin and Parent_Math( level 1 and 2) fixed

model_4a <- lmer(Multi ~ 1 + time + Math + Gender + Origin + Parent_Math + (1|student), REML = F, data = math_08)

summary(model_4a)  
summ(model_4a)

anova(model_4a, model_3)

## include only significant variables 

model_4b <- lmer(Multi ~ 1 + time + Math + Gender + (1|student), REML = F, data = math_08)

summary(model_4b)  
summ(model_4b)

anova(model_4b, model_3)

# given models 
screenreg(list(model_1, model_2, model_3, model_4a, model_4b))

## exam models from here 

model_1r  <- lmer(Multi ~ 1 + time + Math + Gender + Origin + Parent_Math + (time|student), REML = F, data = math_08)

summary(model_1r)

vc_model_1r <- VarCorr(model_1r)
as.data.frame(vc_model_1r, order = "lower.tri")

anova(model_1r, model_4b)

## with Math random

model_2r  <- lmer(Multi ~ 1 + time + Math + Gender + Origin + Parent_Math + (Math|student), REML = F, data = math_08)

summary(model_2r)

vc_model_2r <- VarCorr(model_2r)
as.data.frame(vc_model_2r, order = "lower.tri")

anova(model_2r, model_1r) # not signifcant 


## interaction 

model_i  <- lmer(Multi ~ 1 + time + Math + Gender + Origin + Parent_Math + time*Gender + (Math|student), REML = F, data = math_08)

summary(model_i)

vc_model_i <- VarCorr(model_i)
as.data.frame(vc_model_i, order = "lower.tri")

anova(model_1r, model_i) 


### next, only significant 

model_ii  <- lmer(Multi ~ 1 + time + Math + Gender + time*Gender + (Math|student), REML = F, data = math_08)

summary(model_ii) 

vc_model_ii <- VarCorr(model_ii)
as.data.frame(vc_model_ii, order = "lower.tri")

anova(model_i, model_ii) 



screenreg(list(model_1r, model_2r, model_i, model_ii))


