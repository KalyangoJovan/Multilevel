
library(lme4)
library(lmerTest)
library(foreign)
library(jtools)
library(texreg)


library(readr)
readach <- read_csv("readach.csv")
View(readach)

model_0 <- lm(readach ~ 1, data  = readach)
summary(model_0) ## not included in the excel file

model_1 <- lmer(readach ~ 1 + (1|student), REML = F, data = readach)
summary(model_1) 
summ(model_1) 
anova(model_1, model_0)


## time included 

model_2 <- lmer(readach ~ 1 + time + (1|student), REML = F, data = readach )

summary(model_2)  
summ(model_2)

anova(model_2, model_1)


## include time and Math( level 1 predictors )

model_3 <- lmer(readach ~ 1 + time + genvoc + (1|student), REML = F, data = readach)

summary(model_3)  
summ(model_3)

anova(model_3, model_2)


## include time, Math, Gender, Origin and Parent_Math( level 1 and 2) fixed

model_4a <- lmer(readach ~ 1 + time + genvoc + Gender + Origin + Parent_vocab + (1|student), REML = F, data = readach)

summary(model_4a)  
summ(model_4a)

anova(model_4a, model_)

## include only significant variables 

model_4b <- lmer(readach ~ 1 + time + genvoc + Gender + Parent_vocab + (1|student), REML = F, data = readach)

summary(model_4b)  
summ(model_4b)

anova(model_4b, model_3)

# given models 
screenreg(list(model_1, model_2, model_3, model_4a, model_4b))

## exam models from here 

model_1r  <- lmer(readach ~ 1 + time + genvoc+ Gender + Origin + Parent_vocab + (time|student), REML = F, data = readach)

summary(model_1r)

vc_model_1r <- VarCorr(model_1r)
as.data.frame(vc_model_1r, order = "lower.tri")

anova(model_1r, model_4b)
summ(model_1r)
## with Math random

model_2r  <- lmer(readach ~ 1 + time + genvoc + Gender + Origin + Parent_vocab + (genvoc|student), REML = F, data = readach)

summary(model_2r)

vc_model_2r <- VarCorr(model_2r)
as.data.frame(vc_model_2r, order = "lower.tri")

anova(model_2r, model_1r) # not signifcant 


## interaction 

model_i  <- lmer(readach ~ 1 + time + genvoc + Gender + Origin + Parent_vocab + time*Gender + (genvoc|student), REML = F, data = readach)

summary(model_i)

vc_model_i <- VarCorr(model_i)
as.data.frame(vc_model_i, order = "lower.tri")

anova(model_1r, model_i) 


### next, only significant 

model_ii  <- lmer(readach ~ 1 + time + genvoc + Gender + time*Gender + (genvoc|student), REML = F, data = readach)

summary(model_ii) 

vc_model_ii <- VarCorr(model_ii)
as.data.frame(vc_model_ii, order = "lower.tri")

anova(model_i, model_ii) 

## needed models 

screenreg(list(model_1r, model_2r, model_i, model_ii))

