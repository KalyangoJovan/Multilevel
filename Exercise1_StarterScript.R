#################################################### 
### Optional Starter Script for Lab 1            ###
#################################################### 


#################################################### 
### Read in the data set                         ###
#################################################### 

popular <- read.csv(file = "popular.csv")
head(popular)

summary(popular)

mod1<-lm(popular ~ 1, data = popular) 

summary(mod1)
#popular = ð¾00 + ei

#Intercept-only Model
library(lme4)

mod2<-lmer(popular ~ 1 + (1|class), REML = FALSE,
     data = popular) 

summary(mod2)


anova(mod2, mod1)

library(lmerTest)

#icc
icc <- (0.69/(0.69+1.22))
icc

#2. Fixed Model: 1st level predictors

mod3 <- lmer(popular ~ gender + extrav + (1|class),
             REML = FALSE, data = popular) 
summary(mod3)

anova(mod2, mod3)
#3. Fixed Model: 1st and 2nd level predictors

mod4 <- lmer(popular ~ gender + extrav + texp +
               (1|class),
             REML = FALSE, data = popular) 
summary(mod4)


#4. Random Coefficient Model

mod5 <- lmer(popular ~ 1 + extrav + gender + texp + (extrav|class),
             REML = FALSE, data = popular) 

summary(mod5)


mod6 <- lmer(popular~1 + gender + extrav + texp + extrav*texp + (extrav|class),
                REML = FALSE, data = popular)
summary(mod6)
# to evaluate the signif
summary(mod5)

library(jtools)
summ(mod5)
summ(mod6)
summary(mod4)

#extreg(list(mod1,mod2, mod3))

anova(mod4, mod3)
resummary(mod3)
summ(mod3)
summ(mod2)

install.packages("textreg")

require(textreg)

screenreg(list(mod1,mod2, mod3, mod4, mod5))


A boxplot is a visualization tool for quantitative variable by displaying five common location summary (minimum, median, first and third quartiles and maximum) and any observation that was classified as a suspected outlier using the interquartile range $IQR$ criterion. The IQR criterion means that all observations above $q0.75+1.5.IQR$ or below $q0.25???1.5IQR$ ( where $q0.25$ and $q0.75$ correspond to first and third quartile respectively) and $IQR$ is the difference between the third and first quartile. Are considered as potential outliers, we can conclude that all the observations outside of the following interval will be considered as outliers, there are outliers in our dataset. 
$$I = [q0.25???1.5IQR;q0.75+1.5.IQR]$$  