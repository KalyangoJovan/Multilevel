#################################################### 
### Optional Starter Script for Lab 1            ###
#################################################### 


#################################################### 
### Read in the data set                         ###
#################################################### 

popular <- read.csv(file = "popular.csv")
head(popular)

#################################################### 
### Always nice to look at the data              ###
####################################################

head(popular)

#################################################### 
### Make sure to load and call packages to use   ###
####################################################

#install.packages("lme4")

library(lme4)

#Nice to review the function
# ?lmer  
