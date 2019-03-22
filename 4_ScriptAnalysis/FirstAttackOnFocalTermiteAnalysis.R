#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data analysis drop rate
#	 Start : 21 march 2019
#	 commit: create function to apply to all datasets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## rk:
# palatability = 0 > DB termite
# palatability = 1 > water termite
# groupname water > no prior exposure
# group name DB > prior exposure

rm(list = ls(all = TRUE))

# packages
library(lme4)
library(arm)

FirstAttacks0 <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks0.csv", header=TRUE, sep=",")
FocalAttacks1 <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks1.csv", header=TRUE, sep=",")
FocalAttacks15 <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks15.csv", header=TRUE, sep=",")
FocalAttacks2 <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks2.csv", header=TRUE, sep=",")
FocalAttacks3 <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks3.csv", header=TRUE, sep=",")
FocalAttacks3F <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks3F.csv", header=TRUE, sep=",")


# DB concentration = 1%

# question 1 (confirmatory): reluctant to attack bitrex termite first especially if trained?   
# --> interaction not significant, effect direction opposite expectation = less likely to attack a focal termite that is palatable if they were exposed
# question 2 (confirmatory): reluctant to attack bitrex termite first regardless of their training?  
# --> main effect of Palatability not significant, effect direction according to expectation = palatable one more likely to be attacked first (no effect)                                                           
# question 3 (exploratory): bias against a color ?
# --> no, effect direction: have a slight (no) preference to attack the green first

str(FocalAttacks1)

mod1 <- glm (FocalAttackedYN ~ FocalColor + FocalPalatabilityTreatment*PriorExposure, family = 'binomial', data = FocalAttacks1)
summary(mod1)
drop1(mod1, test="Chisq")




# DB concentration = 0%
# question 3 (exploratory): bias against a color ?
# --> no, effect direction: slight preference to attack the green first

table(FirstAttacks$AttackedColor)

chisq.test(c(13,17),p=c(0.5,0.5))










