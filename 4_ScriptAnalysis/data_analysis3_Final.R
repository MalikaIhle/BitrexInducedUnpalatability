#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data analysis
#	 Start : 27 november 2018
#	 last modif : 20190219
#	 commit: change variables to factors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

# packages
library(lme4)
library(arm)

AllAttacks <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks3_Final.csv", header=TRUE, sep=",")
FocalTermiteAttack <- read.csv(file = "3_ExtractedData/FocalAttacks/FocalTermiteAttack3_Final.csv", header=TRUE, sep=",")
FirstAttacks <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks3_Final.csv", header=TRUE, sep=",")

FocalTermiteAttack$PriorExposureYN <- as.factor(FocalTermiteAttack$PriorExposureYN)
FocalTermiteAttack$FocalTermitePalatability <- as.factor(FocalTermiteAttack$FocalTermitePalatability)
AllAttacks$AttackedTermitePalatability <- as.factor(AllAttacks$AttackedTermitePalatability)
AllAttacks$PriorExposureYN <- as.factor(AllAttacks$PriorExposureYN)
FirstAttacks$AttackedTermitePalatability <- as.factor(FirstAttacks$AttackedTermitePalatability)


head(AllAttacks)    
head(FocalTermiteAttack)
head(FirstAttacks)




# model 1: 
# question 1 (confirmatory): reluctant to attack bitrex termite first especially if trained?   
# --> interaction not significant, effect direction opposite expectation = less likely to attack a focal termite that is palatable if they were exposed
# question 2 (confirmatory): reluctant to attack bitrex termite first regardless of their training?  
# --> main effect of Palatability not significant, effect direction opposite expectation = palatable one less likely to be attacked first (medium effect)                                                           
# question 3 (exploratory): bias against a color ?
# --> no, effect direction: have a slight preference to attack the green first

str(FocalTermiteAttack)

mod1 <- glm (FocalTermiteAttackedYN ~ FocalTermiteColor + FocalTermitePalatability*PriorExposureYN 
             ,family = 'binomial', data = FocalTermiteAttack)
summary(mod1)
drop1(mod1, test="Chisq")

