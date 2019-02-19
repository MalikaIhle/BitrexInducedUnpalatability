#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data analysis
#	 Start : 27 november 2018
#	 last modif : 20190219
#	 commit: change variable to factors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

# packages
library(lme4)
library(arm)

AllAttacks <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks3.csv", header=TRUE, sep=",")
FocalTermiteAttack <- read.csv(file = "3_ExtractedData/FocalAttacks/FocalTermiteAttack3.csv", header=TRUE, sep=",")
FirstAttacks <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks3.csv", header=TRUE, sep=",")

FocalTermiteAttack$FocalTermitePalatability <- as.factor(FocalTermiteAttack$FocalTermitePalatability)
AllAttacks$AttackedTermitePalatability <- as.factor(AllAttacks$AttackedTermitePalatability)
FirstAttacks$AttackedTermitePalatability <- as.factor(FirstAttacks$AttackedTermitePalatability)


head(AllAttacks)    
head(FocalTermiteAttack)
head(FirstAttacks)



# model 1: 
# question 2 (confirmatory): reluctant to attack bitrex termite first regardless of their training?  
# --> main effect of Palatability not significant, effect direction opposite expectation = palatable one less likely to be attacked first                                                       
# question 3 (exploratory): bias against a color ?
# --> no, effect direction: have a slight preference to attack the green first

str(FocalTermiteAttack)

mod1 <- glm (FocalTermiteAttackedYN ~ FocalTermiteColor + FocalTermitePalatability, family = 'binomial', data = FocalTermiteAttack)
summary(mod1)



# model 2: 
# question 2 (confirmatory): are bitrex termite more likely to be dropped?
# --> yes, **
# question 3 (exploratory): are termite from a certain color more likely to be dropped?
# --> yes, they tend to be more likely . to drop a brown termite (different color than with bitrex 1%)
# question 4 (exploratory): are bitrex termites always dropped
# no, out of 26 bitrex termites attacked, 5 were consumed (19.2%)
# dropping rate didn't increase, it may not be wroth trying a higher concentration
# however, dropping rate of palatable ones went from 10% (bitrex 1%) to 50% (bitrex 2%) to 24.1%:
# risk of contamination has been better handled but not completely solved
# worth trying to compare to actual basal dropping rate (when no bitrex involved)

str(AllAttacks)

mod2 <- glmer (DropYN ~ AttackedTermiteColor + AttackedTermitePalatability  + (1|FID)
               ,family = 'binomial', data = AllAttacks)
summary(mod2)

sunflowerplot(AllAttacks$DropYN,AllAttacks$AttackedTermitePalatability)
table(AllAttacks$Outcome,AllAttacks$AttackedTermitePalatability) # dropping rate of bitrex termite = 85.3%


# model 3
# question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
# no, effect direction according to expectation
# palatable termite attacked first were attacked after a shorter delay than bitrex termites attacked first

str(FirstAttacks)

mod3 <- lm(DelayToAttack ~ AttackedTermitePalatability, data = FirstAttacks)
summary(mod3)




