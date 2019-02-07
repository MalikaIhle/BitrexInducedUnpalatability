#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Preregistration manipulation color and unpalatability 
#  data analysis
#	 Start : 27 november 2018
#	 last modif : 2018 11 21
#	 commit: first
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

# packages
library(lme4)
library(arm)

AllAttacks <- read.csv(file="3_ExtractedData/AllAttacks3_Final.csv", header=TRUE, sep=",")
FocalTermiteAttack <- read.csv(file = "3_ExtractedData/FocalTermiteAttack3_Final.csv", header=TRUE, sep=",")
FirstAttacks <- read.csv(file = "3_ExtractedData/FirstAttacks3_Final.csv", header=TRUE, sep=",")



head(AllAttacks)    
head(FocalTermiteAttack)
head(FirstAttacks)



# model 1: 
# question 2 (confirmatory): reluctant to attack bitrex termite first regardless of their training?  
# --> main effect of Palatability not significant, effect direction according to expectation = palatable one more likely to be attacked first                                                       
# question 3 (exploratory): bias against a color ?
# --> yes, effect direction: have a significant preference to attack the green first

head(FocalTermiteAttack)

mod1 <- glm (FocalTermiteAttackedYN ~ FocalTermiteColor + FocalTermitePalatability, family = 'binomial', data = FocalTermiteAttack)
summary(mod1)





# model 2: 
# question 2 (confirmatory): are bitrex termite more likely to be dropped?
# --> yes, **
# question 3 (exploratory): are termite from a certain color more likely to be dropped?
# --> yes, they tend to be more likely . to drop a brown termite (different color than with bitrex 1%)
# question 4 (exploratory): are bitrex termites always dropped
# no, out of 34 bitrex termites attacked, 5 were consumed (14.7%)
# it might be worth trying with a higher concentration of bitrex
# however, dropping rate of palatable ones went from 10% (bitrex 1%) to 50%: risk of contamination has increased?? -> take more precaution

head(AllAttacks)

mod2 <- glmer (DropYN ~ AttackedTermiteColor + AttackedTermitePalatability  + (1|FID)
               ,family = 'binomial', data = AllAttacks)
summary(mod2)

sunflowerplot(AllAttacks$DropYN,AllAttacks$AttackedTermitePalatability)
table(AllAttacks$Outcome,AllAttacks$AttackedTermitePalatability) # dropping rate of bitrex termite = 85.3%


# model 3
# question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
# no, effect direction opposite expectation = palatable termite attacked first were attacked after a longer delay than bitrex termites attacked first

head(FirstAttacks)

mod3 <- lm(DelayToAttack ~ AttackedTermitePalatability, data = FirstAttacks)
summary(mod3)




