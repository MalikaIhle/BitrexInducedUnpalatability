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

AllAttacks <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks1_5.csv", header=TRUE, sep=",")
FocalTermiteAttack <- read.csv(file = "3_ExtractedData/FocalAttacks/FocalTermiteAttack1_5.csv", header=TRUE, sep=",")
FirstAttacks <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks1_5.csv", header=TRUE, sep=",")

FocalTermiteAttack$FocalTermitePalatability <- as.factor(FocalTermiteAttack$FocalTermitePalatability)
AllAttacks$AttackedTermitePalatability <- as.factor(AllAttacks$AttackedTermitePalatability)
FirstAttacks$AttackedTermitePalatability <- as.factor(FirstAttacks$AttackedTermitePalatability)


head(AllAttacks)    
head(FocalTermiteAttack)
head(FirstAttacks)



# model 1: 
# question 2 (confirmatory): reluctant to attack bitrex termite first regardless of their training?  
# --> main effect of Palatability not significant, effect direction according to expectation = palatable one more likely to be attacked first                                                       
# question 3 (exploratory): bias against a color ?
# --> no

str(FocalTermiteAttack)

mod1 <- glm (FocalTermiteAttackedYN ~ FocalTermiteColor + FocalTermitePalatability, family = 'binomial', data = FocalTermiteAttack)
summary(mod1)



# model 2: 
# question 2 (confirmatory): are bitrex termite more likely to be dropped?
# --> yes, **
# question 3 (exploratory): are termite from a certain color more likely to be dropped?
# --> yes, they tend to be more likely . to drop a brown termite (different color than with bitrex 1%)
# question 4 (exploratory): are bitrex termites always dropped
# no, out of 36 bitrex termites attacked, 7 were consumed (19.4%)
# it might be worth trying with a higher concentration of bitrex
# however, dropping rate of palatable ones went from 10% (bitrex 1%) to 50% again:
# risk of contamination not resolved??

str(AllAttacks)

mod2 <- glmer (DropYN ~ AttackedTermiteColor + AttackedTermitePalatability  + (1|FID)
               ,family = 'binomial', data = AllAttacks)
summary(mod2)

#sunflowerplot(AllAttacks$DropYN,AllAttacks$AttackedTermitePalatability)
table(AllAttacks$Outcome,AllAttacks$AttackedTermitePalatability) # dropping rate of bitrex termite = 85.3%


# model 3
# question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
# no, effect according to expectation = palatable termite attacked first were attacked after a shorter delay than bitrex termites attacked first

str(FirstAttacks)

mod3 <- lm(DelayToAttack ~ AttackedTermitePalatability, data = FirstAttacks)
summary(mod3)




