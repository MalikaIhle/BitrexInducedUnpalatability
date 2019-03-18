#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data analysis
#	 Start : 21 november 2018
#	 last modif : 20190219
#	 commit: change variable to factors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

# packages
library(lme4)
library(arm)

AllAttacks <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks2.csv", header=TRUE, sep=",")
FocalTermiteAttack <- read.csv(file = "3_ExtractedData/FocalAttacks/FocalTermiteAttack2.csv", header=TRUE, sep=",")
FirstAttacks <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks2.csv", header=TRUE, sep=",")

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
# --> no, effect direction: preference to attack the green first

str(FocalTermiteAttack)

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

str(AllAttacks)

mod2 <- glmer (DropYN ~ AttackedTermiteColor + AttackedTermitePalatability  + (1|FID)
               ,family = 'binomial', data = AllAttacks)
summary(mod2)

#sunflowerplot(AllAttacks$DropYN,AllAttacks$AttackedTermitePalatability)
table(AllAttacks$Outcome,AllAttacks$AttackedTermitePalatability) # dropping rate of bitrex termite = 85.3%


# model 3
# question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
# no, effect direction opposite expectation = palatable termite attacked first were attacked after a longer delay than bitrex termites attacked first

head(FirstAttacks)

mod3 <- lm(DelayToAttack ~ AttackedTermitePalatability, data = FirstAttacks)
summary(mod3)


# model 4
# question: are palatable prey (palatability = 1) more likely to get dropped 
# if attack occur after an attack on a bitrex prey (palatability = 0)?

# second attack on a palatble prey, when first attack on unpalatable prey
attackafterbitrex <- table(AllAttacks$DropYN[AllAttacks$AttackNb == 2 & AllAttacks$AttackedTermitePalatability == "1" & AllAttacks$PrevPalatabality == "0"])
# first attack on a palatable prey
firstattack <- table(AllAttacks$DropYN[AllAttacks$AttackNb == 1 & AllAttacks$AttackedTermitePalatability == "1"])

fisher.test(cbind(attackafterbitrex,firstattack))
# https://stats.stackexchange.com/questions/316195/fishers-exact-test-meaning-of-greater-and-less
# ?fisher.test
fisher.test(cbind(attackafterbitrex,firstattack), alternative='greater') # ?
fisher.test(cbind(attackafterbitrex,firstattack), alternative='less') # ?

