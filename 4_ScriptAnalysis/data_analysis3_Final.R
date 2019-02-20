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


# model 2: 
# question 1 (exploratory):  drop bitrex termite less so if trained/habituated?
# --> absolutely no interaction between training and palatability on dropYN; interaction removed from model 
# question 2 (confirmatory): are bitrex termite more likely to be dropped?
# --> yes, ***
# question 3 (exploratory): are termite from a certain color more likely to be dropped?
# --> yes, they tend . to be more likely dropped if brown (opposite effect compared to 1% bitrex) 
# question 4 (exploratory): are bitrex termites always dropped
# no, out of 83 bitrex termites attacked, 29 were consumed (34.9%)
# it could have been worth trying with a higher concentration of bitrex?
# dropping rate of palatable termites: 22.8% (contamination to quite resolved? but better under control?)

str(AllAttacks)

mod2 <- glmer (DropYN ~ AttackedTermiteColor + AttackedTermitePalatability
               #*PriorExposureYN 
               + (1|FID)
               ,family = 'binomial', data = AllAttacks)
summary(mod2)

#sunflowerplot(AllAttacks$DropYN,AllAttacks$AttackedTermitePalatability)
table(AllAttacks$Outcome,AllAttacks$AttackedTermitePalatability)


# model 3
# question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
# no, effect direction opposite expectation = palatable termite attacked first were attacked after a longer delay than bitrex termites attacked first

str(FirstAttacks)

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

