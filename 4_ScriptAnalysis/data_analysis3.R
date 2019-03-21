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
# --> main effect of Palatability not significant, effect direction according to expectation = palatable one less likely to be attacked first                                                       
# question 3 (exploratory): bias against a color ?
# --> no, effect direction: have a slight preference to attack the green first

str(FocalTermiteAttack)

mod1 <- glm (FocalTermiteAttackedYN ~ FocalTermiteColor + FocalTermitePalatability, family = 'binomial', data = FocalTermiteAttack)
summary(mod1)
drop1(mod1, test="Chisq")




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
fisher.test(cbind(attackafterbitrex,firstattack), alternative='less') # p=0.7

