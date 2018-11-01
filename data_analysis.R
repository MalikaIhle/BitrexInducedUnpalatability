#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Preregistration manipulation color and unpalatability 
#  data analysis
#	 Start : 31 october 2018
#	 last modif : 
#	 commit: first commit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

# packages
library(lme4)
library(arm)

AllAttacks <- read.csv(file="AllAttacks.csv", header=TRUE, sep=",")
FocalTermiteAttack <- read.csv(file = "FocalTermiteAttack.csv", header=TRUE, sep=",")
FirstAttacks <- read.csv(file = "FirstAttacks.csv", header=TRUE, sep=",")



head(AllAttacks)    
head(FocalTermiteAttack)
head(FirstAttacks)



# model 1: 
# question 1 (confirmatory): reluctant to attack bitrex termite first especially if trained?   
# --> interaction not significant, effect direction opposite expectation = less likely to attack a focal termite that is palatable if they were exposed
# question 2 (confirmatory): reluctant to attack bitrex termite first regardless of their training?  
# --> main effect of Palatability not significant, effect direction according to expectation = palatable one more likely to be attacked first (excessively small effect)                                                           
# question 3 (exploratory): bias against a color ?
# --> no, effect direction: have a slight preference to attack the green first

head(FocalTermiteAttack)

mod1 <- glm (FocalTermiteAttackedYN ~ FocalTermiteColor + FocalTermitePalatability*PriorExposureYN, family = 'binomial', data = FocalTermiteAttack)
summary(mod1)





# model 2: 
# question 1 (exploratory):  drop bitrex termite less so if trained/habituated?
# --> absolutely no interaction between training and palatability on dropYN; interaction removed from model 
# question 2 (confirmatory): are bitrex termite more likely to be dropped?
# --> yes, ***
# question 3 (exploratory): are termite from a certain color more likely to be dropped?
# --> yes, they are more likely ** to drop a green termite
# question 4 (exploratory): are bitrex termites always dropped
# no, out of 80 bitrex termites attacked, 35 were consumed (43.7%)
# it might be worth trying with a higher concentration of bitrex

head(AllAttacks)

mod2 <- glmer (DropYN ~ AttackedTermiteColor + AttackedTermitePalatability
               #*PriorExposureYN 
               + (1|FID), family = 'binomial', data = AllAttacks)
summary(mod2)

sunflowerplot(AllAttacks$DropYN,AllAttacks$AttackedTermitePalatability)
table(AllAttacks$Outcome,AllAttacks$AttackedTermitePalatability)




# model 3
# question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
# no, effect direction opposite expectation = palatable termite attacked first were attacked after a longer delay than bitrex termites attacked first

head(FirstAttacks)

mod3 <- lm(DelayToAttack ~ AttackedTermitePalatability, data = FirstAttacks)
summary(mod3)




