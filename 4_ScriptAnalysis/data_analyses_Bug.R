#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data analysis
#	 Start : 18 march 2019
#	 last modif : 20190318
#	 commit: first
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

# packages
library(lme4)
library(arm)

AllAttacks <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacksBug.csv", header=TRUE, sep=",")
FocalBugAttack <- read.csv(file = "3_ExtractedData/FocalAttacks/FocalBugAttack.csv", header=TRUE, sep=",")
FirstAttacks <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacksBug.csv", header=TRUE, sep=",")

FocalBugAttack$FocalBugPalatability <- as.factor(FocalBugAttack$FocalBugPalatability)
AllAttacks$AttackedBugPalatability <- as.factor(AllAttacks$AttackedBugPalatability)
FirstAttacks$AttackedBugPalatability <- as.factor(FirstAttacks$AttackedBugPalatability)


head(AllAttacks)    
head(FocalBugAttack)
head(FirstAttacks)



# model 1: 
# question 2 (confirmatory): reluctant to attack bitrex Bug first regardless of their training?  
# --> main effect of Palatability trendy, effect direction against expectation = palatable one less likely to be attacked first                                                       
# question 3 (exploratory): bias against a color ?
# --> no (green less likely to be attacked first)

str(FocalBugAttack)

mod1 <- glm (FocalBugAttackedYN ~ FocalBugColor + FocalBugPalatability, family = 'binomial', data = FocalBugAttack)
summary(mod1)
drop1(mod1, test="Chisq")


# model 2: 
# question 2 (confirmatory): are MW Bug more likely to be dropped?
# --> yes, *******
# question 3 (exploratory): are Bug from a certain color more likely to be dropped?
# --> no
# question 4 (exploratory): are MW Bugs always dropped
# yes


str(AllAttacks)

mod2 <- glm (DropYN ~ AttackedBugColor + AttackedBugPalatability  #+ (1|FID)
               ,family = 'binomial', data = AllAttacks)
summary(mod2) ## <<<<<< super weird P value for ultra significant effect >>>>>>>>>>>>>> use LRT drop1 function
drop1(mod2, test="Chisq")

#sunflowerplot(AllAttacks$DropYN,AllAttacks$AttackedBugPalatability)
table(AllAttacks$Outcome,AllAttacks$AttackedBugPalatability) # dropping rate of bitrex Bug = 85.3%


# model 3
# question 1 (exploratory): delay to first attack longer if attack the bitrex Bug?
# no, effect according to expectation = palatable Bug attacked first were attacked after a shorter delay than MW Bugs attacked first

str(FirstAttacks)

mod3 <- lm(DelayToAttack ~ AttackedBugPalatability, data = FirstAttacks)
summary(mod3)
drop1(mod3, test="Chisq")

