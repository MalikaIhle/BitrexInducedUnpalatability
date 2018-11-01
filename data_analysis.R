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



# model 1: reluctant to attack bitrex termite first ? especially if trained?
head(FocalTermiteAttack)

mod1 <- glm (FocalTermiteAttackedYN ~ FocalTermiteColor + FocalTermitePalatability*PriorExposureYN, family = 'binomial', data = FocalTermiteAttack)
summary(mod1)



# model 2: always drop bitrex termite? less so if trained/habituated?
head(AllAttacks)

mod2 <- glmer (DropYN ~ AttackedTermiteColor + AttackedTermitePalatability
              # *PriorExposureYN 
               + (1|FID), family = 'binomial', data = AllAttacks)
summary(mod2)
invlogit(coef(summary(mod2))[3,1])


# explo: delay to first attack longer if attack the bitrex termite?
head(FirstAttacks)

mod3 <- lm(DelayToAttack ~ AttackedTermitePalatability, data = FirstAttacks)
summary(mod3)




