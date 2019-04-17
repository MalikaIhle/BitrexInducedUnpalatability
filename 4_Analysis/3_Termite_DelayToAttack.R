#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data analysis delay to attack
#	 Start : 21 march 2019
#	 commit: create function to apply to all datasets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## rk:
# palatability = 0 > DB termite
# palatability = 1 > water termite
# groupname water > no prior exposure
# group name DB > prior exposure

rm(list = ls(all = TRUE))

# packages
library(lme4)
library(arm)


FirstAttacks1 <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks1.csv", header=TRUE, sep=",")
FirstAttacks15 <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks15.csv", header=TRUE, sep=",")
FirstAttacks2 <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks2.csv", header=TRUE, sep=",")
FirstAttacks3 <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks3.csv", header=TRUE, sep=",")
FirstAttacks3F <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks3F.csv", header=TRUE, sep=",")


# DB concentration = 1%
  # question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
  # no, effect direction opposite expectation = palatable termite attacked first were attacked after a longer delay than bitrex termites attacked first
  
  str(FirstAttacks1)

mod1 <- lm(DelayToAttack ~ AttackedPalatabilityTreatment, data = FirstAttacks1)
summary(mod1)
drop1(mod1, test="F")

mod1_inter <- lm(DelayToAttack ~ AttackedPalatabilityTreatment*PriorExposure, data = FirstAttacks1)
summary(mod1_inter)
drop1(mod1_inter, test="F")

# DB concentration = 1.5%
# question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
# no, effect according to expectation = palatable termite attacked first were attacked after a shorter delay than bitrex termites attacked first

str(FirstAttacks15)

mod15 <- lm(DelayToAttack ~ AttackedPalatabilityTreatment, data = FirstAttacks15)
summary(mod15)
drop1(mod15, test="F")


# DB concentration = 2%
# question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
# no, effect direction opposite expectation = palatable termite attacked first were attacked after a longer delay than bitrex termites attacked first

str(FirstAttacks2)

mod2 <- lm(DelayToAttack ~ AttackedPalatabilityTreatment, data = FirstAttacks2)
summary(mod2)
drop1(mod2, test="F")


# DB concentration = 3%
# question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
# no, effect direction according to expectation
# palatable termite attacked first were attacked after a shorter delay than bitrex termites attacked first

str(FirstAttacks3)

mod3 <- lm(DelayToAttack ~ AttackedPalatabilityTreatment, data = FirstAttacks3)
summary(mod3)
drop1(mod3, test="F")


# DB concentration = 3F%
# question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
# no, effect direction opposite expectation = palatable termite attacked first were attacked after a longer delay than bitrex termites attacked first

str(FirstAttacks3F)

mod3F <- lm(DelayToAttack ~ AttackedPalatabilityTreatment, data = FirstAttacks3F)
summary(mod3F)
drop1(mod3F, test="F")


mod3F_inter <- lm(DelayToAttack ~ AttackedPalatabilityTreatment*PriorExposure, data = FirstAttacks3F)
summary(mod3F_inter)
drop1(mod3F_inter, test="F")

