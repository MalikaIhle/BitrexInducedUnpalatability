#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data analysis drop rate
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


AllAttacks0 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks0.csv", header=TRUE, sep=",")
AllAttacks1 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks1.csv", header=TRUE, sep=",")
AllAttacks15 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks15.csv", header=TRUE, sep=",")
AllAttacks2 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks2.csv", header=TRUE, sep=",")
AllAttacks3 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks3.csv", header=TRUE, sep=",")
AllAttacks3F <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks3F.csv", header=TRUE, sep=",")


# concentration DB = 1% 
  # question 1 (exploratory):  drop bitrex termite less so if trained/habituated?
  # --> absolutely no interaction between training and palatability on dropYN; interaction removed from model 
  # question 2 (confirmatory): are bitrex termite more likely to be dropped?
  # --> yes, ***
  # question 3 (exploratory): are termite from a certain color more likely to be dropped?
  # --> yes, they are more likely ** to drop a green termite
  # question 4 (exploratory): are bitrex termites always dropped
  # no, out of 84 bitrex termites attacked, 39 were consumed (46.4%)
  # it might be worth trying with a higher concentration of bitrex
  
  str(AllAttacks1)
  table(AllAttacks1$Fate,AllAttacks1$AttackedPalatabilityTreatment) # dropping rate of bitrex termite = 85.3%

mod1 <- glmer (DropYN ~ AttackedColor + AttackedPalatabilityTreatment
               *GroupName 
               + (1|FID)
               ,family = 'binomial', data = AllAttacks1)
summary(mod1)
drop1(mod1, test="Chisq")


# concentration DB = 1.5% 
  # question 2 (confirmatory): are bitrex termite more likely to be dropped?
  # --> yes, ***
  # question 3 (exploratory): are termite from a certain color more likely to be dropped?
  # --> yes, they tend to be more likely . to drop a brown termite (different color than with bitrex 1%)
  # question 4 (exploratory): are bitrex termites always dropped
  # no, out of 36 bitrex termites attacked, 7 were consumed (19.4%)
  # it might be worth trying with a higher concentration of bitrex
  # however, dropping rate of palatable ones went from 10% (bitrex 1%) to 50% again:
  # risk of contamination not resolved??

  str(AllAttacks15)
  table(AllAttacks15$Fate,AllAttacks15$AttackedPalatabilityTreatment) # dropping rate of bitrex termite = 85.3%

mod15 <- glmer (DropYN ~ AttackedColor + AttackedPalatabilityTreatment  + (1|FID)
               ,family = 'binomial', data = AllAttacks15)
summary(mod15)
drop1(mod15, test="Chisq")


# concentration DB = 2%
  # question 2 (confirmatory): are bitrex termite more likely to be dropped?
  # --> yes, **
  # question 3 (exploratory): are termite from a certain color more likely to be dropped?
  # --> yes, they tend to be more likely . to drop a brown termite (different color than with bitrex 1%)
  # question 4 (exploratory): are bitrex termites always dropped
  # no, out of 34 bitrex termites attacked, 5 were consumed (14.7%)
  # it might be worth trying with a higher concentration of bitrex
  # however, dropping rate of palatable ones went from 10% (bitrex 1%) to 50%: risk of contamination has increased?? -> take more precaution
  
  str(AllAttacks2)
  table(AllAttacks2$Fate,AllAttacks2$AttackedPalatabilityTreatment) # dropping rate of bitrex termite = 85.3%

mod2 <- glmer (DropYN ~ AttackedColor + AttackedPalatabilityTreatment  + (1|FID)
               ,family = 'binomial', data = AllAttacks2)
summary(mod2)
drop1(mod2, test="Chisq")


# concentration DB = 3%
  # question 2 (confirmatory): are bitrex termite more likely to be dropped?
  # --> yes, ***
  # question 3 (exploratory): are termite from a certain color more likely to be dropped?
  # --> yes, they tend to be more likely . to drop a brown termite (different color than with bitrex 1%)
  # question 4 (exploratory): are bitrex termites always dropped
  # no, out of 26 bitrex termites attacked, 5 were consumed (19.2%)
  # dropping rate didn't increase, it may not be wroth trying a higher concentration
  # however, dropping rate of palatable ones went from 10% (bitrex 1%) to 50% (bitrex 2%) to 24.1%:
  # risk of contamination has been better handled but not completely solved
  # worth trying to compare to actual basal dropping rate (when no bitrex involved)
  
  str(AllAttacks3)
  table(AllAttacks3$Fate,AllAttacks3$AttackedPalatabilityTreatment) # dropping rate of bitrex termite = 85.3%
  

mod3 <- glmer (DropYN ~ AttackedColor + AttackedPalatabilityTreatment  + (1|FID)
               ,family = 'binomial', data = AllAttacks3)
summary(mod3)
drop1(mod3, test="Chisq")


# concentration DB = 3F%
  # question 1 (exploratory):  drop bitrex termite less so if trained/habituated?
  # --> absolutely no interaction between training and palatability on dropYN; interaction removed from model 
  # question 2 (confirmatory): are bitrex termite more likely to be dropped?
  # --> yes, ****
  # question 3 (exploratory): are termite from a certain color more likely to be dropped?
  # --> yes, they are significantly to be more likely dropped if green 
  # question 4 (exploratory): are bitrex termites always dropped
  # no, out of 83 bitrex termites attacked, 29 were consumed (34.9%)
  # it could have been worth trying with a higher concentration of bitrex?
  # dropping rate of palatable termites: 22.8% (contamination to quite resolved? but better under control?), and 65% dropping rate for the bitrex
  
  str(AllAttacks3F)
  table(AllAttacks3F$Fate,AllAttacks3F$AttackedPalatabilityTreatment)
  
mod3F <- glmer (DropYN ~ AttackedColor + AttackedPalatabilityTreatment
               *PriorExposureYN 
               + (1|FID)
               ,family = 'binomial', data = AllAttacks3F)
summary(mod3F)
drop1(mod3F, test="Chisq")


