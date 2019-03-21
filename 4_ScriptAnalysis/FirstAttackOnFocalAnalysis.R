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

FirstAttacks0 <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks0.csv", header=TRUE, sep=",")
FocalAttacks1 <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks1.csv", header=TRUE, sep=",")
FocalAttacks15 <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks15.csv", header=TRUE, sep=",")
FocalAttacks2 <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks2.csv", header=TRUE, sep=",")
FocalAttacks3 <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks3.csv", header=TRUE, sep=",")
FocalAttacks3F <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks3F.csv", header=TRUE, sep=",")


# DB concentration = 1%


# DB concentration = 0%
# question 3 (exploratory): bias against a color ?
# --> no, effect direction: slight preference to attack the green first

table(FirstAttacks$AttackedTermiteColor)

chisq.test(c(13,17),p=c(0.5,0.5))
