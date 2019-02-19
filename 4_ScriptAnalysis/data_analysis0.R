#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data analysis
#	 Start : 19 february 2019
#	 last modif : 20190219
#	 commit: first
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

# packages
library(lme4)
library(arm)

AllAttacks <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks0.csv", header=TRUE, sep=",")
FirstAttacks <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks0.csv", header=TRUE, sep=",")


head(AllAttacks)    
head(FirstAttacks)



# model 1: 
# question 3 (exploratory): bias against a color ?
# --> no, effect direction: slight preference to attack the green first

str(FocalTermiteAttack)
table(FirstAttacks$AttackedTermiteColor)

chisq.test(c(13,17),p=c(0.5,0.5))



# model 2: 
# question 3 (exploratory): are termite from a certain color more likely to be dropped?
# --> no, effect direction:  more likely to drop a brown termite
# dropping rate of palatable ones is 14.3% (close to when the most precautions against contamination were taken)

str(AllAttacks)

mod2 <- glmer (DropYN ~ AttackedTermiteColor  + (1|FID)
               ,family = 'binomial', data = AllAttacks)
summary(mod2)

table(AllAttacks$Outcome)





