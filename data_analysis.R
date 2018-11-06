#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Preregistration manipulation color and unpalatability 
#  data analysis
#	 Start : 31 october 2018
#	 last modif : 20118 11 06
#	 commit: determine sample size for follow up experiment with higher bitrex concentration
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
               + (1|FID)
               ,family = 'binomial', data = AllAttacks)
summary(mod2)

sunflowerplot(AllAttacks$DropYN,AllAttacks$AttackedTermitePalatability)
table(AllAttacks$Outcome,AllAttacks$AttackedTermitePalatability)

DropUnpalatable <- invlogit(coef(summary(mod2))[1, 1]) # 39.4%
DropUnpalatable_CI_low <-  invlogit(coef(summary(mod2))[1, 1]-coef(summary(mod2))[1, 2]*1.96) # 24.6%
DropUnpalatable_CI_high <-invlogit(coef(summary(mod2))[1, 1]+coef(summary(mod2))[1, 2]*1.96) # 56.5%

DropPalatable <- invlogit(coef(summary(mod2))[1, 1] + coef(summary(mod2))[3, 1]) # 9.9%
DropPalatable_CI_low <-invlogit(coef(summary(mod2))[1, 1]+ coef(summary(mod2))[3, 1]-coef(summary(mod2))[3, 2]*1.96) # 4.6%
DropPalatable_CI_high <-invlogit(coef(summary(mod2))[1, 1]+ coef(summary(mod2))[3, 1]+coef(summary(mod2))[3, 2]*1.96) # 20.1%

# trying to follow guidelines:
# https://besjournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2F2041-210X.12306&file=mee312306-sup-0001-AppendixS1.pdf
# from this paper
# https://besjournals.onlinelibrary.wiley.com/doi/epdf/10.1111/2041-210X.12306

OddsDrop <- ((invlogit(coef(summary(mod2))[1, 1])/(1-invlogit(coef(summary(mod2))[1, 1]))))/
            ((invlogit(coef(summary(mod2))[1, 1] + coef(summary(mod2))[3, 1]))/(1-(invlogit(coef(summary(mod2))[1, 1] + coef(summary(mod2))[3, 1])))) # 5.9

qlogis(invlogit(coef(summary(mod2))[1, 1] + coef(summary(mod2))[3, 1]))
log(OddsDrop)

# .... 

# will consider mod2 as simple chisquare
mod2glm <- glm (DropYN ~ AttackedTermitePalatability ,family = 'binomial', data = AllAttacks)
summary(mod2glm)

mod2quis <- chisq.test(AllAttacks$DropYN,AllAttacks$AttackedTermitePalatability)
table(AllAttacks$Outcome,AllAttacks$AttackedTermitePalatability)
# H0 reference: 10% dropping rate for control prey
# H1, with 1% concentration, 40% dropping rate
  
w <- sqrt((0.10-0.40)^2/0.10  + (0.90-0.60)^2/0.90) # following formula: https://www.statmethods.net/stats/power.html

library(pwr)
?pwr.chisq.test
pwr.chisq.test(w=0.5,N=, df=1, sig.level=0.05,power=0.8)
pwr.chisq.test(w=0.9,N=, df=1, sig.level=0.05,power=0.8)
pwr.chisq.test(w=1,N=, df=1, sig.level=0.05,power=0.8)





# model 3
# question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
# no, effect direction opposite expectation = palatable termite attacked first were attacked after a longer delay than bitrex termites attacked first

head(FirstAttacks)

mod3 <- lm(DelayToAttack ~ AttackedTermitePalatability, data = FirstAttacks)
summary(mod3)




