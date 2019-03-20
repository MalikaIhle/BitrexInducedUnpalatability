#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data analysis
#	 Start : 31 october 2018
#	 last modif : 20190219
#	 commit: determine sample size for follow up experiment with higher bitrex concentration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## rk:
# palatability = 0 > DB termite
# palatability = 1 > water termite

rm(list = ls(all = TRUE))

# packages
library(lme4)
library(arm)



AllAttacks <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks.csv", header=TRUE, sep=",")
FocalTermiteAttack <- read.csv(file = "3_ExtractedData/FocalAttacks/FocalTermiteAttack.csv", header=TRUE, sep=",")
FirstAttacks <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks.csv", header=TRUE, sep=",")

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
# --> main effect of Palatability not significant, effect direction according to expectation = palatable one more likely to be attacked first (no effect)                                                           
# question 3 (exploratory): bias against a color ?
# --> no, effect direction: have a slight (no) preference to attack the green first

str(FocalTermiteAttack)

mod1 <- glm (FocalTermiteAttackedYN ~ FocalTermiteColor + FocalTermitePalatability*PriorExposureYN, family = 'binomial', data = FocalTermiteAttack)
summary(mod1)
drop1(mod1, test="Chisq")


# model 2: 
# question 1 (exploratory):  drop bitrex termite less so if trained/habituated?
# --> absolutely no interaction between training and palatability on dropYN; interaction removed from model 
# question 2 (confirmatory): are bitrex termite more likely to be dropped?
# --> yes, ***
# question 3 (exploratory): are termite from a certain color more likely to be dropped?
# --> yes, they are more likely ** to drop a green termite
# question 4 (exploratory): are bitrex termites always dropped
# no, out of 84 bitrex termites attacked, 39 were consumed (46.4%)
# it might be worth trying with a higher concentration of bitrex

str(AllAttacks)

mod2 <- glmer (DropYN ~ AttackedTermiteColor + AttackedTermitePalatability
               *PriorExposureYN 
               + (1|FID)
               ,family = 'binomial', data = AllAttacks)
summary(mod2)
drop1(mod2, test="Chisq")

#sunflowerplot(AllAttacks$DropYN,AllAttacks$AttackedTermitePalatability)
table(AllAttacks$Outcome,AllAttacks$AttackedTermitePalatability)

DropUnpalatable <- invlogit(coef(summary(mod2))[1, 1]) # 39.4%
DropUnpalatable_CI_low <-  invlogit(coef(summary(mod2))[1, 1]-coef(summary(mod2))[1, 2]*1.96) # 24.6%
DropUnpalatable_CI_high <-invlogit(coef(summary(mod2))[1, 1]+coef(summary(mod2))[1, 2]*1.96) # 56.5%

DropPalatable <- invlogit(coef(summary(mod2))[1, 1] + coef(summary(mod2))[3, 1]) # 9.9%
DropPalatable_CI_low <-invlogit(coef(summary(mod2))[1, 1]+ coef(summary(mod2))[3, 1]-coef(summary(mod2))[3, 2]*1.96) # 4.6%
DropPalatable_CI_high <-invlogit(coef(summary(mod2))[1, 1]+ coef(summary(mod2))[3, 1]+coef(summary(mod2))[3, 2]*1.96) # 20.1%

{# try out power analyses for next exp
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

}



# model 3
# question 1 (exploratory): delay to first attack longer if attack the bitrex termite?
# no, effect direction opposite expectation = palatable termite attacked first were attacked after a longer delay than bitrex termites attacked first

str(FirstAttacks)

mod3 <- lm(DelayToAttack ~ AttackedTermitePalatability, data = FirstAttacks)
summary(mod3)
drop1(mod3, test="Chisq")


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
fisher.test(cbind(attackafterbitrex,firstattack), alternative='less') # p =1
