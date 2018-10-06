#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Preregistration manipulation color and unpalatability 
#  simulation of data to see whether planned analyses code works
#	 Start : 20 August 2018
#	 last modif : 05 october 2018
#	 commit: add prior exposure to half the subjects 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



rm(list = ls(all = TRUE))

{# packages
  library(lme4) # for mixed effect models (not needed for simple glm)
  library(pbapply)
}



nF <- 100 # number of females to be tested
pbrep <- 2 # number of simulation replicates
probsnaive <- 0.3 # probability of attacking the bitter prey when never exposed to the bitter compound
probswhenexposed <- 0.3  # probability of the bitter prey when trained on the bitter compound


FPriorExposure <- c(1,1,1,1,0,0,0,0)
FColorGroup <- c('Green','Green','Beige','Beige','Green','Green','Beige','Beige')
TermiteEaten <- c('Water','DB','Water','DB','Water','DB','Water','DB')
TermiteEatenColor <- c('Beige','Green','Green','Beige','Beige','Green','Green','Beige')

# simulation of an effect of the bitter compound (say smell) onto that attack likelihood of the focal termite, if the termite has the bitter compound, prob of attack of the focal is = probs

        GreenDBNoExp <- sum(sample(c(1,0),nF/4, prob = c(probsnaive, 1-probsnaive), replace=TRUE))
        BeigeDBNoExp <- sum(sample(c(1,0),nF/4, prob = c(probsnaive, 1-probsnaive), replace=TRUE))
        GreenDBExp <-sum(sample(c(1,0),1, prob = c(probswhenexposed, 1-probswhenexposed)))
        BeigeDBExp <-sum(sample(c(1,0),1, prob = c(probswhenexposed, 1-probswhenexposed)))
    
   
Freq <- c(nF/4 - GreenDBExp,GreenDBExp, nF/4 - BeigeDBExp, BeigeDBExp, nF/4 - GreenDBNoExp,GreenDBNoExp, nF/4 - BeigeDBNoExp, BeigeDBNoExp)


contingencytable <- xtabs(Freq~TermiteEatenColor+TermiteEaten+FPriorExposure)
FreqTable <- as.data.frame.table(contingencytable)

summary(glm(Freq ~ TermiteEatenColor+TermiteEaten*FPriorExposure, family = 'poisson', data = FreqTable))















nF <- 100 # number of females to be tested
pbrep <- 2 # number of simulation replicates
probsnaive <- 0.3 # probability of FirstAttackYN being yes when never exposed to the bitter compound
probswhenexposed <- 0.3  # probability of FirstAttackYN being yes when trained on the bitter compound

#Function to check number of significant result by chance
#Simulate_and_analyse <-function(){
  
# simulation data

FID <- 1:nF
FTrt <- c(rep('Green',nF/2), rep ('Beige',nF/2)) # color containing the bitter compound
FocalTermiteColor <- rep(c('Green','Beige'), nF/2 ) # random (alternated) focal termite
FocalTermiteBitterness <- c(rep(c('1','0'), nF/4 ),rep(c('0','1'), nF/4 )) # whether or not the focal termite is the one with the bitter compound
FpriorExposure <- rep(c(rep(1,nF/4),rep(0,nF/4)),2)

MY_TABLE_FirstAttack <- data.frame(FID, FTrt, FocalTermiteColor,FocalTermiteBitterness, FpriorExposure, row.names = NULL)

# simulation of an effect of the bitter compound (say smell) onto that attack likelihood of the focal termite, if the termite has the bitter compound, prob of attack of the focal is = probs
for (i in 1:nrow(MY_TABLE_FirstAttack)) 
{
 if (MY_TABLE_FirstAttack$FpriorExposure[i] == 0) 
 {
   if (MY_TABLE_FirstAttack$FocalTermiteBitterness[i] == 1)
         MY_TABLE_FirstAttack$FirstAttackYN[i] <- sample(c(1,0),1, prob = c(probsnaive, 1-probsnaive))
  
   if (MY_TABLE_FirstAttack$FocalTermiteBitterness[i] == 0)
         MY_TABLE_FirstAttack$FirstAttackYN[i] <- sample(c(0,1),1, prob = c(probsnaive, 1-probsnaive))
  }
   
 if (MY_TABLE_FirstAttack$FpriorExposure[i] == 1) 
 {
   if (MY_TABLE_FirstAttack$FocalTermiteBitterness[i] == 1)
     MY_TABLE_FirstAttack$FirstAttackYN[i] <- sample(c(1,0),1, prob = c(probswhenexposed, 1-probswhenexposed))
   
   if (MY_TABLE_FirstAttack$FocalTermiteBitterness[i] == 0)
     MY_TABLE_FirstAttack$FirstAttackYN[i] <- sample(c(0,1),1, prob = c(probswhenexposed, 1-probswhenexposed))
 }   
}

for (i in 1:nrow(MY_TABLE_FirstAttack)) 
{
  if (MY_TABLE_FirstAttack$FocalTermiteBitterness[i] == 0 & MY_TABLE_FirstAttack$FirstAttackYN[i] == 0)
  {MY_TABLE_FirstAttack$AttackBitterYN[i] <- 1}
  if (MY_TABLE_FirstAttack$FocalTermiteBitterness[i] == 0 & MY_TABLE_FirstAttack$FirstAttackYN[i] == 1)
  {MY_TABLE_FirstAttack$AttackBitterYN[i] <- 0}
  if (MY_TABLE_FirstAttack$FocalTermiteBitterness[i] == 1 & MY_TABLE_FirstAttack$FirstAttackYN[i] == 0)
  {MY_TABLE_FirstAttack$AttackBitterYN[i] <- 0}
  if (MY_TABLE_FirstAttack$FocalTermiteBitterness[i] == 1 & MY_TABLE_FirstAttack$FirstAttackYN[i] == 1)
  {MY_TABLE_FirstAttack$AttackBitterYN[i] <- 1}
}


FPriorExposure <- c(1,1,1,1,0,0,0,0)
FColorGroup <- c('Green','Green','Beige','Beige','Green','Green','Beige','Beige')
TermiteEaten <- c('Water','DB','Water','DB','Water','DB','Water','DB')
Freq <- c(length(MY_TABLE_FirstAttack$AttackBitterYN[MY_TABLE_FirstAttack$FpriorExposure == 1 & 
                                                     MY_TABLE_FirstAttack$FocalTermiteColor == 'Green' & 
                                                     MY_TABLE_FirstAttack$AttackBitterYN == 0]),
          length(MY_TABLE_FirstAttack$AttackBitterYN[MY_TABLE_FirstAttack$FpriorExposure == 1 & 
                                                       MY_TABLE_FirstAttack$FocalTermiteColor == 'Green' & 
                                                       MY_TABLE_FirstAttack$AttackBitterYN == 1]),
          length(MY_TABLE_FirstAttack$AttackBitterYN[MY_TABLE_FirstAttack$FpriorExposure == 1 & 
                                                       MY_TABLE_FirstAttack$FocalTermiteColor == 'Beige' & 
                                                       MY_TABLE_FirstAttack$AttackBitterYN == 0]), 
          length(MY_TABLE_FirstAttack$AttackBitterYN[MY_TABLE_FirstAttack$FpriorExposure == 1 & 
                                                       MY_TABLE_FirstAttack$FocalTermiteColor == 'Beige' & 
                                                       MY_TABLE_FirstAttack$AttackBitterYN == 1]), 
          length(MY_TABLE_FirstAttack$AttackBitterYN[MY_TABLE_FirstAttack$FpriorExposure == 0 & 
                                                       MY_TABLE_FirstAttack$FocalTermiteColor == 'Green' & 
                                                       MY_TABLE_FirstAttack$AttackBitterYN == 0]), 
          length(MY_TABLE_FirstAttack$AttackBitterYN[MY_TABLE_FirstAttack$FpriorExposure == 0 & 
                                                       MY_TABLE_FirstAttack$FocalTermiteColor == 'Green' & 
                                                       MY_TABLE_FirstAttack$AttackBitterYN == 1]), 
          length(MY_TABLE_FirstAttack$AttackBitterYN[MY_TABLE_FirstAttack$FpriorExposure == 0 & 
                                                       MY_TABLE_FirstAttack$FocalTermiteColor == 'Beige' & 
                                                       MY_TABLE_FirstAttack$AttackBitterYN == 0]), 
          length(MY_TABLE_FirstAttack$AttackBitterYN[MY_TABLE_FirstAttack$FpriorExposure == 0 & 
                                                       MY_TABLE_FirstAttack$FocalTermiteColor == 'Beige' & 
                                                       MY_TABLE_FirstAttack$AttackBitterYN == 1])) 
contingencytable <- xtabs(Freq~+FColorGroup+TermiteEaten+FPriorExposure)
FreqTable <- as.data.frame.table(contingencytable)

MY_TABLE_FirstAttack

modfirstattack <- glm( FirstAttackYN ~ FocalTermiteColor + FocalTermiteBitterness*FpriorExposure, family = "binomial",data = MY_TABLE_FirstAttack)
summary(modfirstattack)

library(sjPlot)
library(sjmisc)
library(ggplot2)
plot_model(modfirstattack, type = "pred", terms = c("FocalTermiteBitterness", "FpriorExposure"))

## to p value
modfirstattackp <-  coef(summary(modfirstattack))[-1, 4]

return(list(modfirstattackp))  
}  

OutputSimulation <- do.call(rbind, pbreplicate(pbrep,Simulate_and_analyse())) # collect all p values for both factors in the models
OutputSimulation <- OutputSimulation<0.05 # determine whetehr or not their are significant
colSums(OutputSimulation)/pbrep # count the number of significant p values out of the number of simulation replicate. 
# factors where no effect was simulated should have a percentage of false positive effect under 5%
# factors with simulated effect should detect an effect in at least more than 5% of the cases
