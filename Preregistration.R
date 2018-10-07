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
  #library(lme4) # for mixed effect models (not needed for simple glm)
  library(pbapply) # to replicate a function / a simulation multiple time with a bar of progress
  #library(ggplot2) # for plot
  #library(sjPlot) # for interaction plot
  #library(sjmisc) # for interaction plot
  }


nF <- 100 # number of females to be tested
pbrep <- 100 # number of simulation replicates
probsnaive <- 0.5 # probability of attacking the bitter prey when never exposed to the bitter compound - needs to be 0.25 to always detect the effect
probswhenexposed <- 0.3 # probability of attacking the bitter prey when trained on the bitter compound - needs to be 0.05 to always detect the interaciton (if previous is 0.25)

### two-by-two design 
FPriorExposure <- c(1,1,1,1,0,0,0,0)
FColorGroup <- c('Green','Green','Beige','Beige','Green','Green','Beige','Beige') # the color that will contain DB, the other color will contain water
### headers of contingency table
TermiteEaten <- c('Water','DB','Water','DB','Water','DB','Water','DB') # in one test, either the DB termite or the water termite has to be attacked for the test to end
TermiteEatenColor <- c('Beige','Green','Green','Beige','Beige','Green','Green','Beige') # deduced from FcolorGroup and Termite Eaten

# simulation of an effect of the bitter compound (say smell) onto that attack, if the termite has the bitter compound, prob of attack is = probs
 
## Function to check number of significant result by chance
Simulate_and_analyse <-function(){

 ### generate a number of spider attacking the DB termite, given the probability of attacking it
        GreenDBNoExp <- sum(sample(c(1,0),nF/4, prob = c(probsnaive, 1-probsnaive), replace=TRUE))
        BeigeDBNoExp <- sum(sample(c(1,0),nF/4, prob = c(probsnaive, 1-probsnaive), replace=TRUE))
        GreenDBExp <-sum(sample(c(1,0),nF/4, prob = c(probswhenexposed, 1-probswhenexposed),replace=TRUE))
        BeigeDBExp <-sum(sample(c(1,0),nF/4, prob = c(probswhenexposed, 1-probswhenexposed), replace=TRUE))
    
  ### the number of spiders attacking the water termite is the number of spider tested in the two-by-two group minus the number of spider that attacked the DB termite 
Freq <- c(nF/4 - GreenDBExp,GreenDBExp, nF/4 - BeigeDBExp, BeigeDBExp, nF/4 - GreenDBNoExp,GreenDBNoExp, nF/4 - BeigeDBNoExp, BeigeDBNoExp)

  ### in contingency table, diagnals should sum up to nF/4
contingencytable <- xtabs(Freq~TermiteEatenColor+TermiteEaten+FPriorExposure)
FreqTable <- as.data.frame.table(contingencytable)

modFreq0 <- glm(Freq ~ TermiteEatenColor+TermiteEaten+FPriorExposure, family = 'poisson', data = FreqTable)
modFreq1 <- glm(Freq ~ TermiteEatenColor+TermiteEaten*FPriorExposure, family = 'poisson', data = FreqTable)
summary(modFreq1)

anova(modFreq0,modFreq1,test='Chi')


        ##### plot_model(modFreq1, type = "pred", terms = c("TermiteEaten", "FPriorExposure"))


## to p value
modFreq1p <-  coef(summary(modFreq1))[-1, 4]

return(list(modFreq1p))  
}  


OutputSimulation <- do.call(rbind, pbreplicate(pbrep,Simulate_and_analyse())) # collect all p values for both factors in the models
OutputSimulation <- OutputSimulation<0.05 # determine whether or not their are significant
colSums(OutputSimulation)/pbrep # count the number of significant p values out of the number of simulation replicate. 
##### factors where no effect was simulated should have a percentage of false positive effect under 5%
##### factors with simulated effect should detect an effect in at least more than 5% of the cases