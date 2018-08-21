#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Preregistration Quinine induced Unpalatability 
#  simulation of data to see whether planned analyses code works
#	 Start : 20 August 2018
#	 last modif : 
#	 commit: start
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



rm(list = ls(all = TRUE))

{# packages
  library(lme4)
  library(pbapply)
}


nF <- 60 # number of females to be tested
pbrep <- 1000 # number of simulation replicates
probs <- 0.55 # probability of DpdtYN being yes


#Function to check number of significant result by chance
Simulate_and_analyse <-function(){
  
# simulation data

FID <- 1:nF
FTrt <- c(rep('Green',nF/2), rep ('Beige',nF/2)) # color containing quinine
FocalTermiteColor <- rep(c('Green','Beige'), nF/2 ) # random (alternated) focal termite
FocalTermiteQuinine <- c(rep(c('1','0'), nF/4 ),rep(c('0','1'), nF/4 )) # whether or not the focal termite is the one with quinine
DelayTo <- rnorm(nF, mean = 15, sd = 7) 

MY_TABLE <- data.frame(FID, FTrt, DelayTo, FocalTermiteColor,FocalTermiteQuinine, row.names = NULL)

# simulation of an effect of quinine (say smell) onto that attack likelihood of the focal termite, if the termite has quinine, prob of attack of the focal is = probs
for (i in 1:nrow(MY_TABLE)) {
if (MY_TABLE$FocalTermiteQuinine[i] == 1)
  MY_TABLE$DpdtYN[i] <- sample(c(1,0),1, prob = c(probs, 1-probs))
  
if (MY_TABLE$FocalTermiteQuinine[i] == 0)
  MY_TABLE$DpdtYN[i] <- sample(c(0,1),1, prob = c(probs, 1-probs))
}

#sunflowerplot(MY_TABLE$DpdtYN,FocalTermiteQuinine)


mod <- glm( DpdtYN ~ FocalTermiteColor + FocalTermiteQuinine, data = MY_TABLE)
summary(mod)

## to get one sided test p value
modp <-  coef(summary(mod))[-1, 4]


return(list(modp))  
}  

OutputSimulation <- do.call(rbind, pbreplicate(pbrep,Simulate_and_analyse())) # collect all p values for both factors in the models
OutputSimulation <- OutputSimulation<0.05 # determine whetehr or not their are significant
colSums(OutputSimulation)/pbrep # count the number of significant p values out of the number of simulation replicate. 
# factors where no effect was simulated should have a percentage of false positive effect under 5%
# factors with simulated effect should detect an effect in at least more than 5% of the cases

