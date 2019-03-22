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



# functions to replicate

nrep <- 1000

sample_focal_run_model_with_training <- function(df) {
  
  FocalAttack <- split(df, df$FID)
  
  FocalAttack_fun <- function(x){
    x$FocalYN <- sample(c(0,1), 2,replace=FALSE) # randomly assigning YN, determining whether the termite is actually focal or not
    
    return(x[x$FocalYN == 1,])
  }
  
  FocalAttack <- do.call(rbind,lapply(FocalAttack,FocalAttack_fun))
  rownames(FocalAttack) <- NULL
  
  mod <- glm (FocalAttackedYN ~ FocalColor + FocalPalatabilityTreatment*PriorExposure, family = 'binomial', data = FocalAttack)
  summary(mod)
  drop1(mod, test="Chisq")[,c('LRT','Pr(>Chi)')]

  return(list(drop1(mod, test="Chisq")[,c('LRT','Pr(>Chi)')]))
  
}


sample_focal_run_model_without_training <- function(df) {
  
  FocalAttack <- split(df, df$FID)
  
  FocalAttack_fun <- function(x){
    x$FocalYN <- sample(c(0,1), 2,replace=FALSE) # randomly assigning YN, determining whether the termite is actually focal or not
    
    return(x[x$FocalYN == 1,])
  }
  
  FocalAttack <- do.call(rbind,lapply(FocalAttack,FocalAttack_fun))
  rownames(FocalAttack) <- NULL
  
  mod <- glm (FocalAttackedYN ~ FocalColor + FocalPalatabilityTreatment, family = 'binomial', data = FocalAttack)
  summary(mod)
  drop1(mod, test="Chisq")[,c('LRT','Pr(>Chi)')]
  
  return(list(drop1(mod, test="Chisq")[,c('LRT','Pr(>Chi)')]))
  
}



# DB concentration = 1%

effects_table1_list <- pbreplicate(nrep, sample_focal_run_model_with_training(FocalAttacks1))
effects_table1 <- Reduce(`+`, effects_table1_list) / length(effects_table1_list)
effects_table1

# DB concentration = 1.5%

effects_table15_list <- pbreplicate(nrep, sample_focal_run_model_without_training(FocalAttacks15))
effects_table15 <- Reduce(`+`, effects_table15_list) / length(effects_table15_list)
effects_table15


# DB concentration = 2%

effects_table2_list <- pbreplicate(nrep, sample_focal_run_model_without_training(FocalAttacks2))
effects_table2 <- Reduce(`+`, effects_table2_list) / length(effects_table2_list)
effects_table2

# DB concentration = 3%

effects_table3_list <- pbreplicate(nrep, sample_focal_run_model_without_training(FocalAttacks3))
effects_table3 <- Reduce(`+`, effects_table3_list) / length(effects_table3_list)
effects_table3

# DB concentration = 3F%

effects_table3F_list <- pbreplicate(nrep, sample_focal_run_model_with_training(FocalAttacks3F))
effects_table3F <- Reduce(`+`, effects_table3F_list) / length(effects_table3F_list)
effects_table3F


# DB concentration = 0%
# question 3 (exploratory): bias against a color ?
# --> no, effect direction: slight preference to attack the green first

table(FirstAttacks$AttackedColor)

chisq.test(c(13,17),p=c(0.5,0.5))










