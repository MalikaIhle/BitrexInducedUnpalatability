#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data analysis
#	 Start : 18 march 2019
#	 last modif : 20190318
#	 commit: first
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

# packages
library(lme4)
library(arm)
library(ggplot2)
library(here)


# load data
FocalAttacks <- read.csv(file = "3_ExtractedData/FocalAttacks/FocalAttacks_Bug.csv", header=TRUE, sep=",")
head(FocalAttacks)



# function to replicate

nrep <- 1000

sample_focal_run_model <- function(df) {
  
  FocalAttack <- split(df, df$FID)
  
  FocalAttack_fun <- function(x){
    x$FocalYN <- sample(c(0,1), 2,replace=FALSE) # randomly assigning YN, determining whether the bug is actually focal or not
    
    return(x[x$FocalYN == 1,])
  }
  
  FocalAttack <- do.call(rbind,lapply(FocalAttack,FocalAttack_fun))
  rownames(FocalAttack) <- NULL
  
  mod <- glm (FocalAttackedYN ~ FocalColor + FocalPalatabilityTreatment, family = 'binomial', data = FocalAttack)
  summary(mod)
  drop1(mod, test="Chisq")[,c('LRT','Pr(>Chi)')]
  
  return(list(drop1(mod, test="Chisq")[,c('LRT','Pr(>Chi)')]))
  
}



# results

effects_table_list <- pbreplicate(nrep, sample_focal_run_model(FocalAttacks))
effects_table <- Reduce(`+`, effects_table_list) / length(effects_table_list)
effects_table




# figure likelihood attack

sample_focal_run_model_NoIntercept <- function(df) {
  
  FocalAttack <- split(df, df$FID)
  
  FocalAttack_fun <- function(x){
    x$FocalYN <- sample(c(0,1), 2,replace=FALSE) # randomly assigning YN, determining whether the bug is actually focal or not
    
    return(x[x$FocalYN == 1,])
  }
  
  FocalAttack <- do.call(rbind,lapply(FocalAttack,FocalAttack_fun))
  rownames(FocalAttack) <- NULL
  
  mod_noIntercept <- glm (FocalAttackedYN ~ -1 + FocalPalatabilityTreatment + scale(FocalColorCode)
                          , family = 'binomial', data = FocalAttack)
  summary(mod_noIntercept)
  
  effects_table <- as.data.frame(cbind(est=invlogit(summary(mod_noIntercept)$coeff[,1]),
                                       CIhigh=invlogit(summary(mod_noIntercept)$coeff[,1]+summary(mod_noIntercept)$coeff[,2]*1.96),
                                       CIlow=invlogit(summary(mod_noIntercept)$coeff[,1]-summary(mod_noIntercept)$coeff[,2]*1.96)))
  effects_table <- effects_table[-nrow(effects_table),]

  return(list(effects_table))
  
}

effects_table_NoIntercept_list <- pbreplicate(nrep, sample_focal_run_model_NoIntercept(FocalAttacks))
effects_table_NoIntercept <- Reduce(`+`, effects_table_NoIntercept_list) / length(effects_table_NoIntercept_list)
effects_table_NoIntercept$Palatability <- c("Milkweed","Sunflower")


BugAttack_lkh <- ggplot(data=effects_table_NoIntercept, aes(x=Palatability, y=est)) + 
  scale_y_continuous(name="Prey probability of being attacked first", 
                     limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1), labels=scales::percent)+ # 0.75 converted to 75%
  theme_classic() + # white backgroun, x and y axis (no box)
  #labs(title = "...") +
  
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4)+ # don't plot bor bars on x axis tick, but separate them (dodge)
  geom_point(size =4, stroke = 1) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "grey48") +
  theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 10))



setEPS() 
pdf(paste(here(), "5_Figures/AttackLikelihood/FigBugLkh.pdf",sep="/"), height=5, width=3.3)
BugAttack_lkh
dev.off()

