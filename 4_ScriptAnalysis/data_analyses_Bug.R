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
AllAttacks <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacksBug.csv", header=TRUE, sep=",")
FocalBugAttack <- read.csv(file = "3_ExtractedData/FocalAttacks/FocalBugAttack.csv", header=TRUE, sep=",")
FirstAttacks <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacksBug.csv", header=TRUE, sep=",")

FocalBugAttack$FocalBugPalatability <- as.factor(FocalBugAttack$FocalBugPalatability)
AllAttacks$AttackedBugPalatability <- as.factor(AllAttacks$AttackedBugPalatability)
FirstAttacks$AttackedBugPalatability <- as.factor(FirstAttacks$AttackedBugPalatability)


head(AllAttacks)    
head(FocalBugAttack)
head(FirstAttacks)



# model 1: 
# question 2 (confirmatory): reluctant to attack bitrex Bug first regardless of their training?  
# --> main effect of Palatability trendy, effect direction against expectation = palatable one less likely to be attacked first                                                       
# question 3 (exploratory): bias against a color ?
# --> no (green less likely to be attacked first)

str(FocalBugAttack)

mod1 <- glm (FocalBugAttackedYN ~ FocalBugColor + FocalBugPalatability, family = 'binomial', data = FocalBugAttack)
summary(mod1)
drop1(mod1, test="Chisq")


# model 2: 
# question 2 (confirmatory): are MW Bug more likely to be dropped?
# --> yes, *******
# question 3 (exploratory): are Bug from a certain color more likely to be dropped?
# --> no
# question 4 (exploratory): are MW Bugs always dropped
# yes


str(AllAttacks)

mod2 <- glm (DropYN ~ AttackedBugColor + AttackedBugPalatability  #+ (1|FID)
               ,family = 'binomial', data = AllAttacks)
summary(mod2) ## <<<<<< super weird P value for ultra significant effect >>>>>>>>>>>>>> use LRT drop1 function
drop1(mod2, test="Chisq")

#sunflowerplot(AllAttacks$DropYN,AllAttacks$AttackedBugPalatability)
table(AllAttacks$Outcome,AllAttacks$AttackedBugPalatability) # dropping rate of milkweed Bug = 100%


# model 3
# question 1 (exploratory): delay to first attack longer if attack the bitrex Bug?
# no, effect according to expectation = palatable Bug attacked first were attacked after a shorter delay than MW Bugs attacked first

str(FirstAttacks)

mod3 <- lm(DelayToAttack ~ AttackedBugPalatability, data = FirstAttacks)
summary(mod3)
drop1(mod3, test="Chisq")



# figure likelihood attack

  mod1_noIntercept <- glm (FocalBugAttackedYN ~  - 1 +FocalBugPalatability + FocalBugColor, family = 'binomial', data = FocalBugAttack)
  summary(mod1_noIntercept)
  
  effects_table <- as.data.frame(cbind(est=invlogit(summary(mod1_noIntercept)$coeff[,1]),
                                       CIhigh=invlogit(summary(mod1_noIntercept)$coeff[,1]+summary(mod1_noIntercept)$coeff[,2]*1.96),
                                       CIlow=invlogit(summary(mod1_noIntercept)$coeff[,1]-summary(mod1_noIntercept)$coeff[,2]*1.96)))
  effects_table <- effects_table[-nrow(effects_table),]
  effects_table$Palatability <- c("Milkweed","Sunflower")
  effects_table


BugAttack_lkh <- ggplot(data=effects_table, aes(x=Palatability, y=est)) + 
  scale_y_continuous(name="Prey probability of being attacked first", 
                     limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1), labels=scales::percent)+ # 0.75 converted to 75%
  theme_classic() + # white backgroun, x and y axis (no box)
  #labs(title = "...") +
  
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4)+ # don't plot bor bars on x axis tick, but separate them (dodge)
  geom_point(size =4, stroke = 1) +
  theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 10))

setEPS() 
pdf(paste(here(), "5_Figures/AttackLikelihood/FigBugLkh.pdf",sep="/"), height=5, width=3.3)
BugAttack_lkh
dev.off()

# Figure drop rate

mod2_noIntercept <- mod2 <- glm (DropYN ~  -1 + AttackedBugPalatability + AttackedBugColor #+ (1|FID)
                                 ,family = 'binomial', data = AllAttacks)

effects_table_dr <- as.data.frame(cbind(est=invlogit(summary(mod2_noIntercept)$coeff[,1]),
                                     CIhigh=invlogit(summary(mod2_noIntercept)$coeff[,1]+summary(mod2_noIntercept)$coeff[,2]*1.96),
                                     CIlow=invlogit(summary(mod2_noIntercept)$coeff[,1]-summary(mod2_noIntercept)$coeff[,2]*1.96)))
effects_table_dr <- effects_table_dr[-nrow(effects_table_dr),]
effects_table_dr$Palatability <- c("Milkweed","Sunflower")
effects_table_dr



BugAttack_dr <- ggplot(data=effects_table_dr, aes(x=Palatability, y=est)) + 
  scale_y_continuous(name="Prey rejection probability", 
                     limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1), labels=scales::percent)+ # 0.75 converted to 75%
  theme_classic() + # white backgroun, x and y axis (no box)
  #labs(title = "") +
  
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4)+ # don't plot bor bars on x axis tick, but separate them (dodge)
  geom_point(size =4, stroke = 1) +
  theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 10))

setEPS() 
pdf(paste(here(), "5_Figures/DropRate/FigBugDR.pdf",sep="/"), height=5, width=3.3)
BugAttack_dr
dev.off()


