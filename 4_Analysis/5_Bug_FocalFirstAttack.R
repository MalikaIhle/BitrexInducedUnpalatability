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
FocalBugAttacks <- read.csv(file = "3_ExtractedData/FocalAttacks/FocalBugAttacks.csv", header=TRUE, sep=",")
head(FocalBugAttacks)


# model 
mod1 <- glm (FocalBugAttackedYN ~ FocalBugColor + FocalBugPalatabilityTreatment, family = 'binomial', data = FocalBugAttacks)
summary(mod1)
drop1(mod1, test="Chisq")


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

