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
AllAttacks <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks_Bug.csv", header=TRUE, sep=",")
FirstAttacks <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks_Bug.csv", header=TRUE, sep=",")

AllAttacks$rowID <- 1:nrow(AllAttacks)

head(AllAttacks)    
head(FirstAttacks)




# model drop rate 
# question 2 (confirmatory): are MW Bug more likely to be dropped?
# --> yes, *******
# question 3 (exploratory): are Bug from a certain color more likely to be dropped?
# --> no
# question 4 (exploratory): are MW Bugs always dropped
# yes


mod2 <- glm (DropYN ~ AttackedColor + AttackedPalatabilityTreatment  #+ (1|FID)
             ,family = 'binomial', data = AllAttacks)
summary(mod2) ## <<<<<< super weird P value for ultra significant effect >>>>>>>>>>>>>> use LRT drop1 function
drop1(mod2, test="Chisq")
par(mfrow=c(2,2))
plot(mod2)  ## I believe test not valid as residual variance super heteroscedastic

#sunflowerplot(AllAttacks$DropYN,AllAttacks$AttackedBugPalatability)
contingency_tbl <- table(AllAttacks$Fate,AllAttacks$AttackedPalatabilityTreatment) # dropping rate of milkweed Bug = 100%, SF bugs = 40%
chisq.test(table(AllAttacks$Fate,AllAttacks$AttackedPalatability))
chisq.test(table(AllAttacks$Fate,AllAttacks$AttackedColor))



# model delay
# question 1 (exploratory): delay to first attack longer if attack the bitrex Bug?
# no, effect according to expectation = palatable Bug attacked first were attacked after a shorter delay than MW Bugs attacked first

mod3 <- lm(DelayToAttack ~ AttackedPalatabilityTreatment, data = FirstAttacks)
summary(mod3)
drop1(mod3, test="Chisq")



# Several Opetions for figure drop rate

par(mfrow=c(2,2))
barplot(c(100,40),
        ylim = c(0,100),
        xlab="Palatability",
        ylab= "Prey rejection probability (%)",
        names.arg = c("Milkweed", "Sunflower")
)

barplot(contingency_tbl,
        ylim = c(0,50),
        xlab="Palatability treatment",
        ylab= "Number of prey attacked",
        names.arg = c("Milkweed", "Sunflower"),
        legend = rownames(contingency_tbl)
)

contingency_tbl
tbl_ggplot <- data.frame(Palatability = c('Milkweed', 'Milkweed', 'Sunflower', 'Sunflower'),
                         Outcome = c('Rejected', 'Consumed', 'Rejected', 'Consumed'),
                         Count = c(41,0,10,15))


BugAttack_dr <- ggplot(tbl_ggplot, aes(x=Palatability, y=Count, fill=Outcome)) + 
  geom_bar(stat="identity")+
  labs(y = "Number of prey rejected or consumed after attack", x = 'Palatability treatment')+      
  scale_x_discrete(labels=c("Milkweed", "Sunflower"))  +
  theme_classic() +
  scale_fill_grey() +  
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.title=element_blank(),
        legend.position = c(0.75,0.75))#+
#guides(fill = guide_legend(label.hjust = 6)) # only working for one of the two keys...




setEPS() 
pdf(paste(here(), "5_Figures/DropRate/FigBugDR.pdf",sep="/"), height=5, width=3.3)
BugAttack_dr
dev.off()


