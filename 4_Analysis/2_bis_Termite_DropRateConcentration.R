#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data analysis drop rate in function of concentration
#	 Start : 21 march 2019
#	 commit: one test using ALL data
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
library(ggplot2)


# load ALL data
AllAttacks1 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks1.csv", header=TRUE, sep=",") # 100
AllAttacks15 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks15.csv", header=TRUE, sep=",") # 30 
AllAttacks2 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks2.csv", header=TRUE, sep=",") # 30
AllAttacks3 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks3.csv", header=TRUE, sep=",") # 30
AllAttacks3F <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks3F.csv", header=TRUE, sep=",") # 79


AllAttacks1$Concentration <- 1
AllAttacks15$Concentration <- 1.5
AllAttacks2$Concentration <- 2
AllAttacks3$Concentration <- 3
AllAttacks3F$Concentration <- 3

AllAttacks1 <- AllAttacks1[AllAttacks1$AttackedPalatabilityTreatment == 'DB',]
AllAttacks15 <- AllAttacks15[AllAttacks15$AttackedPalatabilityTreatment == 'DB',]
AllAttacks2 <- AllAttacks2[AllAttacks2$AttackedPalatabilityTreatment == 'DB',]
AllAttacks3 <- AllAttacks3[AllAttacks3$AttackedPalatabilityTreatment == 'DB',]
AllAttacks3F <- AllAttacks3F[AllAttacks3F$AttackedPalatabilityTreatment == 'DB',]


AllAttacks <- rbind(
  AllAttacks1 ,
  AllAttacks15,
  AllAttacks2 ,
  AllAttacks3 ,
  AllAttacks3F )
  
head(AllAttacks)
nrow(AllAttacks)


# stats

table(AllAttacks$Fate,AllAttacks$Concentration) # dropping rate of bitrex termite = 85.3%

mod1 <- glmer (DropYN ~ poly(Concentration,2) + (1|FID)
               ,family = 'binomial', data = AllAttacks) # only DB attacks
summary(mod1)
drop1(mod1, test="Chisq")

    # sunflowerplot(AllAttacks$DropYN~ AllAttacks$Concentration) # ugly



# plot

mod <- glmer (DropYN ~ -1 + as.factor(Concentration) + (1|FID)
               ,family = 'binomial', data = AllAttacks) # only DB attacks
summary(mod)

effects_table <- as.data.frame(cbind(est=invlogit(summary(mod)$coeff[,1]),
                                     CIhigh=invlogit(summary(mod)$coeff[,1]+summary(mod)$coeff[,2]*1.96),
                                     CIlow=invlogit(summary(mod)$coeff[,1]-summary(mod)$coeff[,2]*1.96)))
effects_table$Concentration <- c("1", "1.5","2", "3")


ggplot(data=effects_table, aes(x=Concentration, y=est)) + 
  scale_y_continuous(name="DB prey rejection probability", 
                     limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1), labels=scales::percent)+ # 0.75 converted to 75%
  theme_classic() + # white backgroun, x and y axis (no box)
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4)+ # don't plot bor bars on x axis tick, but separate them (dodge)
  geom_point(size =4, stroke = 1) +
   theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 10))








