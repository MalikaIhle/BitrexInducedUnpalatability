# Create panelled figure for drop rate

rm(list = ls(all = TRUE))

# packages

library(here) # to get path of the Rproj root
library(lme4) # for glmer
library(arm) # for invlogit
library(ggplot2) # to plot
library(gridExtra) # for function gridarrange


# load data

AllAttacks0 <- read.csv(file = paste(here(),"3_ExtractedData/AllAttacks/AllAttacks0.csv", sep='/'), header=TRUE, sep=",")
AllAttacks1 <- read.csv(file = paste(here(),"3_ExtractedData/AllAttacks/AllAttacks1.csv", sep='/'), header=TRUE, sep=",")
AllAttacks15 <- read.csv(file = paste(here(),"3_ExtractedData/AllAttacks/AllAttacks15.csv", sep='/'), header=TRUE, sep=",")
AllAttacks2 <- read.csv(file = paste(here(),"3_ExtractedData/AllAttacks/AllAttacks2.csv", sep='/'), header=TRUE, sep=",")
AllAttacks3 <- read.csv(file = paste(here(),"3_ExtractedData/AllAttacks/AllAttacks3.csv", sep='/'), header=TRUE, sep=",")
AllAttacks3F <- read.csv(file = paste(here(),"3_ExtractedData/AllAttacks/AllAttacks3F.csv", sep='/'), header=TRUE, sep=",")

# basal drop rate
basaldr <- table(AllAttacks0$Fate)[2]/(table(AllAttacks0$Fate)[1]+table(AllAttacks0$Fate)[2])


# function to prep data (get est+CI from models)

PrepDF <- function(df){

  mod2 <- glmer (DropYN ~ -1+AttackedPalatabilityTreatment + AttackedColor + (1|FID), family = 'binomial', data = df)
  summary(mod2)
  
  effects_table <- as.data.frame(cbind(est=invlogit(summary(mod2)$coeff[,1]),
                                       CIhigh=invlogit(summary(mod2)$coeff[,1]+summary(mod2)$coeff[,2]*1.96),
                                       CIlow=invlogit(summary(mod2)$coeff[,1]-summary(mod2)$coeff[,2]*1.96)))
  effects_table <- effects_table[-nrow(effects_table),]
  effects_table$Palatability <- c("Control","DB")
  
  return(effects_table)
}

effects_table1_5 <- PrepDF(AllAttacks15)
effects_table2 <- PrepDF(AllAttacks2)
effects_table3 <- PrepDF(AllAttacks3)


PrepDF_withtraining <- function(df){

  mod2 <- glmer (DropYN ~ -1 + PalatExpo + AttackedColor + (1|FID), family = 'binomial', data = df)
  summary(mod2)
  
  effects_table <- as.data.frame(cbind(est=invlogit(summary(mod2)$coeff[,1]),
                                       CIhigh=invlogit(summary(mod2)$coeff[,1]+summary(mod2)$coeff[,2]*1.96),
                                       CIlow=invlogit(summary(mod2)$coeff[,1]-summary(mod2)$coeff[,2]*1.96)))
  effects_table <- effects_table[-nrow(effects_table),]
  effects_table$PriorExposure <- c("Naive", "Trained", "Naive","Trained")
  effects_table$Palatability <- c("Control", "Control", "DB","DB")
  
  return(effects_table)
}

effects_table1 <- PrepDF_withtraining(AllAttacks1)
effects_table3F <- PrepDF_withtraining(AllAttacks3F)


# plot

{plot1_5_dr <- 
    
    ggplot(data=effects_table1_5, aes(x=Palatability, y=est)) + 
    scale_y_continuous(name="Prey rejection probability", 
                       limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1), labels=scales::percent)+ # 0.75 converted to 75%
    theme_classic() + # white backgroun, x and y axis (no box)
    labs(title = "DB solution concentration: 1.5%") +
    
    geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4)+ # don't plot bor bars on x axis tick, but separate them (dodge)
    geom_point(size =4, stroke = 1) +
    geom_hline(yintercept=basaldr, linetype="dashed", color = "grey48") +
    theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 10))
}

{plot2_dr <- 
    
    ggplot(data=effects_table2, aes(x=Palatability, y=est)) + 
    scale_y_continuous(limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1))+ 
    theme_classic() + # white backgroun, x and y axis (no box)
    labs(title = "DB solution concentration: 2%") +
    
    geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4)+ # don't plot bor bars on x axis tick, but separate them (dodge)
    geom_point(size =4, stroke = 1) +
    geom_hline(yintercept=basaldr, linetype="dashed", color = "grey48") +
    theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
          axis.title.x=element_text(size=10),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10))
}

{plot3_dr <- 
    
    ggplot(data=effects_table3, aes(x=Palatability, y=est)) + 
    scale_y_continuous(limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1))+ 
    theme_classic() + # white backgroun, x and y axis (no box)
    labs(title = "DB solution concentration: 3%") +
    
    geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4)+ # don't plot bor bars on x axis tick, but separate them (dodge)
    geom_point(size =4, stroke = 1) +
    geom_hline(yintercept=basaldr, linetype="dashed", color = "grey48") +
    theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
          axis.title.x=element_text(size=10),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10))
}


plot1_5_dr_g <- ggplotGrob(plot1_5_dr)
plot2_dr_g <- ggplotGrob(plot2_dr)
plot3_dr_g <- ggplotGrob(plot3_dr)


#setEPS() 
#pdf("5_Figures/DropRate/Fig2A.pdf", height=5, width=6.85)
grid.arrange(cbind(plot1_5_dr_g,plot2_dr_g, plot3_dr_g, size="last"))
#dev.off()



{plot1_dr <- 
    
    ggplot(data=effects_table1, aes(x=Palatability, y=est,colour=PriorExposure, shape = PriorExposure)) + 
    scale_y_continuous(name="Prey rejection probability", 
                       limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1), labels=scales::percent)+ # 0.75 converted to 75%
    theme_classic() + # white backgroun, x and y axis (no box)
    labs(title = "DB solution concentration: 1%") +
    
    geom_errorbar(aes(ymin=CIlow, ymax=CIhigh, col=PriorExposure), width =0.4,na.rm=TRUE, position = position_dodge(width=0.5))+ # don't plot bor bars on x axis tick, but separate them (dodge)
    geom_point(size =4, aes(shape=PriorExposure, col=PriorExposure), stroke = 1, position = position_dodge(width=0.5)) +
    geom_hline(yintercept=basaldr, linetype="dashed", color = "grey48") +
    scale_colour_manual(name= "Prior exposure to DB", values=c("Black","Grey")) +
    scale_shape_manual(name= "Prior exposure to DB", values=c(16,17))+ # duplicate title to combine legend
    theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
          legend.position=c(0.5,0.85),
          legend.title = element_text(size=rel(0.8)),
          legend.text = element_text(size=rel(0.7)),
          legend.key.size = unit(0.8, 'lines'),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 10)) +
    guides(shape = guide_legend(override.aes = list(linetype = 0, size = 2))) # remove bar o top of symbol in legend
  
}

{plot3F_dr <- 
    
    ggplot(data=effects_table3F, aes(x=Palatability, y=est,colour=PriorExposure, shape = PriorExposure)) + 
    scale_y_continuous(limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1))+ 
    theme_classic() + # white backgroun, x and y axis (no box)
    labs(title = "DB solution concentration: 3%") +
    geom_errorbar(aes(ymin=CIlow, ymax=CIhigh, col=PriorExposure),  width =0.4,na.rm=TRUE, position = position_dodge(width=0.5))+ # don't plot bor bars on x axis tick, but separate them (dodge)
    geom_point(size =4, aes(shape=PriorExposure, col=PriorExposure), stroke = 1, position = position_dodge(width=0.5)) +
    geom_hline(yintercept=basaldr, linetype="dashed", color = "grey48") +
    scale_colour_manual(values=c("Black","Grey")) +
    scale_shape_manual(values=c(16,17))+ # duplicate title to combine legend
    theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
          legend.position="none",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_text(size=10),
          plot.title = element_text(hjust = 0.5, size=10))
  
}

plot1_dr_g <- ggplotGrob(plot1_dr)
plot3F_dr_g <- ggplotGrob(plot3F_dr)

#setEPS() 
#pdf("5_Figures/DropRate/Fig2B.pdf", height=5, width=5)
grid.arrange(cbind(plot1_dr_g,plot3F_dr_g, size="last"))
#dev.off()





