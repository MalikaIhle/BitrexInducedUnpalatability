#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Data sampling for focal first attack, analyse and plot
#  manipulation palatability with bitrex 1% and 3F% (with prior exposure for half the spiders)
#	 Start : 21 march 2019
#	 last modif : write function to apply to all DBs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

library(here)
library(pbapply)
library(ggplot2) # to plot
require(gridExtra) # for function gridarrange
library(arm)

FocalAttacks1 <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks1.csv", header=TRUE, sep=",")
FocalAttacks15 <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks15.csv", header=TRUE, sep=",")
FocalAttacks2 <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks2.csv", header=TRUE, sep=",")
FocalAttacks3 <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks3.csv", header=TRUE, sep=",")
FocalAttacks3F <- read.csv(file="3_ExtractedData/FocalAttacks/FocalAttacks3F.csv", header=TRUE, sep=",")

  
# select one at random (1000 times), determine focal color, focal palatability, focal attackedYN, run the model, save est, CI, p 
  
nrep <- 10


  sample_focal_with_training <- function(df) {
  
  FocalAttack <- split(df, df$FID)
  
  FocalAttack_fun <- function(x){
    x$FocalYN <- sample(c(0,1), 2,replace=FALSE) # randomly assigning YN, determining whether the termite is actually focal or not
    
    return(x[x$FocalYN == 1,])
  }
  
  FocalAttack <- do.call(rbind,lapply(FocalAttack,FocalAttack_fun))
  rownames(FocalAttack) <- NULL
  
  modInter <- glm (FocalAttackedYN ~  -1+ PalatExpo 
                   + scale(FocalColorCode)
                   ,family = 'binomial', data = FocalAttack)
  summary(modInter)

  effects_table <- as.data.frame(cbind(est=invlogit(summary(modInter)$coeff[,1]),
                                       CIhigh=invlogit(summary(modInter)$coeff[,1]+summary(modInter)$coeff[,2]*1.96),
                                       CIlow=invlogit(summary(modInter)$coeff[,1]-summary(modInter)$coeff[,2]*1.96)))
  effects_table <- effects_table[-nrow(effects_table),]
  
  return(list(effects_table))
  
  }
  
  sample_focal_without_training <- function(df) {
    
    FocalAttack <- split(df, df$FID)
    
    FocalAttack_fun <- function(x){
      x$FocalYN <- sample(c(0,1), 2,replace=FALSE) # randomly assigning YN, determining whether the termite is actually focal or not
      
      return(x[x$FocalYN == 1,])
    }
    
    FocalAttack <- do.call(rbind,lapply(FocalAttack,FocalAttack_fun))
    rownames(FocalAttack) <- NULL
    
    modInter <- glm (FocalAttackedYN ~  -1+ FocalPalatabilityTreatment 
                     + scale(FocalColorCode)
                     ,family = 'binomial', data = FocalAttack)
    summary(modInter)
    
    effects_table <- as.data.frame(cbind(est=invlogit(summary(modInter)$coeff[,1]),
                                         CIhigh=invlogit(summary(modInter)$coeff[,1]+summary(modInter)$coeff[,2]*1.96),
                                         CIlow=invlogit(summary(modInter)$coeff[,1]-summary(modInter)$coeff[,2]*1.96)))
    effects_table <- effects_table[-nrow(effects_table),]
    
    return(list(effects_table))
    
  }
  

  
# estimates + CI, DB concentration 1.5% 2% and 3%  
  
  effects_table15_list <- pbreplicate(nrep, sample_focal_without_training(FocalAttacks15))
  effects_table15 <- Reduce(`+`, effects_table15_list) / length(effects_table15_list)
  effects_table15$Palatability <- c("Control", "DB")
  
  effects_table2_list <- pbreplicate(nrep, sample_focal_without_training(FocalAttacks2))
  effects_table2 <- Reduce(`+`, effects_table2_list) / length(effects_table2_list)
  effects_table2$Palatability <- c("Control", "DB")
  
  effects_table3_list <- pbreplicate(nrep, sample_focal_without_training(FocalAttacks3))
  effects_table3 <- Reduce(`+`, effects_table3_list) / length(effects_table3_list)
  effects_table3$Palatability <- c("Control", "DB")
 
# estimates + CI, DB concentration 1% and 3F%

  effects_table1_list <- pbreplicate(nrep, sample_focal_with_training(FocalAttacks1))
  effects_table1 <- Reduce(`+`, effects_table1_list) / length(effects_table1_list)
  effects_table1$PriorExposure <- c("Naive", "Trained", "Naive","Trained")
  effects_table1$Palatability <- c("Control", "Control", "DB","DB")
  
  
  effects_table3F_list <- pbreplicate(nrep, sample_focal_with_training(FocalAttacks3F))
  effects_table3F <- Reduce(`+`, effects_table3F_list) / length(effects_table3F_list)
  effects_table3F$PriorExposure <- c("Naive", "Trained", "Naive","Trained")
  effects_table3F$Palatability <- c("Control", "Control", "DB","DB")
  
  
# plot DB concentration 1.5% 2% and 3%
  
{plot15 <- 
    
    ggplot(data=effects_table15, aes(x=Palatability, y=est)) + 
    scale_y_continuous(name="Prey probability of being attacked first", 
                       limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1), labels=scales::percent)+ # 0.75 converted to 75%
    theme_classic() + # white backgroun, x and y axis (no box)
    labs(title = "DB solution concentration: 1.5%") +
    
    geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4)+ # don't plot bor bars on x axis tick, but separate them (dodge)
    geom_point(size =4, stroke = 1) +
    geom_hline(yintercept=0.5, linetype="dashed", color = "grey48") +
    theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 10))
}

{plot2 <- 
    
    ggplot(data=effects_table2, aes(x=Palatability, y=est)) + 
    scale_y_continuous(limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1))+ 
    theme_classic() + # white backgroun, x and y axis (no box)
    labs(title = "DB solution concentration: 2%") +
    
    geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4)+ # don't plot bor bars on x axis tick, but separate them (dodge)
    geom_point(size =4, stroke = 1) +
    geom_hline(yintercept=0.5, linetype="dashed", color = "grey48") +
    theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
          axis.title.x=element_text(size=10),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10))
}

{plot3 <- 
    
    ggplot(data=effects_table3, aes(x=Palatability, y=est)) + 
    scale_y_continuous(limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1))+ 
    theme_classic() + # white backgroun, x and y axis (no box)
    labs(title = "DB solution concentration: 3%") +
    
    geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4)+ # don't plot bor bars on x axis tick, but separate them (dodge)
    geom_point(size =4, stroke = 1) +
    geom_hline(yintercept=0.5, linetype="dashed", color = "grey48") +
    theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
          axis.title.x=element_text(size=10),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10))
}


plot15 <- ggplotGrob(plot15)
plot2 <- ggplotGrob(plot2)
plot3 <- ggplotGrob(plot3)

setEPS() 
pdf("5_Figures/AttackLikelihood/Fig1A_focal1000.pdf", height=5, width=6.85)
grid.arrange(cbind(plot15,plot2, plot3, size="last"))
dev.off()
  
  
  
# plot DB concentration 1% 2% and 3F%
  
   plot1 <- ggplot(data=effects_table1, aes(x=Palatability, y=est,colour=PriorExposure, shape = PriorExposure)) + 
    scale_y_continuous(name="Prey probability of being attacked first", 
                       limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1), labels=scales::percent)+ # 0.75 converted to 75%
    theme_classic() + # white backgroun, x and y axis (no box)
    labs(title = "DB solution concentration: 1%") +
    
    geom_errorbar(aes(ymin=CIlow, ymax=CIhigh, col=PriorExposure), width =0.4,na.rm=TRUE, position = position_dodge(width=0.5))+ # don't plot bor bars on x axis tick, but separate them (dodge)
    geom_hline(yintercept=0.5, linetype="dashed", color = "grey48") +
    geom_point(size =4, aes(shape=PriorExposure, col=PriorExposure), stroke = 1, position = position_dodge(width=0.5)) +
    scale_colour_manual(name= "Prior exposure to DB", values=c("Black","Grey")) +
    scale_shape_manual(name= "Prior exposure to DB", values=c(16,17))+ # duplicate title to combine legend
    theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
          legend.position=c(0.5,0.85),
          legend.title = element_text(size=rel(0.8)),
          legend.text = element_text(size=rel(0.7)),
          legend.key.size = unit(0.8, 'lines'),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 10)) +
    guides(shape = guide_legend(override.aes = list(linetype = 0, size = 2))) # remove bar o top of symbol in legend
  

  plot3F <- ggplot(data=effects_table3F, aes(x=Palatability, y=est,colour=PriorExposure, shape = PriorExposure)) + 
     scale_y_continuous(name="Prey probability of being attacked first", 
                        limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1), labels=scales::percent)+ # 0.75 converted to 75%
     theme_classic() + # white backgroun, x and y axis (no box)
     labs(title = "DB solution concentration: 3%") +
     
     geom_errorbar(aes(ymin=CIlow, ymax=CIhigh, col=PriorExposure), width =0.4,na.rm=TRUE, position = position_dodge(width=0.5))+ # don't plot bor bars on x axis tick, but separate them (dodge)
     geom_hline(yintercept=0.5, linetype="dashed", color = "grey48") +
     geom_point(size =4, aes(shape=PriorExposure, col=PriorExposure), stroke = 1, position = position_dodge(width=0.5)) +
     scale_colour_manual(name= "Prior exposure to DB", values=c("Black","Grey")) +
     scale_shape_manual(name= "Prior exposure to DB", values=c(16,17))+ # duplicate title to combine legend
     theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
           legend.position="none",
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
           axis.text.y=element_blank(),
           plot.title = element_text(hjust = 0.5, size = 10)) +
     guides(shape = guide_legend(override.aes = list(linetype = 0, size = 2))) # remove bar o top of symbol in legend
   
   
  
  plot1 <- ggplotGrob(plot1)
  plot3F <- ggplotGrob(plot3F)
  tgrob <- textGrob("Palatability treatment")
  
  
  blank2y <-ggplot()+
    scale_x_continuous(limits = c(0, 10))+
    scale_y_continuous(name="Prey probability of being attacked first", 
                       limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1), labels=scales::percent)+
    
    annotate("text", x = 5, y = 0.5, label = "Palatability treatment",  hjust = 0.5, angle=0)+
    theme_classic()+
    
    theme(
      panel.border = element_rect(colour = "white", fill=NA),
        axis.title.y=element_text(size=10, color = "white"),
      axis.title.x = element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_text(color = "white"),
      axis.ticks.x=element_blank(),
      axis.ticks.y=element_blank(),
     axis.line = element_line("white"),
           plot.margin = unit(c(0,0.2,0,0.1), "cm"))
  blank2yGrob <- ggplotGrob(blank2y)
  
  
  setEPS() 
  pdf("5_Figures/AttackLikelihood/Fig1B_focal1000.pdf", height=5, width=5)
  grid.arrange(grobs = list(cbind(plot1,plot3F,size="last"),blank2yGrob) , nrow=2, heights=c(15,1))
  dev.off()
  
