#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Data sampling for focal first attack, analyse and plot
#  manipulation palatability with bitrex 1% and 3F% (with prior exposure for half the spiders)
#	 Start : 21 march 2019
#	 last modif : write function to apply to all DBs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

library(RODBC)
library(here)
library(pbapply)
library(ggplot2) # to plot
require(gridExtra) # for function gridarrange

FocalAttacks1 <- read.csv(file = paste(here(),"3_ExtractedData/FocalAttacks/FocalAttacks1.csv", sep='/'), header=TRUE, sep=",")
FocalAttacks3F <- read.csv(file = paste(here(),"3_ExtractedData/FocalAttacks/FocalAttacks3F.csv", sep='/'), header=TRUE, sep=",")

  
# select one at random (1000 times), run the model, save est, CI, p 
  
  sample_focal <- function(df) {
  
  FocalAttack <- split(df, df$FID)
  
  FocalAttack_fun <- function(x){
    x$FocalYN <- sample(c(0,1), 2,replace=FALSE) # randomly assigning YN, determining whether the  is focal or not
    return(x[x$FocalYN == 1,])
  }
  
  FocalAttack <- do.call(rbind,lapply(FocalAttack,FocalAttack_fun))
  rownames(FocalAttack) <- NULL
  

  modInter <- glm (FocalAttackedYN ~  PalatExpo + scale(FocalColorCode), family = 'binomial', data = FocalAttack)
  summary(modInter)
  drop1(modInter, test="Chisq")
  
  effects_table <- as.data.frame(cbind(est=invlogit(summary(modInter)$coeff[,1]),
                                       CIhigh=invlogit(summary(modInter)$coeff[,1]+summary(modInter)$coeff[,2]*1.96),
                                       CIlow=invlogit(summary(modInter)$coeff[,1]-summary(modInter)$coeff[,2]*1.96)))
  effects_table <- effects_table[-nrow(effects_table),]
  
  return(list(effects_table))
  
  }
  
  effects_table1_list <- pbreplicate(1000, sample_focal(FocalAttacks1))
  
  effects_table1 <- Reduce(`+`, effects_table1_list) / length(effects_table1_list)
  effects_table1$PriorExposure <- c("Naive", "Trained", "Naive","Trained")
  effects_table1$Palatability <- c("Control", "Control", "DB","DB")
  

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
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 10)) +
    guides(shape = guide_legend(override.aes = list(linetype = 0, size = 2))) # remove bar o top of symbol in legend
  

   effects_table3F_list <- pbreplicate(1000, sample_focal(FocalAttacks3F))
   
   effects_table3F <- Reduce(`+`, effects_table3F_list) / length(effects_table3F_list)
   effects_table3F$PriorExposure <- c("Naive", "Trained", "Naive","Trained")
   effects_table3F$Palatability <- c("Control", "Control", "DB","DB")
   
   
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
            axis.title.x=element_text(size=10),
           axis.title.y=element_text(size=10),
           plot.title = element_text(hjust = 0.5, size = 10)) +
     guides(shape = guide_legend(override.aes = list(linetype = 0, size = 2))) # remove bar o top of symbol in legend
   
   
  
  plot1 <- ggplotGrob(plot1)
  plot3F <- ggplotGrob(plot3F)
  
  #setEPS() 
  #pdf("Fig2B.pdf", height=5, width=5)
  grid.arrange(cbind(plot1,plot3F, size="last"))
  #dev.off()
  

