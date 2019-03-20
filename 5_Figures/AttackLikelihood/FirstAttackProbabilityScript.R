# create panelled figure for attack probability observed

rm(list = ls(all = TRUE))


library(here) # to get path of the Rproj root
library(ggplot2) # to plot
require(gridExtra) # for function gridarrange
library(dplyr)

FirstAttack1 <- read.csv(file = paste(here(),"3_ExtractedData/FirstAttacks/FirstAttacks.csv", sep='/'), header=TRUE, sep=",")
# FirstAttack1_5 <- read.csv(file = paste(here(),"3_ExtractedData/FirstAttacks/FirstAttacks1_5.csv", sep='/'), header=TRUE, sep=",")
# FirstAttack2 <- read.csv(file = paste(here(),"3_ExtractedData/FirstAttacks/FirstAttacks2.csv", sep='/'), header=TRUE, sep=",")
# FirstAttack3 <- read.csv(file = paste(here(),"3_ExtractedData/FirstAttacks/FirstAttacks3.csv", sep='/'), header=TRUE, sep=",")
# FirstAttack3F <- read.csv(file = paste(here(),"3_ExtractedData/FirstAttacks/FirstAttacks3_Final.csv", sep='/'), header=TRUE, sep=",")



contingency_tbl <-   data.frame(FirstAttack1 %>%
    group_by(AttackedTermitePalatability,PriorExposureYN) %>%
   summarize( Count_perc = n()/nrow(FirstAttack1[FirstAttack1$PriorExposureYN==1,]), 
              Count = n()))




plot1 <- ggplot(contingency_tbl, aes(x = factor(AttackedTermitePalatability), y = Count_perc, fill = factor(PriorExposureYN) )) +  
  geom_bar(stat = "identity", position=position_dodge()) + 
  labs(x = 'Palatability treatment', y= 'Prey probability of being attacked first')+
  geom_hline(yintercept=0.5, linetype="dashed", color = "grey48") +
  scale_y_continuous(limits = c(0, 0.6), breaks = c(0,0.1,0.2,0.3,0.4,0.5, 0.6), labels=scales::percent)+
  scale_fill_grey(name = "Prior exposure to DB", labels = c('Naive', 'Trained')) +
  scale_x_discrete(limits=c("1", "0"), labels=c("0" = "DB", "1" = "Control"))+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = c(0.25,0.9))
  
plot2 <- ggplot(contingency_tbl, aes(x = factor(AttackedTermitePalatability), y = Count, fill = factor(PriorExposureYN) )) +  
  geom_bar(stat = "identity", position=position_dodge()) + 
  labs(x = 'Palatability treatment', y= 'Number of prey attacked first')+
  scale_y_continuous(limits = c(0, 35), breaks = c(0,10,20,30))+
  scale_fill_grey(name = "Prior exposure to DB", labels = c('Naive', 'Trained')) +  
  scale_x_discrete(limits=c("1", "0"), labels=c("0" = "DB", "1" = "Control"))+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = c(0.25,0.85))
       




FocalTermiteAttack1 <- read.csv(file = paste(here(),"3_ExtractedData/FocalAttacks/FocalTermiteAttack.csv", sep='/'), header=TRUE, sep=",")

PrepDF_withtraining <- function(df){
  df$FocalTermitePalatability[df$FocalTermitePalatability == "1"] <- 'Control'
  df$FocalTermitePalatability[df$FocalTermitePalatability == "0"] <- 'DB'
  df$PriorExposure[df$PriorExposureYN == 1] <- 'Trained'
  df$PriorExposure[df$PriorExposureYN == 0] <- 'Naive'
  
  
  df$PalatExp <- paste(df$FocalTermitePalatability, df$PriorExposure, sep="")
  mod1 <- glm (FocalTermiteAttackedYN ~ -1+PalatExp + FocalTermiteColor, family = 'binomial', data = df)
  summary(mod1)
  
  effects_table <- as.data.frame(cbind(est=invlogit(summary(mod1)$coeff[,1]),
                                       CIhigh=invlogit(summary(mod1)$coeff[,1]+summary(mod1)$coeff[,2]*1.96),
                                       CIlow=invlogit(summary(mod1)$coeff[,1]-summary(mod1)$coeff[,2]*1.96)))
  effects_table <- effects_table[-nrow(effects_table),]
  effects_table$PriorExposure <- c("Naive", "Trained", "Naive","Trained")
  effects_table$Palatability <- c("Control", "Control", "DB","DB")
  
  return(effects_table)
}

effects_table1 <- PrepDF_withtraining(FocalTermiteAttack1)

 plot3 <-   ggplot(data=effects_table1, aes(x=Palatability, y=est,colour=PriorExposure, shape = PriorExposure)) + 
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
  
    
 plot1 <- ggplotGrob(plot1)
 plot2 <- ggplotGrob(plot2)
 plot3 <- ggplotGrob(plot3)
    
    grid.arrange(cbind(plot2,plot1, plot3, size="last"))
    