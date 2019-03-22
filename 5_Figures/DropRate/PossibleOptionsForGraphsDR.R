# create panelled figure for attack probability observed

rm(list = ls(all = TRUE))


library(here) # to get path of the Rproj root
library(ggplot2) # to plot
require(gridExtra) # for function gridarrange
library(dplyr)

AllAttacks1 <- read.csv(file = paste(here(),"3_ExtractedData/AllAttacks/AllAttacks1.csv", sep='/'), header=TRUE, sep=",")


AllAttack0 <- read.csv(file = paste(here(),"3_ExtractedData/AllAttacks/AllAttacks0.csv", sep='/'), header=TRUE, sep=",")
basaldr <- table(AllAttack0$Fate)[2]/(table(AllAttack0$Fate)[1]+table(AllAttack0$Fate)[2])



contingency_tbl <-   data.frame(AllAttacks1 %>%
                                  group_by(AttackedPalatability,PriorExposureYN, DropYN) %>%
                                  summarize( Count = n()))

contingency_tbl$PalatExp <- paste(contingency_tbl$AttackedPalatability,contingency_tbl$PriorExposureYN, sep="-")

plot1 <- ggplot(contingency_tbl, aes(x = PalatExp , y = Count, fill = factor(DropYN) )) +  
  geom_bar(stat = "identity") + 
  labs(x = 'Palatability treatment - Prior Exposure', y= 'Number of prey')+
  scale_y_continuous(limits = c(0, 60), breaks = c(0,10,20,30, 40, 50))+
  scale_fill_grey(name = "Outcome", labels = c('Consumed', 'Rejected')) +  
  scale_x_discrete( labels=c("0-0" = "DB - Naive", "0-1" = "DB - Trained", "1-0"="Control - Naive", "1-1" = "Control - Trained"))+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = c(0.25,0.85))


contingency_tbl2 <-   data.frame(AllAttacks1 %>%
                                  group_by(AttackedPalatability,PriorExposureYN) %>%
                                  summarize( Count_Dropped = sum(DropYN), 
                                             Count_Consumed = length(DropYN[DropYN==0]),
                                             count_dropped_perc = Count_Dropped/(Count_Dropped+Count_Consumed)))

plot2 <- ggplot(contingency_tbl2, aes(x = factor(AttackedPalatability), y = count_dropped_perc, fill = factor(PriorExposureYN) )) +  
  geom_bar(stat = "identity", position=position_dodge()) + 
  labs(x = 'Palatability treatment', y= 'Prey probability of being rejected')+
  geom_hline(yintercept=basaldr, linetype="dashed", color = "grey48") +
  scale_y_continuous(limits = c(0, 0.7), breaks = c(0,0.1,0.2,0.3,0.4,0.5, 0.6), labels=scales::percent)+
  scale_fill_grey(name = "Prior exposure to DB", labels = c('Naive', 'Trained')) +
  scale_x_discrete(limits=c("1", "0"), labels=c("0" = "DB", "1" = "Control"))+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = c(0.25,0.9))


PrepDF_withtraining <- function(df){

  mod2 <- glmer (DropYN ~ -1 + PalatExpo + scale(ColorCode) + (1|FID), family = 'binomial', data = df)
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


plot3 <-  ggplot(data=effects_table1, aes(x=Palatability, y=est,colour=PriorExposure, shape = PriorExposure)) + 
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



plot1 <- ggplotGrob(plot1)
plot2 <- ggplotGrob(plot2)
plot3 <- ggplotGrob(plot3)

grid.arrange(cbind(plot1,plot2, plot3, size="last"))
