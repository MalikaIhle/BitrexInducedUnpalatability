# Create panelled figure for drop rate

rm(list = ls(all = TRUE))


library(here) # to get path of the Rproj root
library(sjPlot) # for automatic model plot
library(effects) # for automatic model plot of effect
library(ggplot2) # to plot
require(gridExtra) # for function gridarrange


source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}



file0 <- paste(here(),'4_ScriptAnalysis/data_analysis0.R', sep='/')
file1 <- paste(here(),'4_ScriptAnalysis/data_analysis1.R', sep='/')
file1_5 <- paste(here(),'4_ScriptAnalysis/data_analysis1_5.R', sep='/')
file2 <- paste(here(),'4_ScriptAnalysis/data_analysis2.R', sep='/')
file3 <- paste(here(),'4_ScriptAnalysis/data_analysis3.R', sep='/')
file3F <- paste(here(),'4_ScriptAnalysis/data_analysis3_Final.R', sep='/')


source_lines(file0, 12:length(readLines(file0)))
df <- data.frame()
plot0_dr <-
  ggplot(df) + 
  scale_y_continuous(name="Drop Rate", 
        limits=c(0, 1), breaks =c(0,0.25,0.5,0.75,1), labels=scales::percent)+
  scale_x_continuous(limits=c(0, 2), breaks =1, labels="Palatable")+
  theme_classic() + 
  theme(axis.title.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) + 
  geom_point(aes(x=1, y=5/35), colour="Black", cex=2)+
  labs(title = "0%")
 




source_lines(file1, 16:length(readLines(file1)))

plot1_dr <- 
  plot_model(mod2, type = "eff", terms = c('AttackedTermitePalatability'))+
  ylim(c(0,1))+ 
 scale_x_continuous(breaks=c(0,1), labels=c("Unapalatable","Palatable"))+
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    legend.position="none", 
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank()
  )+
  labs(title = "1%")


source_lines(file1_5, 16:length(readLines(file1_5)))

plot1_5_dr <- 
  plot_model(mod2, type = "eff", terms = c('AttackedTermitePalatability'))+
  ylim(c(0,1))+ 
  scale_x_continuous(breaks=c(0,1), labels=c("Unapalatable","Palatable"))+
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    legend.position="none", 
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank()
  )+
  labs(title = "1.5%")


source_lines(file2, 16:length(readLines(file2)))

plot2_dr <- 
  plot_model(mod2, type = "eff", terms = c('AttackedTermitePalatability'))+
  ylim(c(0,1))+ 
  scale_x_continuous(breaks=c(0,1), labels=c("Unapalatable","Palatable"))+
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    legend.position="none", 
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank()
  )+
  labs(title = "2%")


source_lines(file3, 16:length(readLines(file3)))

plot3_dr <- 
  plot_model(mod2, type = "eff", terms = c('AttackedTermitePalatability'))+
  ylim(c(0,1))+ 
  scale_x_continuous(breaks=c(0,1), labels=c("Unapalatable","Palatable"))+
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    legend.position="none", 
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank()
  )+
  labs(title = "3%")


source_lines(file3F, 16:length(readLines(file3F)))

plot3F_dr <- 
  plot_model(mod2, type = "eff", terms = c('AttackedTermitePalatability'))+
  ylim(c(0,1))+ 
  scale_x_continuous(breaks=c(0,1), labels=c("Unapalatable","Palatable"))+
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    legend.position="none", 
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank()
  )+
  labs(title = "3F%")


grid.arrange(plot0_dr,
             plot1_dr,
             plot1_5_dr,
             plot2_dr, 
             plot3_dr,
             plot3F_dr,
             ncol=6)

