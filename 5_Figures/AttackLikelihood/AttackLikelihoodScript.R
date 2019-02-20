# create pannelled figure for attack likelihood

rm(list = ls(all = TRUE))


library(here) # to get path of the Rproj root
library(sjPlot) # for automatic model plot
library(effects) # for automatic model plot of effect
library(ggplot2) # to plot
require(gridExtra) # for function gridarrange


source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}



file1 <- paste(here(),'4_ScriptAnalysis/data_analysis1.R', sep='/')
file1_5 <- paste(here(),'4_ScriptAnalysis/data_analysis1_5.R', sep='/')
file2 <- paste(here(),'4_ScriptAnalysis/data_analysis2.R', sep='/')
file3 <- paste(here(),'4_ScriptAnalysis/data_analysis3.R', sep='/')
file3F <- paste(here(),'4_ScriptAnalysis/data_analysis3_Final.R', sep='/')



source_lines(file1, 16:length(readLines(file1)))
plot1_lkh <- plot_model(mod1, 
                        type = "eff", 
                        terms = c("FocalTermitePalatability", "PriorExposureYN"))+
                        ylim(c(0,1))+ 
                    #pb sinc egrouped data (interaction term) 
                    #scale_x_discrete('FocalTermitePalatability',labels=c("0"="Unapalt", '1'="Pala"))+
                        scale_y_continuous(name="Likelihood of first attack", 
                          limits=c(0, 1), breaks =c(0,0.25,0.5,0.75,1), labels=scales::percent)+
                        theme_classic() +
                        theme(
                         panel.border = element_rect(colour = "black", fill=NA),
                         legend.position=c(0.2,0.10), 
                         axis.title.x=element_blank(),
                         #axis.text.x=element_text(),
                         axis.ticks.x=element_blank(),
                         axis.title.y=element_text("Likelihood of first attack"))+
                       labs(title = "1%")+
                       scale_colour_discrete(name  ="Prior Exposure to Bitrex",
                         breaks=c("0", "1"),
                         labels=c("No", "Yes"))
                       



source_lines(file1_5, 12:length(readLines(file1_5)))
plot1_5_lkh <- 
  plot_model(mod1, 
             type = "eff", 
             terms = c("FocalTermitePalatability"))+
             ylim(c(0,1))+ 
  scale_x_continuous(limits=c(0, 1), breaks =c(0,1), labels=c("Unpalatable", "Palatable"))+
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank())+
  labs(title = "1.5%")

source_lines(file2, 12:length(readLines(file2)))
plot2_lkh <-   
  plot_model(mod1, 
                          type = "eff", 
                          terms = c("FocalTermitePalatability"))+
  ylim(c(0,1))+ 
  scale_x_continuous(limits=c(0, 1), breaks =c(0,1), labels=c("Unpalatable", "Palatable"))+
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank())+
  labs(title = "2%")

source_lines(file3, 12:length(readLines(file3)))
plot3_lkh <- 
  plot_model(mod1, 
             type = "eff", 
             terms = c("FocalTermitePalatability"))+
  ylim(c(0,1))+ 
  scale_x_continuous(limits=c(0, 1), breaks =c(0,1), labels=c("Unpalatable", "Palatable"))+
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank())+
  labs(title = "3%")

source_lines(file3F, 12:length(readLines(file3F)))
plot3F_lkh <- plot_model(mod1, 
                         type = "eff", 
                         terms = c("FocalTermitePalatability", "PriorExposureYN"))+
  ylim(c(0,1))+ 
 # scale_x_continuous(limits=c(0, 1), breaks =c(0,1), labels=c("Unpalatable", "Palatable"))+
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank(),
    legend.position="none")+
  labs(title = "3F%") 



grid.arrange(plot1_lkh, 
             plot1_5_lkh, 
             plot2_lkh,
             plot3_lkh,
             plot3F_lkh,
             ncol=5)



