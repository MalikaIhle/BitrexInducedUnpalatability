#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Plot reflectance of brown and green paint used to paint prey
#	 Start : 2019 03 29
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table <- read.table("1_RawData/Reflectance_withoutSpikes.txt", header=TRUE)
head(table)

library(ggplot2)
library(here)

setEPS()
pdf(paste(here(), "5_Figures/Reflectance/ReflectancePaints.pdf", sep="/"), height=3, width=6.8)

ggplot(data=table, aes(x=Wavelength, y=Reflectance, colour = Paint, fill = Paint)) +
  scale_y_continuous(name = "% reflectance (relative to white standard)") +
  scale_x_continuous(name = "Wavelength (nm)") +
  scale_color_manual(values = c("Brown"="saddlebrown","Green" ="olivedrab4")) +
  geom_line(aes(colour = Paint, linetype = Paint)) +
  geom_ribbon(aes(x = Wavelength, ymin=LowCI, ymax=UpperCI), linetype=1, alpha=0.2, color=NA) +
  scale_fill_manual(values = c("Brown"="saddlebrown","Green" ="olivedrab4"), guide = FALSE) + 
  scale_linetype_manual(values = c("Brown"="dashed","Green" ="solid")) + 
  theme_classic() +
  theme (panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
         axis.title.x=element_text(size=10),
         axis.title.y=element_text(size=10),
         legend.position=c(0.2,0.85),
         legend.title = element_text(size=rel(0.8)),
         legend.text = element_text(size=rel(0.7)),
         legend.key.size = unit(0.8, 'lines'))


dev.off()
