#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data extraction and handling: bug mvt test
#	 Start : 18 March 2019
#	 last commit: first commit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
  # in remarks,we noted that some insects seemed more lethargic than others 
  # without us predicting it in advance (no difference in painting noticeable)
  # these will not be excluded as they are part of the range of prey that we provide our spiders with when we manipulate prey color
}


rm(list = ls(all = TRUE))


{# packages
  library(RODBC) # this require R AND ACCESS to run on 32 bits ! (and apparently can't do it on MAC)
  library(here)
}


{# load data
  conDB= odbcConnectAccess2007(paste(here(),"1_RawData/VideoAnalyses_MilkweedMovement.accdb", sep="/"))
  sqlTables(conDB)
  
  
  tbl <- merge(sqlFetch(conDB, 'Data'),sqlFetch(conDB, 'Treatment'), by='TestID')
  close(conDB)
  
  
  tbl <- tbl[,c('NbGridCrossed','Treatment')]
  

}

head(tbl)

table(tbl$Treatment)


# stats

mod <-glm(NbGridCrossed ~ Treatment, data = tbl, family = 'poisson') # intercept is MW non painted bug
summary(mod)
drop1(mod, test="Chisq")



setEPS()
pdf(paste(here(), "5_Figures/Mvt/BugMvt.pdf", sep="/"), height=5, width=3.3)

ggplot(tbl, aes(x=Treatment, y=NbGridCrossed)) + 
  geom_boxplot()+
  labs(y = "Number of gridlines crossed", x = NULL)+
  scale_x_discrete(labels=c("Milkweed", "Painted milkweed", "Sunflower"))+
theme_classic() +
theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 10))  
  
dev.off()






