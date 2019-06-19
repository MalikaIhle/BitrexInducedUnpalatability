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
  library(lme4)
  library(ggplot2)
  library(multcomp) # for glht
}


{# load data
  conDB= odbcConnectAccess2007(paste(here(),"1_RawData/VideoAnalyses_MilkweedMovement.accdb", sep="/"))
  sqlTables(conDB)
  
  
  tbl <- merge(sqlFetch(conDB, 'Data'),sqlFetch(conDB, 'Treatment'), by='TestID')
  close(conDB)
  
  
  tbl <- tbl[,c('NbGridCrossed','Treatment')]
  tbl$rowID <- 1:nrow(tbl)

}

head(tbl)

table(tbl$Treatment)


# stats

mod <-glmer(NbGridCrossed ~ Treatment + (1|rowID) # account for overdispersion: makes the whole difference !!! <<<<<<<<<<<<<<<
            ,data = tbl, family = 'poisson') # intercept is MW non painted bug
summary(mod)
#drop1(mod, test="Chisq")
summary(glht(mod, linfct=mcp(Treatment="Tukey")))


mod_NoIntercept <-glmer(NbGridCrossed ~ -1+ Treatment + (1|rowID) # account for overdispersion: makes the whole difference !!! <<<<<<<<<<<<<<<
                        , data = tbl, family = 'poisson')
summary(mod_NoIntercept)

effects_table <- as.data.frame(cbind(est=exp(summary(mod_NoIntercept)$coeff[,1]),
                                     CIhigh=exp(summary(mod_NoIntercept)$coeff[,1]+summary(mod_NoIntercept)$coeff[,2]*1.96),
                                     CIlow=exp(summary(mod_NoIntercept)$coeff[,1]-summary(mod_NoIntercept)$coeff[,2]*1.96)))
effects_table$Palatability <- c("Unpainted 
Milkweed Bug", "Painted 
Milkweed Bug", "Unpainted 
Sunflower Bug")
effects_table



setEPS()
pdf(paste(here(), "5_Figures/Mvt/BugMvt.pdf", sep="/"), height=5, width=5)

ggplot(effects_table, aes(x=Palatability, y=est)) + 
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4)+ # don't plot bor bars on x axis tick, but separate them (dodge)
  geom_point(shape = 15, size =4, stroke = 1) +
  
  labs(y = "Number of gridlines crossed", x = NULL)+
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 10))  


dev.off()





