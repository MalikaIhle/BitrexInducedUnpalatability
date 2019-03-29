#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data extraction and handling: termite mvt test
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
}


{# load data
conDB= odbcConnectAccess2007(paste(here(),"1_RawData/VideoAnalysis_TermiteMovement.accdb", sep="/"))
sqlTables(conDB)


tbl_wide <- merge(sqlFetch(conDB, 'Data'),sqlFetch(conDB, 'Treatment'), by='TestID')
close(conDB)


tbl_long <- data.frame(TestID = c(tbl_wide$TestID,tbl_wide$TestID),
           NbGrid = c(tbl_wide$BrownNbGridCrossed, tbl_wide$GreenNbGridCrossed),
           Trt = c(as.character(tbl_wide$Treatment), as.character(tbl_wide$Treatment)),
           Col = c(rep('Brown',nrow(tbl_wide)), rep('Green',nrow(tbl_wide))),
           ColCode = c(rep(-2,nrow(tbl_wide)), rep(2,nrow(tbl_wide))) )

for (i in 1:nrow(tbl_long)) {
  
  if(tbl_long$Trt[i] == "GreenDB" & tbl_long$Col[i] == 'Green')
  {tbl_long$Palatability[i] <- 'No'}
  
  if(tbl_long$Trt[i] == "GreenDB" & tbl_long$Col[i] == 'Brown')
  {tbl_long$Palatability[i] <- 'A_Yes'}
  
  if(tbl_long$Trt[i] == "BrownDB" & tbl_long$Col[i] == 'Brown')
  {tbl_long$Palatability[i] <- 'No'}
  
  if(tbl_long$Trt[i] == "BrownDB" & tbl_long$Col[i] == 'Green')
  {tbl_long$Palatability[i] <- 'A_Yes'}
  
}

tbl_long$rowID <- 1:nrow(tbl_long)

}

head(tbl_long)

table(tbl_long$Trt)
table(tbl_long$Col)

# stats

hist(tbl_long$NbGrid)

mod <-glmer(NbGrid ~ Palatability + Col + (1|rowID) # account for overdispersion: makes the whole difference !!! <<<<<<<<<<<<<<<
            , data = tbl_long, family = 'poisson')
summary(mod)
drop1(mod, test="Chisq")



mod_NoIntercept <-glmer(NbGrid ~ -1+ Palatability + scale(ColCode) + (1|rowID) # account for overdispersion: makes the whole difference !!! <<<<<<<<<<<<<<<
            , data = tbl_long, family = 'poisson')
summary(mod_NoIntercept)

effects_table <- as.data.frame(cbind(est=exp(summary(mod_NoIntercept)$coeff[,1]),
                                     CIhigh=exp(summary(mod_NoIntercept)$coeff[,1]+summary(mod_NoIntercept)$coeff[,2]*1.96),
                                     CIlow=exp(summary(mod_NoIntercept)$coeff[,1]-summary(mod_NoIntercept)$coeff[,2]*1.96)))
effects_table <- effects_table[-nrow(effects_table),]
effects_table$Palatability <- c("Control", "DB")
effects_table



setEPS()
pdf(paste(here(), "5_Figures/Mvt/TermiteMvt.pdf", sep="/"), height=5, width=3.3)

ggplot(effects_table, aes(x=Palatability, y=est)) + 
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width =0.4)+ # don't plot bor bars on x axis tick, but separate them (dodge)
  geom_point(shape = 15, size =4, stroke = 1) +
  
  labs(y = "Number of gridlines crossed", x = NULL)+
  labs(title = "DB solution concentration: 3%") +
  scale_x_discrete(labels=c("Control termite", "DB termite"))+
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 10))  


dev.off()




