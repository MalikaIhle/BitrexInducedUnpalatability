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



# stats

mod <-glm(NbGridCrossed ~ Treatment, data = tbl, family = 'poisson') # intercept is MW non painted bug
summary(mod)



#setEPS()
#pdf(paste(here(), "5_Figures/Mvt/BugMvt.pdf", sep="/"), height=7, width=15)
boxplot(tbl$NbGridCrossed~tbl$Treatment, main = 'BugMvt')
#dev.off()






