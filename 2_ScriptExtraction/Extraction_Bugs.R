#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data extraction and handling of bug test
#	 Start : 18 March
#	 commit: first commit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
  ## color:  1=Green or 2=Brown
  ## outcome: 1= Consum or -1=Drop
  ## in DB: all times are stored in character format with 6 digits '000000'
  
  ## AllAttacks table include all females tested since requirement for stopping the test was that they need to attack at least once 
  ## or otherwise the test was repeated the day after, which did happen many times - see remarks in basic trial 
  
}


rm(list = ls(all = TRUE))


{# packages
  library(RODBC) # this require R AND ACCESS to run on 32 bits ! (and apparently can't do it on MAC)
  library(stringr) # this is needed for the function str_pad in ConvertTime function
  library(dplyr)
  library(here)
}


{# genereric functions
  
  ConvertToTime <- function(x){
    as.POSIXct(str_pad(x, 6, pad = "0"), format="%H%M%S") # this adds the date of today to every time... stupid but hard to get around and not important
  }
  
}


{# load data
  
  conDB= odbcConnectAccess2007(paste(here(),"1_RawData/VideoAnalyses_MilkweedTest.accdb", sep='/'))
  sqlTables(conDB)	# list all the tables in the DB  
  
  AllAttacks <- sqlQuery(conDB, "
                         SELECT Behav_Video_Metadata.FID, Basic_Trials.SubGroupName AS Trt, Behav_Female_Attacks.AttackTime, Behav_Female_Attacks.Color AS AttackedBugColor, Behav_Female_Attacks.Outcome, Behav_Video_Metadata.VideoTimeStart
FROM ((Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID) INNER JOIN Behav_Video_Metadata ON Behav_Female.FID = Behav_Video_Metadata.FID) LEFT JOIN Behav_Female_Attacks ON Behav_Video_Metadata.VideoID = Behav_Female_Attacks.VideoID
                         GROUP BY Behav_Video_Metadata.FID, Basic_Trials.SubGroupName, Behav_Female_Attacks.AttackTime, Behav_Female_Attacks.Color, Behav_Female_Attacks.Outcome, Behav_Video_Metadata.VideoTimeStart;
                         " )
  

  
  # reformat columns in tables
  AllAttacks$AttackTime <- ConvertToTime(AllAttacks$AttackTime)
  AllAttacks$VideoTimeStart <- ConvertToTime(AllAttacks$VideoTimeStart)
  AllAttacks$AttackedBugColor[AllAttacks$AttackedBugColor == 1] <- "Green"
  AllAttacks$AttackedBugColor[AllAttacks$AttackedBugColor == 2] <- "Brown"
  AllAttacks$Outcome[AllAttacks$Outcome == 1] <- "Consumed"
  AllAttacks$Outcome[AllAttacks$Outcome == -1] <- "Dropped"
  str(AllAttacks)
  
  # add variables
  AllAttacks$DelayToAttack <- as.numeric(as.character(AllAttacks$AttackTime-AllAttacks$VideoTimeStart)) # in seconds
  summary(AllAttacks$DelayToAttack) # looks good
  
  
  AllFemales <- sqlQuery(conDB, "
                         SELECT Basic_Trials.Ind_ID AS FID, 
                                Basic_Trials.SubGroupName As Trt
                         FROM Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID;
                         ")
  

  close(conDB)
  
}

head(AllFemales)
head(AllAttacks)


{# create table first attack
  
  FirstAttacks <- split(AllAttacks, AllAttacks$FID)
  
  FirstAttacks_fun <- function(x){
    x <- x[x$AttackTime == min(x$AttackTime),] 
    return(x)
  }
  
  FirstAttacks <- do.call(rbind,lapply(FirstAttacks,FirstAttacks_fun))
  
  nrow(FirstAttacks) # 40 looks good
  rownames(FirstAttacks) <- NULL
  
  
}

head(FirstAttacks)

{# create table 2 lines per test: FocalBugAttack
  FocalBugAttack <- rbind(AllFemales[,c('FID', 'Trt')],AllFemales[,c('FID', 'Trt')])
  FocalBugAttack$FocalBugColor <- c(rep('Brown',nrow(AllFemales)),rep('Green',nrow(AllFemales)))
  FocalBugAttack <- FocalBugAttack[order(FocalBugAttack$FID),]
  nrow(FocalBugAttack) # 80 looks good
  
  
  FocalBugAttack <- split(FocalBugAttack, FocalBugAttack$FID)
  
  FocalBugAttack_fun <- function(x){
    x$FocalBugYN <- sample(c(0,1), 2,replace=FALSE) # randomly assigning YN, determining whether the bug is focal or not
    return(x[x$FocalBugYN == 1,])
  }
  
  FocalBugAttack <- do.call(rbind,lapply(FocalBugAttack,FocalBugAttack_fun))
  rownames(FocalBugAttack) <- NULL
  
  
  FocalBugAttack <- merge(FocalBugAttack, FirstAttacks[,c('FID','AttackedBugColor')], by='FID', all.x=TRUE)
  
  
  FocalBugAttack$FocalBugAttackedYN[FocalBugAttack$FocalBugColor == FocalBugAttack$AttackedBugColor] <- 1
  FocalBugAttack$FocalBugAttackedYN[FocalBugAttack$FocalBugColor != FocalBugAttack$AttackedBugColor] <- 0
  
  
  for (i in 1:nrow(FocalBugAttack)) {
    
    if(FocalBugAttack$Trt[i] == "GreenMW" & FocalBugAttack$FocalBugColor[i] == 'Green')
    {FocalBugAttack$FocalBugPalatability[i] <- 0}
    
    if(FocalBugAttack$Trt[i] == "GreenMW" & FocalBugAttack$FocalBugColor[i] == 'Brown')
    {FocalBugAttack$FocalBugPalatability[i] <- 1}
    
    if(FocalBugAttack$Trt[i] == "BrownMW" & FocalBugAttack$FocalBugColor[i] == 'Brown')
    {FocalBugAttack$FocalBugPalatability[i] <- 0}
    
    if(FocalBugAttack$Trt[i] == "BrownMW" & FocalBugAttack$FocalBugColor[i] == 'Green')
    {FocalBugAttack$FocalBugPalatability[i] <- 1}
    
  }
  
  
  
}

head(FocalBugAttack) # dataset for model 1

{# create table 1 line per attack with outcome: AllAttacks
  head(AllAttacks)
  
  AllAttacks$DropYN[AllAttacks$Outcome == 'Consumed'] <- 0
  AllAttacks$DropYN[AllAttacks$Outcome == 'Dropped'] <- 1
  
  
  for (i in 1:nrow(AllAttacks)) {
    
    if(AllAttacks$Trt[i] == "GreenMW" & AllAttacks$AttackedBugColor[i] == 'Green')
    {AllAttacks$AttackedBugPalatability[i] <- 0}
    
    if(AllAttacks$Trt[i] == "GreenMW" & AllAttacks$AttackedBugColor[i] == 'Brown')
    {AllAttacks$AttackedBugPalatability[i] <- 1}
    
    if(AllAttacks$Trt[i] == "BrownMW" & AllAttacks$AttackedBugColor[i] == 'Brown')
    {AllAttacks$AttackedBugPalatability[i] <- 0}
    
    if(AllAttacks$Trt[i] == "BrownMW" & AllAttacks$AttackedBugColor[i] == 'Green')
    {AllAttacks$AttackedBugPalatability[i] <- 1}
    
  }
  
}

head(AllAttacks) # dataset for model 2

{# create table for exploratory analysis on delay to first attack
  for (i in 1:nrow(FirstAttacks)) {
    
    if(FirstAttacks$Trt[i] == "GreenMW" & FirstAttacks$AttackedBugColor[i] == 'Green')
    {FirstAttacks$AttackedBugPalatability[i] <- 0}
    
    if(FirstAttacks$Trt[i] == "GreenMW" & FirstAttacks$AttackedBugColor[i] == 'Brown')
    {FirstAttacks$AttackedBugPalatability[i] <- 1}
    
    if(FirstAttacks$Trt[i] == "BrownMW" & FirstAttacks$AttackedBugColor[i] == 'Brown')
    {FirstAttacks$AttackedBugPalatability[i] <- 0}
    
    if(FirstAttacks$Trt[i] == "BrownMW" & FirstAttacks$AttackedBugColor[i] == 'Green')
    {FirstAttacks$AttackedBugPalatability[i] <- 1}
    
  }
}

head(FirstAttacks) # dataset for exploratory analyses

<<<<<<< HEAD
=======
{# add info on previous attack termite palatability
  AllAttacks <- as.data.frame(AllAttacks %>% group_by(FID) %>% arrange(AttackTime, .by_group = TRUE) %>% mutate(AttackNb = row_number()))
  
  AllAttacks_perFID <- split(AllAttacks, AllAttacks$FID)
  
  AllAttacks_perFID_fun <- function(x){
    x$PrevPalatabality <- c(NA,x$AttackedTermitePalatability[-nrow(x)])
    return(x)
  }
  
  AllAttacks <- do.call(rbind,lapply(AllAttacks_perFID,AllAttacks_perFID_fun))
  rownames(AllAttacks) <- NULL
  
}

head(AllAttacks) # dataset for exploration on contamination because of chemical on mouth parts
>>>>>>> d32cab263bac3b6ea042e999d0ecac98ceec3528


 output_folder <- paste(here(),"3_ExtractedData", sep='/')
 write.csv(FocalBugAttack, file = paste(output_folder,"FocalAttacks/FocalBugAttack.csv", sep="/"), row.names = FALSE) 
 write.csv(AllAttacks, file = paste(output_folder,"AllAttacks/AllAttacksBug.csv", sep="/"), row.names = FALSE) 
 write.csv(FirstAttacks, file = paste(output_folder,"FirstAttacks/FirstAttacksBug.csv", sep="/"), row.names = FALSE) 
# 20190318 first time (focal table with just one line)
