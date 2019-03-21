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

{# create table 2 lines per test: FocalAttacks
  FocalAttacks <- rbind(AllFemales[,c('FID', 'Trt')],AllFemales[,c('FID', 'Trt')])
  FocalAttacks$FocalBugColor <- c(rep('Brown',nrow(AllFemales)),rep('Green',nrow(AllFemales)))
  FocalAttacks <- FocalAttacks[order(FocalAttacks$FID),]
  nrow(FocalAttacks) # 80 looks good
  
  
  FocalAttacks <- split(FocalAttacks, FocalAttacks$FID)
  
  FocalAttacks_fun <- function(x){
    x$FocalBugYN <- sample(c(0,1), 2,replace=FALSE) # randomly assigning YN, determining whether the bug is focal or not
    return(x)
  }
  
  FocalAttacks <- do.call(rbind,lapply(FocalAttacks,FocalAttacks_fun))
  rownames(FocalAttacks) <- NULL
  
  
  FocalAttacks <- merge(FocalAttacks, FirstAttacks[,c('FID','AttackedBugColor')], by='FID', all.x=TRUE)
  
  
  FocalAttacks$FocalAttacksedYN[FocalAttacks$FocalBugColor == FocalAttacks$AttackedBugColor] <- 1
  FocalAttacks$FocalAttacksedYN[FocalAttacks$FocalBugColor != FocalAttacks$AttackedBugColor] <- 0
  
  
  for (i in 1:nrow(FocalAttacks)) {
    
    if(FocalAttacks$Trt[i] == "GreenMW" & FocalAttacks$FocalBugColor[i] == 'Green')
    {FocalAttacks$FocalBugPalatability[i] <- 0}
    
    if(FocalAttacks$Trt[i] == "GreenMW" & FocalAttacks$FocalBugColor[i] == 'Brown')
    {FocalAttacks$FocalBugPalatability[i] <- 1}
    
    if(FocalAttacks$Trt[i] == "BrownMW" & FocalAttacks$FocalBugColor[i] == 'Brown')
    {FocalAttacks$FocalBugPalatability[i] <- 0}
    
    if(FocalAttacks$Trt[i] == "BrownMW" & FocalAttacks$FocalBugColor[i] == 'Green')
    {FocalAttacks$FocalBugPalatability[i] <- 1}
    
  }
  
  
  
}

head(FocalAttacks) # dataset for model 1

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



 output_folder <- paste(here(),"3_ExtractedData", sep='/')
 write.csv(FocalAttacks, file = paste(output_folder,"FocalAttacks/FocalBugAttacks.csv", sep="/"), row.names = FALSE) 
 write.csv(AllAttacks, file = paste(output_folder,"AllAttacks/AllBugAttacks.csv", sep="/"), row.names = FALSE) 
 write.csv(FirstAttacks, file = paste(output_folder,"FirstAttacks/FirstBugAttacks.csv", sep="/"), row.names = FALSE) 
# 20190319 FocalAttacks still has two lines per test
