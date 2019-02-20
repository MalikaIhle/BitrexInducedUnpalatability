#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Preregistration manipulation color and unpalatability 
#  data extraction and handling
#	 Start : 31 october 2018
#	 last modif : add prevattackpalatability to allattacks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
### excluded FID:
  # 18604 : died during training
  # 18630 : protocol failed
  # 18632 : video failed
  # 18650 : video failed
  # 18687 : video failed

## color:  1=Green or 2=Brown
## outcome: 1= Consum or -1=Drop
## in DB: all times are sotred in character format with 6 digits '000000'

## AllAttacks table include all females tested since requirement for stopping the test was that they need to attack at least once 
## (or otherwise the test would have been repeated the day after, which did not happen) 
  
}


rm(list = ls(all = TRUE))


{# packages
library(RODBC) # this require R AND ACCESS to run on 32 bits ! (and apparently can't do it on MAC)
library(stringr) # this is needed for the function str_pad in ConvertTime function
library(dplyr)
}


{# genereric functions

ConvertToTime <- function(x){
  as.POSIXct(str_pad(x, 6, pad = "0"), format="%H%M%S") # this adds the date of today to every time... stupid but hard to get around and not important
}

}


{# load data

conDB= odbcConnectAccess2007("1_RawData/VideoAnalyses_1BitrexTermites.accdb")
sqlTables(conDB)	# list all the tables in the DB  


AllAttacks <- sqlQuery(conDB, "
                          SELECT Behav_Video_Metadata.FID, 
                                Basic_Trials.SubGroupName, 
                                Basic_Trials.GroupName, 
                                Behav_Female_Attacks.AttackTime, 
                                Behav_Female_Attacks.Color AS AttackedTermiteColor, 
                                Behav_Female_Attacks.Outcome, 
                                Behav_Video_Metadata.VideoTimeStart, 
                                Behav_Female.ExcludeYN, 
                                Behav_Female.ReasonExclusion
                          FROM ((Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID) INNER JOIN Behav_Video_Metadata ON Behav_Female.FID = Behav_Video_Metadata.FID) LEFT JOIN Behav_Female_Attacks ON Behav_Video_Metadata.VideoID = Behav_Female_Attacks.VideoID
                         GROUP BY Behav_Video_Metadata.FID, Basic_Trials.SubGroupName, Basic_Trials.GroupName, Behav_Female_Attacks.AttackTime, Behav_Female_Attacks.Color, Behav_Female_Attacks.Outcome, Behav_Video_Metadata.VideoTimeStart, Behav_Female.ExcludeYN, Behav_Female.ReasonExclusion;
                         " )

AllAttacks <- AllAttacks[AllAttacks$ExcludeYN == 0,] # see remarks


# reformat columns in tables
AllAttacks$AttackTime <- ConvertToTime(AllAttacks$AttackTime)
AllAttacks$VideoTimeStart <- ConvertToTime(AllAttacks$VideoTimeStart)
AllAttacks$AttackedTermiteColor[AllAttacks$AttackedTermiteColor == 1] <- "Green"
AllAttacks$AttackedTermiteColor[AllAttacks$AttackedTermiteColor == 2] <- "Brown"
AllAttacks$Outcome[AllAttacks$Outcome == 1] <- "Consumed"
AllAttacks$Outcome[AllAttacks$Outcome == -1] <- "Dropped"
str(AllAttacks)

# add variables
AllAttacks$DelayToAttack <- as.numeric(as.character(AllAttacks$AttackTime-AllAttacks$VideoTimeStart)) # in seconds
summary(AllAttacks$DelayToAttack) # looks good


AllFemales <- sqlQuery(conDB, "
SELECT Basic_Trials.Ind_ID AS FID, Basic_Trials.GroupName, Basic_Trials.SubGroupName, Behav_Female.ExcludeYN, Behav_Female.ReasonExclusion
FROM Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID;
                       ")

AllFemales <- AllFemales[AllFemales$ExcludeYN == 0,] # see remarks

  
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

nrow(FirstAttacks) # 95 looks good
rownames(FirstAttacks) <- NULL


}

head(FirstAttacks)

{# create table 2 lines per test: FocalTermiteAttack
FocalTermiteAttack <- rbind(AllFemales[,c('FID', 'GroupName', 'SubGroupName')],AllFemales[,c('FID', 'GroupName', 'SubGroupName')])
FocalTermiteAttack$FocalTermiteColor <- c(rep('Brown',nrow(AllFemales)),rep('Green',nrow(AllFemales)))
FocalTermiteAttack <- FocalTermiteAttack[order(FocalTermiteAttack$FID),]
nrow(FocalTermiteAttack) # 190 looks good


FocalTermiteAttack <- split(FocalTermiteAttack, FocalTermiteAttack$FID)

FocalTermiteAttack_fun <- function(x){
  x$FocalTermiteYN <- sample(c(0,1), 2,replace=FALSE) # randomly assigning YN, determining whether the termite is focal or not
  return(x)
}

FocalTermiteAttack <- do.call(rbind,lapply(FocalTermiteAttack,FocalTermiteAttack_fun))
rownames(FocalTermiteAttack) <- NULL


FocalTermiteAttack <- merge(FocalTermiteAttack, FirstAttacks[,c('FID','AttackedTermiteColor')], by='FID', all.x=TRUE)


FocalTermiteAttack$FocalTermiteAttackedYN[FocalTermiteAttack$FocalTermiteColor == FocalTermiteAttack$AttackedTermiteColor] <- 1
FocalTermiteAttack$FocalTermiteAttackedYN[FocalTermiteAttack$FocalTermiteColor != FocalTermiteAttack$AttackedTermiteColor] <- 0


for (i in 1:nrow(FocalTermiteAttack)) {
  
  if(FocalTermiteAttack$SubGroupName[i] == "GreenDB" & FocalTermiteAttack$FocalTermiteColor[i] == 'Green')
  {FocalTermiteAttack$FocalTermitePalatability[i] <- 0}
  
  if(FocalTermiteAttack$SubGroupName[i] == "GreenDB" & FocalTermiteAttack$FocalTermiteColor[i] == 'Brown')
  {FocalTermiteAttack$FocalTermitePalatability[i] <- 1}
  
  if(FocalTermiteAttack$SubGroupName[i] == "BrownDB" & FocalTermiteAttack$FocalTermiteColor[i] == 'Brown')
  {FocalTermiteAttack$FocalTermitePalatability[i] <- 0}

  if(FocalTermiteAttack$SubGroupName[i] == "BrownDB" & FocalTermiteAttack$FocalTermiteColor[i] == 'Green')
  {FocalTermiteAttack$FocalTermitePalatability[i] <- 1}
  
  }

FocalTermiteAttack$PriorExposureYN[FocalTermiteAttack$GroupName == 'DB'] <- 1
FocalTermiteAttack$PriorExposureYN[FocalTermiteAttack$GroupName == 'Water'] <- 0

}

head(FocalTermiteAttack) # dataset for model 1

{# create table 1 line per attack with outcome: AllAttacks
head(AllAttacks)

AllAttacks$DropYN[AllAttacks$Outcome == 'Consumed'] <- 0
AllAttacks$DropYN[AllAttacks$Outcome == 'Dropped'] <- 1
AllAttacks$PriorExposureYN[AllAttacks$GroupName == 'DB'] <- 1
AllAttacks$PriorExposureYN[AllAttacks$GroupName == 'Water'] <- 0

for (i in 1:nrow(AllAttacks)) {
  
  if(AllAttacks$SubGroupName[i] == "GreenDB" & AllAttacks$AttackedTermiteColor[i] == 'Green')
  {AllAttacks$AttackedTermitePalatability[i] <- 0}
  
  if(AllAttacks$SubGroupName[i] == "GreenDB" & AllAttacks$AttackedTermiteColor[i] == 'Brown')
  {AllAttacks$AttackedTermitePalatability[i] <- 1}
  
  if(AllAttacks$SubGroupName[i] == "BrownDB" & AllAttacks$AttackedTermiteColor[i] == 'Brown')
  {AllAttacks$AttackedTermitePalatability[i] <- 0}
  
  if(AllAttacks$SubGroupName[i] == "BrownDB" & AllAttacks$AttackedTermiteColor[i] == 'Green')
  {AllAttacks$AttackedTermitePalatability[i] <- 1}
  
}

}

head(AllAttacks) # dataset for model 2

{# create table for exploratory analysis on delay to first attack
for (i in 1:nrow(FirstAttacks)) {
  
  if(FirstAttacks$SubGroupName[i] == "GreenDB" & FirstAttacks$AttackedTermiteColor[i] == 'Green')
  {FirstAttacks$AttackedTermitePalatability[i] <- 0}
  
  if(FirstAttacks$SubGroupName[i] == "GreenDB" & FirstAttacks$AttackedTermiteColor[i] == 'Brown')
  {FirstAttacks$AttackedTermitePalatability[i] <- 1}
  
  if(FirstAttacks$SubGroupName[i] == "BrownDB" & FirstAttacks$AttackedTermiteColor[i] == 'Brown')
  {FirstAttacks$AttackedTermitePalatability[i] <- 0}
  
  if(FirstAttacks$SubGroupName[i] == "BrownDB" & FirstAttacks$AttackedTermiteColor[i] == 'Green')
  {FirstAttacks$AttackedTermitePalatability[i] <- 1}
  
}
}

head(FirstAttacks) # dataset for exploratory analyses

{# add info on previous attack termite palatability
AllAttacks <- as.data.frame(AllAttacks %>% group_by(FID) %>% arrange(AttackTime, .by_group = TRUE) %>% mutate(AttackNb = row_number()))

AllAttacks_perFID <- split(AllAttacks, AllAttacks$FID)

AllAttacks_perFID_fun <- function(x){
x$PrevPalatabality <- c(NA,x$AttackedTermitePalatability[-nrow(x)])
return(x)
}

AllAttacks <- do.call(rbind,lapply(AllAttacks_perFID,AllAttacks_perFID_fun))
rownames(AllAttacks) <- NULL

  #AllAttacks[AllAttacks$FID %in% AllAttacks$FID[AllAttacks$AttackNb == 2 & AllAttacks$AttackedTermitePalatability == "1"],]

  #AllAttacks$DropYN[AllAttacks$AttackNb == 2 & AllAttacks$AttackedTermitePalatability == "1" & AllAttacks$PrevPalatabality == "0"]
  #AllAttacks$DropYN[AllAttacks$AttackNb == 1 & AllAttacks$AttackedTermitePalatability == "1"]
}

head(AllAttacks) # dataset for exploration on contamination because of chemical on mouth parts
  

## output_folder <- "3_ExtractedData"

## write.csv(FocalTermiteAttack, file = paste(output_folder,"FocalTermiteAttack.csv", sep="/"), row.names = FALSE) 
## write.csv(AllAttacks, file = paste(output_folder,"AllAttacks.csv", sep="/"), row.names = FALSE) 
## write.csv(FirstAttacks, file = paste(output_folder,"FirstAttacks.csv", sep="/"), row.names = FALSE) 
# 20181031 first time
# 20181101 with prior exposure to allattack table
# 20181101 correct a SERIOUS typo reversing all the colors !!!!!
# 20181121 added data from cohort 3
# 20190220 add prevattackpalatability to allattacks

