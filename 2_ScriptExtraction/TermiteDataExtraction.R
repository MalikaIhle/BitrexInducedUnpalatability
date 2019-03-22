#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Data extraction for manipulation of palatability with bitrex
#	 Start : 21 march 2019
#	 last modif : write function to apply to all DBs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))

{# Remarks
  ## color:  1=Green or 2=Brown
  ## outcome: 1= Consum or -1=Drop
  ## in DB: all times are sotred in character format with 6 digits '000000'
  
  ## AllAttacks table include all females tested since requirement for stopping the test was that they need to attack at least once 
  ## (or otherwise the test would have been repeated the day after, which did not happen) 
  
  
  ### excluded FID in 1%:
  # 18604 : died during training
  # 18630 : protocol failed
  # 18632 : video failed
  # 18650 : video failed
  # 18687 : video failed
  
  
}


# packages

library(RODBC) # this require R AND ACCESS to run on 32 bits ! (and apparently can't do it on MAC)
library(stringr) # for function convert time
library(dplyr) # to reformat / summarize data
library(here) # to write csv in subfolder


# functions

ConvertToTime <- function(x){
  as.POSIXct(str_pad(x, 6, pad = "0"), format="%H%M%S") # this adds the date of today to every time... stupid but hard to get around and not important
}

callDB_and_create_AllAttacks <- function(conDB){
  
AllAttacks <- sqlQuery(conDB, "
                          SELECT Behav_Video_Metadata.FID, 
                                Basic_Trials.SubGroupName, 
                                Basic_Trials.GroupName, 
                                Behav_Female_Attacks.AttackTime, 
                                Behav_Female_Attacks.Color AS ColorCode, 
                                Behav_Female_Attacks.Outcome, 
                                Behav_Video_Metadata.VideoTimeStart, 
                                Behav_Female.ExcludeYN, 
                                Behav_Female.ReasonExclusion
                          FROM ((Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID) INNER JOIN Behav_Video_Metadata ON Behav_Female.FID = Behav_Video_Metadata.FID) LEFT JOIN Behav_Female_Attacks ON Behav_Video_Metadata.VideoID = Behav_Female_Attacks.VideoID
                         GROUP BY Behav_Video_Metadata.FID, Basic_Trials.SubGroupName, Basic_Trials.GroupName, Behav_Female_Attacks.AttackTime, Behav_Female_Attacks.Color, Behav_Female_Attacks.Outcome, Behav_Video_Metadata.VideoTimeStart, Behav_Female.ExcludeYN, Behav_Female.ReasonExclusion;
                         " )

AllAttacks <- AllAttacks[AllAttacks$ExcludeYN == 0,] # see remarks


# reformat columns in tables to have choice of factors or numerical variables
AllAttacks$AttackTime <- ConvertToTime(AllAttacks$AttackTime)
AllAttacks$VideoTimeStart <- ConvertToTime(AllAttacks$VideoTimeStart)
AllAttacks$AttackedColor[AllAttacks$ColorCode == 1] <- "Green"
AllAttacks$AttackedColor[AllAttacks$ColorCode == 2] <- "Brown"
AllAttacks$Fate[AllAttacks$Outcome == 1] <- "Consumed"
AllAttacks$Fate[AllAttacks$Outcome == -1] <- "Rejected"
AllAttacks$DropYN[AllAttacks$Outcome == 1] <- 0
AllAttacks$DropYN[AllAttacks$Outcome == -1] <- 1
AllAttacks$PriorExposureYN[AllAttacks$GroupName == 'DB'] <- "1"
AllAttacks$PriorExposureYN[AllAttacks$GroupName == 'Water'] <- "0"
AllAttacks$PriorExposure[AllAttacks$PriorExposureYN == 1] <- 'Trained'
AllAttacks$PriorExposure[AllAttacks$PriorExposureYN == 0] <- 'Naive'
AllAttacks$DelayToAttack <- as.numeric(as.character(AllAttacks$AttackTime-AllAttacks$VideoTimeStart))

for (i in 1:nrow(AllAttacks)) {
if(!is.na(AllAttacks$SubGroupName[i])){  
  if(AllAttacks$SubGroupName[i] == "GreenDB" & AllAttacks$AttackedColor[i] == 'Green')
  {AllAttacks$AttackedPalatability[i] <- 0
  AllAttacks$AttackedPalatabilityTreatment[i] <- "DB"}
  
  if(AllAttacks$SubGroupName[i] == "GreenDB" & AllAttacks$AttackedColor[i] == 'Brown')
  {AllAttacks$AttackedPalatability[i] <- 1
  AllAttacks$AttackedPalatabilityTreatment[i] <- "Control"}
  
  if(AllAttacks$SubGroupName[i] == "BrownDB" & AllAttacks$AttackedColor[i] == 'Brown')
  {AllAttacks$AttackedPalatability[i] <- 0
  AllAttacks$AttackedPalatabilityTreatment[i] <- "DB"}
  
  if(AllAttacks$SubGroupName[i] == "BrownDB" & AllAttacks$AttackedColor[i] == 'Green')
  {AllAttacks$AttackedPalatability[i] <- 1
  AllAttacks$AttackedPalatabilityTreatment[i] <- "Control"}
 
}}

## info on previous attacked prey palatability
AllAttacks <- as.data.frame(AllAttacks %>% group_by(FID) %>% arrange(AttackTime, .by_group = TRUE) %>% mutate(AttackNb = row_number()))

AllAttacks_perFID <- split(AllAttacks, AllAttacks$FID)

AllAttacks_perFID_fun <- function(x){
  x$PrevPalatabality <- c(NA,x$AttackedPalatability[-nrow(x)])
  return(x)
}

AllAttacks <- do.call(rbind,lapply(AllAttacks_perFID,AllAttacks_perFID_fun))
rownames(AllAttacks) <- NULL


return(AllAttacks)

} # to analyses drop rates

create_FirstAttacks <- function(AllAttacks) {
FirstAttacks <- split(AllAttacks, AllAttacks$FID)

FirstAttacks_fun <- function(x){
  x <- x[x$AttackTime == min(x$AttackTime),] 
  return(x)
}

FirstAttacks <- do.call(rbind,lapply(FirstAttacks,FirstAttacks_fun))
rownames(FirstAttacks) <- NULL
 return(FirstAttacks)
} # to explore delay to attack (and build Focaldatasets)

callDB_and_create_Structure_AttacksFocal <- function(conDB, FirstAttacks){

AllFemales <- sqlQuery(conDB, "
SELECT Basic_Trials.Ind_ID AS FID, Basic_Trials.GroupName, Basic_Trials.SubGroupName, Behav_Female.ExcludeYN, Behav_Female.ReasonExclusion
FROM Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID;
                     ")
AllFemales <- AllFemales[AllFemales$ExcludeYN == 0,] # see remarks

FocalTermiteAttack <- rbind(AllFemales[,c('FID', 'GroupName', 'SubGroupName')],AllFemales[,c('FID', 'GroupName', 'SubGroupName')])
FocalTermiteAttack$FocalTermiteColor <- c(rep('Brown',nrow(AllFemales)),rep('Green',nrow(AllFemales)))
FocalTermiteAttack <- FocalTermiteAttack[order(FocalTermiteAttack$FID),]
nrow(FocalTermiteAttack) # 200 looks good


FocalTermiteAttack <- merge(FocalTermiteAttack, FirstAttacks[,c('FID','AttackedColor')], by='FID', all.x=TRUE)


FocalTermiteAttack$FocalTermiteAttackedYN[FocalTermiteAttack$FocalTermiteColor == FocalTermiteAttack$AttackedColor] <- 1
FocalTermiteAttack$FocalTermiteAttackedYN[FocalTermiteAttack$FocalTermiteColor != FocalTermiteAttack$AttackedColor] <- 0


for (i in 1:nrow(FocalTermiteAttack)) {
  
  if(FocalTermiteAttack$SubGroupName[i] == "GreenDB" & FocalTermiteAttack$FocalTermiteColor[i] == 'Green')
  {FocalTermiteAttack$FocalPalatability[i] <- 0
  FocalTermiteAttack$FocalPalatabilityTreatment[i] <- "DB"}
  
  if(FocalTermiteAttack$SubGroupName[i] == "GreenDB" & FocalTermiteAttack$FocalTermiteColor[i] == 'Brown')
  {FocalTermiteAttack$FocalPalatability[i] <- 1
  FocalTermiteAttack$FocalPalatabilityTreatment[i] <- "Control"}
  
  if(FocalTermiteAttack$SubGroupName[i] == "BrownDB" & FocalTermiteAttack$FocalTermiteColor[i] == 'Brown')
  {FocalTermiteAttack$FocalPalatability[i] <- 0
  FocalTermiteAttack$FocalPalatabilityTreatment[i] <- "DB"}
  
  if(FocalTermiteAttack$SubGroupName[i] == "BrownDB" & FocalTermiteAttack$FocalTermiteColor[i] == 'Green')
  {FocalTermiteAttack$FocalPalatability[i] <- 1
  FocalTermiteAttack$FocalPalatabilityTreatment[i] <- "Control"}
  
}

FocalTermiteAttack$PriorExposureYN[FocalTermiteAttack$GroupName == 'DB'] <- 1
FocalTermiteAttack$PriorExposureYN[FocalTermiteAttack$GroupName == 'Water'] <- 0
FocalTermiteAttack$PriorExposure[FocalTermiteAttack$PriorExposureYN == 1] <- 'Trained'
FocalTermiteAttack$PriorExposure[FocalTermiteAttack$PriorExposureYN == 0] <- 'Naive'

return(FocalTermiteAttack)
} # full table (2 lines per test) to sample focal from and create 1000 datasets for model 1


# extract and process (apply functions)

conDB0= odbcConnectAccess2007("1_RawData/VideoAnalyses_0_control_BitrexTermites.accdb")
AllAttacks0 <- callDB_and_create_AllAttacks(conDB0)
head(AllAttacks0)
FirstAttacks0 <- create_FirstAttacks(AllAttacks0)
head(FirstAttacks0)
close(conDB0)

conDB1= odbcConnectAccess2007("1_RawData/VideoAnalyses_1BitrexTermites.accdb")
AllAttacks1 <- callDB_and_create_AllAttacks(conDB1)
head(AllAttacks1)
FirstAttacks1 <- create_FirstAttacks(AllAttacks1)
head(FirstAttacks1)
FocalAttacks1 <- callDB_and_create_Structure_AttacksFocal(conDB1,FirstAttacks1)
head(FocalAttacks1)
close(conDB1)

conDB15= odbcConnectAccess2007("1_RawData/VideoAnalyses_1_5BitrexTermites.accdb")
AllAttacks15 <- callDB_and_create_AllAttacks(conDB15)
head(AllAttacks15)
FirstAttacks15 <- create_FirstAttacks(AllAttacks15)
head(FirstAttacks15)
FocalAttacks15 <- callDB_and_create_Structure_AttacksFocal(conDB15,FirstAttacks15)
head(FocalAttacks15)
close(conDB15)

conDB2 = odbcConnectAccess2007("1_RawData/VideoAnalyses_2BitrexTermites.accdb")
AllAttacks2 <- callDB_and_create_AllAttacks(conDB2)
head(AllAttacks2)
FirstAttacks2 <- create_FirstAttacks(AllAttacks2)
head(FirstAttacks2)
FocalAttacks2 <- callDB_and_create_Structure_AttacksFocal(conDB2,FirstAttacks2)
head(FocalAttacks2)
close(conDB2)

conDB3 = odbcConnectAccess2007("1_RawData/VideoAnalyses_3BitrexTermites.accdb")
AllAttacks3 <- callDB_and_create_AllAttacks(conDB3)
head(AllAttacks3)
FirstAttacks3 <- create_FirstAttacks(AllAttacks3)
head(FirstAttacks3)
FocalAttacks3 <- callDB_and_create_Structure_AttacksFocal(conDB3,FirstAttacks3)
head(FocalAttacks3)
close(conDB3)

conDB3F = odbcConnectAccess2007("1_RawData/VideoAnalyses_3_Final_BitrexTermites.accdb")
AllAttacks3F <- callDB_and_create_AllAttacks(conDB3F)
head(AllAttacks3F)
FirstAttacks3F <- create_FirstAttacks(AllAttacks3F)
head(FirstAttacks3F)
FocalAttacks3F <- callDB_and_create_Structure_AttacksFocal(conDB3F,FirstAttacks3F)
head(FocalAttacks3F)
close(conDB3F)


# write csv files

output_folder <- paste(here(),"3_ExtractedData", sep='/')

write.csv(AllAttacks0, file = paste(output_folder,"AllAttacks/AllAttacks0.csv", sep="/"), row.names = FALSE) 
write.csv(FirstAttacks0, file = paste(output_folder,"FirstAttacks/FirstAttacks0.csv", sep="/"), row.names = FALSE) 

write.csv(FocalAttacks1, file = paste(output_folder,"FocalAttacks/FocalAttacks1.csv", sep="/"), row.names = FALSE) 
write.csv(AllAttacks1, file = paste(output_folder,"AllAttacks/AllAttacks1.csv", sep="/"), row.names = FALSE) 
write.csv(FirstAttacks1, file = paste(output_folder,"FirstAttacks/FirstAttacks1.csv", sep="/"), row.names = FALSE) 

write.csv(FocalAttacks15, file = paste(output_folder,"FocalAttacks/FocalAttacks15.csv", sep="/"), row.names = FALSE) 
write.csv(AllAttacks15, file = paste(output_folder,"AllAttacks/AllAttacks15.csv", sep="/"), row.names = FALSE) 
write.csv(FirstAttacks15, file = paste(output_folder,"FirstAttacks/FirstAttacks15.csv", sep="/"), row.names = FALSE) 

write.csv(FocalAttacks2, file = paste(output_folder,"FocalAttacks/FocalAttacks2.csv", sep="/"), row.names = FALSE) 
write.csv(AllAttacks2, file = paste(output_folder,"AllAttacks/AllAttacks2.csv", sep="/"), row.names = FALSE) 
write.csv(FirstAttacks2, file = paste(output_folder,"FirstAttacks/FirstAttacks2.csv", sep="/"), row.names = FALSE) 

write.csv(FocalAttacks3, file = paste(output_folder,"FocalAttacks/FocalAttacks3.csv", sep="/"), row.names = FALSE) 
write.csv(AllAttacks3, file = paste(output_folder,"AllAttacks/AllAttacks3.csv", sep="/"), row.names = FALSE) 
write.csv(FirstAttacks3, file = paste(output_folder,"FirstAttacks/FirstAttacks3.csv", sep="/"), row.names = FALSE) 

write.csv(FocalAttacks3F, file = paste(output_folder,"FocalAttacks/FocalAttacks3F.csv", sep="/"), row.names = FALSE) 
write.csv(AllAttacks3F, file = paste(output_folder,"AllAttacks/AllAttacks3F.csv", sep="/"), row.names = FALSE) 
write.csv(FirstAttacks3F, file = paste(output_folder,"FirstAttacks/FirstAttacks3F.csv", sep="/"), row.names = FALSE) 




