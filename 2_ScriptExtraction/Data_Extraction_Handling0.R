#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data extraction and handling
#	 Start : 19 february 2019
#	 last commit: first commit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
  ## color:  1=Green or 2=Brown
  ## outcome: 1= Consum or -1=Drop
  ## in DB: all times are sotred in character format with 6 digits '000000'
  
  ## AllAttacks table include all females tested
  
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
  
  conDB= odbcConnectAccess2007("1_RawData/VideoAnalyses_0_control_BitrexTermites.accdb")
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
  AllAttacks$DropYN[AllAttacks$Outcome == 'Consumed'] <- 0
  AllAttacks$DropYN[AllAttacks$Outcome == 'Dropped'] <- 1
  str(AllAttacks)
  
  # add variables
  AllAttacks$DelayToAttack <- as.numeric(as.character(AllAttacks$AttackTime-AllAttacks$VideoTimeStart)) # in seconds
  summary(AllAttacks$DelayToAttack) # looks good
  
  close(conDB)
  
}

head(AllAttacks)


{# create table first attack
  
  FirstAttacks <- split(AllAttacks, AllAttacks$FID)
  
  FirstAttacks_fun <- function(x){
    x <- x[x$AttackTime == min(x$AttackTime),] 
    return(x)
  }
  
  FirstAttacks <- do.call(rbind,lapply(FirstAttacks,FirstAttacks_fun))
  
  nrow(FirstAttacks) # 30 looks good
  rownames(FirstAttacks) <- NULL
  
  
}

head(FirstAttacks)


## output_folder <- "3_ExtractedData"

## write.csv(AllAttacks, file = paste(output_folder,"AllAttacks0.csv", sep="/"), row.names = FALSE) 
## write.csv(FirstAttacks, file = paste(output_folder,"FirstAttacks0.csv", sep="/"), row.names = FALSE) 
### 20190219 first time

