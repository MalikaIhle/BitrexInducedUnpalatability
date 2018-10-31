#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Preregistration manipulation color and unpalatability 
#  data extraction and handling
#	 Start : 31 october 2018
#	 last modif : 
#	 commit: first commit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


{# Remarks
### excluded stuff ID, Video ID
  ####  variable calculatin weird
  
  
  ## color:  1=Green or 2=Brown
  # outcome
  # attack time secv
}


rm(list = ls(all = TRUE))


{# packages
library(RODBC) # this require R AND ACCESS to run on 32 bits ! (and apparently can't do it on MAC)
library(stringr) # this is needed for the function str_pad in ConvertTime function
  
  }


# genereric functions

ConvertToTime <- function(x){
  as.POSIXct(str_pad(x, 6, pad = "0"), format="%H%M%S") # this adds the date of today to every time... stupid but hard to get around and not important
}




{# load data
  
  conDB= odbcConnectAccess2007("C:\\Users\\malika.ihle\\Dropbox\\HabronatusPyrrithrix\\VideoAnalyses_BitrexTermites.accdb")
  sqlTables(conDB)	# list all the tables in the DB  


  Basic_Trials = sqlFetch(conDB, "Basic_Trials")
  
  
  AllAttacks <- sqlQuery(conDB, "
                          SELECT Behav_Video_Metadata.FID, 
                          Basic_Trials.SubGroupName,
                          Basic_Trials.GroupName, 
                          Behav_Female_Attacks.AttackTime, 
                          Behav_Female_Attacks.Color, 
                          Behav_Female_Attacks.Outcome
                          FROM ((Basic_Trials 
                          INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID) 
                          INNER JOIN Behav_Video_Metadata ON Behav_Female.FID = Behav_Video_Metadata.FID) 
                          LEFT JOIN Behav_Female_Attacks ON Behav_Video_Metadata.VideoID = Behav_Female_Attacks.VideoID
                          GROUP BY Behav_Video_Metadata.FID, 
                          Basic_Trials.SubGroupName, 
                          Basic_Trials.GroupName, 
                          Behav_Female_Attacks.AttackTime, 
                          Behav_Female_Attacks.Color, 
                          Behav_Female_Attacks.Outcome;
                         " )

FirstAttacks <- sqlQuery(conDB, "
                       SELECT Basic_Trials.Ind_ID AS FID, Min(Behav_Female_Attacks.AttackTime) AS MinOfAttackTime, Behav_Video_Metadata.VideoTimeStart
FROM (Basic_Trials LEFT JOIN Behav_Video_Metadata ON Basic_Trials.Ind_ID = Behav_Video_Metadata.FID) LEFT JOIN Behav_Female_Attacks ON Behav_Video_Metadata.VideoID = Behav_Female_Attacks.VideoID
GROUP BY Basic_Trials.Ind_ID, Behav_Video_Metadata.VideoTimeStart;")


close(conDB)

}


{# Exclude Some females for reasons specified a priori (see remarks)

ExcludedIDs <- c(18630,18632,18650,18687) # see remarks

AllAttacks[AllAttacks$FID %in% ExcludedIDs,]
nrow(AllAttacks)
AllAttacksValid <- AllAttacks[!AllAttacks$FID %in% ExcludedIDs ,]
nrow(AllAttacksValid)

FirstAttacksValid <- FirstAttacks[!FirstAttacks$FID %in% ExcludedIDs ,]

}

head(AllAttacksValid)
head(FirstAttacksValid)


# reformat columns in tables
AllAttacksValid$AttackTime <- ConvertToTime(AllAttacksValid$AttackTime)
FirstAttacksValid$MinOfAttackTime <- ConvertToTime(FirstAttacksValid$MinOfAttackTime)
FirstAttacksValid$VideoTimeStart <- ConvertToTime(FirstAttacksValid$VideoTimeStart)

AllAttacksValid$Color[AllAttacksValid$Color == 1] <- "Brown"
AllAttacksValid$Color[AllAttacksValid$Color == 2] <- "Green"



# add variables

FirstAttacksValid$DelayToAttack <- FirstAttacksValid$MinOfAttackTime-FirstAttacksValid$VideoTimeStart

FirstAttacksValid[FirstAttacksValid$DelayToAttack<0,] ## need checking

















