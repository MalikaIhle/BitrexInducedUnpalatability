#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Data extraction for manipulation of palatability with bitrex with other type of preys
#	 Start : 22 march 2019
#	 last modif : first
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
  ## outcome: 1= Consum or -1=Drop
  
  
  # Additionally, separatekym with other spiders, one palatable prey was provided per spider (n = number of spiders):
  # colored crickets (n=10)
  # caped termites (n=10)
  # fruit flies (n=10)
  # they all consumed that given palatable prey
  # we will add those lines below
  
  
}


rm(list = ls(all = TRUE))

# packages

library(RODBC) # this require R AND ACCESS to run on 32 bits ! (and apparently can't do it on MAC)
library(stringr) # for function convert time
library(dplyr) # summarize
require(pairwiseCI) # for odds ratio with one cell zero, function Prop.or

# function
ConvertToTime <- function(x){
  as.POSIXct(str_pad(x, 6, pad = "0"), format="%H%M%S") # this adds the date of today to every time... stupid but hard to get around and not important
}

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



# load data
{
conDB= odbcConnectAccess2007("1_RawData/VideoAnalyses_OtherPreyBitrexTests.accdb")
sqlTables(conDB)

AllAttacks <- sqlQuery(conDB, "
SELECT Behav_Female.TestName, Behav_Video_Metadata.FID, Behav_Female_Attacks.Outcome, Behav_Female_Attacks.AttackTime
FROM (Behav_Female INNER JOIN Behav_Video_Metadata ON Behav_Female.FID = Behav_Video_Metadata.FID) LEFT JOIN Behav_Female_Attacks ON Behav_Video_Metadata.VideoID = Behav_Female_Attacks.VideoID;
                       ")
close(conDB)

AllAttacks$AttackTime <- ConvertToTime(AllAttacks$AttackTime)
AllAttacks$Fate[AllAttacks$Outcome == 1] <- "Consumed"
AllAttacks$Fate[AllAttacks$Outcome == -1] <- "Rejected" 
AllAttacks$DropYN[AllAttacks$Outcome == 1] <- 0
AllAttacks$DropYN[AllAttacks$Outcome == -1] <- 1
AllAttacks$Trt <- "DB"

AllAttacks <- rbind(AllAttacks, data.frame( 
            TestName = rep(unique(AllAttacks$TestName), each = 10),
            FID  = 1:40,
            Outcome = 1,
            AttackTime = NA,
            DropYN = 0,
            Fate = "Consumed",
            Trt = "Z_Control"))
            
}

head(AllAttacks)
nrow(AllAttacks)


FirstAttacks <- rbind(create_FirstAttacks(AllAttacks[AllAttacks$Trt == "DB",]), data.frame( 
  TestName = rep(unique(AllAttacks$TestName), each = 10),
  FID  = 1:40,
  Outcome = 1,
  AttackTime = NA,
  DropYN = 0,
  Fate = "Consumed",
  Trt = "Z_Control"))
  
head(FirstAttacks)
nrow(FirstAttacks)

# descriptive stats

NbAttack_perFemale <-  as.data.frame(AllAttacks %>%
  group_by(FID) %>%
  summarize(NbAttacks = n()))

AllAttacks <- merge(AllAttacks, NbAttack_perFemale, by = 'FID', all.x = TRUE)

summary(AllAttacks$NbAttacks[AllAttacks$TestName =='BitrexCapedTermite' & AllAttacks$Trt == 'DB'])
summary(AllAttacks$NbAttacks[AllAttacks$TestName =='BitrexCricket_3' & AllAttacks$Trt == 'DB'])
summary(AllAttacks$NbAttacks[AllAttacks$TestName =='BitrexCricket_5' & AllAttacks$Trt == 'DB'])
summary(AllAttacks$NbAttacks[AllAttacks$TestName =='BitrexDrosophila' & AllAttacks$Trt == 'DB'])



# stats
  ## we cannot run glmer with FID as random effect to account for several attacks from the same female 
  ## because there is no variation in fate for the control group

table(AllAttacks$Outcome,  AllAttacks$Trt, AllAttacks$TestName) # dropping rate of bitrex prey


# not pseudoreplicated and one tailed test as prereg  - with rejected DB as reference

fisher.test (table(FirstAttacks$Outcome[FirstAttacks$TestName =='BitrexCapedTermite' ], 
                   FirstAttacks$Trt[FirstAttacks$TestName =='BitrexCapedTermite' ]), alternative = "greater" )

fisher.test (table(FirstAttacks$Outcome[FirstAttacks$TestName =='BitrexCricket_3' ], 
                   FirstAttacks$Trt[FirstAttacks$TestName =='BitrexCricket_3' ]) , alternative = "greater" )

fisher.test (table(FirstAttacks$Outcome[FirstAttacks$TestName =='BitrexCricket_5' ], 
                   FirstAttacks$Trt[FirstAttacks$TestName =='BitrexCricket_5' ]), alternative = "greater"  )

fisher.test (table(FirstAttacks$Outcome[FirstAttacks$TestName =='BitrexDrosophila' ], 
                   FirstAttacks$Trt[FirstAttacks$TestName =='BitrexDrosophila' ]) , alternative = "greater" )


## with Woolf correction

Prop.or(table(FirstAttacks$Outcome[FirstAttacks$TestName =='BitrexCapedTermite' ]), 
                   table(FirstAttacks$Trt[FirstAttacks$TestName =='BitrexCapedTermite' ]), alternative = "greater", CImethod="Woolf")

Prop.or(table(FirstAttacks$Outcome[FirstAttacks$TestName =='BitrexCricket_3' ]), 
                  table(FirstAttacks$Trt[FirstAttacks$TestName =='BitrexCricket_3' ]) , alternative = "greater", CImethod="Woolf" )

Prop.or(table(FirstAttacks$Outcome[FirstAttacks$TestName =='BitrexCricket_5' ]), 
                  table(FirstAttacks$Trt[FirstAttacks$TestName =='BitrexCricket_5' ]), alternative = "greater", CImethod="Woolf" )

Prop.or(table(FirstAttacks$Outcome[FirstAttacks$TestName =='BitrexDrosophila' ]), 
                 table(FirstAttacks$Trt[FirstAttacks$TestName =='BitrexDrosophila' ]) , alternative = "greater" , CImethod="Woolf")

