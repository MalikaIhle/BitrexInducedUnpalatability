#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#  data analysis drop rate after attacking bitter prey (contamination in mouth ?)
#	 Start : 21 march 2019
#	 commit: create function to apply to all datasets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## rk:
# palatability = 0 > DB termite
# palatability = 1 > water termite
# groupname water > no prior exposure
# group name DB > prior exposure

# question: are palatable prey (palatability = 1) more likely to get dropped 
# if attack occur after an attack on a bitrex prey (palatability = 0)?
# https://stats.stackexchange.com/questions/316195/fishers-exact-test-meaning-of-greater-and-less
# ?fisher.test

rm(list = ls(all = TRUE))

# packages
library(lme4)
library(arm)

AllAttacks1 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks1.csv", header=TRUE, sep=",")
AllAttacks15 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks15.csv", header=TRUE, sep=",")
AllAttacks2 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks2.csv", header=TRUE, sep=",")
AllAttacks3 <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks3.csv", header=TRUE, sep=",")
AllAttacks3F <- read.csv(file="3_ExtractedData/AllAttacks/AllAttacks3F.csv", header=TRUE, sep=",")

create_contingency_tbl <- function(AllAttacks) {
# second attack on a palatble prey, when first attack on unpalatable prey
attackafterbitrex <- table(AllAttacks$DropYN[AllAttacks$AttackNb == 2 & AllAttacks$AttackedPalatability == 1 & AllAttacks$PrevPalatabality == "0"])
# first attack on a palatable prey
firstattack <- table(AllAttacks$DropYN[AllAttacks$AttackNb == 1 & AllAttacks$AttackedPalatability == "1"])

contingency_tbl <- cbind(attackafterbitrex,firstattack)

return(contingency_tbl)
}



# DB concentration = 1%
fisher.test(create_contingency_tbl(AllAttacks1), alternative='less') # p =0.9

# DB concentration = 1.5%
fisher.test(create_contingency_tbl(AllAttacks15), alternative='less') # p =0.45

# DB concentration = 2%
fisher.test(create_contingency_tbl(AllAttacks2), alternative='less') # p =0.9

# DB concentration = 3%
fisher.test(create_contingency_tbl(AllAttacks3), alternative='less') # p =0.7

# DB concentration = 3F%
fisher.test(create_contingency_tbl(AllAttacks3F), alternative='less') # p =0.33


