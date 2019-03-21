#focal analyses bitrex 1%

rm(list = ls(all = TRUE))

library(RODBC)
library(here)
library(pbapply)

conDB= odbcConnectAccess2007(paste(here(),"1_RawData/VideoAnalyses_1BitrexTermites.accdb", sep='/'))
sqlTables(conDB)	# list all the tables in the DB  

AllFemales <- sqlQuery(conDB, "
                       SELECT Basic_Trials.Ind_ID AS FID, Basic_Trials.GroupName, Basic_Trials.SubGroupName, Behav_Female.ExcludeYN, Behav_Female.ReasonExclusion
                       FROM Basic_Trials INNER JOIN Behav_Female ON Basic_Trials.Ind_ID = Behav_Female.FID;
                       ")

AllFemales <- AllFemales[AllFemales$ExcludeYN == 0,] # see remarks


close(conDB)
      
FirstAttacks <- read.csv(file = "3_ExtractedData/FirstAttacks/FirstAttacks.csv", header=TRUE, sep=",")




## create table 2 lines per test 
  FocalTermiteAttack <- rbind(AllFemales[,c('FID', 'GroupName', 'SubGroupName')],AllFemales[,c('FID', 'GroupName', 'SubGroupName')])
  FocalTermiteAttack$FocalTermiteColor <- c(rep('Brown',nrow(AllFemales)),rep('Green',nrow(AllFemales)))
  FocalTermiteAttack <- FocalTermiteAttack[order(FocalTermiteAttack$FID),]
  nrow(FocalTermiteAttack) # 200 looks good
  
  
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
  
  head(FocalTermiteAttack) 
  
# select one at random (1000 times), run the model, save est, CI, p 
  
  sample_focal <- function(df) {
  
  FocalTermiteAttack <- split(df, df$FID)
  
  FocalTermiteAttack_fun <- function(x){
    x$FocalTermiteYN <- sample(c(0,1), 2,replace=FALSE) # randomly assigning YN, determining whether the termite is focal or not
    return(x[x$FocalTermiteYN == 1,])
  }
  
  FocalTermiteAttack <- do.call(rbind,lapply(FocalTermiteAttack,FocalTermiteAttack_fun))
  rownames(FocalTermiteAttack) <- NULL
  
  FocalTermiteAttack$FocalTermitePalatability[FocalTermiteAttack$FocalTermitePalatability == "1"] <- 'Control'
  FocalTermiteAttack$FocalTermitePalatability[FocalTermiteAttack$FocalTermitePalatability == "0"] <- 'DB'
  FocalTermiteAttack$PriorExposure[FocalTermiteAttack$PriorExposureYN == 1] <- 'Trained'
  FocalTermiteAttack$PriorExposure[FocalTermiteAttack$PriorExposureYN == 0] <- 'Naive'
  FocalTermiteAttack$PalatExp <- paste(FocalTermiteAttack$FocalTermitePalatability, FocalTermiteAttack$PriorExposure, sep="")
  
  modInter <- glm (FocalTermiteAttackedYN ~  PalatExp + FocalTermiteColor, family = 'binomial', data = FocalTermiteAttack)
  summary(modInter)
  drop1(modInter, test="Chisq")
  
  effects_table <- as.data.frame(cbind(est=invlogit(summary(modInter)$coeff[,1]),
                                       CIhigh=invlogit(summary(modInter)$coeff[,1]+summary(modInter)$coeff[,2]*1.96),
                                       CIlow=invlogit(summary(modInter)$coeff[,1]-summary(modInter)$coeff[,2]*1.96)))
  effects_table <- effects_table[-nrow(effects_table),]
  
  return(list(effects_table))
  
  }
  
  effects_table1_list <- pbreplicate(1000, replicate(10, sample_focal(FocalTermiteAttack)))
  
  effects_table1 <- Reduce(`+`, effects_table1_list) / length(effects_table1_list)
  effects_table1$PriorExposure <- c("Naive", "Trained", "Naive","Trained")
  effects_table1$Palatability <- c("Control", "Control", "DB","DB")
  

   plot3 <- ggplot(data=effects_table1, aes(x=Palatability, y=est,colour=PriorExposure, shape = PriorExposure)) + 
    scale_y_continuous(name="Prey probability of being attacked first", 
                       limits=c(0, 1), breaks =c(0,0.25,0.50,0.75,1), labels=scales::percent)+ # 0.75 converted to 75%
    theme_classic() + # white backgroun, x and y axis (no box)
    labs(title = "DB solution concentration: 1%") +
    
    geom_errorbar(aes(ymin=CIlow, ymax=CIhigh, col=PriorExposure), width =0.4,na.rm=TRUE, position = position_dodge(width=0.5))+ # don't plot bor bars on x axis tick, but separate them (dodge)
    geom_hline(yintercept=0.5, linetype="dashed", color = "grey48") +
    geom_point(size =4, aes(shape=PriorExposure, col=PriorExposure), stroke = 1, position = position_dodge(width=0.5)) +
    scale_colour_manual(name= "Prior exposure to DB", values=c("Black","Grey")) +
    scale_shape_manual(name= "Prior exposure to DB", values=c(16,17))+ # duplicate title to combine legend
    theme(panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
          legend.position=c(0.5,0.85),
          legend.title = element_text(size=rel(0.8)),
          legend.text = element_text(size=rel(0.7)),
          legend.key.size = unit(0.8, 'lines'),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 10)) +
    guides(shape = guide_legend(override.aes = list(linetype = 0, size = 2))) # remove bar o top of symbol in legend
  
  
  
  

