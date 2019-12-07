#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Data extraction for manipulation of palatability with bitrex with other type of preys
#	 Start : 22 march 2019
#	 last modif : first
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
  ## outcome: 1= Consum or -1=Drop
  
  
  # Additionally, separately, with other spiders, one palatable prey was provided per spider (n = number of spiders):
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
require(gridExtra) # for function gridarrange
library(cowplot) # for function plot_grid
library(ggplot2)

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


# figure 

contingency_tbl <- table(FirstAttacks$Trt,FirstAttacks$Outcome, FirstAttacks$TestName) # dropping rate of milkweed Bug = 100%, SF bugs = 40%

tbl_ggplot_CapedTermite <- data.frame(Palatability = c('DB', 'DB', 'Control', 'Control'),
                         Outcome = c('Rejected', 'Consumed', 'Rejected', 'Consumed'),
                         Count = c(5,5,0,10))

tbl_ggplot_Cricket <- data.frame(Palatability = c('DB 3%', 'DB 3%', 'DB 5%', 'DB 5%','Control', 'Control'),
                                      Outcome = c('Rejected', 'Consumed', 'Rejected', 'Consumed','Rejected', 'Consumed'),
                                      Count = c(2,8,16,4,0,10))

tbl_ggplot_Droso <- data.frame(Palatability = c('DB', 'DB', 'Control', 'Control'),
                                  Outcome = c('Rejected', 'Consumed', 'Rejected', 'Consumed'),
                                  Count = c(5,5,0,10))



CapedTermite_dr <- ggplot(tbl_ggplot_CapedTermite, aes(x=Palatability, y=Count, fill=Outcome)) + 
  geom_bar(stat="identity")+
  ylim(0,21.5)+
  labs(title = "Caped Termites") +
  labs(y = "Number of prey rejected or consumed after attack", x = 'Palatability treatment')+      
  #scale_x_discrete(labels=c("DB", "Control"))  +
  theme_classic() +
  scale_fill_manual("legend", values = c("Consumed" = "grey", "Rejected" = "black"))  +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position=c(0.4,0.85),
        legend.title = element_blank(),
        legend.text = element_text(size=rel(0.7)),
        legend.key.size = unit(0.8, 'lines'),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 10))

Cricket_dr <- ggplot(tbl_ggplot_Cricket, aes(x=Palatability, y=Count, fill=Outcome)) + 
  geom_bar(stat="identity")+
  ylim(0,21.5)+
  labs(title = "Crickets") +
  labs(y = "Number of prey rejected or consumed after attack", x = 'Palatability treatment')+      
  #scale_x_discrete(labels=c("DB", "Control"))  +
  theme_classic() +
  scale_fill_manual("legend", values = c("Consumed" = "grey", "Rejected" = "black"))  +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.title=element_blank(),
        legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(color = "white"),
        plot.title = element_text(hjust = 0.5, size = 10))

Droso_dr <- ggplot(tbl_ggplot_Droso, aes(x=Palatability, y=Count, fill=Outcome)) + 
  geom_bar(stat="identity")+
  labs(title = "Fruit flies") +
  labs(y = "Number of prey rejected or consumed after attack", x = 'Palatability treatment')+      
  ylim(0,21.5)+
  theme_classic() +
  scale_fill_manual("legend", values = c("Consumed" = "grey", "Rejected" = "black"))  +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.title=element_blank(),
        legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(color = "white"),
        plot.title = element_text(hjust = 0.5, size = 10))

{blank2y <-ggplot()+
    scale_x_continuous(limits = c(0, 1))+
    scale_y_continuous(name="PPP", 
                       limits=c(0, 20))+
    
    annotate("text", x = 0.5, y = 10, label = "Palatability treatment",  hjust = 0.5, angle=0)+
    theme_classic()+
    
    theme(
      panel.border = element_rect(colour = "white", fill=NA),
      axis.title.y=element_text(size=10, color = "white"),
      axis.title.x = element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_text(color = "white"),
      axis.ticks.x=element_blank(),
      axis.ticks.y=element_blank(),
      axis.line = element_line("white"),
      plot.margin = unit(c(0,0.2,0,0.2), "cm")
    )
  }  


blank2yGrob <- ggplotGrob(blank2y)

PLOT <- ggplotGrob(plot_grid(CapedTermite_dr, Cricket_dr, Droso_dr, align = "h", ncol = 3, rel_widths = c(3, 3.5, 2.55)))

setEPS() 
pdf("5_Figures/DropRate/OtherPreyDR3.pdf", height=5,  width=6.85)
grid.arrange(grobs = list(PLOT, blank2yGrob), nrow=2, heights=c(19,1))
dev.off()  

