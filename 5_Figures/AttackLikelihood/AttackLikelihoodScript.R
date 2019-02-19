

rm(list = ls(all = TRUE))

library(here)
library(sjPlot)
library(effects)



source(paste(here(),'4_ScriptAnalysis/data_analysis1.R', sep='/'))

plot_model(mod1, type = "eff", terms = c("FocalTermitePalatability", "PriorExposureYN"))
plot_model(mod1, type = "eff", terms = c("PriorExposureYN",'FocalTermitePalatability'))
plot_model(mod2, type = "eff", terms = c('AttackedTermitePalatability'))


source(paste(here(),'4_ScriptAnalysis/data_analysis1_5.R', sep='/'))

plot_model(mod1, type = "eff", terms = c("FocalTermitePalatability"))
plot_model(mod2, type = "eff", terms = c('AttackedTermitePalatability'))


source(paste(here(),'4_ScriptAnalysis/data_analysis2.R', sep='/'))

plot_model(mod1, type = "eff", terms = c("FocalTermitePalatability"))
plot_model(mod2, type = "eff", terms = c('AttackedTermitePalatability'))


source(paste(here(),'4_ScriptAnalysis/data_analysis3.R', sep='/'))

plot_model(mod1, type = "eff", terms = c("FocalTermitePalatability"))
plot_model(mod2, type = "eff", terms = c('AttackedTermitePalatability'))



source(paste(here(),'4_ScriptAnalysis/data_analysis3_Final.R', sep='/'))

plot_model(mod1, type = "eff", terms = c("FocalTermitePalatability", "PriorExposureYN"))
plot_model(mod1, type = "eff", terms = c("PriorExposureYN",'FocalTermitePalatability'))
plot_model(mod2, type = "eff", terms = c('AttackedTermitePalatability'))



