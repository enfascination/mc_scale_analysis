

### initialize globals
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_redditscrape.r"))
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_step6_analysis.r"))
source(paste0(pathLocal,"plugin_classes.r"))

library(boot)
library(ggthemes)
library(scales)
### notes:
###  if there is lots of data 50/50 training/test is fine, and you shouldn't calculate full lasso paths (dfmax=50 or 100) and it's important to filter columns down before widening the matrix.  

### Load Data
spings <- readRDS(paste0(pathData, "step5_serversweeks.rds"))
splugins <- readRDS(paste0(pathData, "step5_serversweeksplugins.rds"))
pluginstats <- as.data.table(read.csv(paste0(pathData, 'step45_curse_plugins_metadata_full.csv')))

sfeat <- buildPickDependent(spings, dependent='ncomm4visits')
sfeat <- buildFeatureTable(sfeat, splugins, pluginstats)

