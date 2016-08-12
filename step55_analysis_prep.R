pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_redditscrape.r"))
source(paste0(pathLocal,"lib_step6_analysis.r"))
source(paste0(pathLocal,"plugin_classes.r"))

### Load Data
spings <- readRDS(paste0(pathData, "step5_serversweeks.rds"))
splugins <- readRDS(paste0(pathData, "step5_serversweeksplugins.rds"))
pluginstats <- as.data.table(read.csv(paste0(pathData, 'step45_curse_plugins_metadata_full.csv')))


sfeat <- buildFeatureTablePickDependent(spings, splugins, pluginstats, dependent='ncomm4visits')

### ### this is a function for getting a template to hand code most popular plugins
writeBlankFeatureCodingTable <- function(mc, filename, featureCountMin) {
    #mc <- mc[feat_source != 'keyword']
    ### create wide format: one column per plugin with binary checks
    ### filter plugins used only once or twice before attempting widening
    mc <- mc[feat_count > featureCountMin]
    write.csv(unique(mc[feat_count > featureCountMin,list( feat_count, feat_url, action_admin_up=0, action_other_down=0, grief=0, inoutworld=0, inst=0, normpath=0, forbid=0, boundary=0, position=0, choice=0, info=0, infopath=0, aggregation=0, payoff=0, scope=0, shop=0, tech=0, game=0, loopadmin=0, poly=0, property=0, chat=0, apply=0, resource=0),by=.(feat_code)][order(-feat_count)]), file=filename)
}
if (0) {
    n_servers <- mc[,length(unique(srv_addr))]
    feat_count_min <- max(2, as.integer(n_servers/5000))
    writeBlankFeatureCodingTable(mc, paste0(pathData, "plugin_codes_raw.csv"), feat_count_min)
}


### Save Everyting
saveRDS(sfeat, paste0(pathData, "step55_serversweeksplugins.rds"))
