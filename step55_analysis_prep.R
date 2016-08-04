pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_redditscrape.r"))
source(paste0(pathLocal,"lib_step6_analysis.r"))
source(paste0(pathLocal,"plugin_classes.r"))

### Load Data
spings <- readRDS(paste0(pathData, "step5_serversweeks.rds"))
splugins <- readRDS(paste0(pathData, "step5_serversweeksplugins.rds"))
pluginstats <- as.data.table(read.csv(paste0(pathData, 'step45_curse_plugins_metadata_full.csv')))
plugin_codes_byhand <- as.data.table(read.csv(paste0(pathData, 'step45_curse_plugins_metadata_full.csv')))
plugin_codes_byhand <- as.data.table(read.csv(file=paste0(pathData, "plugin_codes_byhand.csv")))


sfeat <- buildFeatureTablePickDependent(spings, splugins, dependent='ncomm4visits')


### integrating hand codes into mc
if (0) {

    ### prep for category based analysis
    if(0) {
        plugin_codes <- read.csv(file=paste0(pathData, "plugin_codes_byhand.csv"))
        plugin_codes <- plugin_codes[1:(nrow(plugin_codes)-1),]
        #cor(plugin_codes[4:ncol(plugin_codes)])
        mc <- merge(mc, plugin_codes, by=c('feat_code'), all.x=T, all.y=F)
        mc_h <- merge(
                mc[, lapply(.SD, unique), by=.(srv_addr), .SDcols=c("post_uid", "srv_max", "srv_max_log", "dataset_reddit", "dataset_omni", "dataset_mcs_org", "jubilees", "y", "ylog", "srv_repquery", "srv_repplug", "srv_repsample", "weeks_up_todate", "date_ping_int", "plugin_count", "keyword_count", "tag_count", "sign_count", "norm_count" )],
                mc[, lapply(.SD, function(x) sum(x, na.rm=T)), by=.(srv_addr), .SDcols=c("action_admin_up", "action_other_down", "grief", "inoutworld", "inst", "isnorm", "normpath", "forbid", "boundary", "position", "choice", "info", "infopath", "aggregation", "payoff", "scope", "shop", "tech", "game", "loopadmin", "poly", "property", "chat", "apply", "resource")]
                , by="srv_addr", all=T)
    } else if(0) {
        mc_p <- merge(
                mc[, lapply(.SD, unique), by=.(srv_addr), .SDcols=c("post_uid", "srv_max", "srv_max_log", "dataset_reddit", "dataset_omni", "dataset_mcs_org", "jubilees", "y", "ylog", "srv_repquery", "srv_repplug", "srv_repsample", "weeks_up_todate", "date_ping_int", "plugin_count", "keyword_count", "tag_count", "sign_count", 'norm_count' )],
                mc[, lapply(.SD, function(x) sum(x, na.rm=T)), by=.(srv_addr), .SDcols=grep("^cat_*", names(mc))], by="srv_addr", all=T)
        #sposts <- splugins[,lapply(.SD, unique),by=.(post_uid, date_post), .SDcols=c(grep("^srv_*", names(splugins)), which(names(splugins) %in% c("dataset_source")), grep("*_count$", names(splugins)))]
        mc_p <- (mc_p[ !is.na(norm_count) & !is.na(plugin_count)])
        mc_p[,':='(mcat_gov=sum(cat_admintools, cat_antigrief, cat_chat, cat_economy, cat_informational, cat_webadmin, cat_devtools, na.rm=T), mcat_play=sum(cat_fun, cat_general, cat_mechanics, cat_misc, cat_roleplay, cat_teleportation, cat_world, cat_fixes,  cat_worldgen, na.rm=T), mcat_all=sum(cat_admintools, cat_antigrief, cat_chat, cat_economy, cat_informational, cat_webadmin, cat_devtools, cat_fun, cat_general, cat_mechanics, cat_misc, cat_roleplay, cat_teleportation, cat_world, cat_fixes,  cat_worldgen, na.rm=T), mcat_admin=sum(cat_admintools, cat_webadmin, cat_devtools, cat_worldgen, na.rm=T), mcat_player=sum(cat_chat, cat_economy, cat_informational, cat_fun, cat_general, cat_mechanics, cat_misc, cat_roleplay, cat_teleportation, cat_world, na.rm=T), mcat_outside=sum( cat_webadmin, cat_devtools, cat_worldgen, na.rm=T), mcat_govcent=sum(cat_admintools, cat_antigrief, cat_webadmin, na.rm=T), mcat_govdecent=sum( cat_chat, cat_economy, cat_informational, na.rm=T), mcat_friv=sum(cat_fun, cat_general, cat_mechanics, cat_misc, cat_roleplay, cat_teleportation, na.rm=T), mcat_gameplay=sum(cat_fun, cat_general, cat_mechanics, cat_roleplay, cat_teleportation, cat_world,  cat_worldgen, na.rm=T), mcat_misc=sum(cat_devtools, cat_misc, cat_fixes,  na.rm=T) ), by=.(srv_addr)]
    }
}


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
