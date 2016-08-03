pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_redditscrape.r"))
source(paste0(pathLocal,"plugin_classes.r"))

spings <- readRDS(paste0(pathData, "step5_serversweeks.rds"))
splugins <- readRDS(paste0(pathData, "step5_serversweeksplugins.rds"))

pluginstats <- as.data.table(read.csv(paste0(pathData, 'step45_curse_plugins_metadata_full.csv')))

#get server/week down to server.  merge with posts, performance metrics, other things (topic data?)
### I'M COMMITTING HERE TO MY DEPENDEENT VARIABLE, so really everything from heredown should go ina  new file, since it isn't analysis agnostic
### keep developing spings a bit further
#setkey(spings, dataset_source, ping_uid, date_ping, srv_addr)
### reduce data down to one dependent type
sserv <- spings
sserv <- sserv[(bestweek4visits==T & bestweek30visits==T) | (is.na(bestweek4visits) & bestweek30visits==T)] ### get unique server rows, with a bias for the 4 visits measure over the 30visists measure
sserv[,':='(bestweek30visits=NULL, bestweek4visits=NULL, ncomm30visits=NULL, ncomm4visits=NULL)] ### erase these columsn to ge tthe ones from spings instead
sserv <- spings[bestweek4visits==T,.(srv_addr, ncomm4visits)][sserv,on=c("srv_addr")]
sserv <- sserv[!is.na(ncomm4visits)]
sserv <- sserv[srv_max >0]
sserv[,srv_max_log:=log10(srv_max)]

#spings <- spings[dataset_source=='reddit']
sserv[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]

### implictly, an important thing happening here is that null postuid means getting rid of pings that didn't match to plugin hauls
sfeat <- splugins[,list(post_uid, feat, feat_code, feat_type, feat_source, feat_trust, plugin_count, keyword_count, tag_count, sign_count)][sserv[!is.na(post_uid)], on=c("post_uid")]  ### CAREFUL!!!  the same IPs came from a few different data sources.  argg.
### sfeat[is.na(srv_repsample),srv_repsample:=i.srv_repsample]  ### be explicit about what gets mapped from splugins, or you'll get bad overrides
expect_true(sfeat[,length(unique(srv_addr))] == sfeat[,length(unique(srv_addr)), by=dataset_source][,sum(V1)])  ### CAREFUL!!!  the same IPs came from a few different data sources.  argg.
expect_true(sfeat[,lapply(list(srv_repstat, srv_repquery, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source][3,V5] == 0)
sfeat[,lapply(list(srv_repstat, srv_repquery, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]
### merge plugin properties into sfeat
#setnames(pluginstats, "build.y", "build")
sfeat <- merge(sfeat, pluginstats[build=='curse', list(feat, feat_source_coded = feat_source, feat_url = url, date_created, date_updated, dls_total, dls_recent, likes, cat_admintools, cat_antigrief, cat_chat, cat_devtools, cat_economy, cat_fixes, cat_fun, cat_general, cat_informational, cat_mechanics, cat_misc, cat_roleplay, cat_teleportation, cat_webadmin, cat_world, cat_worldgen)], all.x=T, all.y=F, by=c('feat'))
sfeat[,feat_count:=.N,by="feat_code"]


#save everyting
saveRDS(sserv, paste0(pathData, "step5_servers.rds"))
saveRDS(sfeat, paste0(pathData, "step5_serversweeksplugins.rds"))

