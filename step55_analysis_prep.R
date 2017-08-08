pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_redditscrape.r"))
source(paste0(pathLocal,"lib_step6_analysis.r"))
source(paste0(pathLocal,"plugin_classes.r"))

### Load Data
spings <- readRDS(paste0(pathData, "step5_serversweeks.rds"))
splugins <- readRDS(paste0(pathData, "step5_serversweeksplugins.rds"))
pluginstats <- as.data.table(read.csv(paste0(pathData, 'step45_curse_plugins_metadata_full.csv')))


sfeat <- buildPickDependent(spings, dependent= 'ncomm4visits_bestweek')
sfeat <- buildFeatureTable(sfeat, splugins, pluginstats)

### ### this is a function for getting a template to hand code most popular plugins
writeBlankFeatureCodingTable <- function(sfeat, filename) {
    ### filter plugins used only once or twice 
    #write.csv(unique(sfeat[feat_count > 2,list( feat_count, feat_url, action_admin_up=0, action_other_down=0, grief=0, inoutworld=0, inst=0, normpath=0, forbid=0, boundary=0, position=0, choice=0, info=0, infopath=0, aggregation=0, payoff=0, scope=0, shop=0, tech=0, game=0, loopadmin=0, poly=0, property=0, chat=0, apply=0, resource=0),by=.(feat_code)][order(-feat_count)]), file=filename)
    write.csv(unique(sfeat[feat_count > 2,list( feat_count, feat_url, na='', foreign='', gov_auto=(ifelse(!is.na(cat_admintools) & (cat_admintools == 1 | cat_antigrief == 1 | cat_chat == 1 | cat_economy == 1 | cat_informational ==1  ), 1,0)), gov_hand='', resource=ifelse(!is.na(cat_antigrief) & (cat_antigrief == 1) ,'grief',''), audience='', upkeep='', enable_forbid='', institution=ifelse(!is.na(cat_economy) & cat_economy == 1 & cat_chat != 1, 'shop',''), actionsituation='', notes='' ), by=.(feat_code)][order(-feat_count)]), file=filename, row.names=FALSE)
}
if (0) {
    n_servers <- sfeat[,length(unique(srv_addr))]
    feat_count_min <- max(2, as.integer(n_servers/5000))
    sfeat2 <- filterDataSetDown(sfeat, cutUnrealistic=TRUE, cutNonVanilla=TRUE, cutNonPositiveDependent=TRUE, featureCountMin=feat_count_min, keepFeatTypes=c('plugin', 'property', 'tag'), keepDataSource=c('reddit', 'omni', 'mcs_org'))
    writeBlankFeatureCodingTable(sfeat2, paste0(pathData, "plugin_codes_raw.csv"))
}


### SAVE eVERYTING
saveRDS(sfeat, paste0(pathData, "step55_serversweeksplugins.rds"))

### PREP DATA FOR WIDE LONG ANALYSIS OF SURVIVAL
## DATA PREP
maxPopulationEverObserved = spings[order(-nmaxpop), unique(nmaxpop)][1]
mc <- buildPickDependent(spings, dependent= 'ncomm4visits_bestweek')
mc <- filterDataSetDown(mc, cutUnrealistic=TRUE, cutNonVanilla=FALSE, cutNonPositiveDependent=FALSE, featureCountMin=0)
mw <- mc[, lapply(.SD, unique), by=.(srv_addr), .SDcols=c("post_uid", "srv_max", "srv_max_log", "srv_max_bak", "jubilees", "y", "ylog", "nuvisits12", "nvisitsobs12", "nvisitsunobs", "srv_votes", "srv_repquery", "srv_repplug", "srv_repsample", "weeks_up_total", "weeks_up_todate", "date_ping_1st", "date_ping_lst", "srv_retired", "plugin_count", "keyword_count", "tag_count", "sign_count")]

# ENRICH FOR PLOTTING (VARS AND THEIR VALUES ONLY FOR PLOTTING)
mw[,pop_size_factor:=cut(log2(srv_max+1), breaks=c(0,2,4,6,12,24), labels=c("\u22644", "4 to 16", "16 to 64", "64 to 1024", ">1024"), ordered_result=TRUE, right=TRUE)]
mw[,perf_factor:=cut(log2(y+1), breaks=c(-1,0,1,2,4,6,8,24), labels=c("0","1", "1 to 4", "4 to 16", "16 to 64", "64 to 256", ">256"), ordered_result=TRUE, right=TRUE)]
mw[,yrug:=(log2(ifelse(y>150, 150, y)+1)+1.0)*1.0+rnorm(nrow(.SD),sd=0.02)]
mw[,xrug:=(log2(srv_max+1)+1.0)*0.25+rnorm(nrow(.SD),sd=0.02)]

# SAMPLING
mc_split <- splitDataTestTrain(mw, proportions=c(0.2, 0.8), validation_set=FALSE)
mw_train <- mc_split$train
mw_test <- mc_split$test
mw_full <- mw
### MAIN DATASET
#mw <- mw_full
mw <- mw_train
saveRDS(mw, paste0(pathData, "step6_servers_wide_tallanalysis.rds"))



### PREP DATA FOR WIDE ANALYSIS OF GOVERNANCE
### LOAD DATA
mc <- sfeat
expect_true(mc[,length(unique(srv_addr))] == mc[,length(unique(post_uid))])
mc[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]
n_servers <- mc[,length(unique(srv_addr))]; n_servers 
dim(mc)
## DATA PREP
mc <- filterDataSetDown(mc, cutUnrealistic=TRUE, cutNonVanilla=TRUE, cutNonPositiveDependent=FALSE, featureCountMin=25, keepFeatTypes=c('plugin', 'property'), keepDataSource=c('reddit', 'omni', 'mcs_org'))
n_servers <- mc[,length(unique(srv_addr))]; n_servers 
dim(mc)
#writeBlankFeatureCodingTable(mc, paste0(pathData, "plugin_widehandcodes_raw.csv"))
### am I missing any plugins or features?
plugin_codes_byhand <- get_plugin_codes()
mc[mc$feat_code %ni% plugin_codes_byhand$feat_code, unique(feat_code) ]  ### more things to code and integrate back into get_plugin_codes()
#cor(plugin_codes_byhand[4:ncol(plugin_codes_byhand)])
mw <- merge(
        mc[, lapply(.SD, unique), by=.(srv_addr), .SDcols=c("post_uid", "srv_max", "srv_max_log", "srv_max_bak", "dataset_reddit", "dataset_omni", "dataset_mcs_org", "jubilees", "y", "ylog", "nuvisits12", "nvisitsobs12", "nvisitsunobs", "srv_votes", "srv_repquery", "srv_repplug", "srv_repsample", "weeks_up_total", "weeks_up_todate", "date_ping_int", "date_ping_1st", "date_ping_lst", "srv_retired", "plugin_count", "log_plugin_count", "keyword_count", "tag_count", "sign_count", "norm_count", "plugin_specialization")],
        #mc[, lapply(.SD, function(x) sum(x, na.rm=T)), by=.(srv_addr), .SDcols=c("action_admin_up", "action_other_down", "grief", "inoutworld", "inst", "isnorm", "normpath", "forbid", "boundary", "position", "choice", "info", "infopath", "aggregation", "payoff", "scope", "shop", "tech", "game", "loopadmin", "poly", "hierarchy", "property", "chat", "apply", "resource")]
        mc[, lapply(.SD, function(x) sum(x, na.rm=T)), by=.(srv_addr), .SDcols=c("gov", "res_grief", "res_ingame", "res_none", "res_performance", "res_players", "res_realmoney", "aud_none", "aud_users", "aud_admin", "actions_user", "actions_audience", "use_na", "use_coarseauto", "use_coarsemanual", "use_fineauto", "use_finemanual", "inst_none", "inst_broadcast", "inst_chat",  "inst_privateproperty", "inst_shop", "inst_action_space", "inst_action_space_up", "inst_action_space_down", "inst_boundary", "inst_monitor_by_peer", "inst_monitor_by_admin", "inst_position_h", "inst_position_v", "inst_payoff")]
        , by="srv_addr", all=T)
mw <- merge(
        mw,
        mc[, lapply(.SD, function(x) sum(x, na.rm=T)), by=.(srv_addr), .SDcols=grep("^cat_*", names(mc))]  ### this na.rm is important here because property don't have categories. there are also a small number of plugins with na categories becaue i couldn't match them. 
        , by="srv_addr", all=T)
# ENRICH WIDE FORMAT
mw[,':='(res_realworld=res_realmoney+res_performance, res_realmoney=NULL,res_performance=NULL)]
mw[,sum_institution:=rowSums(.SD[, grep("^inst_[^n]", names(mw), value=TRUE), with=F ])]
mw[,sum_resource:=rowSums(.SD[, grep("^res_[^n]", names(mw), value=TRUE), with=F ])]
#mw[,sum_res_grief:=colSums(.SD[, grep("res_", names(mw), value=TRUE), with=F ])]
mw[,paste("sum", grep("^res_", names(mw), value=TRUE), sep='_'):=lapply(.SD[, grep("^res_", names(mw), value=TRUE), with=F], sum)]
mw[,paste("sum", grep("^inst_", names(mw), value=TRUE), sep='_'):=lapply(.SD[, grep("^inst_", names(mw), value=TRUE), with=F], sum)]
### add a column measuring the diversity of solutions used by a server
library(entropy) ### use small-n bias correction, Chao-Shen, which is fast and comparable to NSB  ##  NOOOOOOO: CS is awful: using shirnkage
mw[,srv_entropy:={inst_dist<-.SD[,grep("^inst_[^n]", names(mw)),with=FALSE][1]; inst_dist<-(inst_dist+0.000001)/(sum(inst_dist)+0.000001); sum(sapply(inst_dist, entropy_calc)) }, by=srv_addr]
# JUST FOR PLOTTING (VARS AND THEIR VALUES ONLY FOR PLOTTING)
### add variables unique to the wide format
#mw <- cbind(mw, interact_xsrv)
mw[,pop_size_factor:=cut(srv_max_log, breaks=c(0,0.7,1,1.7,2,2.7,3), ordered_result=TRUE, right=FALSE)]
mw[,pop_size_factor_coarse:=cut(srv_max_log, breaks=c(0,1,2,3), labels=c("<10", "10s", "\u2265100"), ordered_result=TRUE, right=FALSE)]
mw[,pop_size_factor_fine:=cut(srv_max_log, breaks=25, ordered_result=TRUE, right=FALSE)]
mw[,pop_size_factor:=cut(log2(srv_max+1), breaks=c(0,2,4,6,8,12), labels=c("<4", "4 to 16", "16 to 64", "64 to 256", "\u2265256"), ordered_result=TRUE, right=FALSE)]
mw[,pop_size_factor:=cut(log2(srv_max+1), breaks=c(0,2,4,6,12), labels=c("\u22644", "4 to 16", "16 to 64", "64 to 1024"), ordered_result=TRUE, right=TRUE)]
#mw[,perf_factor:=cut(log2(y+1), 7, ordered_result=TRUE)]
mw[,perf_factor:=cut(log2(y+1), breaks=c(-1,0,1,2,4,6,8), labels=c("0","1", "1 to 4", "4 to 16", "16 to 64", "64 to 256"), ordered_result=TRUE, right=TRUE)]
#mw[,perf_factor:=cut(log2(y+1), breaks=c(-1,1,2,4,6,8), labels=c("\u22641", "1 to 4", "4 to 16", "16 to 64", "64 to 256"), ordered_result=TRUE, right=TRUE)]
mw[,perf_factor_ratio:=cut(log2(y+1)/srv_max_log, 6, ordered_result=TRUE)]
### these two are for the marginal density plots
mw[,yrug:=(log2(ifelse(y>100, 100, y)+1)+1.0)*0.7+rnorm(nrow(.SD),sd=0.02)]
mw[,xrug:=(log2(srv_max+1)+1.0)*0.4+rnorm(nrow(.SD),sd=0.02)]
### resource types
mw[,':='(total_res=sum(res_grief, res_ingame, res_realworld), pct_grief=sum(res_grief)/sum(res_grief, res_ingame, res_realworld), pct_ingame=sum(res_ingame)/sum(res_grief, res_ingame, res_realworld), pct_realworld=sum(res_realworld)/sum(res_grief, res_ingame, res_realworld)),by=.(srv_addr)]
#this introduces NA's because many servers have total_res == res_grief + res_ingame + res_realworld == 0
mw[total_res==0,':='(pct_grief=0, pct_ingame=0, pct_realworld=0)]
mw[,':='(sanity_pct=sum(pct_grief, pct_ingame, pct_realworld), entropy_res=as.numeric(entropy_calc(c(pct_grief, pct_ingame, pct_realworld)))),by=.(srv_addr)]
# SAMPLING
### split data up
### notes:
###  if there is lots of data 50/50 training/test is fine, and you shouldn't calculate full lasso paths (dfmax=50 or 100) and it's important to filter columns down before widening the matrix.  
mc_split <- splitDataTestTrain(mw, proportions=c(0.5, 0.25, 0.25), validation_set=TRUE)
mw_train <- mc_split$train
mw_test <- mc_split$test
mw_full <- mw
### MAIN DATASET
#mw <- mw_full
mw <- mw_train
saveRDS(mw, paste0(pathData, "step6_servers_wide_govanalysis.rds"))
