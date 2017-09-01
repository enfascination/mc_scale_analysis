pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_redditscrape.r"))
source(paste0(pathLocal,"plugin_classes.r"))
source(paste0(pathLocal,"lib_step6_analysis.r"))

### lOAD DATA
spings <- readRDS(paste0(pathData, "step5_serversweeks.rds"))
splugins <- readRDS(paste0(pathData, "step5_serversweeksplugins.rds"))
pluginstats <- as.data.table(read.csv(paste0(pathData, 'step45_curse_plugins_metadata_full.csv')))


### CUT SERVERS WITH EVIDENCE OF HACKED apiS THAT CAN UNDERMINE COMMUNITY AND OTHER SUCCESS MEASUERS
spings_clean <- spings[srv_addr %ni% spings[hackedapi == TRUE,unique(srv_addr)]]
#spings_clean <- spings
if (TRUE ) {  ### minmally comparable
    spings_clean <- spings_clean[srv_max >0]  ### due to a now-fixed oversight, hackedapi failed to exclude the inappropriate case that srv_max == 0.
    spings_clean <- spings_clean[srv_addr %ni% c("131.153.5.218", "alpa.playmcm.net", "playmcm.net", "pvp.originmc.org")]
    #mc <- mc[!hackedapi]
    #mc <- mc[srv_max <= 1000]  ### don't need ot delete it, can just reset it to the highest realistic value
    #mc <- mc[nmaxpop <= 1000]  ### its bad when admins edit this to be unrealistic, but it doesn't affect me: bounding values I actually measure catches everything that this catches.
    #max_srv_max_observed <- mc[order(-nmaxpop), unique(nmaxpop)][1] ### first value, after censoring 1000074, is 593. The max number of players that a server lists can't exceed the max that I've ever actually observed, over all servers ever.
}

### PICK DEPENDENT
sfeat_dep <- buildPickDependent(spings_clean, dependent= 'ncomm4visits_bestweek')
maxPopulationEverObserved <- sfeat_dep[order(-nmaxpop), unique(nmaxpop)][1]
sfeat_dep[srv_max >=  maxPopulationEverObserved, ':='(srv_max=  maxPopulationEverObserved, srv_max_log=log2(  maxPopulationEverObserved+1))]   ### I don't like doing this, but some values of srv_max are fake or meaningless, particularly very high ones.  i oriignlaly picked 5000  subjectively basedon the distirbution of server sizes but a better way to do this will be to set the max to the most trustworthy max in the data. I must do this after buildPickDependent instead of before because more servers that are subjectively fake servers are not being caught by hackedapi, and calculating this max after the merge inciedentally catches more of those.

if (0) {  ### for building blank feature table
    mc <- buildFeatureTable(sfeat_dep, splugins, pluginstats)
    n_servers <- sfeat[,length(unique(srv_addr))]
    feat_count_min <- max(2, as.integer(n_servers/5000))
    sfeat2 <- filterDataSetDown(sfeat, cutNonVanilla=TRUE, cutNonPositiveDependent=TRUE, featureCountMin=feat_count_min, keepFeatTypes=c('plugin', 'property', 'tag'), keepDataSource=c('reddit', 'omni', 'mcs_org'))
    writeBlankFeatureCodingTable(mc, paste0(pathData, "plugin_codes_raw.csv"))
}


### PREP DATA FOR WIDE LONG ANALYSIS OF SURVIVAL
## DATA PREP
mc <- sfeat_dep
mc <- filterDataSetDown(mc, cutNonVanilla=FALSE, cutNonPositiveDependent=FALSE, featureCountMin=0)
mw <- mc[, lapply(.SD, unique), by=.(srv_addr), .SDcols=c("post_uid", "srv_max", "srv_max_bak", "srv_details", "dataset_source", "jubilees", "y", "ylog", "nuvisits12", "nvisitsobs12", "nvisitsunobs", "srv_votes", "srv_repquery", "srv_repplug", "srv_repsample", "weeks_up_total", "weeks_up_todate", "date_ping_1st", "date_ping_lst", "srv_retired", "plugin_count", "keyword_count", "tag_count", "sign_count")]

# ENRICH FOR PLOTTING (VARS AND THEIR VALUES ONLY FOR PLOTTING)
mw[,pop_size_factor:=cut(log2(srv_max+1), breaks=c(0,2,4,6,12,24), labels=c("\u22644", "4 to 16", "16 to 64", "64 to 1024", ">1024"), ordered_result=TRUE, right=TRUE)]
mw[,perf_factor:=cut(log2(y+1), breaks=c(-1,0,1,2,4,6,8,24), labels=c("0","1", "1 to 4", "4 to 16", "16 to 64", "64 to 256", ">256"), ordered_result=TRUE, right=TRUE)]
mw[,yrug:=(log2(ifelse(y>150, 150, y)+1)+1.0)*1.0+rnorm(nrow(.SD),sd=0.02)]
mw[,xrug:=(log2(srv_max+1)+1.0)*0.25+rnorm(nrow(.SD),sd=0.02)]

# SAMPLING
mw_split <- splitDataTestTrain(mw, proportions=c(0.2, 0.8), validation_set=FALSE)
mw_train <- mw_split$train
mw_test <- mw_split$test
mw_full <- mw
### MAIN DATASET
#mw <- mw_full
mw <- mw_train
saveRDS(mw, paste0(pathData, "step6_servers_wide_tallanalysis.rds"))



### PREP DATA FOR WIDE ANALYSIS OF GOVERNANCE
### LOAD DATA
mc <- buildFeatureTable(sfeat_dep, splugins, pluginstats)
expect_true(mc[,length(unique(srv_addr))] == mc[,length(unique(post_uid))])
mc[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]
n_servers <- mc[,length(unique(srv_addr))]; n_servers 
dim(mc)
## DATA PREP
mc <- filterDataSetDown(mc, cutNonVanilla=TRUE, cutNonPositiveDependent=FALSE, featureCountMin=25, keepFeatTypes=c('plugin', 'property'), keepDataSource=c('reddit', 'omni', 'mcs_org'))
n_servers <- mc[,length(unique(srv_addr))]; n_servers 
dim(mc)
#writeBlankFeatureCodingTable(mc, paste0(pathData, "plugin_widehandcodes_raw.csv"))
### am I missing any plugins or features?
plugin_codes_byhand <- get_plugin_codes()
mc[mc$feat_code %ni% plugin_codes_byhand$feat_code, unique(feat_code) ]  ### more things to code and integrate back into get_plugin_codes()
#cor(plugin_codes_byhand[4:ncol(plugin_codes_byhand)])
mw <- merge(
        mc[, lapply(.SD, unique), by=.(srv_addr), .SDcols=c("post_uid", "srv_max", "srv_max_log", "srv_max_bak", "srv_details", "dataset_source", "dataset_reddit", "dataset_omni", "dataset_mcs_org", "jubilees", "y", "ylog", "nuvisits12", "nvisitsobs12", "nvisitsunobs", "srv_votes", "srv_repquery", "srv_repplug", "srv_repsample", "weeks_up_total", "weeks_up_todate", "date_ping_int", "date_ping_1st", "date_ping_lst", "srv_retired", "plugin_count", "log_plugin_count", "keyword_count", "tag_count", "sign_count", "norm_count", "plugin_specialization")],
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
mw[,pop_size_factor:=cut(log2(srv_max+1), breaks=c(0,2,4,6,12,24), labels=c("\u22644", "4 to 16", "16 to 64", "64 to 1024", ">1024"), ordered_result=TRUE, right=TRUE)]
#mw[,perf_factor:=cut(log2(y+1), 7, ordered_result=TRUE)]
mw[,perf_factor:=cut(log2(y+1), breaks=c(-1,0,1,2,4,6,8), labels=c("0","1", "1 to 4", "4 to 16", "16 to 64", "64 to 256"), ordered_result=TRUE, right=TRUE)]
#mw[,perf_factor:=cut(log2(y+1), breaks=c(-1,1,2,4,6,8), labels=c("\u22641", "1 to 4", "4 to 16", "16 to 64", "64 to 256"), ordered_result=TRUE, right=TRUE)]
mw[,perf_factor_ratio:=cut(log2(y+1)/srv_max_log, 6, ordered_result=TRUE)]
### these two are for the marginal density plots
mw[,yrug:=(log2(ifelse(y>100, 100, y)+1)+1.0)*0.7+rnorm(nrow(.SD),sd=0.02)]
mw[,xrug:=(log2(srv_max+1)+1.0)*0.4+rnorm(nrow(.SD),sd=0.02)]
### resource types
indc8 <- function(x)ifelse((x>0),1,0)  ### indicator function
mw[,':='(total_res=sum(res_grief, res_ingame, res_realworld), pct_grief=sum(res_grief)/sum(res_grief, res_ingame, res_realworld), pct_ingame=sum(res_ingame)/sum(res_grief, res_ingame, res_realworld), pct_realworld=sum(res_realworld)/sum(res_grief, res_ingame, res_realworld), total_aud=sum(aud_users,aud_admin), ratio_aud=(aud_admin)/sum(aud_users,aud_admin), count_res_type=indc8(res_grief)+ indc8(res_ingame)+ indc8(res_realworld)),by=.(srv_addr)]
#this introduces NA's because many servers have total_res == res_grief + res_ingame + res_realworld == 0
mw[total_res==0,':='(pct_grief=0, pct_ingame=0, pct_realworld=0)]
mw[total_aud==0,':='(ratio_aud=0)]
### i dedied not to include admintools as an "instiuttion"
mw[,':='(sanity_pct=sum(pct_grief, pct_ingame, pct_realworld), entropy_res=as.numeric(entropy_calc(c(pct_grief, pct_ingame, pct_realworld))), count_inst_type=indc8(cat_chat)+indc8(cat_informational)+indc8(cat_economy)+indc8(cat_admintools+cat_webadmin)-indc8(cat_admintools+cat_webadmin)),by=.(srv_addr)]
mw[,administration:=sum(cat_admintools),by=.(srv_addr)]
mw[,behavior_management:=sum(cat_antigrief),by=.(srv_addr)]
# SAMPLING
### split data up
### notes:
###  if there is lots of data 50/50 training/test is fine, and you shouldn't calculate full lasso paths (dfmax=50 or 100) and it's important to filter columns down before widening the matrix.  
mw_split <- splitDataTestTrain(mw, proportions=c(0.5, 0.25, 0.25), validation_set=TRUE)
mw_train <- mw_split$train
mw_test <- mw_split$test
mw_full <- mw
### MAIN DATASET
#mw <- mw_full
mw <- mw_train

### SAVE eVERYTING
saveRDS(mw, paste0(pathData, "step6_servers_wide_govanalysis.rds"))
saveRDS(mw_test, paste0(pathData, "step6_servers_wide_govanalysis_test.rds"))
saveRDS(mw_full, paste0(pathData, "step6_servers_wide_govanalysis_full.rds"))
