#source('/Users/sfrey/projecto/research_projects/minecraft/server_surveyor/step5_plot_population_scaling.r')
library(ggplot2)
library(gridExtra)
library(ggthemes)
`%ni%` = Negate(`%in%`) ### help functions
options(gsubfn.engine = "R")
options(sqldf.driver= "SQLite")
library(sqldf)
library(data.table)
library(dplyr)
library(pls) ### pcr
library(stringr)
library(MASS)
library(testthat)
library(microbenchmark)

library(reshape2)
library(car)
library(caret)
library(caretEnsemble)
library(mlbench)
library(randomForest)
library(glmnet)
library(doMC)
registerDoMC(cores = 8)
#library(pROC)
#library(sfsmisc)
library(rms)
library(broom)

### functions before data prep
buildPickDependent <- function(spings, dependent='ncomm4visits') {
    ### Pick Variable
    ### to do it, I have to create a one-row-per-server table 
    ###  it is occasionally useful so I'll save it maybe
    #get server/week down to server.  merge with posts, performance metrics, other things (topic data?)
    ### I'M COMMITTING HERE TO MY DEPENDEENT VARIABLE, so really everything from heredown should go ina  new file, since it isn't analysis agnostic
    ### keep developing spings a bit further
    #setkey(spings, dataset_source, ping_uid, date_ping, srv_addr)
    ### reduce data down to one dependent type
    sserv <- spings
    if (dependent == 'ncomm4visits') {
        sserv <- sserv[(bestweek4visits==T & bestweek30visits==T) | (is.na(bestweek4visits) & bestweek30visits==T)] ### get unique server rows, with a bias for the 4 visits measure over the 30visists measure
        sserv[,':='(bestweek30visits=NULL, bestweek4visits=NULL, ncomm30visits=NULL, ncomm4visits=NULL)] ### erase these columsn to ge tthe ones from spings instead
        sserv <- spings[bestweek4visits==T,.(srv_addr, ncomm4visits)][sserv,on=c("srv_addr")]
        sserv <- sserv[!is.na(ncomm4visits)]
        #sserv[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]
        sserv[,':='(y=ncomm4visits, ylog=log10(ncomm4visits+1)) ]
    } else {
        stop("ERROR GKJSDGKHJHHOI")
    }
    #saveRDS(sserv, paste0(pathData, "step55_servers_w_", dependent,".rds"))
    return(sserv)
}

buildFeatureTable <- function(sserv, splugins, pluginstats) {
    ### Enrich sfeat table with some more features
    ### implictly, an important thing happening here is that null postuid means getting rid of pings that didn't match to plugin hauls
    sfeat <- splugins[,list(post_uid, feat, feat_code, feat_type, feat_source, feat_trust, plugin_count, keyword_count, tag_count, sign_count)][sserv[!is.na(post_uid)], on=c("post_uid")]  ### CAREFUL!!!  the same IPs came from a few different data sources.  argg.
    ### sfeat[is.na(srv_repsample),srv_repsample:=i.srv_repsample]  ### be explicit about what gets mapped from splugins, or you'll get bad overrides
    #sfeat[,lapply(list(srv_repstat, srv_repquery, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]
    ### merge plugin properties into sfeat
    #setnames(pluginstats, "build.y", "build")
    ### the curse line is because i may expand into forge mods, but I think I won't, so that'll stay saying 'curse'
    sfeat <- merge(sfeat, pluginstats[build=='curse', list(feat, feat_source_coded = feat_source, feat_url = url, date_created, date_updated, dls_total, dls_recent, likes, cat_admintools, cat_antigrief, cat_chat, cat_devtools, cat_economy, cat_fixes, cat_fun, cat_general, cat_informational, cat_mechanics, cat_misc, cat_roleplay, cat_teleportation, cat_webadmin, cat_world, cat_worldgen)], all.x=T, all.y=F, by=c('feat'))
    ### QC
    expect_true(sfeat[,length(unique(srv_addr))] == sfeat[,length(unique(srv_addr)), by=dataset_source][,sum(V1)])  ### CAREFUL!!!  the same IPs came from a few different data sources.  argg.
    #expect_true(sfeat[,lapply(list(srv_repstat, srv_repquery, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source][2,V5] == 0)
    expect_true(sfeat[,length(unique(srv_addr))] == sfeat[,length(unique(post_uid))])

    ### Pre-widening Enrichment and Cleaning
    sfeat[,feat_count:=.N,by="feat_code"]
    sfeat[,plugin_specialization:= .SD[feat_source=="plugin", as.numeric(median(feat_count))], by="srv_addr"]
    sfeat[srv_max>0,srv_max_log:=log10(srv_max)]
    sfeat[,log_plugin_count:=log10(plugin_count+1)]
    #sserv <- sserv[dataset_source=='reddit']
    #sfeat[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]
    #sfeat[,dataset_source:=as.numeric(factor(dataset_source))]
    sfeat[,dataset_reddit:=ifelse(dataset_source=="reddit",1,0)]
    sfeat[,dataset_omni:=ifelse(dataset_source=="omni",1,0)]
    sfeat[,dataset_mcs_org:=ifelse(dataset_source=="mcs_org",1,0)]
    sfeat[,':='(jubilees=as.integer(jubilees), srv_repquery=as.integer(srv_repquery), srv_repplug=as.integer(srv_repplug), srv_repsample=as.integer(srv_repsample), srv_repsniff=as.integer(srv_repsniff), date_ping_int=as.integer(date_ping), weeks_up_todate=as.numeric(weeks_up_todate))]
    sfeat[!is.na(keyword_count) | !is.na(tag_count) ,norm_count:=sum(keyword_count,tag_count, na.rm=T), by=.(srv_addr)]
    
    ### merge in plugin info to help filtering specifically, banning of non-vanilla servers
    ### maybe refresh this occasionally 
    plugin_codes_byhand <- get_plugin_codes()
    sfeat <- merge(sfeat, plugin_codes_byhand[,c(1,3:ncol(plugin_codes_byhand)),with=FALSE], by=c('feat_code'), all.x=T, all.y=F)

    return(sfeat)
}

filterDataSetDown <- function(mc, cutUnrealistic=TRUE, cutNonVanilla=FALSE, cutNonPositiveDependent=TRUE, featureCountMin=0, keepFeatTypes=c('plugin', 'tag', 'keyword', 'sign', 'property'), keepDataSource=c('reddit', 'omni', 'mcs_org')) {

    print(c("Filter 0:", mc[,length(unique(srv_addr))], nrow(mc) ))

    ### this should go first, otherwise DataProvision will delete mention of 
    ###  some plugins with blacklist=TRUE and this function will keep servers
    ###  that should have been excluded
    mc <- filterDataSetDownComparability(mc, cutUnrealistic=cutUnrealistic, cutNonVanilla=cutNonVanilla)

    print(c("Filter 1:", mc[,length(unique(srv_addr))], nrow(mc) ))

    mc <- filterDataSetDownDataProvision(mc, featureCountMin=featureCountMin, keepFeatTypes=keepFeatTypes, keepDataSource=keepDataSource)

    print(c("Filter 2:", mc[,length(unique(srv_addr))], nrow(mc) ))

    mc <- filterDataSetDownViability(mc, cutNonPositiveDependent=cutNonPositiveDependent)

    print(c("Filter 3:", mc[,length(unique(srv_addr))], nrow(mc) ))

    return(mc)
}

filterDataSetDownDataProvision <- function(mc, featureCountMin, keepFeatTypes, keepDataSource) {
    mc <- mc[dataset_source %in% keepDataSource]  ### minimally comparable
    #mc <- mc[dataset_source != 'omni']
    ### analysis specific searchy filtering 
    mc <- mc[feat_source %in% keepFeatTypes]
    ### for every category type I keep, I have to get rid of servers that don't report that type
    ###   otherwise I get wierd selection bias issues in the analyses:w
    if ('plugin' %in% keepFeatTypes) { mc <- mc[!is.na(plugin_count),] }
    if ('tag' %in% keepFeatTypes) { mc <- mc[!is.na(tag_count),] }
    if ('sign' %in% keepFeatTypes) { mc <- mc[!is.na(sign_count),] }
    if ('keyword' %in% keepFeatTypes) { mc <- mc[!is.na(keyword_count),] }
    #mc <- mc[feat_source != 'keyword']
    #mc <- mc[feat_source=='tag']
    #mc <- mc[feat_source=='plugin']
    #mc[!is.na(keyword_count) | !is.na(sign_count) ,length(unique(srv_addr))]
    print(c("Filter 1.3:", mc[,length(unique(srv_addr))], nrow(mc) ))
    ### measurability
    ### by default, no filtering happens on features, but it's a good idea to do so for or before some analysis, like the widening
    mc <- mc[feat_count > featureCountMin]  # minimally measurable
    ### filter plugins used only a few time before attempting widening
    #n_servers <- mc[,length(unique(srv_addr))]
    #feat_count_min <- max(2, as.integer(n_servers/5000))
    #mc <- mc[feat_count > 2]
    return(mc)
}

filterDataSetDownComparability <- function(mc, cutUnrealistic=TRUE, cutNonVanilla=FALSE, keepFeatTypes=c('plugin', 'tag', 'keyword', 'sign', 'property'), keepDataSource=c('reddit', 'omni', 'mcs_org')) {
    ### Pre-widening Filtering
    print(c("Filter 1.0:", mc[,length(unique(srv_addr))], nrow(mc) ))
    if (cutUnrealistic) {  ### minmally comparable
        mc <- mc[srv_max >0]
        #mc <- mc[srv_max <= 1000]  ### don't need ot delete it, can just reset it to the highest realistic value
        #mc <- mc[nmaxpop <= 1000]  ### its bad when admins edit this to be unrealistic, but it doesn't affect me: bounding values I actually measure catches everything that this catches.
        max_srv_max_observed <- mc[order(-nmaxpop), unique(nmaxpop)][1] ### first value, after censoring 1000074, is 593. The max number of players that a server lists can't exceed the max that I've ever actually observed, over all servers ever.
        mc[srv_max >= max_srv_max_observed, ':='(srv_max=max_srv_max_observed, srv_max_log=log10(max_srv_max_observed))]   ### I don't like doing this, but some values of srv_max are fake or meaningless, particularly very high ones.  i oriignlaly picked 5000  subjectively basedon the distirbution of server sizes but a better way to do this will be to set the max to the most trustworthy max in the data.
    }

    print(c("Filter 1.1:", mc[,length(unique(srv_addr))], nrow(mc) ))
    ### need to filter out servers that deiate too far from vanilla gameplay
    ###  and when I do this, I need to do this before performing and filtering on plugin counts, else I'll end up with plugins used only once by the unfilitered sites
    if (cutNonVanilla) { ### minmally comparable
        #word_forbid_short <- matrix(c("hub", "minigames", "mini games", "multiverse", "multiverse-core", "multiverse-inventories", "multiverse-netherportals", "multiverse-portals", "robbit", "mcmmo", "prison", "raiding", "kitpvp", "parkour", "skyblock", "survival games", "survivalgames", "skywars", "ftb", "pixelmon", "tekkit", "multiworld", "quests", "mobarena", "bedwars", "massivecore"), ncol=1, byrow=T)
        #whitelist <- c("smp", "pve", "vanilla", "semi-vanilla", "survival", "anarchy")
        #mc_whitelist <- mc[feat %in% whitelist, unique(srv_addr)]
        #mc_forbid  <- mc[feat %in% word_forbid_short[,1], unique(srv_addr)]
        #mc_coded  <- mc[feat %in% coded_key_keywords_2[,feat], unique(srv_addr)]
        #mc <- mc[(srv_addr %ni% mc_forbid) ,]
        mc_forbid  <- mc[!is.na(blacklist) & blacklist == 1 , unique(srv_addr)]
        mc <- mc[(srv_addr %ni% mc_forbid) ,]
    }

    print(c("Filter 1.2:", mc[,length(unique(srv_addr))], nrow(mc) ))

    return(mc)
}

filterDataSetDownViability <- function(mc, cutNonPositiveDependent=TRUE ) {
    if (cutNonPositiveDependent) { ### minimally viable
        mc <- mc[y>0]
        mc <- mc[nmaxpop > 0]
        mc <- mc[nuvisits12 > 1]
        #mc <- mc[srv_retired==TRUE]  ### I have to worry about right censoring if I omit this, but I keep over 20% of data
    } else {
        mc <- mc[y>=0]
        mc <- mc[nmaxpop > 0]
        mc <- mc[nuvisits12 >= 0]
    }
    return(mc)
}

### Widening
makeWideModelTable <- function(mc, server_level_vars) {
    mc[,xxx:=999] ### this is bs I don't want to have to deal with.  keeps an error from getting thrwon and wierd spuriousnesses from being hard to catch.
    #id_vars <- c("post_uid", "srv_addr")  ### these names should be the same as in the forumla below, and be updated all the time with every change, or exle everything will be badness and bad
    mc_formula = as.formula( paste(paste0(server_level_vars, collapse=' + '), "feat_code", sep=' ~ ') )
    mc_w <- as.data.table(dcast(formula=mc_formula, data=mc, value.var="xxx", fun.aggregate = function(x) (length(x) > 0) + 0.0))
    #mc_w <- mc_w[,(c(id_vars, pred_vars, names(which(colSums(mc_w[,-(which(names(mc_w) %in% c(id_vars, pred_vars))), with=F]) > 2)))), with=F] ### get rid of features that occur only once  ### there are wierd interactions here with the nzv filtering below.  lower threshold (currently 5) raises the freqCut ratio and counterintuitively increases the number of columns that get censored out
    mc[,xxx:=NULL] 
    #
    ### Post-widening Filtering
    ### selection bias dealt with here
    #mc_w <- (mc_w[ (!is.na(keyword_count) | !is.na(tag_count)) & !is.na(sign_count)])
    #mc_w <- (mc_w[!is.na(sign_count)])
    #mc_w[is.na(keyword_count),keyword_count:=0]
    #mc_w[is.na(tag_count),tag_count:=0]
    #mc_w[is.na(plugin_count),plugin_count:=0]
    #mc_w[is.na(sign_count),sign_count:=0]
    #mc_w[is.na(property_count),property_count:=0]
    return(mc_w)
}


### produce training and test sets
splitDataTestTrain <- function(data, proportions=c(0.6, 0.4), validation_set=FALSE, seed=42) {
    if (sum(proportions) != 1) stop("proportions don't sum to one:")
    if (!validation_set && (length(proportions) != 2)) stop("wrong nubmer of arguments 1")
    if (validation_set && (length(proportions) != 3)) stop("wrong nubmer of arguments 2")
    tt <- list()
    if (is.numeric(seed)) {
        set.seed(seed)
        tt$seed <- seed
    }
    #inTrain <- createDataPartition(y = mc_w$y, p = .6, list = FALSE)  ### can't handle NAs
    inTrain <- sample(1:nrow(data), proportions[1]*nrow(data), replace=F)
    tt$train <- data[ inTrain,]
    if (validation_set) {
        inValidation <- sample(1:nrow(data[-inTrain,]), proportions[2]*nrow(data[-inTrain,]), replace=F)
        tt$validate <- data[-inTrain,][inValidation]
        tt$test <- data[-inTrain,][-inValidation]
    } 
    else {
        tt$validate <- NULL
        tt$test <- data[-inTrain,]
    }
    tt$validation_set <- validation_set
    return(tt)
}

get_plugin_codes <- function() {
    ###   cp /Users/sfrey/Downloads/Categorized\ Minecraft\ Servers\ -\ plugin_widehandcodes_rawXXX.csv  ~/projecto/research_projects/minecraft/redditcommunity/data/plugin_codes_byhand20160905XXX.csv
    #plugin_codes_byhand <- as.data.table(read.csv(file=paste0(pathData, "plugin_codes_byhand20160805.csv")))
    #plugin_codes_byhand <- as.data.table(read.csv(file=paste0(pathData, "plugin_codes_byhand20160826.csv")))
    #plugin_codes_byhand <- as.data.table(read.csv(file=paste0(pathData, "plugin_codes_byhand20160905.csv"), stringsAsFactors=FALSE))
    #plugin_codes_byhand <- as.data.table(read.csv(file=paste0(pathData, "plugin_codes_byhand20160909.csv"), stringsAsFactors=FALSE))
    plugin_codes_byhand <- as.data.table(read.csv(file=paste0(pathData, "plugin_codes_byhand20170727.csv"), stringsAsFactors=FALSE))
    pcodes <- plugin_codes_byhand
    #pcodes <- pcodes[1:(nrow(pcodes)-1),]
    pcodes[,feat_count:=NULL]
    pcodes[,notes:=NULL]
    pcodes$na <- with(pcodes, ifelse(is.na(na), 0, 1))
    pcodes$foreign <- with(pcodes, ifelse(is.na(foreign), 0, 1))
    pcodes$gov <- with( pcodes, ifelse(gov_auto==0 | gov_hand==0 | is.na(gov_hand), 0, 1))
    pcodes$blacklist <- with( pcodes, ifelse(!is.na(blacklist) & blacklist == 1,1,0))
    pcodes$blacklist_justmultiserver <- NULL
    pcodes$blacklist_inclminigames <- NULL
    pcodes$na <- NULL
    pcodes$foreign <- NULL
    pcodes$gov_auto <- NULL
    pcodes$gov_hand <- NULL
    pcodes$resource <- with(pcodes, ifelse(resource=='', 'noresource', resource) )
    pcodes$audience <- with(pcodes, ifelse(audience=='', 'noaudience', audience) )
    pcodes$upkeep <- with(pcodes, ifelse(upkeep=='', 'noupkeep', upkeep) )
    pcodes$enable_forbid_user <- with(pcodes, ifelse(enable_forbid_user == "",0,enable_forbid_user) %>%
                                      ifelse(enable_forbid_user == "\"+\"",1,.) %>%
                                      ifelse(enable_forbid_user == "\"-\"",-1,.) %>%
                                      as.numeric()
                                      )
    pcodes$enable_forbid_audience <- ifelse(pcodes$enable_forbid_audience == "",0,
                                                         pcodes$enable_forbid_audience 
                                                  ) %>%
                                    ifelse(pcodes$enable_forbid_audience == "\"+\"",1,.) %>%
                                    ifelse(pcodes$enable_forbid_audience == "\"-\"",-1,.) %>%
                                    as.numeric()
    pcodes$institution <- with(pcodes, ifelse(institution=='', 'noinstitution', institution) )
    pcodes$actionsituation <- with(pcodes, ifelse(actionsituation=='', 'noinstitution', actionsituation) )
    pcodes$institution <- with(pcodes, ifelse(institution=='noinstitution', actionsituation, institution))
    pcodes$actionsituation <- NULL
    pcodes$institution <- with(pcodes, ifelse(institution=='action_space' & enable_forbid_user == 1, "action_space_up", institution))
    pcodes$institution <- with(pcodes, ifelse(institution=='action_space' & enable_forbid_user == -1, "action_space_down", institution))
    pcodes$institution <- with(pcodes, ifelse(institution=='monitor' & audience == 'users', "monitor_by_peer", institution))
    pcodes$institution <- with(pcodes, ifelse(institution=='monitor' & audience == 'admin', "monitor_by_admin", institution))
    pcodes$resource <- factor(pcodes$resource)
    pcodes$audience <- factor(pcodes$audience)
    pcodes$upkeep <- factor(pcodes$upkeep)
    pcodes$institution <- factor(pcodes$institution)
    m1 <- dcast(pcodes, formula = feat_code + feat_url + blacklist + gov + enable_forbid_user + enable_forbid_audience ~ as.character(resource), value.var="feat_code", fun.aggregate = function(x) (length(x) > 0) + 0.0)
    m2 <- dcast(pcodes, formula = feat_code + feat_url + blacklist + gov + enable_forbid_user + enable_forbid_audience ~ audience, value.var="feat_code", fun.aggregate = function(x) (length(x) > 0) + 0.0)
    m3 <- dcast(pcodes, formula = feat_code + feat_url + blacklist + gov + enable_forbid_user + enable_forbid_audience ~ upkeep, value.var="feat_code", fun.aggregate = function(x) (length(x) > 0) + 0.0)
    m4 <- dcast(pcodes, formula = feat_code + feat_url + blacklist + gov + enable_forbid_user + enable_forbid_audience ~ institution, value.var="feat_code", fun.aggregate = function(x) (length(x) > 0) + 0.0)
    mm1 <- merge(m1, m2[,c(1,7:ncol(m2))], by='feat_code') 
    mm2 <- merge(m3, m4[,c(1,7:ncol(m4))], by='feat_code')
    gg <- merge( mm1, mm2[,c(1,7:ncol(mm2))], by='feat_code') 
    gg <- asdt(gg)
    ### I removed monitor from here, but i might want it back in t inthe future
    setnames(gg, c("grief", "ingame", "noresource", "performance", "players", "realmoney", "attention", "noaudience", "users", "admin", "enable_forbid_user", "enable_forbid_audience", "noupkeep", "coarseauto", "coarsemanual", "fineauto", "finemanual", "noinstitution", "broadcast", "chat",  "privateproperty", "shop", "action_space", "action_space_up", "action_space_down", "boundary", "monitor_by_peer", "monitor_by_admin", "position_h", "position_v", "payoff"), c("res_grief", "res_ingame", "res_none", "res_performance", "res_players", "res_realmoney", "res_attention", "aud_none", "aud_users", "aud_admin", "actions_user", "actions_audience", "use_na", "use_coarseauto", "use_coarsemanual", "use_fineauto", "use_finemanual", "inst_none", "inst_broadcast", "inst_chat",  "inst_privateproperty", "inst_shop", "inst_action_space", "inst_action_space_up", "inst_action_space_down", "inst_boundary", "inst_monitor_by_peer", "inst_monitor_by_admin", "inst_position_h", "inst_position_v", "inst_payoff"))
    setcolorder(gg, c("feat_code", "feat_url", "blacklist", "gov", "res_none", "res_grief", "res_ingame", "res_performance", "res_players", "res_realmoney", "res_attention", "aud_none", "aud_users", "aud_admin", "actions_user", "actions_audience", "use_na", "use_coarseauto", "use_coarsemanual", "use_fineauto", "use_finemanual", "inst_none", "inst_broadcast", "inst_chat",  "inst_privateproperty", "inst_shop", "inst_action_space", "inst_action_space_up", "inst_action_space_down", "inst_boundary", "inst_monitor_by_peer", "inst_monitor_by_admin", "inst_position_h", "inst_position_v", "inst_payoff"))
    ### now merge original columns back into the dmmy variable ones
    gg <- merge(gg, pcodes[,.(feat_code, resource, audience, upkeep, institution)], by='feat_code')
    ### sets of columns are mutually exclusive, making validity tests easy
    expect_true(all(rowSums(gg[,5:11,with=FALSE]) == 1))
    expect_true(all(rowSums(gg[,12:14,with=FALSE]) == 1))
    expect_true(all(gg[,actions_user %in% c(-1,0,1)]))
    expect_true(all(gg[,actions_audience %in% c(-1,0,1)]))
    expect_true(all(rowSums(gg[,17:21,with=FALSE]) == 1))
    expect_true(all(rowSums(gg[,22:35,with=FALSE]) == 1))
    return(gg )
}

### after data prep
coef_nonzero <- function(mc_rlm_fit) {
    gg <- data.frame(feat=as.character(as.character(coef(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$alpha)@Dimnames[[1]][coef(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$alpha)@i+1])), beta=as.numeric(format( (coef(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$alpha)@x), scientific=FALSE)) )
    gg$feat <- as.character(gg$feat)
    return(gg)
}
get_rmse_from_nulllm <- function(fit, xdata, y) {
    comp <- cbind(predict(fit, newdata=xdata), y )
    mean(apply(comp, 1, function(x) (x[1] - x[2])^2))^0.5
}
get_rme_from_nulllm <- function(fit, xdata, y) {
    comp <- cbind(predict(fit, newdata=xdata), y )
    mean(apply(comp, 1, function(x) (x[1] - x[2])))
}
get_rmse_from_caret <- function(object, xdata, y) {
    #print(class(y))
    #print(class(object))
    #print(class(xdata))
    comp <- cbind(predict(object$finalModel, newx=xdata, s=object$bestTune$alpha, type="response"), y )
    #print(head(comp))
    mean(apply(comp, 1, function(x) (x[1] - x[2])^2))^0.5
    #return(head(comp))
}
get_rme_from_caret <- function(object, xdata, y) {
    comp <- cbind(predict(object$finalModel, newx=xdata, s=object$bestTune$alpha, type="response"), y )
    mean(apply(comp, 1, function(x) (x[1] - x[2])))
}

### enet degrees of freedom from https://openaccess.leidenuniv.nl/bitstream/handle/1887/12096/04.pdf
enet.edf <- function(data, selected_variables, lambda2){
    tr <- function(m) sum(diag(m))
    A <- selected_variables
    X_A <- as.matrix(data[,A])
    evs <- eigen(t(X_A) %*% X_A, only.values=T)$values 
    
    #library(microbenchmark)
    #microbenchmark(
      #tr(X_A %*% solve(t(X_A) %*% X_A + lambda2 * diag(ncol(X_A))) %*% t(X_A))
    #, tr(t(X_A) %*% solve(t(t(X_A)) %*% t(X_A) + lambda2 * diag(nrow(X_A))) %*% X_A)
    #, sum(evs/(evs+lambda2))
    #)
    
    return(sum(evs/(evs+lambda2)))
}
glmnet.edf <- function(data, selected_variables, lambda, alpha){
    ###alpha is the [0,1] mixing parameter
    lambda1 <-     alpha*lambda
    lambda2 <- (1-alpha)*lambda
    tr <- function(m) sum(diag(m))
    A <- selected_variables
    X_A <- as.matrix(data[,A])
    evs <- eigen(t(X_A) %*% X_A, only.values=T)$values 
    
    #library(microbenchmark)
    #microbenchmark(
      #tr(X_A %*% solve(t(X_A) %*% X_A + lambda2 * diag(ncol(X_A))) %*% t(X_A))
    #, tr(t(X_A) %*% solve(t(t(X_A)) %*% t(X_A) + lambda2 * diag(nrow(X_A))) %*% X_A)
    #, sum(evs/(evs+lambda2))
    #)
    
    return(sum(evs/(evs+lambda2)))
}
logLik_glmnet <- function(model, y) {
    lm_fit <- lm(y~1)
    dev_null <- deviance(lm_fit)
    ll_null <- logLik(lm_fit)
    ll_sat <- 0.5*dev_null + ll_null
    dev_model <- tail(deviance(model),1)
    ll_model <- 0.5*(-dev_model+dev_null) + ll_null
    ll_model <- -0.5*dev_model + ll_sat
    dev_ratio <- tail(model$dev_ratio,1)
    dev_ratio <- 1 - dev_model/dev_null
    return(ll_model)
}

coef_codable <- function(mc_caret_fit) {
    coefs <- list()
    coefs$basic <- list()
    coefs$xsrv <- list()
    restring_feat <- "^(?:plugin|tag|keyword|property)_"
    restring_xsrv <- "^srvmax_"
    fit_results <- asdt(coef_nonzero( mc_caret_fit ))
    fit_results[,feat_trunc:={
        feat_trunc=sub(restring_xsrv, '', as.character(feat)); 
        feat_trunc }
    ]
    fit_results[,feat_raw:={
        feat_raw=sub(restring_feat, '', feat_trunc); 
        feat_raw }
    ]
    basic_cols <- grep(restring_feat, fit_results$feat, value=TRUE)
    xsrv_cols  <- paste( "srvmax", grep( restring_feat, sub( restring_xsrv , '' , fit_results$feat[ grep( restring_xsrv, fit_results$feat) ] ) , value=TRUE) , sep='_') ### get rid of first prefix vefore searchingfor secondone
    bookkeeping_cols <- fit_results$feat[fit_results$feat %ni% c(basic_cols, xsrv_cols)]
    expect_equal(nrow(fit_results), length(c(basic_cols, xsrv_cols, bookkeeping_cols)))
    positive_colnums <- which(fit_results$beta > 0)
    negative_colnums <- which(fit_results$beta < 0)
    positive_cols <- fit_results[beta > 0, feat]
    negative_cols <- fit_results[beta < 0, feat]
    coefs$basic$positive <- fit_results[feat %in% intersect(basic_cols,positive_cols),feat_trunc]
    coefs$basic$negative <- fit_results[feat %in% intersect(basic_cols,negative_cols),feat_trunc]
    coefs$xsrv$positive  <- fit_results[feat %in% intersect(xsrv_cols,positive_cols),feat_trunc]
    coefs$xsrv$negative  <- fit_results[feat %in% intersect(xsrv_cols,negative_cols),feat_trunc]
    coefs$all <- c(coefs$basic$positive, coefs$xsrv$positive, coefs$basic$negative, coefs$xsrv$negative)
    expect_equal( sum(sapply(coefs$all, length)) + length(bookkeeping_cols), length(fit_results$feat) )
    return(coefs)
}
