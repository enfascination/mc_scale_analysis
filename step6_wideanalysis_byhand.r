### initialize globals
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_step6_analysis.r"))

### notes:
###  if there is lots of data 50/50 training/test is fine, and you shouldn't calculate full lasso paths (dfmax=50 or 100) and it's important to filter columns down before widening the matrix.  

mc <- readRDS(paste0(pathData, "step55_serversweeksplugins.rds"))
expect_true(mc[,length(unique(srv_addr))] == mc[,length(unique(post_uid))])
mc[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]
n_servers <- mc[,length(unique(srv_addr))]; n_servers 
dim(mc)
mc <- filterDataSetDown(mc, cutUnrealistic=TRUE, cutNonVanilla=TRUE, cutNonPositiveDependent=TRUE, featureCountMin=max(2, as.integer(n_servers/1000)), keepFeatTypes=c('plugin', 'property'), keepDataSource=c('reddit', 'omni', 'mcs_org'))
dim(mc)

writeBlankFeatureCodingTable(mc, paste0(pathData, "plugin_widehandcodes_raw.csv"))

### maybe refresh this occasionally 
plugin_codes_byhand <- get_plugin_codes()
#cor(plugin_codes_byhand[4:ncol(plugin_codes_byhand)])
mc[mc$feat_code %ni% plugin_codes_byhand$feat_code, unique(feat_code) ]  ### more things to code and integrate back into get_plugin_codes()
mc <- merge(mc, plugin_codes_byhand[,c(1,3:ncol(plugin_codes_byhand)),with=FALSE], by=c('feat_code'), all.x=T, all.y=F)
mc_h <- merge(
        mc[, lapply(.SD, unique), by=.(srv_addr), .SDcols=c("post_uid", "srv_max", "srv_max_log", "srv_max_bak", "dataset_reddit", "dataset_omni", "dataset_mcs_org", "jubilees", "y", "ylog", "srv_votes", "srv_repquery", "srv_repplug", "srv_repsample", "weeks_up_total", "weeks_up_todate", "date_ping_int", "date_ping_1st", "date_ping_lst", "plugin_count", "log_plugin_count", "keyword_count", "tag_count", "sign_count", "norm_count")],
        #mc[, lapply(.SD, function(x) sum(x, na.rm=T)), by=.(srv_addr), .SDcols=c("action_admin_up", "action_other_down", "grief", "inoutworld", "inst", "isnorm", "normpath", "forbid", "boundary", "position", "choice", "info", "infopath", "aggregation", "payoff", "scope", "shop", "tech", "game", "loopadmin", "poly", "hierarchy", "property", "chat", "apply", "resource")]
        mc[, lapply(.SD, function(x) sum(x, na.rm=T)), by=.(srv_addr), .SDcols=c("gov", "res_grief", "res_ingame", "res_none", "res_performance", "res_players", "res_realmoney", "aud_none", "aud_users", "aud_admin", "actions_user", "actions_audience", "use_na", "use_coarseauto", "use_coarsemanual", "use_fineauto", "use_finemanual", "inst_none", "inst_broadcast", "inst_chat",  "inst_privateproperty", "inst_shop", "inst_action_space", "inst_action_space_up", "inst_action_space_down", "inst_boundary", "inst_monitor", "inst_position_h", "inst_position_v")]
        , by="srv_addr", all=T)
mc_h <- merge(
        mc_h,
        mc[, lapply(.SD, function(x) sum(x, na.rm=T)), by=.(srv_addr), .SDcols=grep("^cat_*", names(mc))]  ### this na.rm is important here because property don't have categories. there are also a small number of plugins with na categories becaue i couldn't match them. 
        , by="srv_addr", all=T)

### define predictors
vars_non_model <- c(c("post_uid", "srv_addr", "srv_max", "srv_max_bak", "srv_repquery", "srv_repplug", "srv_repsample", "dataset_omni", "keyword_count", "tag_count", "sign_count"), c("date_ping_1st", "date_ping_lst", "plugin_count", "weeks_up_total", "srv_votes", "y"))
vars_out <- c('ylog')
vars_in_nonfeat <- c(c("srv_max_log", "date_ping_int", "weeks_up_todate", 'jubilees'), c("log_plugin_count", "dataset_reddit", "dataset_mcs_org", "aud_none", "use_na", "inst_none" ))
#vars_in_feat <-  names(mc_h)[which(names(mc_h) %ni% c(vars_non_model, vars_out, vars_in_nonfeat))] 
#vars_in_feat <- c("action_admin_up", "action_other_down", "grief", "inoutworld", "inst", "estnorm", "forbid", "boundary", "position", "choice", "info", "infopath", "aggregation", "payoff", "scope", "shop", "tech", "game", 'loopadmin', 'poly', 'property', 'chat')
vars_in_feat <- c("res_grief", "res_ingame", "res_none", "res_performance", "res_players", "res_realmoney" , "aud_users", "aud_admin", "actions_user", "actions_audience", "use_coarseauto", "use_coarsemanual", "use_fineauto", "use_finemanual", "inst_broadcast", "inst_chat",  "inst_privateproperty", "inst_shop", "inst_action_space", "inst_action_space_up", "inst_action_space_down", "inst_boundary", "inst_monitor", "inst_position_h", "inst_position_v", "cat_admintools", "cat_antigrief", "cat_chat", "cat_economy", "cat_informational", "cat_webadmin", "cat_devtools", "cat_fun", "cat_general", "cat_mechanics", "cat_misc", "cat_roleplay", "cat_teleportation", "cat_world", "cat_fixes", "cat_worldgen", "resource", "audience", "upkeep", "institution")
#interact_xsrv <- as.data.table(mc_h[,vars_in_feat,with=F][,apply(.SD, 2, function(x) x*mc_h$srv_max_log )])
#vars_in_feat_xsrv <- paste("srvmax", vars_in_feat, sep='_')
#names(interact_xsrv) <- vars_in_feat_xsrv
#mc_h <- cbind(mc_h, interact_xsrv)

mc_split <- splitDataTestTrain(mc_h, proportions=c(0.5, 0.25, 0.25), validation_set=TRUE)
train <- mc_split$train
validate <- mc_split$validate
testing <- mc_split$test
#training_full_lasso <- rbind(training, mc_split$validate)
training_full_lasso <- train

### internal governance plugin patterns
ggplot(mc, aes(x=institution, y=upkeep)) + geom_jitter()
### more governance by size: (gov ~ srv_max)
ggplot(data=training_full_lasso,aes(x=srv_max, y=gov)) + geom_jitter(size=0.1, height=0.1, width=0.1) + scale_x_log10() + geom_smooth(method='rlm') 
ggplot(data=training_full_lasso[!is.na(gov),.(srv_max_log=cut(srv_max_log, 10), gov=gov)],aes(x=srv_max_log)) + geom_bar(position='fill', aes(y=..prop.., group=gov,fill=gov)) 
### resource managemanet style by size:
ggplot(data=melt(training_full_lasso, id.vars = c("srv_addr", "srv_max, 'y'"),  measure.vars = c("res_grief", "res_ingame", "res_realmoney", "res_performance", "res_players"), variable.name = 'resource', value.name='resource_count'),aes(x=srv_max, y=resource_count)) + geom_jitter(size=0.1, height=0.1, width=0.1) + scale_x_log10() + geom_smooth(method='rlm') + facet_wrap(~resource, ncol=2) 
### institution by size:
ggplot(data=melt(training_full_lasso, id.vars = c("srv_addr", "srv_max", 'y'),  measure.vars = grep("^inst_", names(training_full_lasso)), variable.name = 'institution', value.name='institution_count'),aes(x=srv_max, y=institution_count)) + geom_jitter(size=0.1, height=0.1, width=0.1) + scale_x_log10() + geom_smooth(method='rlm') + facet_wrap(~institution, ncol=2) 
ggplot(mc[institution!='noinstitution' & resource!='noresource'], aes(x=institution, y=resource, fill=..count..)) + geom_bin2d()
### governance style counts (*_count) by size: INCONCLUSIVE. not enough data

summary(lm_comm <- rlm(y ~ srv_max_log + srv_max_log*weeks_up_todate + date_ping_int + jubilees + srv_max_log*log_plugin_count + srv_max_log*dataset_reddit + srv_max_log*dataset_mcs_org  + cat_fun + cat_general + cat_mechanics + cat_misc + cat_roleplay + cat_teleportation + cat_world + cat_fixes +  cat_worldgen + res_grief*srv_max_log + res_ingame*srv_max_log + res_performance*srv_max_log + res_players*srv_max_log + res_realmoney*srv_max_log +  aud_users*srv_max_log + aud_admin*srv_max_log + actions_user*srv_max_log + use_coarseauto*srv_max_log + use_coarsemanual*srv_max_log + use_fineauto*srv_max_log + use_finemanual*srv_max_log + inst_broadcast*srv_max_log + inst_chat*srv_max_log + inst_privateproperty*srv_max_log + inst_shop*srv_max_log + inst_action_space_up*srv_max_log + inst_action_space_down*srv_max_log + inst_boundary*srv_max_log + inst_monitor*srv_max_log + inst_position_h*srv_max_log + inst_position_v*srv_max_log +  aud_users:actions_audience:srv_max_log + aud_admin:actions_audience:srv_max_log,  data=training_full_lasso))
asdt(tidy(lm_comm))[abs(statistic)>=2]
(lm_size <- rlm(srv_max_log ~ weeks_up_todate + date_ping_int + jubilees + log_plugin_count + dataset_reddit + dataset_mcs_org + cat_fun + cat_general + cat_mechanics + cat_misc + cat_roleplay + cat_teleportation + cat_world + cat_fixes +  cat_worldgen + res_grief + res_ingame + res_performance + res_players + res_realmoney +  aud_users + aud_admin + actions_user + use_coarseauto + use_coarsemanual + use_fineauto + use_finemanual + inst_broadcast + inst_chat + inst_privateproperty + inst_shop + inst_action_space + inst_boundary + inst_monitor + inst_position_h + inst_position_v +  aud_users:actions_audience:srv_max_log + aud_admin:actions_audience:srv_max_log,,  data=training_full_lasso))
asdt(tidy(lm_size))[abs(statistic)>=2]
(lm_grief <- rlm(res_grief ~ srv_max_log + srv_max_log*weeks_up_todate + date_ping_int + jubilees + srv_max_log*log_plugin_count + dataset_reddit + dataset_mcs_org + inst_broadcast*srv_max_log + inst_chat*srv_max_log + inst_privateproperty*srv_max_log + inst_shop*srv_max_log + inst_action_space*srv_max_log + inst_boundary*srv_max_log + inst_monitor*srv_max_log + inst_position_h*srv_max_log + inst_position_v*srv_max_log,  data=training_full_lasso))
asdt(tidy(lm_grief))[abs(statistic)>=2]

(fff <- rlm(srv_max_log ~ weeks_up_todate + date_ping_int + jubilees + log_plugin_count + dataset_reddit + dataset_mcs_org + grief + tech + poly + hierarchy + chat + inoutworld + shop + property + normpath,  data=training_full_lasso))
(ff <- rlm(y ~ srv_max_log + I(srv_max_log^2) + srv_max_log*weeks_up_todate + date_ping_int + jubilees + srv_max_log*log_plugin_count + srv_max_log*dataset_reddit + srv_max_log*dataset_mcs_org + tech*srv_max_log + poly*srv_max_log + grief*srv_max_log + hierarchy*srv_max_log + chat*srv_max_log + inoutworld*srv_max_log + shop*srv_max_log + property*srv_max_log + normpath*srv_max_log,  data=training_full_lasso))
(ffff <- rlm(weeks_up_total ~ srv_max_log  + I(srv_max_log^2) + date_ping_int + jubilees + log_plugin_count + dataset_reddit + dataset_mcs_org,  data=training_full_lasso[(max(ymd(train$date_ping_lst)) - ymd((train$date_ping_lst))) <= 31]))
(ffvotes <- rlm(srv_votes ~ srv_max_log + I(srv_max_log^2) + srv_max_log*weeks_up_todate + date_ping_int + jubilees + srv_max_log*log_plugin_count + srv_max_log*dataset_reddit + srv_max_log*dataset_mcs_org + tech*srv_max_log + poly*srv_max_log + grief*srv_max_log + hierarchy*srv_max_log + chat*srv_max_log + inoutworld*srv_max_log + shop*srv_max_log + property*srv_max_log + normpath*srv_max_log,  data=training_full_lasso))
asdt(tidy(ff))[abs(statistic)>=2]
asdt(tidy(fff))[abs(statistic)>=2]
asdt(tidy(ffff))[abs(statistic)>=2]
asdt(tidy(ffvotes))[abs(statistic)>=2]
splom(~training_full_lasso[,.( srv_max_log, weeks_up_todate, date_ping_int, jubilees, plugin_count, dataset_reddit, dataset_mcs_org, tech, hierarchy, chat, inoutworld, shop, property, normpath)])
#cor(training_full_lasso[,.( action_admin_up ,normpath  ,infopath,srv_max_log)])
ggplot(data=mc, aes(y=resource, x=srv_max)) + geom_point() + geom_jitter() + scale_x_log10() 
ggplot(data=training_full_lasso, aes(y=y, x=srv_max)) + geom_point() + geom_jitter() + scale_x_log10() + scale_y_log10()
ggplot(data=training_full_lasso, aes(y=plugin_count, x=srv_max)) + geom_point() + geom_jitter() + scale_x_log10() 
ggplot(data=training_full_lasso[(max(ymd(train$date_ping_lst)) - ymd((train$date_ping_lst))) <= 31], aes(y=weeks_up_total, color=plugin_count, x=srv_max)) + geom_point() + geom_jitter() + scale_x_log10()
ggplot(data=training_full_lasso[(max(ymd(train$date_ping_lst)) - ymd((train$date_ping_lst))) <= 31], aes(y=weeks_up_total, color=plugin_count, x=srv_max_log)) + geom_point() + geom_jitter()  + geom_line(data=data.frame(x=log10(1:1000), y=((9.54)+1.94*log10(1:1000)+-0.26*log10(1:1000)^2)),aes(x=x, y=y, color=NULL))  + scale_y_continuous(limits=c(0,60))
#cbind(term=tidy(ff <- rlm(y ~ weeks_up_todate + date_ping_int + jubilees + plugin_count + srv_max_log + resource*srv_max_log + shop*srv_max_log + property*srv_max_log + action_admin_up*srv_max_log  + loopadmin*srv_max_log + normpath*srv_max_log  + infopath*srv_max_log + srv_max_log*action_admin_up*loopadmin + srv_max_log*action_admin_up*normpath + srv_max_log*action_admin_up*infopath,  data=training_full_lasso[]))[,c("term", "statistic")]
        #, signif(cbind(statistic=tidy(ff <- rlm(y ~ weeks_up_todate + date_ping_int + jubilees + plugin_count + srv_max_log + resource*srv_max_log + shop*srv_max_log + property*srv_max_log + action_admin_up*srv_max_log + loopadmin*srv_max_log + normpath*srv_max_log  + infopath*srv_max_log + srv_max_log*action_admin_up*loopadmin + srv_max_log*action_admin_up*normpath + srv_max_log*action_admin_up*infopath,  data=training_full_lasso[]))[,c("statistic")]
        #,  stat2=tidy(ff <- rlm(y ~ weeks_up_todate + date_ping_int + jubilees + plugin_count + srv_max_log + resource*srv_max_log + shop*srv_max_log + property*srv_max_log + action_admin_up*srv_max_log + loopadmin*srv_max_log + normpath*srv_max_log  + infopath*srv_max_log + srv_max_log*action_admin_up*loopadmin + srv_max_log*action_admin_up*normpath + srv_max_log*action_admin_up*infopath,  data=training_full_lasso[y<=200]))[,c("statistic")]
        #, lmstat=tidy(ff <- lm(y ~ weeks_up_todate + date_ping_int + jubilees + plugin_count + srv_max_log + resource*srv_max_log + shop*srv_max_log + property*srv_max_log + action_admin_up*srv_max_log + loopadmin*srv_max_log + normpath*srv_max_log  + infopath*srv_max_log + srv_max_log*action_admin_up*loopadmin + srv_max_log*action_admin_up*normpath + srv_max_log*action_admin_up*infopath,  data=training_full_lasso[]))[,c( "statistic")]
        #,  lmstat2=tidy(ff <- lm(y ~ weeks_up_todate + date_ping_int + jubilees + plugin_count + srv_max_log + resource*srv_max_log + shop*srv_max_log + property*srv_max_log + action_admin_up*srv_max_log + loopadmin*srv_max_log + normpath*srv_max_log  + infopath*srv_max_log  + srv_max_log*action_admin_up*loopadmin + srv_max_log*action_admin_up*normpath + srv_max_log*action_admin_up*infopath,  data=training_full_lasso[y<=200]))[,c("statistic")] 
                     #),2)
      #)

#cbind(term=tidy(ff <- rlm(y ~ weeks_up_todate + date_ping_int + jubilees + plugin_count + srv_max_log + resource*srv_max_log + shop*srv_max_log + property*srv_max_log + normpath*srv_max_log  + infopath*srv_max_log,  data=training_full_lasso[]))[,c("term", "statistic")]
        #, signif(cbind(statistic=tidy(ff <- rlm(y ~ weeks_up_todate + date_ping_int + jubilees + plugin_count + srv_max_log + resource*srv_max_log + shop*srv_max_log + property*srv_max_log +  normpath*srv_max_log  + infopath*srv_max_log,  data=training_full_lasso[]))[,c("statistic")]
        #,  stat2=tidy(ff <- rlm(y ~ weeks_up_todate + date_ping_int + jubilees + plugin_count + srv_max_log + resource*srv_max_log + shop*srv_max_log + property*srv_max_log +  normpath*srv_max_log  + infopath*srv_max_log,  data=training_full_lasso[y<=200]))[,c("statistic")]
        #, lmstat=tidy(ff <- lm(y ~ weeks_up_todate + date_ping_int + jubilees + plugin_count + srv_max_log + resource*srv_max_log + shop*srv_max_log + property*srv_max_log +  normpath*srv_max_log  + infopath*srv_max_log,  data=training_full_lasso[]))[,c( "statistic")]
        #,  lmstat2=tidy(ff <- lm(y ~ weeks_up_todate + date_ping_int + jubilees + plugin_count + srv_max_log + resource*srv_max_log + shop*srv_max_log + property*srv_max_log +  normpath*srv_max_log  + infopath*srv_max_log,  data=training_full_lasso[y<=200]))[,c("statistic")] 
                     #),2)
      #)



### test of design principles
ff <- rlm(y ~ weeks_up_todate + date_ping_int + plugin_count + tech*srv_max_log + game*srv_max_log + inoutworld*srv_max_log + grief*srv_max_log + estnorm*srv_max_log + loopadmin*srv_max_log,  data=training_full_lasso)
### test of action situation rule types
ff <- rlm(y ~ weeks_up_todate + date_ping_int + plugin_count + boundary*srv_max_log + position*srv_max_log + poly*srv_max_log + hierarchy*srv_max_log + choice*srv_max_log + grief*srv_max_log + info*srv_max_log + infopath*srv_max_log + chat*srv_max_log + aggregation*srv_max_log + property*srv_max_log + payoff*srv_max_log + scope*srv_max_log + shop*srv_max_log + tech*srv_max_log + game*srv_max_log,  data=training_full_lasso)
### do we need humans in the loop in tech-driven governance??  how does that scale? 
### loop_admin, srv_max
summary(ff <- rlm(y ~ weeks_up_todate + date_ping_int + plugin_count + loopadmin*srv_max_log,  data=training_full_lasso))
ff_pvalue <- 2*pt(abs(summary(ff)$coefficients[,"t value"]), summary(ff)$df[2], lower.tail=FALSE)
### how do rules and norms interact, and how do those htings interact with scale?
summary(ff <- rlm(y ~ weeks_up_todate + date_ping_int + plugin_count + action_admin_up*normpath*srv_max_log ,  data=training_full_lasso))
### how do rules and norms interact, and how do those htings interact with scale?
summary(ff <- rlm(y ~ weeks_up_todate + date_ping_int + plugin_count + infopath*normpath*srv_max_log ,  data=training_full_lasso))
summary(ff <- rlm(y ~ weeks_up_todate + date_ping_int + plugin_count + action_admin_up*srv_max_log + loopadmin*srv_max_log + normpath*srv_max_log  + infopath*srv_max_log ,  data=training_full_lasso))
summary(ff <- rlm(y ~ weeks_up_todate + date_ping_int + plugin_count + loopadmin*normpath*action_admin_up + loopadmin*srv_max_log + normpath*srv_max_log  + infopath*srv_max_log ,  data=training_full_lasso))
ff
### norm_path, srv_max, scope, info_path

