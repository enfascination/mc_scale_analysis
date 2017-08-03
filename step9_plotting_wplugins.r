### INITIALIZE GLOBALS
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_step6_analysis.r"))
source(paste0(pathLocal,"lib_plotting.r"))
library(boot)
library(ggthemes)
library(scales)
### LOAD dATA
mc <- readRDS(paste0(pathData, "step55_serversweeksplugins.rds"))
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
entropy_calc <- function(x) {entropy(x, method="ML")}
entropy_calc <- function(x) {-x*log(x)}
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
# SAMPLING
### split data up
mc_split <- splitDataTestTrain(mw, proportions=c(0.5, 0.25, 0.25), validation_set=TRUE)
mw_train <- mc_split$train
mw_test <- mc_split$test
mw_full <- mw
### MAIN DATASET
#mw <- mw_full
mw <- mw_train

# PLOTS:
full_data_rug <-  geom_rug(data=mw, mapping=aes(x=xrug, y=yrug), col=rgb(0.7,0.7,0.7,alpha=0.2),sides="tl")
### COLORSCHEMES
ggel_lowbad <- scale_fill_gradient(high="#41ab5d", low="#cccccc") 
ggel <- scale_fill_gradient(high="#3182bd", low="#cccccc") 
ggel_gov <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=2.5, breaks=seq(from=0,to=12,by=2)) 
ggel_gov_prop <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=0.5, breaks=seq(from=0,to=1,by=0.2)) 
ggel_gov_rat <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=0.10) 
ggel_gov_rat_within <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59") 
ggel_gov_by_type <- scale_fill_gradientn(colors=(seq_gradient_pal(low=muted("#91cf60", l=100, c=100), high=muted("#fc8d59", l=100, c=100)))(rescale(seq(from=0,to=10,by=2))), values=rescale(seq(from=0,to=10,by=2)^2)) 

# RESULTS FROM THE GOV-RELATED 5000
### PLOT COLLECTIVE ACTION PROBLEM EMERGENCE
# make 2x3 plot: 
# show the three types of resource problems changing count and also frequency 
for (i in 1:3){
	print(plot_gov_resources <- make_plot_size_by_success(mw, c("res_grief", "res_ingame","res_realworld")[i], gov_mean, ggmore=ggel_gov, ggguide=guide_legend("Mean plugin count", reverse=TRUE), reps=100))
	print(plot_gov_resources_ratio <- make_plot_size_by_success(mw, c("res_grief", "res_ingame","res_realworld"), gov_mean_proportion_2, focal=i, ggmore=ggel_gov_rat, ggguide=guide_legend("Ratio\ngovernance", reverse=TRUE), reps=100))
}

### NOW REDO IN ONE SPECIALIST DATATABLE
mwres <- mw[,.(srv_addr, srv_max, pop_size_factor, perf_factor, res_grief, res_ingame, res_realworld)]
## this gives NAs if there are no res_ plguins in any category. so it goes.
mwres[,':='(total_res=sum(res_grief, res_ingame, res_realworld), pct_grief=sum(res_grief)/sum(res_grief, res_ingame, res_realworld), pct_ingame=sum(res_ingame)/sum(res_grief, res_ingame, res_realworld), pct_realworld=sum(res_realworld)/sum(res_grief, res_ingame, res_realworld)),by=.(srv_addr)]
mwres[,':='(sanity_pct=sum(pct_grief, pct_ingame, pct_realworld)),by=.(srv_addr)]
#(plot_gov_res <- make_plot_size_by_success(mwres, "res_grief", gov_mean_narm , ggmore=ggel_gov, ggguide="none", reps=1000))
#(plot_gov_res <- make_plot_size_by_success(mwres, "pct_grief", gov_mean_narm , ggmore=ggel_gov_rat, ggguide="none", reps=1000))
### institution by size:
gg <- melt(mwres, id.vars = c("srv_addr", "srv_max", "pop_size_factor", "perf_factor", "total_res"),  measure.vars = patterns("^res_", "^pct_"), variable.name = 'resourcegov', value.name='resourcegov_count', variable.factor=FALSE)
setnames(gg, c('resourcegov_count1', 'resourcegov_count2'), c('resourcegov_count', 'resourcegov_pct'))
gginclude <- c("res_grief", "res_ingame", "res_realworld")
#gg <- gg[resourcegov %in% gginclude]
gg[,resourcegov:=factor(resourcegov, levels=1:3, labels=c("Antisociality", "Physical resources", "Virtual resources"))] ### patterns() erases old names with integers that have to be replaced with a leap of faith
print(plot_resgov <- make_plot_size_by_success(gg, c("resourcegov_count"), gov_mean_narm, ggmore=ggel_gov_by_type, ggtext=FALSE, reps=10, facetting=c("resourcegov")) + facet_wrap( ~ resourcegov, nrow=1)+ theme(strip.background=element_rect(color="white", fill="white"), axis.text=element_text(size=6)))
print(plot_resgov_pct <- make_plot_size_by_success(gg, c("resourcegov_pct"), gov_mean_narm , ggmore=ggel_gov_rat, ggtext=FALSE, reps=0, facetting=c("resourcegov")) + facet_wrap( ~ resourcegov, nrow=1)+ theme(strip.background=element_rect(color="white", fill="white"), axis.text=element_text(size=6)))
#(plot_actiondown_scaling <- make_plot_size_by_success(gg[resourcegov == "Proscriptions"], c("institution_count"), gov_median , ggmore=ggel_gov_by_type, ggguide="none", reps=1000) )
ggsave(plot_resgov, file=paste0(pathImages, "plot_resgov.png"), units='cm', width=4.25, height=2.5, scale=3)
ggsave(plot_resgov_pct, file=paste0(pathImages, "plot_resgov_pct.png"), units='cm', width=4.25, height=2.5, scale=3)

### PLOT COLLECTIVE ACTION PROBLEM EMERGENCE (BETTER)
# then make the same pot, witht e bottom row combined into one color coded RGB plot like in that poker paper
data_resgov_spectrum <- mwres[,.(pct_grief=mean(pct_grief,na.rm=T),pct_ingame=mean(pct_ingame,na.rm=T),pct_realworld=mean(pct_realworld,na.rm=T)),by=.(perf_factor, pop_size_factor)]
expect_true( all( round( data_resgov_spectrum$pct_grief + data_resgov_spectrum$pct_ingame + data_resgov_spectrum$pct_realworld, 3) == 1)) ### sanity check (round to taking care of ugly floats)
data_resgov_spectrum[,color_res:=rgb(pct_grief, 0.35+pct_ingame*0.65, 0.35+pct_realworld*0.65, 1)]
color_res <- data_resgov_spectrum$color_res
names(color_res) <- color_res
#plot_resgov_spectrum<-ggplot(data_resgov_spectrum, aes(pop_size_factor, perf_factor)) + geom_bin2d(aes(fill=color_res)) + theme_bw() + theme(panel.grid.major=element_line(0), axis.text.y = element_text(angle = 45)) + coord_fixed(ratio=6/7) + scale_x_discrete("Server size", expand = c(0.035,0) ) +     scale_fill_manual(values = color_res )+ scale_y_discrete("Core members", expand = c(0.035,0)) + guides(fill="none"); plot_resgov_spectrum
(plot_resgov_spectrum <- make_plot_size_by_success(data_resgov_spectrum, c("color_res"), as.character, ggmore=scale_fill_manual(values = color_res ), ggtext=FALSE, reps=0, ggguide="none", ggrug=FALSE) + full_data_rug) 
ggsave(plot_resgov_spectrum, file=paste0(pathImages, "plot_resgov_spectrum.png"), units='cm', width=4.25, height=2.5, scale=3)



# PLOT GOV COMPLEXITY
# plot the three "complexity"results together: intensity, centralization, diversity within 
(plot_gov_scaling <- make_plot_size_by_success(mw, "gov", gov_mean , ggmore=ggel_gov, ggguide="none", reps=1000))
(plot_gov_specialization <- make_plot_size_by_success(mw, "plugin_specialization", gov_mean_narm , ggmore=ggel_gov, ggguide="none", reps=1000))
# PLOT GOV INTENSITY
ggsave(plot_gov_scaling, file=paste0(pathImages, "plot_gov_scaling.png"), units='cm', width=3.25, height=2.5, scale=3)
# PLOT GOV SPECIALIZATION
ggsave(plot_gov_specialization, file=paste0(pathImages, "plot_gov_specialization.png"), units='cm', width=3.25, height=2.5, scale=3)
(plot_srv_institutional_diversity <- make_plot_size_by_success(mw, "srv_entropy", gov_mean_narm, ggmore=ggel_lowbad, ggguide=guide_legend("Rule Diversity", reverse=TRUE), reps=10, ggrug=FALSE) + full_data_rug)
# PLOT GOV CENTRALIZATION
# PLOT GOV COMPLEXITY
ggsave(plot_srv_institutional_diversity, file=paste0(pathImages, "plot_srv_institutional_diversity.png"), units='cm', width=4, height=2.5, scale=3)


# PLOT DIVERSITY ACROSS SERVERS
### server diversity
### bootstrapping fucntion for entropy
(plot_srv_gov_diversity <- (make_plot_size_by_success(mw, grep("^inst_[^n]", names(mw), value=TRUE), gov_dist, ggguide=guide_legend("Population Variability", reverse=TRUE), ggmore=ggel_lowbad, reps=10, ggtext=FALSE, ggrug=FALSE) + full_data_rug))
ggsave(plot_srv_gov_diversity, file=paste0(pathImages, "plot_srv_gov_diversity.png"), units='cm', width=4, height=2.5, scale=3)

