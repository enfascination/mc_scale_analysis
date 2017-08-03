### INITIALIZE GLOBALS
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_redditscrape.r"))
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_step6_analysis.r"))
source(paste0(pathLocal,"lib_plotting.r"))

library(boot)
library(ggthemes)
library(scales)
### notes:
###  if there is lots of data 50/50 training/test is fine, and you shouldn't calculate full lasso paths (dfmax=50 or 100) and it's important to filter columns down before widening the matrix.  

### LOAD dATA
spings <- readRDS(paste0(pathData, "step5_serversweeks.rds"))
splugins <- readRDS(paste0(pathData, "step5_serversweeksplugins.rds"))
pluginstats <- as.data.table(read.csv(paste0(pathData, 'step45_curse_plugins_metadata_full.csv')))

## DATA PREP
sfeat <- buildPickDependent(spings, dependent='ncomm4visits')
mc <- buildPickDependent(spings, dependent='ncomm4visits')
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

# PLOTS:
# RESULTS FROM THE FULL 50000: (80000!)
# (SUCCESS?) DENSITY
full_data_rug <-  geom_rug(data=mw, mapping=aes(x=xrug, y=yrug), col=rgb(0.7,0.7,0.7,alpha=0.02),sides="tl")
(plot_srv_density <- make_plot_size_by_success(mw, "weeks_up_total", function(x,i) nrow(x[i]), ggmore=scale_fill_gradientn(colors=grey(seq(from=0.6,to=0.3,length.out=6)), values=rescale(c(0,4,16,64,256,1024)), breaks=c(0,4,16,64,256,1024)), ggguide=guide_legend("Server\ncount", reverse=TRUE), reps=10, ggrug=F) + full_data_rug + guides(fill="none"))
ggsave(plot_srv_density, file=paste0(pathImages, "plot_srv_density.png"), units='cm', width=3.25, height=2.5, scale=3)
mw[,.(unsuccessful=sum(table(perf_factor)[1:2]),all=sum(table(perf_factor)),ratio=sum(table(perf_factor)[1:2])/sum(table(perf_factor))), by=pop_size_factor]

# LIFETIME 
(plot_srv_hazard_bar1 <- ggplot(mw_train[,.(longevity_count=.N),by=.(weeks_up_total)], aes(x=weeks_up_total, y=longevity_count)) + geom_bar(stat="identity", alpha=0.6) + theme_bw() + scale_y_log10("Count") + xlab("Longevity (weeks)") )
median( mw_train$weeks_up_total)
(plot_srv_hazard <- make_plot_size_by_success(mw_train, "weeks_up_total", gov_median, ggmore=scale_fill_gradient(high="#3182bd", low="#cccccc"), ggguide="none", reps=1000, ggrug=FALSE ) + full_data_rug + guides(fill="none") )
ggsave(plot_srv_hazard, file=paste0(pathImages, "plot_srv_hazard.png"), units='cm', width=3.25, height=2.5, scale=3)
ggsave(plot_srv_hazard_bar1, file=paste0(pathImages, "plot_srv_hazard_bar1.png"), units='cm', width=5, height=1.5, scale=3)

# UNIQUES VS RETURNS
plot_population_distribution <- plot_visitortype(mw, plot_type='horizontal')
plot_population_distribution 
#ggsave(plot_population_distribution, file=paste0(pathImages, "plot_population_distribution.png"), units='cm', width=5, height=2.5, scale=5)
ggsave(plot_population_distribution, file=paste0(pathImages, "plot_population_distribution_rect.png"), units='cm', width=2, height=3, scale=5)
ggsave(plot_population_distribution, file=paste0(pathImages, "plot_population_distribution_rect2.png"), units='cm', width=3, height=2, scale=5)
