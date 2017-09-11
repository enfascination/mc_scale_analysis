### INITIALIZE GLOBALS
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_plotting.r"))

mw <- readRDS(paste0(pathData, "step6_servers_tall_analysis.rds"))

# PLOTS:
# RESULTS FROM THE FULL 50000: (80000!)
# (SUCCESS?) DENSITY
full_data_rug <-  geom_rug(data=mw, mapping=aes(x=xrug, y=yrug), col=rgb(0.7,0.7,0.7,alpha=0.02),sides="tl")
(plot_srv_density <- make_plot_size_by_success(mw, "y", function(x,i) nrow(x[i]), ggmore=scale_fill_gradientn(colors=grey(seq(from=0.6,to=0.3,length.out=6)), values=rescale(c(0,4,16,64,256,1024)), breaks=c(0,4,16,64,256,1024)), ggguide=guide_legend("Server\ncount", reverse=TRUE), reps=10, ggrug=F) + full_data_rug + guides(fill="none"))
ggsave(plot_srv_density, file=paste0(pathImages, "plot_srv_density.png"), units='cm', width=3.25, height=2.5, scale=3)
mw[,.(unsuccessful=sum(table(perf_factor)[1:2]),all=sum(table(perf_factor)),ratio=sum(table(perf_factor)[1:2])/sum(table(perf_factor))), by=pop_size_factor]

# LIFETIME 
#(plot_srv_hazard_bar <- ggplot(mw[,.(longevity_count=.N),by=.(weeks_up_total)], aes(x=weeks_up_total, y=longevity_count)) + geom_bar(stat="identity", alpha=0.6) + theme_bw() + scale_y_log10("Count") + xlab("Longevity (weeks)") )
(plot_srv_hazard_point <- ggplot(mw[,.(longevity_count=.N),by=.(weeks_up_total)], aes(x=weeks_up_total, y=longevity_count)) + geom_point(stat="identity", alpha=0.6) + theme_bw() + scale_y_log10("Count") + xlab("Longevity (weeks)") )+theme_cowplot()
median( mw$weeks_up_total)
(plot_srv_hazard <- make_plot_size_by_success(mw, "weeks_up_total", gov_median, ggmore=scale_fill_gradient(high="#3182bd", low="#cccccc"), ggguide="none", reps=1000, ggrug=FALSE ) + full_data_rug + guides(fill="none") )
ggsave(plot_srv_hazard, file=paste0(pathImages, "plot_srv_hazard.png"), units='cm', width=3.25, height=2.5, scale=3)
ggsave(plot_srv_hazard_point, file=paste0(pathImages, "plot_srv_hazard_point.png"), units='cm', width=5, height=1.5, scale=3)

# UNIQUES VS RETURNS
plot_population_distribution <- plot_visitortype(mw, plot_type='horizontal')
plot_population_distribution 
#ggsave(plot_population_distribution, file=paste0(pathImages, "plot_population_distribution.png"), units='cm', width=5, height=2.5, scale=5)
ggsave(plot_population_distribution, file=paste0(pathImages, "plot_population_distribution_rect.png"), units='cm', width=3, height=2, scale=5)

### AGGREGATE PLOTS
ggsave(plot_grid( plot_srv_hazard, NULL, plot_population_distribution, plot_srv_density,  labels = c("A", "", "B", "C")), file=paste0(pathImages, "plot_survey.png"), units='cm')
