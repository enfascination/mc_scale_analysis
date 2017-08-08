### INITIALIZE GLOBALS
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_plotting.r"))

mw <- readRDS(paste0(pathData, "step6_servers_wide_govanalysis.rds"))

# PLOTS:
full_data_rug <-  geom_rug(data=mw, mapping=aes(x=xrug, y=yrug), col=rgb(0.7,0.7,0.7,alpha=0.2),sides="tl")
### COLORSCHEMES
ggel_lowbad <- scale_fill_gradient(high="#41ab5d", low="#cccccc") 
ggel <- scale_fill_gradient(high="#3182bd", low="#cccccc") 
ggel_gov <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=2.5, breaks=seq(from=0,to=12,by=2)) 
ggel_gov2 <- scale_fill_gradient2(low="#006837", mid="#ffffbf", high="#fdae61", midpoint=0.15) 
ggel_gov_prop <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=0.5, breaks=seq(from=0,to=1,by=0.2)) 
ggel_gov_rat <- scale_fill_gradient2(low="#006837", mid="#ffffbf", high="#fdae61", midpoint=0.05) 
ggel_gov_rat_within <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59") 
ggel_gov_by_type <- scale_fill_gradientn(colors=(seq_gradient_pal(low=muted("#91cf60", l=100, c=100), high=muted("#fc8d59", l=100, c=100)))(rescale(seq(from=0,to=10,by=2))), values=rescale(seq(from=0,to=10,by=2)^2)) 
ggel_govaud <- scale_fill_gradient2(low="#91cf60", mid="#f0f0f0", high="#fc8d59", midpoint=3 )
ggel_govaud2 <- scale_fill_gradient(low="#f0f0f0", high=muted("#fc8d59", l=80,c=100))

# RESULTS FROM THE GOV-RELATED 5000
### PLOT COLLECTIVE ACTION PROBLEM EMERGENCE
# make 2x3 plot: 
# show the three types of resource problems changing count and also frequency 
for (i in 1:3){
	print(plot_gov_resources <- make_plot_size_by_success(mw, c("res_grief", "res_ingame","res_realworld")[i], gov_mean, ggmore=ggel_gov, ggguide=guide_legend("Mean plugin count", reverse=TRUE), reps=100))
	print(plot_gov_resources_ratio <- make_plot_size_by_success(mw, c("res_grief", "res_ingame","res_realworld"), gov_mean_proportion_2, focal=i, ggmore=ggel_gov_rat, ggguide=guide_legend("Ratio\ngovernance", reverse=TRUE), reps=100))
}
print(plot_gov_resources_ratio <- make_plot_size_by_success(mw, c("entropy_res"), gov_mean_narm, ggmore=ggel_lowbad, ggguide=guide_legend("Randge of things being governed"), reps=100))

### NOW REDO IN ONE SPECIALIST DATATABLE
mwres <- mw[,.(srv_addr, srv_max, pop_size_factor, perf_factor, res_grief, res_ingame, res_realworld, total_res, entropy_res, pct_grief, pct_ingame, pct_realworld)]
## this gives NAs if there are no res_ plguins in any category. so it goes.
#(plot_gov_res <- make_plot_size_by_success(mwres, "res_grief", gov_mean_narm , ggmore=ggel_gov, ggguide="none", reps=1000))
#(plot_gov_res <- make_plot_size_by_success(mwres, "pct_grief", gov_mean_narm , ggmore=ggel_gov_rat, ggguide="none", reps=1000))
### institution by size:
gg <- melt(mwres, id.vars = c("srv_addr", "srv_max", "pop_size_factor", "perf_factor", "total_res"),  measure.vars = patterns("^res_", "^pct_"), variable.name = 'resourcegov', value.name='resourcegov_count', variable.factor=FALSE)
setnames(gg, c('resourcegov_count1', 'resourcegov_count2'), c('resourcegov_count', 'resourcegov_pct'))
gginclude <- c("res_grief", "res_ingame", "res_realworld")
#gg <- gg[resourcegov %in% gginclude]
gg[,resourcegov:=factor(resourcegov, levels=1:3, labels=c("Antisociality", "Physical resources", "Virtual resources"))] ### patterns() erases old names with integers that have to be replaced with a leap of faith
print(plot_resgov <- make_plot_size_by_success(gg, c("resourcegov_count"), gov_mean_narm, ggmore=ggel_gov2, ggtext=TRUE, reps=10, ggguide="none", facetting=c("resourcegov")) + facet_wrap( ~ resourcegov, nrow=1)+ theme(strip.background=element_rect(color="white", fill="white"), axis.text=element_text(size=6)))
print(plot_resgov_pct <- make_plot_size_by_success(gg, c("resourcegov_pct"), gov_mean_narm , ggmore=ggel_gov_rat, ggtext="%", reps=0, ggguide="none", facetting=c("resourcegov")) + facet_wrap( ~ resourcegov, nrow=1)+ theme(strip.background=element_rect(color="white", fill="white"), axis.text=element_text(size=6)))
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
# plot the four "complexity"results together: intensity, consolidation, specialization, diversity within 
# PLOT GOV INTENSITY
(plot_gov_scaling <- make_plot_size_by_success(mw, "total_res", gov_median , ggmore=ggel_gov, ggguide="none", reps=1000) + ggtitle("Intensity"))
ggsave(plot_gov_scaling, file=paste0(pathImages, "plot_gov_scaling.png"), units='cm', width=3.25, height=2.5, scale=3)
# PLOT GOV SPECIALIZATION
(plot_gov_specialization <- make_plot_size_by_success(mw, "plugin_specialization", gov_mean_narm , ggmore=ggel_gov, ggguide="none", reps=1000) + ggtitle("Specialization"))
ggsave(plot_gov_specialization, file=paste0(pathImages, "plot_gov_specialization.png"), units='cm', width=3.25, height=2.5, scale=3)
# PLOT GOV COMPLEXITY
(plot_srv_institutional_diversity <- make_plot_size_by_success(mw, "srv_entropy", gov_mean_narm, ggmore=ggel_lowbad, ggguide="none", reps=10, ggrug=FALSE) + full_data_rug + ggtitle("Rule diversity"))
ggsave(plot_srv_institutional_diversity, file=paste0(pathImages, "plot_srv_institutional_diversity.png"), units='cm', width=4, height=2.5, scale=3)
# PLOT GOV CONSOLIDATION
(plot_gov_consolidation <- make_plot_size_by_success(mw[,.(perf_factor, pop_size_factor, pop_size_factor, aud_users, aud_admin, aud_total=aud_users+aud_admin+aud_none)], c("aud_admin","aud_total"), gov_mean_proportion_1, ggmore=ggel_govaud2, reps=100, ggtext="%", ggguide="none", ggrug=FALSE) + full_data_rug + ggtitle("Consolidation (%)") )
ggsave(plot_gov_consolidation, file=paste0(pathImages, "plot_gov_consolidation.png"), units='cm', width=4, height=2.5, scale=3)


# PLOT DIVERSITY ACROSS SERVERS
### server diversity
### bootstrapping fucntion for entropy
(plot_srv_gov_diversity <- (make_plot_size_by_success(mw, grep("^inst_[^n]", names(mw), value=TRUE), gov_dist, ggguide="none", ggmore=ggel_lowbad, reps=10, ggtext=FALSE, ggrug=FALSE) + full_data_rug + ggtitle("Community diversity")))
ggsave(plot_srv_gov_diversity, file=paste0(pathImages, "plot_srv_gov_diversity.png"), units='cm', width=4, height=2.5, scale=3)

### AGGREGATE PLOTS
ggsave(plot_grid(plot_gov_scaling, plot_gov_specialization, plot_srv_institutional_diversity, plot_gov_consolidation,  labels = c("A", "B", "C", "D"), align = "h", nrow=1), file=paste0(pathImages, "plot_development.png"), units='cm')
