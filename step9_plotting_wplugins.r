### INITIALIZE GLOBALS
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_plotting.r"))

mw <- readRDS(paste0(pathData, "step6_servers_wide_govanalysis.rds"))

### PLOTTING DATA
# 2d binning plots are easier to interpret, and more equally faithful to the findings, when "bins" are defined to have population > 1. 
mw <- mw[bin_count>1]
### these two are for the marginal density plots
mw[,yrug:=(log2(ifelse(y>32, 32, y)+1)+1.0)*1.0+rnorm(nrow(.SD),sd=0.17)]
mw[,xrug:=(log2(srv_max+1)+1.0)*0.45+rnorm(nrow(.SD),sd=0.17)]
### PLOTTING DATA FOR PANELED PLOTS
mwfacets <- mw[,.(srv_addr, y, srv_max, srv_max_log, pop_size_factor, perf_factor, res_grief, res_ingame, res_realworld, governance_scope, governance_intensity, entropy_res, pct_grief, pct_ingame, pct_realworld, cat_chat, cat_informational, cat_economy, cat_admin, yrug, xrug)]
## this gives NAs if there are no res_ plguins in any category. so it goes.
#(plot_gov_res <- make_plot_size_by_success(mwfacets, "res_grief", gov_mean_narm , ggmore=ggel_yellow2red, ggguide="none", reps=0))
#(plot_gov_res <- make_plot_size_by_success(mwfacets, "pct_grief", gov_mean_narm , ggmore=ggel_gov_rat, ggguide="none", reps=0))
### institution by size:
mwres <- melt(mwfacets, id.vars = c("srv_addr", "y", "srv_max", "srv_max_log", "pop_size_factor", "perf_factor", "governance_scope", "governance_intensity"),  measure.vars = patterns("^res_", "^pct_"), variable.name = 'resourcegov', value.name='resourcegov_count', variable.factor=FALSE)
setnames(mwres, c('resourcegov_count1', 'resourcegov_count2'), c('resourcegov_count', 'resourcegov_pct'))
mwres[,resourcegov:=factor(resourcegov, levels=1:3, labels=c("Behavior", "Physical resources", "Virtual resources"))] ### patterns() erases old names with integers that have to be replaced with a leap of faith
mwinst <- melt(mwfacets, id.vars = c("srv_addr", "y", "srv_max", "srv_max_log", "pop_size_factor", "perf_factor", "governance_scope", "governance_intensity"),  measure.vars = patterns("^cat_"), variable.name = 'instgov', value.name='instgov_count', variable.factor=FALSE)
#mwinst[order(srv_addr)] 
setnames(mwinst, c('instgov_count1'), c('instgov_count'))
mwinst[,instgov:=factor(instgov, levels=c("cat_chat", "cat_informational", "cat_economy", "cat_admin"), labels=c("Communication", "Information","Exchange","Administrator"))] ### patterns() erases old names with integers that have to be replaced with a leap of faith

### figure sizes http://www.sciencemag.org/site/feature/contribinfo/prep/prep_revfigs.xhtml
###   1, 1.5, 2 cols = 5.8, 8.7, 11.6cm

# PLOTS:
full_data_rug <-  geom_rug(data=mw, mapping=aes(x=xrug, y=yrug), col=rgb(0.7,0.7,0.7,alpha=0.2),sides="tl")
### COLORSCHEMES
ggel_grey2green <- scale_fill_gradient2(high="#238b45", mid="#41ab5d", low="#cccccc", midpoint=4.5) 
ggel_grey2blue <- scale_fill_gradient2(high="#2171b5", mid="#3182bd", low="#cccccc", midpoint=4) 
ggel_yellow2red <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fdbb84", midpoint=2.5, breaks=seq(from=0,to=12,by=2)) 
ggel_reddishpurple <- scale_fill_gradient2(low="#fde0dd", mid="#fa9fb5", high="#c51b8a", midpoint=-0.5 )
### others
ggel_gov2 <- scale_fill_gradient2(low="#66bd63", mid="#f7fcb9", high="#fdbb84", midpoint=0.15) 
ggel_gov_prop <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=0.5, breaks=seq(from=0,to=1,by=0.2)) 
ggel_gov_rat <- scale_fill_gradient2(low="#006837", mid="#ffffbf", high="#fdae61", midpoint=0.05) 
ggel_gov_rat_within <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59") 
ggel_gov_by_type <- scale_fill_gradientn(colors=(seq_gradient_pal(low=muted("#91cf60", l=100, c=100), high=muted("#fc8d59", l=100, c=100)))(rescale(seq(from=0,to=10,by=2))), values=rescale(seq(from=0,to=10,by=2)^2)) 
ggel_govaud <- scale_fill_gradient2(low="#91cf60", mid="#f0f0f0", high="#fc8d59", midpoint=3 )
ggel_govaud2 <- scale_fill_gradient(low="#f0f0f0", high=muted("#fc8d59", l=80,c=100))



# RESULTS FROM THE GOV-RELATED 5000
### FIGURE 1: FOUR PANELS 1 row
### PANEL A
#  DENSITY (results only make sense in the interaction of success and server size)
(plot_srv_density <- make_plot_size_by_success(mw, "y", function(x,i) nrow(x[i]), ggmore=scale_fill_gradientn(colors=grey(seq(from=0.6,to=0.3,length.out=6)), values=rescale(c(0,4,16,64,256,1024)), breaks=c(0,4,16,64,256,1024)), ggtext="raw", ggguide=guide_legend("Server\ncount", reverse=TRUE), reps=0, ggrug=F) + full_data_rug + guides(fill="none") + theme(plot.title=element_text(face="plain")) + labs(title="Data density"))
ggsave(plot=plot_srv_density, device="png", path=pathImages, filename=paste0("plot_srv_density",".png"), units='cm', width=5.8, height=5.8, scale=1.5)
mw[,.(unsuccessful=sum(table(perf_factor)[1:2]),all=sum(table(perf_factor)),ratio=sum(table(perf_factor)[1:2])/sum(table(perf_factor))), by=pop_size_factor]
### PANEL B
# PLOT GOV INTENSITY
(plot_gov_scaling <- make_plot_size_by_success(mw, "governance_intensity", gov_mean_narm , ggmore=ggel_yellow2red , ggguide="none", reps=0, ggrug=F) + full_data_rug + labs(title="Rule count"))
ggsave(plot=plot_gov_scaling, device="png", path=pathImages, filename=paste0("plot_gov_scaling",".png"), units='cm', width=5.8, height=5.8, scale=1.5)
### PANEL C
###   PLOT REPERTOIRE
(plot_srv_institutional_diversity <- make_plot_size_by_success(mw, "rule_diversity", gov_mean_narm , ggmore=ggel_grey2green, ggguide="none", reps=0, ggrug=F) + full_data_rug + labs(title="Rule diversity"))
ggsave(plot=plot_srv_institutional_diversity, device="png", path=pathImages, filename=paste0("plot_srv_institutional_diversity",".png"), units='cm', width=5.8, height=5.8, scale=1.5)
### PANEL D
# PLOT GOV COMPLEXITY
(plot_gov_resourcediv <- make_plot_size_by_success(mw, "governance_scope", gov_mean , ggmore=ggel_grey2blue, ggguide="none", reps=0, ggrug=F) + full_data_rug + ggtitle("Rule scope"))
ggsave(plot_gov_resourcediv , file=paste0(pathImages, "plot_srv_institutional_diversity.png"), units='cm', width=4, height=2.5, scale=3)


### FIGURE 2: 4 plots in 2 panels , 1 row
### PANEL A 3x
#### DIVERSITY IN RULE TYPES (int erms of categories)
# show the 4 types of instituion changing count and also frequency 
#plot_gov_inst_ratio  <- list(4)
print(plot_instgov <- make_plot_size_by_success(mwinst, c("instgov_count"), gov_mean_narm, ggmore=ggel_grey2green, ggtext=TRUE, reps=0, ggguide="none", facetting=c("instgov"), ggrug=F) + full_data_rug + facet_wrap( ~ instgov, nrow=1)+ theme(strip.background=element_rect(color="white", fill="white"), strip.text=element_text(size=10)) + labs(title="Rule count, by type (diversity)"))
ggsave(plot=plot_instgov, device="png", path=pathImages, filename=paste0("plot_instgov",".png"), units='cm', width=11.6, height=5.8, scale=1.5)
### RESOURCE MANAGEMENT BY TYPE
### PANEL B
print(plot_resgov <- make_plot_size_by_success(mwres, c("resourcegov_count"), gov_mean_narm, ggmore=ggel_grey2blue, ggtext=TRUE, reps=0, ggguide="none", facetting=c("resourcegov"), ggrug=F) + full_data_rug + facet_wrap( ~ resourcegov, nrow=1)+ theme(strip.background=element_rect(color="white", fill="white"), strip.text=element_text(size=10)) + labs(title="Rule count, by resource managed (scope)"))
ggsave(plot=plot_resgov, device="png", path=pathImages, filename=paste0("plot_resgov",".png"), units='cm', width=11.6, height=5.8, scale=1.5)



### AGGREGATE PLOTS
### FIGURE 1
ggsave(plot=plot_grid(plot_srv_density, plot_gov_scaling, plot_srv_institutional_diversity, plot_gov_resourcediv, labels = c("A", "B", "C", "D"), hjust=-4.0, align="hv", axis="bl", nrow=1), device="png", path=pathImages, filename=paste0("plot_f1_feature",".png"), units='cm', width=11.6, height=3.8, scale=2.0)
ggsave(plot=plot_grid(plot_srv_density, plot_gov_scaling, plot_srv_institutional_diversity, plot_gov_resourcediv, labels = c("A", "B", "C", "D"), hjust=-4.0, align="hv", axis="bl", nrow=2), device="png", path=pathImages, filename=paste0("plot_f1_feature2",".png"), units='cm', width=8.7, height=8.7, scale=2.0)
### fIGURE 2 
ggsave(plot=plot_grid(plot_instgov, plot_resgov, labels = "AUTO", align = "h", hjust=-4.0, nrow=2), device="png", path=pathImages, filename=paste0("plot_ruletypes",".png"), units='cm', width=11.6, height=11.6, scale=1.3)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### SI
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# LIFETIME 
#(plot_srv_hazard_bar <- ggplot(mw[,.(longevity_count=.N),by=.(weeks_up_total)], aes(x=weeks_up_total, y=longevity_count)) + geom_bar(stat="identity", alpha=0.6) + theme_bw() + scale_y_log10("Count") + xlab("Longevity (weeks)") )
(plot_srv_hazard_point <- ggplot(mw[,.(longevity_count=.N),by=.(weeks_up_total)], aes(x=weeks_up_total, y=longevity_count)) + geom_point(stat="identity", alpha=0.6) + theme_bw() + scale_y_log10("Count") + xlab("Longevity (weeks)") )+theme_cowplot()
median( mw$weeks_up_total)
ggsave(plot=plot_srv_hazard_point, device="png", path=pathImages, filename=paste0("plot_srv_hazard_point",".png"), units='cm', width=5, height=1.5, scale=3)


### PLOT COLLECTIVE ACTION PROBLEM EMERGENCE (BETTER)
###  mud
# then make the same pot, witht e bottom row combined into one color coded RGB plot like in that poker paper
data_resgov_spectrum <- mw[(pct_grief + pct_ingame + pct_realworld) != 0,.(pct_grief=mean(pct_grief,na.rm=T),pct_ingame=mean(pct_ingame,na.rm=T),pct_realworld=mean(pct_realworld,na.rm=T)),by=.(perf_factor, pop_size_factor)]
expect_true( all( round( data_resgov_spectrum$pct_grief + data_resgov_spectrum$pct_ingame + data_resgov_spectrum$pct_realworld, 3) == 1)) ### sanity check (round to taking care of ugly floats)
data_resgov_spectrum[,color_res:=rgb(pct_grief, 0.450+pct_ingame*0.55, 0.450+pct_realworld*0.55, 1)]
color_res <- data_resgov_spectrum$color_res
names(color_res) <- color_res
#plot_resgov_spectrum<-ggplot(data_resgov_spectrum, aes(pop_size_factor, perf_factor)) + geom_bin2d(aes(fill=color_res)) + theme_bw() + theme(panel.grid.major=element_line(0), axis.text.y = element_text(angle = 45)) + coord_fixed(ratio=6/7) + scale_x_discrete("Server size", expand = c(0.035,0) ) +     scale_fill_manual(values = color_res )+ scale_y_discrete("Core members", expand = c(0.035,0)) + guides(fill="none"); plot_resgov_spectrum
(plot_resgov_spectrum <- make_plot_size_by_success(data_resgov_spectrum, c("color_res"), as.character, ggmore=scale_fill_manual(values = color_res ), ggtext=FALSE, reps=0, ggguide="none", ggrug=FALSE) + full_data_rug + labs(title="Resource diversity")) 
ggsave(plot=plot_resgov_spectrum, device="png", path=pathImages, filename=paste0("plot_resgov_spectrum",".png"), units='cm', width=4.25, height=2.5, scale=3)
