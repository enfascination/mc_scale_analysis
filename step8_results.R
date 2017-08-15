
### initialize globals
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_step6_analysis.r"))
source(paste0(pathLocal,"lib_plotting.r"))
library(boot)
library(ggthemes)
library(scales)
mw <- readRDS(paste0(pathData, "step6_servers_wide_govanalysis.rds"))
mw_train <- mw


### mere data density
(plot_srv_density <- make_plot_size_by_success(mw_train, "weeks_up_total", function(x,i) nrow(x[i]), ggmore=scale_fill_gradientn(colors=grey(seq(from=0.6,to=0.3,length.out=6)), values=rescale(c(0,4,16,64,256,1024)), breaks=c(0,4,16,64,256,1024)), ggguide=guide_legend("Server\ncount", reverse=TRUE), reps=10))
plot_srv_density <- plot_srv_density + guides(fill="none")
ggsave(plot_srv_density, file=paste0(pathImages, "plot_srv_density.png"), units='cm', width=3.25, height=2.5, scale=3)
# plot hazard  log and linear
(plot_srv_hazard_bar1 <- ggplot(mw_train[,.(longevity_count=.N),by=.(weeks_up_total)], aes(x=weeks_up_total, y=longevity_count)) + geom_bar(stat="identity") + theme_bw() + scale_y_log10("Count") + xlab("Longevity (weeks)") )
median( mw_train$weeks_up_total)
(plot_srv_hazard_bar2 <- ggplot(mw_train[,.(longevity_count=.N),by=.(weeks_up_total, pop_size_factor)], aes(x=weeks_up_total, y=longevity_count, fill=pop_size_factor )) + geom_bar(stat="identity", position="dodge") + theme_bw() + scale_y_continuous("Count") + xlab("Longevity (weeks)") )
(plot_srv_hazard <- make_plot_size_by_success(mw_train, "weeks_up_total", gov_median, ggmore=scale_fill_gradient(high="#3182bd", low="#cccccc"), ggguide="none", reps=1000 ) )
ggsave(plot_srv_hazard, file=paste0(pathImages, "plot_srv_hazard.png"), units='cm', width=3.25, height=2.5, scale=3)
ggsave(plot_srv_hazard_bar1, file=paste0(pathImages, "plot_srv_hazard_bar1.png"), units='cm', width=5, height=1.5, scale=3)
#plot increase in grief:
### gov going up or down
### governance against size against community
ggel <- scale_fill_gradient(high="#3182bd", low="#cccccc") 
ggel_gov <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=2.5, breaks=seq(from=0,to=12,by=2)) 
ggel_gov_prop <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=0.5, breaks=seq(from=0,to=1,by=0.2)) 
ggel_gov_rat <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=0.10) 
ggel_gov_rat_within <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59") 
(plot_gov_scaling <- make_plot_size_by_success(mw_train, "gov", gov_mean , ggmore=ggel_gov, ggguide="none", reps=1000))
(plot_gov_specialization <- make_plot_size_by_success(mw_train, "plugin_specialization", gov_mean_narm , ggmore=ggel_gov, ggguide="none", reps=1000))
(plot_gov_scaling_ratio <- make_plot_size_by_success(mw_train, c("gov","plugin_count"), gov_median_proportion_1, ggmore=ggel_gov_rat, ggguide=guide_legend("Ratio\ngovernance", reverse=TRUE), reps=100))
(plot_gov_scaling_ratio_antigrief <- make_plot_size_by_success(mw_train, c("res_grief","sum_resource"), gov_median_proportion_1_narm, ggmore=ggel_gov_rat, ggguide=guide_legend("Ratio\ngovernance", reverse=TRUE), reps=100))
ggsave(plot_gov_scaling, file=paste0(pathImages, "plot_gov_scaling.png"), units='cm', width=3.25, height=2.5, scale=3)
ggsave(plot_gov_specialization, file=paste0(pathImages, "plot_gov_specialization.png"), units='cm', width=3.25, height=2.5, scale=3)

### resource managemanet style by size:
(plot_gov_scaling_by_plugin_category <- make_plot_size_by_success(melt(mw_train[gov>0], id.vars = c("srv_addr", "y", "srv_max", "pop_size_factor", "pop_size_factor_coarse", "perf_factor", "perf_factor_ratio", "sum_resource"),  measure.vars = c(grep("^cat_", names(mw_train), value=TRUE)), variable.name = 'resource', value.name='resource_count'), c("resource_count"), gov_mean , ggmore=ggel_gov, ggguide=guide_legend("Governance\nplugins", reverse=TRUE), reps=10, facetting=c("resource")) + facet_wrap( ~ resource, ncol=4)+ theme(strip.background=element_rect(color="white", fill="white")))
#(plot_gov_scaling_by_resource_type_across_proportion <- make_plot_size_by_success(melt(mw_train, id.vars = c("srv_addr", "y", "srv_max", "pop_size_factor", "pop_size_factor_coarse", "perf_factor", "perf_factor_ratio", "sum_resource"),  measure.vars = c("gov", "res_grief", "res_ingame", "res_realworld", "res_players"), variable.name = 'resource', value.name='resource_count', variable.factor=FALSE), c("resource_count", "sum_resource"), gov_median_proportion_1 , ggmore=ggel_gov_rat, ggguide=guide_legend("% governance\nplugins", reverse=TRUE), reps=100, facetting=c("resource")) + facet_wrap( ~ resource, ncol=2)+ theme(strip.background=element_rect(color="white", fill="white")))
ggel_gov_by_type <- scale_fill_gradientn(colors=(seq_gradient_pal(low=muted("#91cf60", l=100, c=100), high=muted("#fc8d59", l=100, c=100)))(rescale(seq(from=0,to=10,by=2))), values=rescale(seq(from=0,to=10,by=2)^2)) 
#scale_fill_gradientn(colors=grey(seq(from=0.6,to=0.3,length.out=6)), values=rescale(c(0,4,16,64,256,1024)), breaks=c(0,4,16,64,256,1024))
{
    gg <- melt(mw_train[gov>0], id.vars = c("srv_addr", "y", "srv_max", "pop_size_factor", "pop_size_factor_coarse", "perf_factor", "perf_factor_ratio", "sum_resource"),  measure.vars = c("res_grief", "res_ingame", "res_realworld", "res_players"), variable.name = 'resource', value.name='resource_count', variable.factor=FALSE)
    gg[,resource:=factor(resource, levels=c("res_grief", "res_ingame", "res_realworld", "res_players", "res_attention"), labels=c("Grief", "In-game", "Real-world", "Player community", "Mod cognitive"))]
    (plot_gov_scaling_by_resource_type <- make_plot_size_by_success(gg, c("resource_count"), gov_median , ggmore=ggel_gov_by_type, ggguide="none", reps=1000, facetting=c("resource")) + facet_wrap( ~ resource, ncol=1)+ theme(strip.background=element_rect(color="white", fill="white"), axis.text=element_text(size=6)))
    ggsave(plot_gov_scaling_by_resource_type, file=paste0(pathImages, "plot_gov_scaling_by_resource_type.png"), units='cm', width=2.25, height=5, scale=3)
    (plot_antigrief_scaling <- make_plot_size_by_success(gg[resource=="Grief"], c("resource_count"), gov_median , ggmore=ggel_gov_by_type, ggguide="none", reps=1000) )
    (plot_antigrief_ratio_scaling <- make_plot_size_by_success(gg, c("resource_count"), gov_median , ggmore=ggel_gov_by_type, ggguide="none", reps=1000) )
(plot_gov_scaling_by_aud_type2 <- make_plot_size_by_success(mw_train[,.(perf_factor, pop_size_factor, pop_size_factor, ratio_aud)], c("ratio_aud"), gov_mean, ggmore=ggel_govaud2, ggguide=guide_legend("Ratio\ngovernance", reverse=TRUE), reps=100, ggtext=FALSE))
    ggsave(plot_antigrief_scaling, file=paste0(pathImages, "plot_antigrief_scaling.png"), units='cm', width=3.25, height=2.5, scale=3)
}
### institution by size:
{
    gg <- melt(mw_train[gov>0], id.vars = c("srv_addr", "y", "srv_max", "pop_size_factor", "pop_size_factor_coarse", "perf_factor", "perf_factor_ratio", "sum_institution"),  measure.vars = c("gov", grep("^inst_", names(mw_train), value=TRUE)), variable.name = 'institution', value.name='institution_count', variable.factor=FALSE)
    gginclude <- c("inst_action_space_down", "inst_chat", "inst_privateproperty", "inst_shop")
    gg <- gg[institution %in% gginclude]
    gg[,institution:=factor(institution, levels=gginclude, labels=c("Proscriptions", "Chat", "Property", "Exchange"))]
    (plot_gov_scaling_by_inst_type <- make_plot_size_by_success(gg, c("institution_count"), gov_median , ggmore=ggel_gov_by_type, ggguide="none", reps=1000, facetting=c("institution")) + facet_wrap( ~ institution, ncol=1)+ theme(strip.background=element_rect(color="white", fill="white"), axis.text=element_text(size=6)))
    (plot_actiondown_scaling <- make_plot_size_by_success(gg[institution == "Proscriptions"], c("institution_count"), gov_median , ggmore=ggel_gov_by_type, ggguide="none", reps=1000) )
    ggsave(plot_actiondown_scaling, file=paste0(pathImages, "plot_actiondown_scaling.png"), units='cm', width=3.25, height=2.5, scale=3)
}
### institution by size as a fraction of total institutions
#(plot_gov_scaling_by_inst_type_proportion <- make_plot_size_by_success(melt(mw_train, id.vars = c("srv_addr", "y", "srv_max", "pop_size_factor", "pop_size_factor_coarse", "perf_factor", "perf_factor_ratio", "sum_institution"),  measure.vars = c("gov", grep("^inst_", names(mw_train), value=TRUE)), variable.name = 'institution', value.name='institution_count'), c("institution_count","sum_institution"), gov_median_proportion_1 , ggmore=ggel_gov_rat, ggguide=guide_legend("% governance\nplugins", reverse=TRUE), reps=0, facetting=c("institution")) + facet_wrap( ~ institution, ncol=4)+ theme(strip.background=element_rect(color="white", fill="white")))
### institution by size as a fraction of within that type of institution
###   but this ultimately gives less info that the original clacuclation, and less valuable, so back to original/
#dataa <- make_plot_size_by_success(melt(mw_train, id.vars = c("srv_addr", "y", "srv_max", "pop_size_factor", "pop_size_factor_coarse", "perf_factor", "perf_factor_ratio", "sum_institution"),  measure.vars = c("gov", grep("^inst_", names(mw_train), value=TRUE)), variable.name = 'institution', value.name='institution_count'), c("institution_count","sum_institution"), gov_mean_proportion_1, reps=0, facetting=c("institution"), return_plot=FALSE)[,pop_var:=pop_var/sum(pop_var),by=institution]
#(plot_gov_scaling_by_inst_type_proportion <- ggplot(dataa[,.(xvar=pop_size_factor_coarse, yvar=perf_factor,institution, pop_var)], aes(x=xvar, y=yvar)) + scale_y_discrete("Core members", labels=c("0", "", "", "10", "", "", "100")) + geom_bin2d(aes(fill=pop_var)) + theme_bw() + theme(panel.grid.major=element_line(0)) + coord_fixed(ratio=6/7) + scale_x_discrete("Server size", labels=c(5,10,50,100,500,1000)) + guides(fill=guide_legend("% governance\nplugins", reverse=TRUE)) + ggel_gov_rat_within + facet_wrap( ~ institution, ncol=4)+ theme(strip.background=element_rect(color="white", fill="white")))
### governance audience
ggel_govaud <- scale_fill_gradient2(low="#91cf60", mid="#f0f0f0", high="#fc8d59", midpoint=3 )
(plot_gov_scaling_by_aud_type <- make_plot_size_by_success(melt(mw_train, id.vars = c("srv_addr", "y", "srv_max", "pop_size_factor", "pop_size_factor_coarse", "perf_factor", "perf_factor_ratio"),  measure.vars = c(grep("^aud_[^n]", names(mw_train), value=TRUE)), variable.name = 'audience', value.name='audience_count'), "audience_count", gov_mean , ggmore=ggel_govaud, ggguide=guide_legend("Governance\nplugins", reverse=TRUE), reps=0, facetting=c("audience")) + facet_wrap( ~ audience, ncol=4)+ theme(strip.background=element_rect(color="white", fill="white")))
ggel_govaud2 <- scale_fill_gradient(low="#f0f0f0", high=muted("#fc8d59", l=80,c=100))
(plot_gov_scaling_by_aud_type2 <- make_plot_size_by_success(mw_train[,.(perf_factor, pop_size_factor, pop_size_factor, ratio_aud)], c("ratio_aud"), gov_mean, ggmore=ggel_govaud2, ggguide=guide_legend("Ratio\ngovernance", reverse=TRUE), reps=100, ggtext=FALSE))
#ggsave(plot_gov_scaling_by_aud_type2, file=paste0(pathImages, "plot_gov_scaling_by_aud_type2.png"), units='cm', width=4, height=2.5, scale=3)
ggsave(plot_gov_scaling_by_aud_type, file=paste0(pathImages, "plot_gov_scaling_by_aud_type.png"), units='cm', width=4, height=2.5, scale=3)

## uniques vs core members
plot_population_distribution_rect <- plot_visitortype(mw, plot_type='vertical')
#ggsave(plot_population_distribution, file=paste0(pathImages, "plot_population_distribution.png"), units='cm', width=5, height=2.5, scale=5)
ggsave(plot_population_distribution_rect, file=paste0(pathImages, "plot_population_distribution_rect.png"), units='cm', width=2, height=3, scale=5)
ggsave(plot_population_distribution_rect, file=paste0(pathImages, "plot_population_distribution_rect2.png"), units='cm', width=3, height=2, scale=5)

### now plot uniques against size and success
(make_plot_size_by_success(mw_train, "nuvisits12", function(x,i) log2(gov_median(x, i)), ggmore=scale_fill_gradient(low="#d9d9d9", high="#525252"), ggguide=guide_legend("Unique visits", reverse=TRUE), reps=10))
(plot_srv_density_uvisits <- make_plot_size_by_success(mw_train, "nuvisits12", function(x,i) gov_median(x, i), ggmore=scale_fill_gradientn(colors=grey(seq(from=0.6,to=0.3,length.out=6)), values=rescale(c(0,4,16,64,256,1024)^2), breaks=c(0,4,16,64,256,1024)), ggguide=guide_legend("Unique visits", reverse=TRUE), reps=10))
mw_train[,.(unsuccessful=sum(table(perf_factor)[1:2]),all=sum(table(perf_factor)),ratio=sum(table(perf_factor)[1:2])/sum(table(perf_factor))), by=pop_size_factor]
ggsave(plot_srv_density_uvisits, file=paste0(pathImages, "plot_srv_density_uvisits.png"), units='cm', width=3.25, height=2.5, scale=3)

### server diversity
### bootstrapping fucntion for entropy
ggel_lowbad <- scale_fill_gradient(high="#41ab5d", low="#cccccc") 
#(make_plot_size_by_success(mw_train, grep("^inst_[^n]", names(mw_train), value=TRUE), gov_entropy_diversity, ggguide=guide_legend("Entropy"), ggmore=ggel_lowbad, reps=10))
(plot_srv_institutional_diversity <- (make_plot_size_by_success(mw_train, grep("^inst_[^n]", names(mw_train), value=TRUE), gov_dist, ggguide=guide_legend("Variability"), ggmore=ggel_lowbad, reps=10, ggtext=FALSE)))
ggsave(plot_srv_institutional_diversity, file=paste0(pathImages, "plot_srv_institutional_diversity.png"), units='cm', width=4, height=2.5, scale=3)
#ggsave(plot_srv_institutional_diversity, file=paste0(pathImages, "plot_srv_institutional_diversity_entropy.png"), units='cm', width=4, height=2.5, scale=3)

### within-server diversity
(make_plot_size_by_success(mw_train, "srv_entropy", gov_median, ggmore=ggel_lowbad, ggguide=guide_legend("Pluralism", reverse=TRUE), reps=10))
### uptime %?
### number of weeks up (longevity)
ggel_longevity <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=15) 
(make_plot_size_by_success(mw_train, "weeks_up_total", gov_median, ggmore=ggel_longevity, ggguide=guide_legend("Longevity", reverse=TRUE), reps=1000))
### number of signs (informal governance)
ggel_signs <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59") 
(make_plot_size_by_success(mw_train, "sign_count", function(x,i) median(as.double(asdf(x[i][!is.na(sign_count)])[,1])), ggmore=ggel_signs, ggguide=guide_legend("Norms", reverse=TRUE), reps=0))
### maintenance style
ggel_maint <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59") 
gov_features <- grep("^use_[^n]", names(mw_train), value=TRUE)
for (i in 1:length(gov_features)){
    print(make_plot_size_by_success(mw_train, gov_features, gov_median_proportion_2, ggguide=guide_legend(paste0(gov_features[i])), ggmore=ggel_maint, reps=5, focal=i))
}
ggel_govmaint <- scale_fill_gradient2(low="#91cf60", mid="#f0f0f0", high="#fc8d59", midpoint=3 )
(plot_gov_scaling_by_maint_type <- make_plot_size_by_success(melt(mw_train, id.vars = c("srv_addr", "y", "srv_max", "pop_size_factor", "pop_size_factor_coarse", "perf_factor", "perf_factor_ratio"),  measure.vars = c(grep("^use_[^n]", names(mw_train), value=TRUE)), variable.name = 'maintain', value.name='maintain_count'), "maintain_count", gov_median , ggmore=ggel_govmaint, ggguide=guide_legend("Governance\nplugins", reverse=TRUE), reps=0, facetting=c("maintain")) + facet_wrap( ~ maintain, ncol=4)+ theme(strip.background=element_rect(color="white", fill="white")))
### jubilees
ggel_v <- scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=1) 
(make_plot_size_by_success(mw_train, "jubilees", gov_median, ggmore=ggel_v, ggguide=guide_legend("Updates", reverse=TRUE), reps=1000) )
ggplot(mw_train, aes(x=weeks_up_total, y=jubilees)) + geom_jitter(height=0.5, width=0)  ### the patern above occurs even though longevity and jubilees are psoitively correlated without controlling for size
### audience  
### cowplot merge



#plot increase in grief:
### fix pred_hist plotting of histograms with fake data
pred_hist <- mc
#pred_hist_fake1 <- pred_hist[srv_max>200 & srv_max<400 & resource=="players", ]
#pred_hist_fake1[,':='(resource='performance')]
#pred_hist_fake2 <- pred_hist[srv_max>200 & srv_max<400 & resource=="players", ]
#pred_hist_fake2[,':='(resource='realmoney')]
#pred_hist <- rbind(pred_hist, pred_hist_fake1, pred_hist_fake2)
pred_hist[ ,':='(
        institution_name={ifelse( gov==1 , "Other gov", "Misc") %>%
                   #ifelse( gov==1 & institution %in% c("noinstitution", "monitor", "action_space"), "Misc", '') %>%
                   ifelse( gov==1 & institution == "boundary", "Entry restrictions", .) %>%
                   ifelse( gov==1 & institution == "action_space_up", "More player actions", .) %>%
                   ifelse( gov==1 & institution == "action_space_down", "Fewer player actions", .) %>%
                   ifelse( gov==1 & institution == "shop", "Economy", .) %>%
                   ifelse( gov==1 & institution == "chat", "Communication", .) %>%
                   ifelse( gov==1 & institution == "privateproperty", "Private property", .) %>%
                   ifelse( gov==1 & institution == "broadcast", "Admin broadcast", .) %>%
                   ifelse( gov==1 & institution == "monitor_by_peer", "Peer monitoring", .) %>%
                   ifelse( gov==1 & institution == "monitor_by_admin", "Admin monitoring", .) %>%
                   ifelse( gov==1 & institution == "position_v", "More groups, vertical", .) %>%
                   ifelse( gov==1 & institution == "position_h", "More groups, horizontal", .) %>%
                   ifelse( gov==1 & institution == "payoff", "Incentives", .) %>%
                   factor(levels=c( "Communication", "Private property", "Economy", "More player actions", "Entry restrictions", "Fewer player actions", "Admin broadcast", "Peer monitoring", "Admin monitoring", "More groups, vertical", "More groups, horizontal", "Other gov", "Misc"))
                   }, 
                resource_name={
                   ifelse( gov==1 & resource == "noresource", "Not resource-related", "Not resource-related") %>%
                   ifelse( gov==1 & resource == "grief", "Anti-grief", .) %>%
                   ifelse( gov==1 & resource == "ingame", "Game-related\nresources", .) %>%
                   ifelse( gov==1 & resource == "performance", "Server performance", .) %>%
                   ifelse( gov==1 & resource == "players", "Player community", .) %>%
                   ifelse( gov==1 & resource == "realmoney", "Server provisioning", .) %>%
                   factor(levels=c( "Anti-grief", "Game-related\nresources", "Server performance", "Server provisioning", "Player community", "Not resource-related"))
                   },
                gov_factor=factor(gov, levels=c(1,0), labels=c("Governance-related", "Game-related"))
                ) ]
xaxis_size_factor <- scale_x_discrete("Server size", labels=c("(0,5]", "(5,10]", "(10, 50]", "(50,100]", "(100, 500]", "(500, 1000]"))
### Each online community can be seen as a bundle of collective action problems. Larger servers are more likely to have to install governance modules that mitigate such problems.  among 4000 plugins on 1300 active servers, large servers are more likely to face problems with server performance (CPU/RAM/lag), server provisioning (paying server fees), and maintaining the player community (aiding and coordinating community members).
plot_color1 <- scale_fill_brewer("Resource type", type="qual",palette=1)
plot_color2 <- scale_fill_manual("Resource type", values=c("#666666", "#bf5b17", "#ffff99"))  ### for consistentcy.  see http://colorbrewer2.org/#type=qualitative&scheme=Accent&n=6
(plot_resource_types_1 <- ggplot(pred_hist[resource %ni% c("grief", "ingame"),], aes(x=srv_max, fill=resource_name)) + geom_histogram(position="fill", breaks=c(0,0.7,1,1.7,2,2.7,3), closed='right')+ scale_x_log10("Server size", breaks=c(0,1,5,10,50,100,500,1000),limits=c(1,1000))+ scale_y_continuous("Plugin proportions by type") + plot_color1 + theme_bw() + theme(aspect.ratio=0.6, plot.margin = unit(c(0,0,0,0), "cm")) + geom_vline(xintercept=c(1,10,100,1000), alpha=0.3))
(plot_resource_types_2 <- ggplot(pred_hist[gov== 0 | resource %in% c("grief", "ingame"),], aes(x=srv_max, fill=resource_name)) + geom_histogram(position="fill", breaks=c(0,0.7,1,1.7,2,2.7,3), closed='right')+ scale_x_log10("Server size", breaks=c(0,1,10,100,1000),limits=c(1,1000))+ scale_y_continuous("Plugin proportions by type") + plot_color2 + theme_bw() + theme(aspect.ratio=0.6, plot.margin = unit(c(0,0,0,0), "cm")) + geom_vline(xintercept=c(1,10,100,1000), alpha=0.3))
(plot_resource_types_x <- ggplot(pred_hist, aes(x=srv_max, fill=resource_name)) + geom_histogram(position="fill", breaks=c(0,0.7,1,1.7,2,2.7,3), closed='right')+ scale_x_log10("Server size", breaks=c(0,1,10,100,1000),limits=c(1,1000))+ scale_y_continuous("Plugin proportions by type") + scale_fill_hue() + theme_bw() + theme(aspect.ratio=0.6, plot.margin = unit(c(0,0,0,0), "cm")) + geom_vline(xintercept=c(1,10,100,1000), alpha=0.3))
(plot_resource_types_abs_x <- ggplot(pred_hist, aes(x=srv_max, fill=resource_name)) + geom_histogram(position="dodge", bins=6, binwidth=0.5)+ scale_x_log10("Server size", breaks=c(0,1,10,100,1000),limits=c(1,1000))+ scale_y_continuous("Plugin proportions by type") + scale_fill_hue() + theme_bw() + theme(aspect.ratio=0.6, plot.margin = unit(c(0,0,0,0), "cm")) + geom_vline(xintercept=c(1,3.1,10,31,100,310,1000), alpha=0.3))

ggsave(plot_resource_types_1, file=paste0(pathImages, "plot_resource_types_1.png"), units='cm', width=2.25, height=1, scale=6)
ggsave(plot_resource_types_2, file=paste0(pathImages, "plot_resource_types_2.png"), units='cm', width=2.25, height=1, scale=6)


plot_color1 <- scale_fill_manual("Institution type", values=c(rainbow(4, start=15/540, end=105/540, s=0.8, v=0.9 ), 'grey50'))
plot_color2 <- scale_fill_manual("Institution type", values=c(rainbow(4, start=200/540, end=360/540, s=0.8, v=0.9 ), 'grey50'))
plot_color3 <- scale_fill_manual("Institution type", values=c(rainbow(3, start=240/360, end=360/360, s=0.8, v=0.9 ), 'grey50'))
filter1 <- c("monitor_by_admin", "position_v", "action_space_down", "broadcast")
filter2 <- c("monitor_by_peer","position_h",  "privateproperty","action_space_up")
filter3 <- c("boundary","chat","shop" )
(plot_institution_types_1 <- ggplot(pred_hist[gov == 1 & (institution_name == "Other gov" | institution %in% filter1) ], aes(x=srv_max, fill=institution_name)) + geom_histogram(position="fill", breaks=c(0,0.7,1,1.7,2,2.7,3), closed='right')+ scale_x_log10("Server size", breaks=c(0,1,10,100,1000),limits=c(1,1000))+ scale_y_continuous("Plugin proportions by type") + plot_color1 + theme_bw() + theme(aspect.ratio=0.6, plot.margin = unit(c(0,0,0,0), "cm")) + geom_vline(xintercept=c(1,10,100,1000), alpha=0.3))
(plot_institution_types_2 <- ggplot(pred_hist[gov == 1 & (institution_name == "Other gov" | institution %in% filter2) ], aes(x=srv_max, fill=institution_name)) + geom_histogram(position="fill", breaks=c(0,0.7,1,1.7,2,2.7,3), closed='right')+ scale_x_log10("Server size", breaks=c(0,1,10,100,1000),limits=c(1,1000))+ scale_y_continuous("Plugin proportions by type") + plot_color2 + theme_bw() + theme(aspect.ratio=0.6, plot.margin = unit(c(0,0,0,0), "cm")) + geom_vline(xintercept=c(1,10,100,1000), alpha=0.3))
plot_institution_types_3 <- ggplot(pred_hist[gov == 1 & (institution_name == "Other gov" | institution %in% filter3) ], aes(x=srv_max, fill=institution_name)) + geom_histogram(position="fill", breaks=c(0,0.7,1,1.7,2,2.7,3), closed='right')+ scale_x_log10("Server size", breaks=c(0,1,10,100,1000),limits=c(1,1000))+ scale_y_continuous("Plugin proportions by type") + plot_color3 + theme_bw() + theme(aspect.ratio=0.6, plot.margin = unit(c(0,0,0,0), "cm")) + geom_vline(xintercept=c(1,10,100,1000), alpha=0.3); plot_institution_types_3
(plot_institution_types_x <- ggplot(pred_hist[gov == 1], aes(x=srv_max, fill=institution_name)) + geom_histogram(position="fill", breaks=c(0,0.7,1,1.7,2,2.7,3), closed='right')+ scale_x_log10("Server size", breaks=c(0,1,10,100,1000),limits=c(1,1000))+ scale_y_continuous("Plugin proportions by type") + scale_fill_hue("Institution type") + theme_bw() + theme(aspect.ratio=0.6, plot.margin = unit(c(0,0,0,0), "cm")) + geom_vline(xintercept=c(1,10,100,1000), alpha=0.3))
ggsave(plot_institution_types_1, file=paste0(pathImages, "plot_institution_types_1.png"), units='cm', width=2.25, height=1, scale=6)
ggsave(plot_institution_types_2, file=paste0(pathImages, "plot_institution_types_2.png"), units='cm', width=2.25, height=1, scale=6)
ggsave(plot_institution_types_3, file=paste0(pathImages, "plot_institution_types_3.png"), units='cm', width=2.25, height=1, scale=6)

### gov going up or down
(plot_gov_count <- ggplot(mw_train, aes(x=srv_max, y=(gov+1))) + geom_jitter(height=0.4, width=0.05, color="dark grey", size=0.5) + scale_x_log10("Server size", breaks=c(0,1,10,100,1000),limits=c(1,1000))+ scale_y_log10("Governance plugins") + plot_color1 + theme_bw() + theme(aspect.ratio=0.6, plot.margin = unit(c(0,0,0,0), "cm")) + geom_vline(xintercept=c(1,10,100,1000), alpha=0.3) + geom_smooth(method="rlm", color="black"))
(plot_gov_relative <- ggplot(pred_hist, aes(x=srv_max, fill=gov_factor)) + geom_histogram(position="fill", breaks=c(0,0.7,1,1.7,2,2.7,3), closed='right')+ scale_x_log10("Server size", breaks=c(0,1,10,100,1000),limits=c(1,1000))+ scale_y_continuous("Increase in governance intensity") + plot_color1 + theme_bw() + theme(aspect.ratio=0.6, plot.margin = unit(c(0,0,0,0), "cm")) + geom_vline(xintercept=c(1,10,100,1000), alpha=0.3))
ggsave(plot_gov_count, file=paste0(pathImages, "plot_gov_count.png"), units='cm', width=2.25, height=1, scale=6)
ggsave(plot_gov_relative, file=paste0(pathImages, "plot_gov_relative.png"), units='cm', width=2.25, height=1, scale=6)

### governance against size against community
(plot_gov_scaling <- ggplot(mw_train[,.(gov=median(gov)),by=.(perf_factor, pop_size_factor_coarse)], aes(x=pop_size_factor_coarse, y=perf_factor)) + geom_bin2d(aes(fill=gov)) + scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=2.5, breaks=seq(from=0,to=12,by=2)) + theme_bw() + theme(panel.grid.major=element_line(0)) + scale_y_discrete("Core members", labels=c("0", "", "", "10", "", "", "100")) + coord_fixed(ratio=6/7) + scale_x_discrete("Server size", labels=c(5,10,50,100,500,1000)) + guides(fill=guide_legend(title="Governance\nplugins", reverse=TRUE)))
(plot_gov_scaling_by_resource_type <- ggplot(melt(mw_train, id.vars = c("srv_addr", "y", "srv_max", "pop_size_factor", "perf_factor"),  measure.vars = c("gov", "res_grief", "res_ingame", "res_realworld", "res_players"), variable.name = 'resource', value.name='resource_count')[,.(gov=mean(resource_count)),by=.(resource, perf_factor, pop_size_factor)], aes(x=pop_size_factor, y=perf_factor)) + geom_bin2d(aes(fill=gov)) + scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=1, breaks=seq(from=0,to=12,by=2)) + theme_bw() + theme(panel.grid.major=element_line(0), strip.background=element_rect(color="white", fill="white")) + scale_y_discrete("Core members", labels=c("0", "", "", "10", "", "", "100")) + coord_fixed(ratio=6/7) + scale_x_discrete("Server size", labels=c(5,10,50,100,500,1000)) + guides(fill=guide_legend(title="Governance\nplugins", reverse=TRUE)) + facet_wrap( ~ resource, ncol=1))
(plot_gov_scaling_by_inst_type <- ggplot(melt(mw_train, id.vars = c("srv_addr", "y", "srv_max", "pop_size_factor", "perf_factor"),  measure.vars = c("gov", grep("^inst_", names(mw_train), value=TRUE)), variable.name = 'institution', value.name='institution_count')[,.(gov=mean(institution_count)),by=.(institution, perf_factor, pop_size_factor)], aes(x=pop_size_factor, y=perf_factor)) + geom_bin2d(aes(fill=gov)) + scale_fill_gradient2(low="#91cf60", mid="#ffffbf", high="#fc8d59", midpoint=1, breaks=seq(from=0,to=12,by=2)) + theme_bw() + theme(panel.grid.major=element_line(0), strip.background=element_rect(color="white", fill="white")) + scale_y_discrete("Core members", labels=c("0", "", "", "10", "", "", "100")) + coord_fixed(ratio=6/7) + scale_x_discrete("Server size", labels=c(5,10,50,100,500,1000)) + guides(fill=guide_legend(title="Governance\nplugins", reverse=TRUE)) + facet_wrap( ~ institution, ncol=4))
### resource managemanet style by size:
ggplot(data=melt(training_full_lasso, id.vars = c("srv_addr", "srv_max", "y"),  measure.vars = c("res_grief", "res_ingame", "res_realworld", "res_players", "res_attention"), variable.name = 'resource', value.name='resource_count'),aes(x=srv_max, y=resource_count)) + geom_jitter(size=0.1, height=0.1, width=0.1) + scale_x_log10() + geom_smooth(method='rlm') + facet_wrap(~resource, ncol=2) 
### institution by size:
ggplot(data=melt(mw_train, id.vars = c("srv_addr", "srv_max", "y"),  measure.vars = grep("^inst_", names(mw_train)), variable.name = 'institution', value.name='institution_count'),aes(x=srv_max, y=institution_count)) + geom_jitter(size=0.1, height=0.1, width=0.1) + scale_x_log10() + geom_smooth(method='rlm') + facet_wrap(~institution, ncol=2) 
ggsave(plot_gov_scaling, file=paste0(pathImages, "plot_gov_scaling.png"), units='cm', width=2.25, height=1, scale=6)




### server diversity
plot_diversity_data <- mw_train[,.(srv_max, srv_max_log,pop_size_factor, srv_entropy), by=srv_addr]
plot_diversity_data2 <- mw_train[,.( pop_entropy={inst_dist<-colSums(.SD[,grep("^inst_", names(mw_train)),with=FALSE]); inst_dist<-(inst_dist+0.000001)/(sum(inst_dist)+0.000001); sum(sapply(inst_dist, function(x) {-x*log(x)})) }), by=pop_size_factor]
plot_diversity_data <- merge(plot_diversity_data, plot_diversity_data2[,.(pop_size_factor, pop_entropy)], all.x=T, all.y=F, by="pop_size_factor")
plot_diversity_data[,srv_entropy_agg1:=mean(srv_entropy), by=pop_size_factor]
plot_diversity_data[srv_entropy!=0,srv_entropy_agg2:=mean(srv_entropy), by=pop_size_factor]
plot_diversity_data[,srv_entropy_agg3:=median(srv_entropy), by=pop_size_factor]
### each server draws ona greater variety of governance styles as it gets larger, but they also become less different from each other .
ggplot(plot_diversity_data, aes(x=srv_max, y=srv_entropy)) + geom_point() + scale_x_log10() + geom_line(data=plot_diversity_data[srv_entropy!=0,],aes(x=srv_max, y=srv_entropy_agg2), color='red') + geom_line(aes(x=srv_max, y=srv_entropy_agg1), color='blue') + geom_line(aes(x=srv_max, y=srv_entropy_agg3), color='orange') + geom_line(aes(x=srv_max, y=pop_entropy), color='green')
### focus on decrease in difference over time
(plot_diversity <- ggplot(plot_diversity_data2, aes(x=pop_size_factor, y=pop_entropy)) + geom_bar(stat='identity') + geom_smooth() + xaxis_size_factor + scale_y_continuous("Population-level diversity in governance style") + theme_bw()   )
#  now bootstrap the stat
gov_diversity <- function(data, i_samp) {
    entropy_calc <- function(x) {-x*log(x)}
    inst_dist<-colSums(data[i_samp,])
    inst_dist<-(inst_dist+0.000001)/(sum(inst_dist)+0.000001)
    return(sum(sapply(inst_dist, entropy_calc)) )
}
plot_diversity_data4 <- mw_train[,{ttt <- boot(.SD[,c(grep("^inst_", names(.SD))), with=F], gov_diversity,  R=1000, parallel = "multicore", ncpus = 8); 
                                 tttq <- unlist(quantile(ttt$t, c(0.99, 0.50, 0.01)))
                                 list(pop_entropy=tttq[2], pop_entropy_low=tttq[3], pop_entropy_high=tttq[1])
},by=pop_size_factor_fine]
(plot_diversity <- ggplot(plot_diversity_data4, aes(x=pop_size_factor_fine, y=pop_entropy)) + geom_bar(stat='identity') + geom_smooth() + scale_x_discrete("Server size", labels=c("(0,5]", "(5,10]", "(10, 50]", "(50,100]", "(100, 500]", "(500, 1000]"))) + scale_y_continuous("Population-level diversity in governance style") + theme_bw() + coord_cartesian(ylim=c(1.5, 2.5)) + geom_errorbar(aes(ymin = pop_entropy_low, ymax = pop_entropy_high))
(plot_diversity_scaling <- ggplot(mw_train[,.(pop_entropy={inst_dist<-colSums(.SD[,grep("^inst_", names(mw_train)),with=FALSE]); inst_dist<-(inst_dist+0.000001)/(sum(inst_dist)+0.000001); sum(sapply(inst_dist, function(x) {-x*log(x)})) }),by=.(perf_factor, pop_size_factor)], aes(x=pop_size_factor, y=perf_factor)) + geom_bin2d(aes(fill=pop_entropy)) + scale_fill_gradient2(high="#91cf60", mid="#ffffbf", low="#fc8d59", midpoint=1.2) + theme_bw() + theme(panel.grid.major=element_line(0)) + scale_y_discrete("Core members", labels=c("0", "", "", "10", "", "", "100")) + coord_fixed(ratio=6/7) + scale_x_discrete("Server size", labels=c(5,10,50,100,500,1000)) + guides(fill=guide_legend(title="Entropy", reverse=TRUE)))
plot_diversity_scaling_boot_data <- mw_train[,.(pop_entropy={
                ttt <- boot(.SD[,c(grep("^inst_", names(.SD))), with=F], gov_diversity,  R=1000, parallel = "multicore", ncpus = 8); 
                tttq <- unlist(quantile(ttt$t, c(0.99, 0.50, 0.01), names=FALSE));
                #list(pop_entropy=tttq[2], pop_entropy_low=tttq[3], pop_entropy_high=tttq[1])
                tttq[2]
}),by=.(perf_factor, pop_size_factor)]
(plot_diversity_scaling_bootstrapped <- ggplot(plot_diversity_scaling_boot_data, aes(x=pop_size_factor, y=perf_factor)) + geom_bin2d(aes(fill=pop_entropy)) + scale_fill_gradient2(high="#91cf60", mid="#ffffbf", low="#fc8d59", midpoint=1.2) + theme_bw() + theme(panel.grid.major=element_line(0)) + scale_y_discrete("Core members", labels=c("0", "", "", "10", "", "", "100")) + coord_fixed(ratio=6/7) + scale_x_discrete("Server size", labels=c(5,10,50,100,500,1000)) + guides(fill=guide_legend(title="Entropy", reverse=TRUE)))



### comunity model
(lm_comm <- rlm(y ~ srv_max_log + srv_max_log*weeks_up_todate + date_ping_int + jubilees + srv_max_log*log_plugin_count + srv_max_log*dataset_reddit + srv_max_log*dataset_mcs_org  + cat_fun + cat_general + cat_mechanics + cat_misc + cat_roleplay + cat_teleportation + cat_world + cat_fixes +  cat_worldgen + gov*srv_max_log + aud_users*srv_max_log + aud_admin*srv_max_log + inst_broadcast*srv_max_log + inst_chat*srv_max_log + inst_privateproperty*srv_max_log + inst_shop*srv_max_log + inst_action_space_up*srv_max_log + inst_action_space_down*srv_max_log + inst_boundary*srv_max_log + inst_monitor_by_peer*srv_max_log + inst_monitor_by_admin*srv_max_log + inst_position_h*srv_max_log + inst_position_v*srv_max_log +  aud_users:actions_audience:srv_max_log + aud_admin:actions_audience:srv_max_log,  data=mw_train))
asdt(tidy(lm_comm))[abs(statistic)>=2]
#### size model (or not)
(lm_size <- rlm(srv_max_log ~ weeks_up_todate + date_ping_int + jubilees + log_plugin_count + dataset_reddit + dataset_mcs_org  + cat_fun + cat_general + cat_mechanics + cat_misc + cat_roleplay + cat_teleportation + cat_world + cat_fixes +  cat_worldgen + gov + inst_broadcast + inst_chat + inst_privateproperty + inst_shop + inst_action_space_up + inst_action_space_down + inst_boundary + inst_monitor_by_peer + inst_monitor_by_admin + inst_position_h + inst_position_v +  aud_users*actions_audience + aud_admin*actions_audience + res_grief + res_ingame + res_players + res_realworld,  data=mw_train))
(lm_size <- rlm(srv_max_log ~ weeks_up_todate + date_ping_int + dataset_reddit + dataset_mcs_org + plugin_count + gov + res_grief + res_ingame + res_players + res_realworld,  data=mw_train))
asdt(tidy(lm_size))[abs(statistic)>=2]
### resource models
(lm_grief <- rlm(res_grief ~ srv_max_log + srv_max_log*log_plugin_count + srv_max_log*dataset_reddit + srv_max_log*dataset_mcs_org + gov*srv_max_log + aud_users*srv_max_log + aud_admin*srv_max_log + inst_broadcast*srv_max_log + inst_chat*srv_max_log + inst_privateproperty*srv_max_log + inst_shop*srv_max_log + inst_action_space_up*srv_max_log + inst_action_space_down*srv_max_log + inst_boundary*srv_max_log + inst_monitor_by_peer*srv_max_log + inst_monitor_by_admin*srv_max_log + inst_position_h*srv_max_log + inst_position_v*srv_max_log +  aud_users:actions_audience:srv_max_log + aud_admin:actions_audience:srv_max_log,  data=mw_train))
asdt(tidy(lm_comm))[abs(statistic)>=2]
summary(lm_comm <- rlm(y ~ srv_max_log + srv_max_log*weeks_up_todate + date_ping_int + jubilees + srv_max_log*log_plugin_count + srv_max_log*dataset_reddit + srv_max_log*dataset_mcs_org  + cat_fun + cat_general + cat_mechanics + cat_misc + cat_roleplay + cat_teleportation + cat_world + cat_fixes +  cat_worldgen + res_grief*srv_max_log + res_ingame*srv_max_log + res_players*srv_max_log + res_realworld*srv_max_log +  aud_users*srv_max_log + aud_admin*srv_max_log + actions_user*srv_max_log + use_coarseauto*srv_max_log + use_coarsemanual*srv_max_log + use_fineauto*srv_max_log + use_finemanual*srv_max_log + inst_broadcast*srv_max_log + inst_chat*srv_max_log + inst_privateproperty*srv_max_log + inst_shop*srv_max_log + inst_action_space_up*srv_max_log + inst_action_space_down*srv_max_log + inst_boundary*srv_max_log + inst_monitor_by_peer*srv_max_log + inst_monitor_by_admin*srv_max_log + inst_position_h*srv_max_log + inst_position_v*srv_max_log +  aud_users:actions_audience:srv_max_log + aud_admin:actions_audience:srv_max_log,  data=mw_train))
