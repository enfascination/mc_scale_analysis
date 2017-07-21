### initialize globals
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_step6_analysis.r"))
source(paste0(pathLocal,"header_redditscrape.r"))
pluginstats <- as.data.table(read.csv(paste0(pathData, 'step45_curse_plugins_metadata_full.csv')))

mc <- readRDS(paste0(pathData, "step55_serversweeksplugins.rds"))
expect_true(mc[,length(unique(srv_addr))] == mc[,length(unique(post_uid))])
mc[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]
### add feature features
mc <- merge(mc, pluginstats[build=='curse', list(feat, action_other_up, action_other_down, action_admin, role_rank, role_type, boundary, economy, communication, rbasic,rbiophys, popvol, popforce, rprevent, rexternal, rhetero, rincntv, rinst, rtech, rspat, rsoccap, rleader, rnorms, rinfopred, inst_type)], all.x=T, all.y=F, by=c('feat'))
### filter down a little
mc_sub <- filterDataSetDown(mc, cutUnrealistic=TRUE, cutNonVanilla=TRUE, cutNonPositiveDependent=TRUE, featureCountMin=15, keepFeatTypes=c('plugin', 'property'), keepDataSource=c('reddit', 'omni', 'mcs_org'))

mc_sub <- mc_sub[,c(lapply(.SD[,.(srv_details, dataset_source, weeks_up_todate, date_ping_int, srv_max, srv_max_log, srv_max_bak, genivisits, ncomm4visits, latency10ppl, latency20ppl, latency50pct, T1, T2, T3, T4, srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic, nuvisits12, nvisitsobs12, nvisitsunobs)], unique), lapply(.SD[,.(action_other_up, action_other_down, action_admin, role_rank, role_type, boundary, economy, communication, rbasic,rbiophys, popvol, popforce, rprevent, rexternal, rhetero, rincntv, rinst, rtech, rspat, rsoccap, rleader, rnorms)], sum), infopred_central=sum(rinfopred==1), infopred_dist=sum(rinfopred==2), strategy=sum(inst_type=="Strategy"), norm=sum(inst_type=="Norm"), rule=sum(inst_type=="Rule"), nodiscretion=sum(inst_type=="No discretion")), by=.(srv_addr)]
mc_sub[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]
expect_true(mc_sub[,length(unique(srv_addr))] == mc_sub[,length(unique(srv_addr)), by=dataset_source][,sum(V1)])
c(dim(mc_sub[,.N,by=srv_addr]) , dim(mc_sub[srv_repstat==T,.N,by=srv_addr]) , dim(mc_sub[srv_repquery==T,.N,by=srv_addr]) , dim(mc_sub[srv_repplug==T,.N,by=srv_addr]) , dim(mc_sub[srv_repsample==T,.N,by=srv_addr]), dim(mc_sub[srv_repsniff==T,.N,by=srv_addr]) , dim(mc_sub[srv_reptopic==T,.N,by=srv_addr]))

mc_split <- splitDataTestTrain(mc_sub, proportions=c(0.5, 0.25, 0.25), validation_set=TRUE)
mc_sub <- mc_split$train
validate <- mc_split$validate

summary(lm(srv_max ~ srv_details + strategy + norm + rule + nodiscretion + economy + action_other_up + action_other_down + action_admin + boundary + communication, mc_sub))
summary(lm(nvisitsunobs ~ srv_details + srv_max*strategy + srv_max*norm + srv_max*rule + srv_max*nodiscretion + srv_max*economy + srv_max*action_other_up + srv_max*action_other_down + srv_max*action_admin + srv_max*boundary + srv_max*communication, mc_sub))
#summary(lm(ncomm30visits ~ srv_details + srv_max*strategy + srv_max*norm + srv_max*rule + srv_max*nodiscretion + srv_max*economy + srv_max*action_other_up + srv_max*action_other_down + srv_max*action_admin + srv_max*boundary + srv_max*communication, mc_sub))
summary(lm(ncomm4visits ~ srv_details + srv_max*strategy + srv_max*norm + srv_max*rule + srv_max*nodiscretion + srv_max*economy + srv_max*action_other_up + srv_max*action_other_down + srv_max*action_admin + srv_max*boundary + srv_max*communication, mc_sub))
summary(lm(genivisits ~ srv_details + srv_max*strategy + srv_max*norm + srv_max*rule + srv_max*nodiscretion + srv_max*economy + srv_max*action_other_up + srv_max*action_other_down + srv_max*action_admin + srv_max*boundary + srv_max*communication, mc_sub))

summary(lm(srv_max ~        weeks_up_todate + date_ping_int + rbiophys + popforce + rprevent + rexternal + rhetero + rinst + rtech + rspat + rleader + infopred_central + infopred_dist + rnorms, mc_sub))
#summary(lm(ncomm30visits ~     weeks_up_todate + date_ping_int +    srv_max*rbiophys + srv_max*popforce + srv_max*rprevent + srv_max*rexternal + srv_max*rhetero + srv_max*rinst + srv_max*rtech + srv_max*rspat + srv_max*rleader + srv_max*infopred_central + srv_max*infopred_dist + srv_max*rnorms, mc_sub))
summary(lm(ncomm4visits ~        weeks_up_todate + date_ping_int + srv_max*rbiophys + srv_max*popforce + srv_max*rprevent + srv_max*rexternal + srv_max*rhetero + srv_max*rinst + srv_max*rtech + srv_max*rspat + srv_max*rleader + srv_max*infopred_central + srv_max*infopred_dist + srv_max*rnorms, mc_sub))
summary(lm(genivisits ~        srv_max*rbiophys + srv_max*popforce + srv_max*rprevent + srv_max*rexternal + srv_max*rhetero + srv_max*rinst + srv_max*rtech + srv_max*rspat + srv_max*rleader + srv_max*infopred_central + srv_max*infopred_dist + srv_max*rnorms, mc_sub))
#summary(lm(ncomm30visits ~        T1 + T2 + T3 + T4 , mc_sub[srv_reptopic==T]))
summary(lm(latency10ppl ~ rbiophys + srv_max*popforce + srv_max*rprevent + rexternal + rhetero + srv_max*rinst + srv_max*rtech + rspat + srv_max*rleader + srv_max*infopred_central + srv_max*infopred_dist, mc_sub[!is.na(latency10ppl)]))
##plot(predict(lm(ncomm30visits ~        srv_max*rbiophys + srv_max*popforce + srv_max*rprevent + srv_max*rexternal + srv_max*rhetero + srv_max*rinst + srv_max*rtech + srv_max*rspat + srv_max*rleader + srv_max*infopred_central + srv_max*infopred_dist + srv_max*rnorms, mc_sub), newdata=data.frame(srv_max=1:100, infopred_dist=1, rbiophys=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rinst=0, rtech=0,rspat=0,rleader=0,infopred_central=0,rnorms=0)))
#summary(rlm(ncomm30visits ~     weeks_up_todate + date_ping_int +    srv_max*rbiophys + srv_max*popforce + srv_max*rprevent + srv_max*rexternal + srv_max*rhetero + srv_max*rinst + srv_max*rtech + srv_max*rspat + srv_max*rleader + srv_max*infopred_central + srv_max*infopred_dist + srv_max*rnorms, mc_sub))
summary(rlm(ncomm4visits ~        weeks_up_todate + date_ping_int + srv_max*rbiophys + srv_max*popforce + srv_max*rprevent + srv_max*rexternal + srv_max*rhetero + srv_max*rinst + srv_max*rtech + srv_max*rspat + srv_max*rleader + srv_max*infopred_central + srv_max*infopred_dist + srv_max*rnorms, mc_sub))
#summary(lm(ncomm30visits ~     weeks_up_todate + date_ping_int +    srv_max*rbiophys + srv_max*popforce + srv_max*rprevent + srv_max*rexternal + srv_max*rhetero + srv_max*rinst + srv_max*rtech + srv_max*rspat + srv_max*rleader + srv_max*infopred_central + srv_max*infopred_dist + srv_max*rnorms, mc_sub[dataset_source %in% c('reddit', 'mcs_org')]))
summary(lm(ncomm4visits ~        weeks_up_todate + date_ping_int  + srv_max*rbiophys + srv_max*popforce + srv_max*rprevent + srv_max*rexternal + srv_max*rhetero + srv_max*rinst + srv_max*rtech + srv_max*rspat + srv_max*rleader + srv_max*infopred_central + srv_max*infopred_dist + srv_max*rnorms, mc_sub[dataset_source %in% c('reddit', 'mcs_org')]))


mm <- mc_sub
summary(mm)
pred_coefs <- coef(mm)
pred_coefs <- 1
pred_hist <- rbind( mc_sub[boundary==1,list(srv_max,srv_max_bak,ncomm4visits,rule_type="Entry restrictions", rule_coef=pred_coefs["boundary"])]
                   ,mc_sub[action_other_up==1,list(srv_max,srv_max_bak,ncomm4visits,rule_type="More actions, players", rule_coef=pred_coefs["action_other_up"])]
                   ,mc_sub[action_admin==1,list(srv_max,srv_max_bak,ncomm4visits,rule_type="More actions, admins", rule_coef=pred_coefs["action_admin"])]
                   ,mc_sub[action_other_down==1,list(srv_max,srv_max_bak,ncomm4visits,rule_type="Fewer actions, players", rule_coef=pred_coefs["action_other_down"])]
                   ,mc_sub[economy==1,list(srv_max,srv_max_bak,ncomm4visits,rule_type="Economy", rule_coef=pred_coefs["economy"])]
                   ,mc_sub[communication==1,list(srv_max,srv_max_bak,ncomm4visits,rule_type="Communication", rule_coef=pred_coefs["communication"])]
                   )
pred_hist$rule_type <- factor(pred_hist$rule_type, levels=c("Entry restrictions", "Communication", "More actions, players", "Economy", "Fewer actions, players", "More actions, admins")) 
ggplot(subset(pred_hist, rule_type %in% c("Entry restrictions", "Communication", "Fewer actions, players", "More actions, players", "More actions, admins")), aes(x=srv_max_bak, fill=rule_type)) + geom_histogram(position="dodge", binwidth=1/5)+ scale_x_log10("Server capacity")+ scale_y_continuous("Count of servers") + scale_fill_brewer("Rule type", type="div",palette=8) + theme_bw() + annotation_logticks(sides = "b", short=unit(0,"cm"))
ggplot(subset(pred_hist, rule_type %in% c("Entry restrictions", "Communication", "Fewer actions, players", "More actions, players", "More actions, admins")), aes(x=(ncomm4visits+1), fill=rule_type)) + geom_histogram(position="dodge", binwidth=5/5)+ scale_x_continuous("Server capacity", limits=c(0,12))+ scale_y_continuous("Count of servers") + scale_fill_brewer("Rule type", type="div",palette=8) + theme_bw() + annotation_logticks(sides = "b", short=unit(0,"cm"))


ggplot(pred_hist, aes(x=srv_max,  group=rule_type, fill=rule_type)) + geom_bar(position="dodge")+ scale_x_log10() + scale_fill_discrete()
ggplot(subset(pred_hist, rule_type %in% c("Entry restrictions", "Fewer actions, players", "Economy")), aes(x=srv_max,  group=rule_type, fill=rule_type)) + geom_bar(position="dodge")+ scale_x_log10("Established server capacity")+ scale_y_continuous("Count of servers") + scale_fill_discrete("Rule type") 
ggplot(subset(pred_hist, rule_type %in% c("Entry restrictions", "Communication", "Fewer actions, players", "More actions, players", "More actions, admins")), aes(x=srv_max, fill=rule_type)) + geom_histogram(position="dodge", binwidth=1/5)+ scale_x_log10("Server capacity")+ scale_y_continuous("Count of servers") + scale_fill_brewer("Rule type", type="div",palette=8) + theme_bw() + annotation_logticks(sides = "b", short=unit(0,"cm"))
ggplot(subset(pred_hist, rule_type %in% c("Entry restrictions", "Communication", "Fewer actions, players", "More actions, players", "More actions, admins")), aes(x=srv_max, fill=rule_type)) + geom_density(alpha=0.3)+ scale_x_continuous("Server capacity", limits=c(0,200))+ scale_y_continuous("Count of servers") + scale_fill_brewer("Rule type", type="div",palette=8) + theme_bw() 
ggplot(pred_hist, aes(x=srv_max)) + geom_histogram()+ scale_x_continuous("Server capacity", limits=c(0,200)) + scale_fill_brewer("Rule type", type="div",palette=8) + theme_bw() 
