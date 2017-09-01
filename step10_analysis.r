
#results:
#"these things are associated with community success, and their connection to success increases as communities get larger"

###CODE
### INITIALIZE GLOBALS
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_plotting.r"))
library(LambertW)
library(DHARMa)
library(proxy)

mw <- readRDS(paste0(pathData, "step6_servers_wide_govanalysis.rds"))
#if ("success" %ni% names(mw)) { mw[,success:=y/srv_max] }
#if ("total_aud" %ni% names(mw)) { 
	#mw[,':='(total_aud=sum(aud_users,aud_admin), ratio_aud=aud_admin/sum(aud_users,aud_admin)),by=srv_addr] 
	#mw[total_aud==0,ratio_aud:=0] 
#}
setnames(mw, c("total_res", "ratio_aud", "aud_admin", "count_res_type", "entropy_res", "count_inst_type", "srv_entropy" ), c( "governance_intensity", "consolidation", "consolidation2", "governance_scope", "governance_scope2", "rule_diversity", "rule_diversity2"))
#mw <- mw[y>0]
mw[,success_dummy:=ifelse(y<=1,0,1)]
mw[,success_dummy0:=ifelse(y==0,1,0)]
mw[,success_dummy1:=ifelse(y==1,1,0)]
mw[,plugin_specialization:=1/plugin_specialization]
mw[,y_norm:=Gaussianize(y,type="h")]
#test_normality(mw[,y_norm])
mw[,srv_max_norm:=Gaussianize(srv_max,type="h")]
#test_normality(mw[,srv_max_norm])

### reduce
mw <- mw[,.(y_norm, srv_max_norm, y, ylog, success_dummy0, success_dummy1, srv_max, srv_max_log, srv_details, dataset_reddit, dataset_mcs_org, date_ping_int, weeks_up_todate, governance_intensity, plugin_specialization, rule_diversity, consolidation, consolidation2, governance_scope, governance_scope2, dataset_source, administration, behavior_management, rule_diversity2, perf_factor, pop_size_factor, cat_chat, cat_informational, cat_economy, cat_admin=(cat_admintools+cat_webadmin), plugin_count )]
### deal with NAs (or not)
#mw[is.na(plugin_specialization),plugin_specialization:=mw[!is.na(plugin_specialization),sample(plugin_specialization,sum(is.na(mw$plugin_specialization)),replace=T)]]

### lm unsatisfactory
summary(srvlm_ctl <- glm.nb(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate , mw))
summary(srvlm_full <- glm.nb(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw))
anova(srvlm_ctl, srvlm_full)

### find good distribution
gofstat(fitdist((mw$y),c("norm"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("lnorm"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("exp"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("pois"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("binom"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("gamma"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("weibull"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("nbinom"),discrete=T,method="mle"))
gofstat(fitdist((mw$y),c("nbinom"),discrete=T,method="mle"))
### applied literally to y, negative binomial is describing the number of drawn visitors necessary to draw a visitor 4 times 
### thats not great, but another "ecological" interpretation of negative binomial, from p 165 of http://ms.mcmaster.ca/~bolker/emdbook/book.pdf
###     is poissonian (What is the number of events (person joins core group) in each community in a month) where the lambda for each community is different.  That's a perfect description of my data.

### negative binomial (beats glm(family=poisson()) and zinb() on AIC and logLik
### plots clean QQ and constant variance in residuals
summary(srvnb_ctl <- glm.nb(y ~  srv_details + date_ping_int + weeks_up_todate + srv_max_log, mw))
#summary(srvnb_full2 <- glm.nb(y ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_log*governance_intensity + srv_max_log*administration + srv_max_log*governance_scope2, mw))
summary(srvnb_full <- glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw))
anova(srvnb_ctl , srvnb_full )
plotSimulatedResiduals(simulationOutput <- simulateResiduals(fittedModel = srvnb_ctl, n = 250))
plotSimulatedResiduals(simulationOutput <- simulateResiduals(fittedModel = srvnb_full, n = 250))
### test effectof plugin specialization, which has too many missing values to test with the other kids
anova(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity, mw[!is.na(plugin_specialization)]), glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity+ srv_max_log*plugin_specialization , mw[!is.na(plugin_specialization)]))
#summary(srvpoisson <- glm(y ~  srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm, mw, family="poisson"))
#summary(srvzi <- zeroinfl(y ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm | 1 , data = mw, dist = "negbin", EM = TRUE))

### "univariate" after controls
summary(srvnb_full <- glm.nb(y ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_log*governance_scope, mw))
summary(srvnb_full <- glm.nb(y ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_log*rule_diversity, mw))
summary(srvnb_full <- glm.nb(y ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_log*consolidation, mw))
summary(srvnb_p1 <- glm.nb(y ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_log*behavior_management, mw))
summary(srvnb_p1 <- glm.nb(y ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_log*governance_intensity, mw))
summary(srvnb_full <- glm.nb(y ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_log*plugin_specialization, mw))

### "all but one"s after controls
summary(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw))
summary(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw))
summary(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw))
summary(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*governance_intensity , mw))
summary(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management , mw))
summary(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw))
### and plots thereof
plot(residuals(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw)), mw$governance_scope)
plot(residuals(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw)), mw$rule_diversity)
plot(residuals(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw)), mw$consolidation)
plot(residuals(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*governance_intensity , mw)), mw$behavior_management)
plot(residuals(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management , mw)), mw$governance_intensity)
plot(residuals(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw)), mw$plugin_specialization)
### and plots of the interactions
plot(residuals(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw)), mw$srv_max_log*mw$governance_scope)
plot(residuals(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw)), mw$srv_max_log*mw$rule_diversity)
plot(residuals(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw)), mw$srv_max_log*mw$consolidation)
plot(residuals(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*governance_intensity , mw)), mw$srv_max_log*mw$behavior_management)
plot(residuals(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management , mw)), mw$srv_max_log*mw$governance_intensity)
plot(residuals(glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*behavior_management + srv_max_log*governance_intensity , mw)), mw$srv_max_log*mw$plugin_specialization)


### now model diversity across servers changing with succes by size
mwbin <- mw[,.(plugin_count=as.numeric(median(plugin_count)), diversity=gov_dist(.SD[,.(cat_chat, cat_informational, cat_economy, cat_admin)], 1:.N, function(d){minkowski(d[1], d[2] , p=-1)})),by=.(perf_factor, pop_size_factor)]
mwbin <- mw[,.(plugin_count=as.numeric(median(plugin_count)), diversity=gov_dist(.SD[,.(cat_chat, cat_informational, cat_economy, cat_admin)], 1:.N, function(d){proxy::dist(d, method="simple matching")})),by=.(perf_factor, pop_size_factor)]
mwbin <- mw[,.(plugin_count=as.numeric(median(plugin_count)), diversity=entropy_calc(colSums(.SD[,.(cat_chat, cat_informational, cat_economy)])/sum(.SD[,.(cat_chat, cat_informational, cat_economy)]))),by=.(perf_factor, pop_size_factor)]
divlm <- lm(as.numeric(pop_size_factor) ~ plugin_count + diversity, mwbin); summary(divlm)
#plot(divlm)

### corr of inputs
xxx <- cor(srvlm$model[,c(1,7:12)])
print.table(local({xxx[xxx<0.3] <- NA; xxx}))

###h testing
summary(glht(srvlm, linfct = c("governance_scope = 0", 
  "governance_intensity = 0", 
  "plugin_specialization = 0", 
  "rule_diversity = 0", 
  "consolidation = 0"))





