
#results:
#"these things are associated with community success, and their connection to success increases as communities get larger"

###CODE
### INITIALIZE GLOBALS
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_step6_analysis.r"))
source(paste0(pathLocal,"lib_plotting.r"))
library(DHARMa)
library(proxy)
library(broom)
library(fitdistrplus)

mw <- readRDS(paste0(pathData, "step6_servers_wide_govanalysis.rds"))
print(summary(srvnb_full <- glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*governance_intensity  + srv_max_log*plugin_specialization + srv_max_log*rule_diversity , mw)))

### reduce
mw <- mw[,.(srv_addr, y_norm, y, ylog, success_dummy0, success_dummy1, srv_max, srv_max_log, srv_max_norm, srv_details, dataset_reddit, dataset_mcs_org, date_ping_int, weeks_up_todate, governance_intensity, plugin_specialization, rule_diversity, governance_scope, dataset_source, administration=cat_admin, perf_factor, pop_size_factor, cat_chat, cat_informational, cat_economy, cat_admin=(cat_admintools+cat_webadmin), plugin_count , nuvisits12, nvisitsobs12, nvisitsunobs, res_grief, res_ingame, res_realworld, pct_grief, pct_ingame, pct_realworld)]
print(nrow(mw))
print(mw[,.(max(srv_max), max(y), max(nuvisits12), max(nvisitsobs12), max(nvisitsunobs))])

### negative binomial (beats glm(family=poisson()) and zinb() on AIC and logLik
### plots clean QQ and constant variance in residuals
summary(srvnb_ctl <- glm.nb(y ~  srv_details + date_ping_int + weeks_up_todate + srv_max_log, mw))
print(summary(srvnb_full <- glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*governance_intensity  + srv_max_log*plugin_specialization + srv_max_log*rule_diversity , mw)))
### other tests
print(anova(srvnb_ctl , srvnb_full ))
plotSimulatedResiduals(simulationOutput <- simulateResiduals(fittedModel = srvnb_ctl, n = 250))
plotSimulatedResiduals(simulationOutput <- simulateResiduals(fittedModel = srvnb_full, n = 250))
#summary(srvpoisson <- glm(y ~  srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm, mw, family="poisson"))
#summary(srvzi <- zeroinfl(y ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm | 1 , data = mw, dist = "negbin", EM = TRUE))

### "univariate" after controls
print(summary(srvnb_p1 <- glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope, mw)))
print(summary(srvnb_p1 <- glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_intensity, mw)))
print(summary(srvnb_p1 <- glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*plugin_specialization, mw)))
print(summary(srvnb_p1 <- glm.nb(y ~ srv_details + date_ping_int + weeks_up_todate + srv_max_log*rule_diversity, mw)))

### ASSUMPTION AND DISTRIBUIONAL CHECKS
### lm unsatisfactory
summary(srvlm_ctl <- glm.nb(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate , mw))
summary(srvlm_full <- glm.nb(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*governance_intensity +srv_max_log*plugin_specialization, mw))
anova(srvlm_ctl, srvlm_full)
plotSimulatedResiduals(simulationOutput <- simulateResiduals(fittedModel = srvlm_ctl, n = 250))

### find good distribution
gofstat(fitdist((mw$y),c("norm"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("lnorm"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("pois"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("binom"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("gamma"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("weibull"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("exp"),discrete=T,method="mle"))
gofstat(fitdist((mw$y+1),c("nbinom"),discrete=T,method="mle"))
gofstat(fitdist((mw$y),c("nbinom"),discrete=T,method="mle"))
### applied literally to y, negative binomial is describing the number of drawn visitors necessary to draw a visitor 4 times 
### thats not great, but another "ecological" interpretation of negative binomial, from p 165 of http://ms.mcmaster.ca/~bolker/emdbook/book.pdf
###     is poissonian (What is the number of events (person joins core group) in each community in a month) where the lambda for each community is different.  That's a perfect description of my data.

### comparable results on two random subsets (many times over)?
ss<- sort(sample(1:nrow(mw), (nrow(mw)/2))); mw1 <- mw[ss]; mw2 <- mw[(1:nrow(mw))[1:nrow(mw) %ni% ss]]
summary(glm.nb(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*governance_intensity + srv_max_log*plugin_specialization, mw1))
summary(glm.nb(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*governance_intensity + srv_max_log*plugin_specialization, mw2))


### now model diversity across servers changing with succes by size
mwbin <- mw[,.(bin_count=.N,plugin_count=as.numeric(median(plugin_count)), diversity=entropy_calc(colSums(.SD[,.(cat_chat, cat_informational, cat_economy, cat_admin)])/sum(.SD[,.(cat_chat, cat_informational, cat_economy, cat_admin)]))),by=.(perf_factor, pop_size_factor)]
divlm <- lm(as.numeric(perf_factor) ~ plugin_count + diversity*as.numeric(pop_size_factor), mwbin); summary(divlm)
#plot(divlm)
print(summary(divlm))



### resources
summary(resnb <- glm.nb(y ~  plugin_count + srv_max_log + srv_max_log*res_grief + srv_max_log*res_realworld + srv_max_log*res_ingame, mw))
summary(resnb <- glm.nb(y ~  plugin_count + srv_max_log + srv_max_log*pct_grief + srv_max_log*pct_realworld + srv_max_log*pct_ingame, mw))
### inst
summary(resnb <- glm.nb(y ~  plugin_count + srv_max_log + srv_max_log*cat_chat + srv_max_log*cat_informational + srv_max_log*cat_economy + srv_max_log*cat_admin, mw))
summary(resnb <- glm.nb(y ~  plugin_count + srv_max_log + srv_max_log*pct_ichat + srv_max_log*pct_iinformational + srv_max_log*pct_ieconomy + srv_max_log*pct_iadmin, mw))




#use training dataset to calculate model that selects variables for final model
	#negative binomial (non-zero inflated) regression
#threshold: 0.01
#report
#run test over bins
#get a table of result (via tidy) and get it into the ms and put in caps what's significant in what direction and start writing!I#



