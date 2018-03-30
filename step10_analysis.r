
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

### reduce
mw <- mw[,.(srv_addr, y_norm, y, ylog, success_dummy0, success_dummy1, srv_max, srv_max_log, srv_max_norm, srv_max_log_norm, srv_details, dataset_reddit, dataset_mcs_org, date_ping_int, weeks_up_todate, governance_intensity, plugin_specialization, rule_diversity, governance_scope, dataset_source, administration=cat_admin, perf_factor, pop_size_factor, cat_chat, cat_informational, cat_economy, cat_admin=(cat_admintools+cat_webadmin), plugin_count , nuvisits12, nvisitsobs12, nvisitsunobs, res_grief, res_ingame, res_realworld, pct_grief, pct_ingame, pct_realworld, pct_ichat, pct_iinformational, pct_ieconomy, pct_iadmin)]
print(nrow(mw))
print(mw[,.(max(srv_max), max(y), max(nuvisits12), max(nvisitsobs12), max(nvisitsunobs))])

### size as output
summary(sizeout_ctl <- lm(srv_max_log ~ srv_details + plugin_count + date_ping_int + weeks_up_todate, mw[y>1]))
logLik(sizeout_ctl);BIC(sizeout_ctl)
summary(mm <- lm(srv_max_log ~ srv_details + plugin_count + date_ping_int + weeks_up_todate + governance_intensity, mw[y>1]))
summary(mm <- lm(srv_max_log ~ srv_details + plugin_count + date_ping_int + weeks_up_todate + rule_diversity, mw[y>1]))
summary(mm <- lm(srv_max_log ~ srv_details + plugin_count + date_ping_int + weeks_up_todate + plugin_specialization, mw[y>1]))
summary(mm <- lm(srv_max_log ~ srv_details + plugin_count + date_ping_int + weeks_up_todate + governance_scope, mw[y>1]))
logLik(mm);BIC(mm)
anova(sizeout_ctl, mm,test="Chisq")
summary(sizeout <- lm(srv_max_log ~ srv_details + plugin_count + date_ping_int + weeks_up_todate + governance_intensity + rule_diversity + plugin_specialization  + governance_scope, mw[y>1]))
logLik(sizeout);BIC(sizeout)
anova(sizeout_ctl, sizeout,test="Chisq")
plotSimulatedResiduals(simulationOutput <- simulateResiduals(fittedModel = sizeout, n = 250))

### negative binomial (beats glm(family=poisson()) and zinb() on AIC and logLik
### plots clean QQ and constant variance in residuals
summary(srvnb_ctl <- glm.nb(y ~  srv_details + plugin_count + date_ping_int + weeks_up_todate + srv_max_log, mw))
srvnb_ctl$rank;logLik(srvnb_ctl);BIC(srvnb_ctl);srvnb_ctl$theta;
### "univariate" after controls
print(summary(srvnb_p1 <- glm.nb(y ~ srv_details + plugin_count + date_ping_int + weeks_up_todate + srv_max_log*governance_intensity, mw)))
print(summary(srvnb_p1 <- glm.nb(y ~ srv_details + plugin_count + date_ping_int + weeks_up_todate + srv_max_log*rule_diversity, mw)))
print(summary(srvnb_p1 <- glm.nb(y ~ srv_details + plugin_count + date_ping_int + weeks_up_todate + srv_max_log*plugin_specialization, mw)))
print(summary(srvnb_p1 <- glm.nb(y ~ srv_details + plugin_count + date_ping_int + weeks_up_todate + srv_max_log*governance_scope, mw)))
srvnb_p1$rank;logLik(srvnb_p1);BIC(srvnb_p1);srvnb_p1$theta;anova(srvnb_ctl, srvnb_p1, test="Chisq");pchisq((anova(srvnb_ctl, srvnb_p1, test="Chisq")$"LR stat."[2]), 2, lower.tail=FALSE)
print(summary(srvnb_full <- glm.nb(y ~ srv_details + plugin_count + date_ping_int + weeks_up_todate + srv_max_log*governance_intensity + srv_max_log*rule_diversity + srv_max_log*plugin_specialization + srv_max_log*governance_scope, mw)))
### other tests
print(anova(srvnb_ctl , srvnb_full ))
srvnb_full$rank;logLik(srvnb_full);BIC(srvnb_full);srvnb_full$theta;anova(srvnb_ctl, srvnb_full, test="Chisq");pchisq((anova(srvnb_ctl, srvnb_full, test="Chisq")$"LR stat."[2]), 2, lower.tail=FALSE)
plotSimulatedResiduals(simulationOutput <- simulateResiduals(fittedModel = srvnb_ctl, n = 250))
plotSimulatedResiduals(simulationOutput <- simulateResiduals(fittedModel = srvnb_full, n = 250))
#summary(srvpoisson <- glm(y ~  srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm, mw, family="poisson"))
#summary(srvzi <- zeroinfl(y ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm | 1 , data = mw, dist = "negbin", EM = TRUE))



### ASSUMPTION AND DISTRIBUIONAL CHECKS
### lm unsatisfactory
summary(srvlm_ctl <- glm.nb(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate , mw))
summary(srvlm_full <- glm.nb(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_log*governance_scope  + srv_max_log*rule_diversity + srv_max_log*governance_intensity +srv_max_log*plugin_specialization, mw))
anova(srvlm_ctl, srvlm_full)
plotSimulatedResiduals(simulationOutput <- simulateResiduals(fittedModel = srvlm_ctl, n = 250))
### with gaussianized size, in which I had to add 2 SD's of random noise! to srv_max_log in order to get a normal QQ.  things are significant despite huge noise injection
#test_normality(srv_max_log_norm)
summary(sizeout <- lm(srv_max_log_norm ~ srv_details + plugin_count + date_ping_int + weeks_up_todate + governance_scope  + governance_intensity  + plugin_specialization  + rule_diversity, mw[y>1]))

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
mwbin <- mw[,.(bin_count=unique(bin_count_fine),plugin_count=as.numeric(median(plugin_count)), diversity_meanhamming=gov_dist(.SD[,.(cat_chat, cat_informational, cat_economy, cat_admin)], 1:.N, distf=hamming)),by=.(perf_factor_fine, pop_size_factor_fine)]
mwbin <- mwbin[bin_count>1 & perf_factor_fine %ni% c("(0,1]", "(-1,0]")]
#mwbin[order(pop_size_factor_fine,perf_factor_fine)]
divlm <- lm(diversity_meanhamming ~ plugin_count + as.numeric(pop_size_factor_fine)*as.numeric(perf_factor_fine), mwbin); summary(divlm)
### now model diversity across servers changing with succes by size
mwbin <- mw[,.(bin_count=unique(bin_count),plugin_count=as.numeric(median(plugin_count)), diversity_meanhamming=gov_dist(.SD[,.(cat_chat, cat_informational, cat_economy, cat_admin)], 1:.N, distf=hamming)),by=.(perf_factor, pop_size_factor)]
mwbin <- mwbin[bin_count>1]
#mwbin <- mwbin[bin_count>1 & perf_factor %ni% c("0", "1")]
mwbin <- mwbin[bin_count>1 & perf_factor %in% c("0", "1")]
#mwbin[order(pop_size_factor,perf_factor)]
divlm <- lm(diversity_meanhamming ~ plugin_count + as.numeric(pop_size_factor)*as.numeric(perf_factor), mwbin); summary(divlm)



### resources
summary(resnb <- glm.nb(y ~  plugin_count + srv_max_log + srv_max_log*res_grief + srv_max_log*res_realworld + srv_max_log*res_ingame, mw))
summary(resnb <- glm.nb(y ~  plugin_count + srv_max_log + srv_max_log*pct_grief + srv_max_log*pct_realworld + srv_max_log*pct_ingame, mw))
### inst
summary(resnb <- glm.nb(y ~  plugin_count + srv_max_log + srv_max_log*cat_chat + srv_max_log*cat_informational + srv_max_log*cat_economy + srv_max_log*cat_admin, mw))
summary(resnb <- glm.nb(y ~  plugin_count + srv_max_log + srv_max_log*pct_ichat + srv_max_log*pct_iinformational + srv_max_log*pct_ieconomy + srv_max_log*pct_iadmin, mw))

### take 2
### diagnose diversity changes
### isntitutions
summary(instnb <- lm(srv_max_log ~  plugin_count + cat_chat + cat_informational + cat_economy + cat_admin, mw[y>1]))
summary(instnb <- glm.nb(y ~  plugin_count + srv_max_log + cat_chat*srv_max_log + cat_informational*srv_max_log + cat_economy*srv_max_log + cat_admin*srv_max_log, mw))
### resources
summary(resnb <- lm(srv_max_log ~  plugin_count + res_grief + res_realworld + res_ingame, mw[y>1]))
summary(resnb <- glm.nb(y ~  plugin_count + srv_max_log + res_grief*srv_max_log  + res_realworld*srv_max_log  + res_ingame*srv_max_log , mw))
summary(resnb <- lm(srv_max_log ~  plugin_count + pct_grief + pct_realworld + pct_ingame, mw[y>1]))
summary(resnb <- glm.nb(y ~  plugin_count + srv_max_log + pct_grief*srv_max_log  + pct_realworld*srv_max_log  + pct_ingame*srv_max_log , mw))




#use training dataset to calculate model that selects variables for final model
	#negative binomial (non-zero inflated) regression
#threshold: 0.01
#report
#run test over bins
#get a table of result (via tidy) and get it into the ms and put in caps what's significant in what direction and start writing!I#


### DESCRIPTIVES
cat("number of servers", nrow(mw))
cat("srv max servers", unlist(mw[, .(min(srv_max), max(srv_max))]))
cat("core group", unlist(mw[, .(min(y), max(y))]))
cat("unique visitors", unlist(mw[, .(min(nuvisits12), max(nuvisits12))]))
cat("visitors", unlist(mw[, .(min(nvisitsobs12), max(nvisitsobs12))]))
median(mw$y)
cat("lifetime", unlist(mw[, .(median(weeks_up_total))]))
cat("rule count", unlist(mw[, .(min(plugin_count), max(plugin_count), min(governance_intensity), max(governance_intensity), min(total_inst), max(total_inst))]))

