### NOTES

#http://scc.stat.ucla.edu/page_attachments/0000/0140/reg_2.pdf
#Prior to any analysis, the data should always be inspected for:
#Data-entry errors
#Missing values
#Outliers
#Unusual (e.g. asymmetric) distributions
#Changes in variability of data (output over input)
#Clustering
#Non-linear bivariate relatioships
#Unexpected patterns
#Distribution of residuals
### old colelcted refs for refresher
#     https://stats.stackexchange.com/questions/75054/how-do-i-perform-a-regression-on-non-normal-data-which-remain-non-normal-when-tr
#     http://www.isixsigma.com/tools-templates/normality/dealing-non-normal-data-strategies-and-tools/
#     http://people.duke.edu/~rnau/testing.htm
#     http://www.basic.northwestern.edu/statguidefiles/linreg_ass_viol.html
#     http://pareonline.net/getvn.asp?n=2&v=8
# descdist!

### consider
###    lmrob() from package 'robustbase' or 
###    library(sfsmisc) with f.robftest(rsl, var = "Air.Flow")


### google roy bargman stedown
### constrast coding of manova

### variables
### admin choices
#serv_max: size aka pressure?
#entropy of trinomial dist: governance scope aka resource complexity aka resource regime variety aka target variety governance scope
#total_res : governance instensity
#median(feat_count) : specialization
#entropy(style) : rule diversity
#%admin focused: consolidation
### results
#core : community size
#success : success (core/size)
#uniques
#longevity
#%uptime
### covariates and onctorls:
###     srv_details
###     srv_details  ### hoe much of API is being provided?
#weeks_up_todate  ### in case plugin count (lifetime at point of data collction: longer up, more plugins?) 
#weeks_up_total  ### eventual longevity (MUST EXLUCDE TOO FORWARD LOOKING)
#dataset_reddit + dataset_mcs_org ### data set sources
#date_ping_int### data of collection
#"date_ping_1st"              "date_ping_lst" 
#srv_retired ### have I now seen the whole lifetime of this server, beginning to end?

### thought models:
#success ~ srv_details + dataset_reddit + dataset_mcs_org + data_ping_int + date_point_1st + srv_retired + weeks_up_todate + weeks_up_total + size:governance_intensity + size:specialization + size:rule_diversity + size:consolidation + size:governance_scope + governance_intensity + specialization + rule_diversity + consolidation + governance_scope 
#results:
#"these things are associated with community success, and their connection to success increases as communities get larger"

library(LambertW) ## for gaussianize
library(sfsmisc) ### for f.robftest
library(robustbase) ### for lmrob

###CODE
### INITIALIZE GLOBALS
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_plotting.r"))
library(Amelia)

mw <- readRDS(paste0(pathData, "step6_servers_wide_govanalysis.rds"))
#if ("success" %ni% names(mw)) { mw[,success:=y/srv_max] }
#if ("total_aud" %ni% names(mw)) { 
	#mw[,':='(total_aud=sum(aud_users,aud_admin), ratio_aud=aud_admin/sum(aud_users,aud_admin)),by=srv_addr] 
	#mw[total_aud==0,ratio_aud:=0] 
#}
if (any(is.na(mw$pop_size_factor))) {
	mw[,pop_size_factor:=cut(log2(srv_max+1), breaks=c(0,2,4,6,12,24), labels=c("\u22644", "4 to 16", "16 to 64", "64 to 1024", ">1024"), ordered_result=TRUE, right=TRUE)]
}
setnames(mw, c("total_res", "srv_entropy", "ratio_aud","entropy_res"), c("governance_intensity", "rule_diversity", "consolidation","governance_scope"))
#mw <- mw[y>0]
mw[,success_dummy:=ifelse(y<=1,0,1)]
mw[,success_dummy0:=ifelse(y==0,1,0)]
mw[,success_dummy1:=ifelse(y==1,1,0)]
mw[,y_norm:=Gaussianize(y,type="h")]
#mw[,y_norm:=Gaussianize(y,type="h")]
#test_normality(mw[,y_norm])
mw[,srv_max_norm:=Gaussianize(srv_max,type="h")]
#test_normality(mw[,srv_max_norm])
mw[,perf_norm:=Gaussianize(as.numeric(perf_factor+rnorm(.N,sd=0.05)))]
#test_normality(mw[,perf_norm])
mw[,pop_size_norm:=Gaussianize(as.numeric(pop_size_factor)+rnorm(.N,sd=0.05))]
#test_normality(mw[,pop_size_norm])
### reduce
mw <- mw[,.(y_norm, srv_max_norm, y, ylog, success_dummy, success_dummy0, success_dummy1, srv_max, srv_max_log, srv_details, dataset_reddit, dataset_mcs_org, date_ping_int, weeks_up_todate, governance_intensity, plugin_specialization, rule_diversity, consolidation, governance_scope, dataset_source)]
mw[is.na(plugin_specialization),plugin_specialization:=mw[!is.na(plugin_specialization),sample(plugin_specialization,sum(is.na(mw$plugin_specialization)),replace=T)]]
mw[is.na(governance_scope),governance_scope:=0]



summary(srvlm_full <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm*governance_intensity + srv_max_norm*plugin_specialization + srv_max_norm*rule_diversity + srv_max_norm*consolidation + srv_max_norm*governance_scope, mw))
summary(srvlm_ctl <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm, mw))
summary(srvlm_ctl0 <- lm(y_norm ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm, mw))
summary(srvlm_ctl1 <- lm(y_norm ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm, mw))
summary(srvlm_ctl2 <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm, mw))
anova(srvlm_ctl, srvlm_full)


### two tier
mw[y_success:=ifelse(y>1,1,0)]
summary(srvnb_ctl <- glm.nb(y ~  srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_log, mw))
summary(srvnb_ctl <- glm.nb(y ~  srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_log, mw, offset=srv_max))
summary(srvnb_full <- glm.nb(y ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_log*governance_intensity + srv_max_log*plugin_specialization + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*governance_scope, mw, offset="srv_max"))
plotSimulatedResiduals(simulationOutput <- simulateResiduals(fittedModel = srvnb_ctl, n = 250))
plotSimulatedResiduals(simulationOutput <- simulateResiduals(fittedModel = srvnb_full, n = 250))
#summary(srvpoisson <- glm(y ~  srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm, mw, family="poisson"))
#summary(srvzi <- zeroinfl(y ~ srv_details + dataset_reddit + dataset_mcs_org + date_ping_int + weeks_up_todate + srv_max_norm | 1 , data = mw, dist = "negbin", EM = TRUE))

#summary(lm(y_norm ~ srv_max_norm*governance_scope, mw))
#summary(lm(y_norm ~ srv_max_norm*governance_intensity, mw))
#summary(lm(y_norm ~ srv_max_norm*plugin_specialization, mw))
#summary(lm(y_norm ~ srv_max_norm*rule_diversity, mw))
#summary(lm(y_norm ~ srv_max_norm*consolidation, mw))
summary(srvlm <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_norm*governance_intensity, mw))
summary(srvlm <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_norm*plugin_specialization, mw))
summary(srvlm <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_norm*rule_diversity, mw))
summary(srvlm <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_norm*consolidation, mw))
summary(srvlm <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_norm*governance_scope, mw))

summary(srvlm <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_norm*governance_intensity + srv_max_norm*plugin_specialization + srv_max_norm*rule_diversity + srv_max_norm*consolidation + srv_max_norm*governance_scope, mw))
summary(srvlm <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_norm*plugin_specialization + srv_max_norm*rule_diversity + srv_max_norm*consolidation + srv_max_norm*governance_scope, mw))
summary(srvlm <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_norm*governance_intensity + srv_max_norm*rule_diversity + srv_max_norm*consolidation + srv_max_norm*governance_scope, mw))
summary(srvlm <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_norm*governance_intensity + srv_max_norm*plugin_specialization + srv_max_norm*consolidation + srv_max_norm*governance_scope, mw))
summary(srvlm <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_norm*governance_intensity + srv_max_norm*plugin_specialization + srv_max_norm*rule_diversity + srv_max_norm*governance_scope, mw))
summary(srvlm <- lm(y_norm ~ success_dummy0 + success_dummy1 + srv_details + date_ping_int + weeks_up_todate + srv_max_norm*governance_intensity + srv_max_norm*plugin_specialization + srv_max_norm*rule_diversity + srv_max_norm*consolidation, mw))
plot(srvlm)

### corr of inputs
xxx <- cor(srvlm$model[,c(1,7:12)])
print.table(local({xxx[xxx<0.3] <- NA; xxx}))

###h testing
summary(glht(srvlm, linfct = c("governance_scope = 0", 
  "governance_intensity = 0", 
  "plugin_specialization = 0", 
  "rule_diversity = 0", 
  "consolidation = 0"))


summary(srvlm <- lm(srv_max_norm ~  srv_details + date_ping_int + weeks_up_todate + y_norm*governance_intensity + y_norm*rule_diversity + y_norm*consolidation + y_norm*governance_scope, mw))
summary(srvlm <- lm(cbind(srv_max_norm, y_norm) ~  srv_details + dataset_reddit + dataset_mcs_org+ date_ping_int + weeks_up_todate + governance_scope + governance_intensity + rule_diversity + consolidation + plugin_specialization , mw))
summary(srvlm_mctl <- lm(cbind(srv_max_norm, y_norm) ~  srv_details + dataset_reddit + dataset_mcs_org+ date_ping_int + weeks_up_todate , mw))
anova(srvlm, srvlm_mctl)





#http://dwoll.de/rexrepos/posts/multRegression.html
(fit <- lm(cbind(Y1, Y2) ~ X1 + X2 + X3, data=dfRegr))
summary(manova(fit), test="Hotelling-Lawley")
summary(manova(fit), test="Wilks")
summary(manova(fit), test="Roy")
summary(manova(fit), test="Pillai")


### try pls:
srvpls <- plsr(  residuals(srvlm_ctl) ~ srv_max_norm*governance_intensity + srv_max_norm*rule_diversity + srv_max_norm*consolidation + srv_max_norm*governance_scope, 8,data=mw)


### try logistic regression
### https://stats.idre.ucla.edu/r/dae/logit-regression/
### https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
library(aod)
library(DHARMa)

mwlgt <- copy(mw)
mwlgt[,ybin:=ifelse(y<=1,0,1)]
mwlgt[,ybino:=ordered(cut(y,breaks=unique(quantile(y, probs=seq(0,1,0.1))), right=FALSE, include.lowest=TRUE, ordered.result=TRUE ))]
mwlgt[,srv_max_bino:=ordered(cut(srv_max,breaks=unique(quantile(srv_max, probs=seq(0,1,0.2))), right=TRUE, include.lowest=TRUE, ordered.result=TRUE ))]
mwlgt[,dataset:=factor(dataset_source, levels=c("omni", "reddit", "mcs_org"))]
mwlgt[,srv_details:=ordered(srv_details)]
#mwlgt[,governance_intensity:=ordered(cut(governance_intensity,breaks=unique(quantile(governance_intensity, probs=seq(0,1,0.25))), right=FALSE, include.lowest=TRUE, ordered.result=TRUE ))]
mwlgt[,plugin_specialization:=log10(plugin_specialization+1)]
#mwlgt[,rule_diversity:=ordered(cut(rule_diversity,breaks=unique(quantile(rule_diversity, probs=seq(0,1,0.25))), right=FALSE, include.lowest=TRUE, ordered.result=TRUE ))]
#mwlgt[,consolidation:=ordered(cut(consolidation,breaks=unique(quantile(consolidation, probs=seq(0,1,0.25))), right=FALSE, include.lowest=TRUE, ordered.result=TRUE ))]
#mwlgt[,governance_scope:=ordered(cut(governance_scope,breaks=unique(quantile(governance_scope, probs=seq(0,1,0.2))), right=FALSE, include.lowest=TRUE, ordered.result=TRUE ))]
   

apply( asdf(mwlgt[,.(governance_intensity, plugin_specialization, rule_diversity, consolidation, governance_scope)]), 2, table)

xtabs( ~ ybin + srv_details, data=mwlgt)
xtabs( ~ ybin + dataset, data=mwlgt)
xtabs( ~ ybin + srv_max_bino, data=mwlgt)
#xtabs( ~ ybin + governance_intensity, data=mwlgt)
#xtabs( ~ ybin + rule_diversity, data=mwlgt)
#xtabs( ~ ybin + consolidation, data=mwlgt)
#xtabs( ~ ybin + governance_scope, data=mwlgt)
summary(srvlgt_ctl <- polr(ybino ~ srv_details + date_ping_int + dataset + weeks_up_todate + srv_max_log, data = mwlgt))
summary(srvlgt_full <- polr(ybino ~ srv_details + date_ping_int + dataset + weeks_up_todate + srv_max_log + srv_max_log*governance_intensity + srv_max_log*plugin_specialization + srv_max_log*rule_diversity + srv_max_log*consolidation + srv_max_log*governance_scope, data = mwlgt))
anova(srvlgt_ctl, srvlgt_full)
signif(cbind(coef=coef(srvlgt),exp(cbind(OR=coef(srvlgt),confint(srvlgt))))[-1,],4) ### odds ratios with conf ints
wald.test(b = coef(srvlgt), Sigma = vcov(srvlgt), Terms = 4) ### is srv_max signficiant
wald.test(b = coef(srvlgt), Sigma = vcov(srvlgt), L = cbind(0,0,0,-1,1,0,0)) ### is reddit source different from mco source?
with(srvlgt, null.deviance - deviance); logLik(srvlgt); 
varImp(srvlgt_full )

plotSimulatedResiduals(simulationOutput <- simulateResiduals(fittedModel = srvlgt, n = 250))

summary(srvlgt_p1 <- glm(ybino ~ srv_details + date_ping_int + dataset + weeks_up_todate + srv_max_log + srv_max_log*governance_intensity, data = mwlgt, family = "binomial"))
summary(srvlgt_p2 <- glm(ybino ~ srv_details + date_ping_int + dataset + weeks_up_todate + srv_max_log + srv_max_log*plugin_specialization, data = mwlgt, family = "binomial"))
summary(srvlgt_p3 <- glm(ybino ~ srv_details + date_ping_int + dataset + weeks_up_todate + srv_max_log + srv_max_log*rule_diversity, data = mwlgt, family = "binomial"))
summary(srvlgt_p4 <- glm(ybino ~ srv_details + date_ping_int + dataset + weeks_up_todate + srv_max_log + srv_max_log*consolidation, data = mwlgt, family = "binomial"))
summary(srvlgt_p5 <- glm(ybino ~ srv_details + date_ping_int + dataset + weeks_up_todate + srv_max_log + srv_max_log*governance_scope, data = mwlgt, family = "binomial"))
