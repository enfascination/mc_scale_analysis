### initialize globals
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_step6_analysis.r"))

### notes:
###  if there is lots of data 50/50 training/test is fine, and you shouldn't calculate full lasso paths (dfmax=50 or 100) and it's important to filter columns down before widening the matrix.  

mc <- readRDS(paste0(pathData, "step55_serversweeksplugins.rds"))
expect_true(mc[,length(unique(srv_addr))] == mc[,length(unique(post_uid))])
mc[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]

mc <- filterDataSetDown(mc, cutUnrealistic=TRUE, cutNonVanilla=FALSE, cutNonPositiveDependent=TRUE, featureCountMin=0, keepFeatTypes=c('plugin', 'tag', 'keyword', 'sign', 'property'), keepDataSource=c('reddit', 'omni', 'mcs_org'))



mc_p <- merge(
        mc[, lapply(.SD, unique), by=.(srv_addr), .SDcols=c("post_uid", "srv_max", "srv_max_log", "dataset_reddit", "dataset_omni", "dataset_mcs_org", "jubilees", "y", "ylog", "srv_repquery", "srv_repplug", "srv_repsample", "weeks_up_todate", "date_ping_int", "plugin_count", "keyword_count", "tag_count", "sign_count", 'norm_count' )],
        mc[, lapply(.SD, function(x) sum(x, na.rm=T)), by=.(srv_addr), .SDcols=grep("^cat_*", names(mc))], by="srv_addr", all=T)
#sposts <- splugins[,lapply(.SD, unique),by=.(post_uid, date_post), .SDcols=c(grep("^srv_*", names(splugins)), which(names(splugins) %in% c("dataset_source")), grep("*_count$", names(splugins)))]
### selection bias dealt with here
#mc_p <- (mc_p[ (!is.na(keyword_count) | !is.na(tag_count)) & !is.na(sign_count)])
#mc_p <- (mc_p[!is.na(sign_count)])
mc_p <- (mc_p[ !is.na(norm_count) & !is.na(plugin_count)])
mc_p[,':='(mcat_gov=sum(cat_admintools, cat_antigrief, cat_chat, cat_economy, cat_informational, cat_webadmin, cat_devtools, na.rm=T), mcat_play=sum(cat_fun, cat_general, cat_mechanics, cat_misc, cat_roleplay, cat_teleportation, cat_world, cat_fixes,  cat_worldgen, na.rm=T), mcat_all=sum(cat_admintools, cat_antigrief, cat_chat, cat_economy, cat_informational, cat_webadmin, cat_devtools, cat_fun, cat_general, cat_mechanics, cat_misc, cat_roleplay, cat_teleportation, cat_world, cat_fixes,  cat_worldgen, na.rm=T), mcat_admin=sum(cat_admintools, cat_webadmin, cat_devtools, cat_worldgen, na.rm=T), mcat_player=sum(cat_chat, cat_economy, cat_informational, cat_fun, cat_general, cat_mechanics, cat_misc, cat_roleplay, cat_teleportation, cat_world, na.rm=T), mcat_outside=sum( cat_webadmin, cat_devtools, cat_worldgen, na.rm=T), mcat_govcent=sum(cat_admintools, cat_antigrief, cat_webadmin, na.rm=T), mcat_govdecent=sum( cat_chat, cat_economy, cat_informational, na.rm=T), mcat_friv=sum(cat_fun, cat_general, cat_mechanics, cat_misc, cat_roleplay, cat_teleportation, na.rm=T), mcat_gameplay=sum(cat_fun, cat_general, cat_mechanics, cat_roleplay, cat_teleportation, cat_world,  cat_worldgen, na.rm=T), mcat_misc=sum(cat_devtools, cat_misc, cat_fixes,  na.rm=T) ), by=.(srv_addr)]




### define predictors
vars_non_model <- c(c("post_uid", "srv_addr", "srv_max", "srv_repquery", "srv_repplug", "srv_repsample", "dataset_reddit", "dataset_omni", "dataset_mcs_org", "keyword_count", "tag_count", "sign_count", 'mcat_play', 'mcat_gov', 'y'))
vars_out <- c('ylog')
vars_in_nonfeat <- c(c("srv_max_log", "date_ping_int", "weeks_up_todate", 'jubilees'), c("plugin_count", "norm_count"))
#vars_in_feat <-  names(mc_p)[which(names(mc_p) %ni% c(vars_non_model, vars_out, vars_in_nonfeat))] 
vars_in_feat <-  c("cat_admintools", "cat_antigrief", "cat_chat", "cat_economy", "cat_informational", "cat_webadmin", "mcat_gameplay", "mcat_misc")
interact_xsrv <- as.data.table(mc_p[,vars_in_feat,with=F][,apply(.SD, 2, function(x) x*mc_p$srv_max_log )])
vars_in_feat_xsrv <- paste("srvmax", vars_in_feat, sep='_')
names(interact_xsrv) <- vars_in_feat_xsrv
interact_xsign <- as.data.table(mc_p[,vars_in_feat,with=F][,apply(.SD, 2, function(x) x*mc_p$norm_count )])
vars_in_feat_xsign <- paste("norms", vars_in_feat, sep='_')
names(interact_xsign) <- vars_in_feat_xsign
interact_xsignxsrv <- as.data.table(mc_p[,vars_in_feat,with=F][,apply(.SD, 2, function(x) x*mc_p$norm_count*mc_p$srv_max_log )])
vars_in_feat_xsignxsrv <- paste("norms_srvmax", vars_in_feat, sep='_')
names(interact_xsignxsrv) <- vars_in_feat_xsignxsrv
mc_p <- cbind(mc_p, interact_xsrv, interact_xsign, interact_xsignxsrv)

mc_split <- splitDataTestTrain(mc_p, proportions=c(0.5, 0.5))
training <- mc_split$train
testing <- mc_split$test

training_full_lasso <- training
#training_full_lasso <- training_full_lasso[!is.na(ylog)]
#setnames(training_full_lasso, (ncol(training)+1):ncol(training_full_lasso), paste("srvmax", names(training_full_lasso[,(ncol(training)+1):ncol(training_full_lasso), with=F]) , sep='_'))
#training <- training[!is.na(ylog)]
#factor_counts <- colSums(training_full_lasso[,vars_in_feat,with=F]) + 0.00001
#factor_penalties <- c(rep(1, length(vars_in_nonfeat)), 1+feat_count_min/factor_counts, 1+feat_count_min/factor_counts )
mc_rlm_fit <- train( x = as.matrix(training_full_lasso[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv, vars_in_feat_xsign, vars_in_feat_xsignxsrv ),with=F])#[,1:300, with=F])
     , y = training_full_lasso$ylog
     #, y = log10(training_full_lasso$y+1)
     #form = y ~ .
     #, data = as.matrix(training_full)
     #, method = "penalized",
     #, method = "enet",
     , method = "glmnet",
     #, method = "lasso",
     #, method = "rlm"
     #, method = "rpart1SE"
     #, method = "rf"
     #, method = "lm"
     ## Center and scale the predictors for the training
     ## set and all future samples.
     #, preProc = c("zv", "nzv", "center", "scale")
     #, preProc = c("zv", "nzv")
     #, preProc = c("zv")
     , metric = "RMSE"
     #, tuneLength = 1
     #, tuneLength = 20
     #, verbose = T
     #, tuneGrid = expand.grid(fraction = c(0.01), lambda=0)
     #, tuneGrid = expand.grid(fraction = c(0.0001, 0.001, 0.01, (1:10)/10), lambda=0)
     #, tuneGrid = expand.grid(lambda1 = c( 1, 2, 5, 10, 20, 50, 100, 200, 500), lambda2=c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500))
     #, tuneGrid = expand.grid( lambda = c( 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100), alpha=c(0, 0.0001, 0.001 ,0.01,0.1, 0.2, 0.5, 1) )
     , tuneGrid = expand.grid( lambda = c( 0.2, 0.5, 1, 2, 5, 10), alpha=c(0.0001, 0.001 ,0.01,0.1, 1) )
     #, tuneGrid = expand.grid(lambda1 = c( 0.01, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100), lambda2=0)
     #, tuneGrid = expand.grid(fraction = c( 0.01, 0.1, 0.5, 1, 2, 5, 10, 100, 1000))
     #, tuneGrid = expand.grid(fraction = c( 0.01, 0.1, 0.5, 1, 2, 5, 10, 100, 1000))
     #, tuneGrid = function(len, data) {
                        #g = createGrid("lasso", len, data)
                        #g = expand.grid(.fraction=g$.fraction)
                        #return(g)
                                       #}
     #, trControl = trainControl(method = "none")
     #, trControl = trainControl(method = "cv", number=2)
     , trControl = trainControl(method = "repeatedcv" # Use cross-validation
                              , number = 5, repeats = 10
                              , preProcOptions=c(na.remove=T) # Use 5 folds for cross-validation
                              )
     ### glmnet parameters
     #, penalty.factor = factor_penalties
     #, nlambda = 30 
     , dfmax=100
     #, pmax=20
     #, lamdba=c(0.1, 1, 2,5, 10, 20, 50, 100, 200, 500, 1000)
     #, exclude = factors_exclude
)
mc_rlm_fit
coef_nonzero(mc_rlm_fit)

ff <- rlm(y ~ weeks_up_todate + date_ping_int + mcat_play*srv_max_log*norm_count + mcat_gov*srv_max_log*norm_count, data=training_full_lasso)
ff <- rlm(y ~ weeks_up_todate + date_ping_int + mcat_friv*srv_max_log*norm_count + mcat_admin*srv_max_log*norm_count + mcat_player*srv_max_log*norm_count + mcat_outside*srv_max_log*norm_count, data=training_full_lasso)
ff <- rlm(y ~ weeks_up_todate + date_ping_int + mcat_friv*srv_max_log*norm_count + mcat_govcent*srv_max_log*norm_count + mcat_govdecent*srv_max_log*norm_count, data=training_full_lasso)
ff <- rlm(y ~ weeks_up_todate + date_ping_int + mcat_friv*srv_max_log + mcat_govcent*srv_max_log + mcat_govdecent*srv_max_log,  data=training_full_lasso)

(mc_rlm_fit$finalModel$df)
lm_fit <- lm(training_full_lasso$y~1)
deviance(lm_fit)
logLik(lm_fit)
logLik_glmnet(mc_rlm_fit$finalModel, training_full_lasso$y)
deviance(mc_rlm_fit$finalModel)
mc_rlm_fit
glance(mc_rlm_fit$finalModel)
coef_nonzero(mc_rlm_fit)
coef(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$alpha)
coef(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$alpha)[which(coef(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$alpha) != 0)]
covTest(mc_rlm_fit)

dim(mc_p)
dim(training_full_lasso)
predict(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$lambda, type="nonzero")
predict(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$lambda, type="coefficients")
plot(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$lambda, xvar="lambda")

2^get_rmse_from_caret(mc_rlm_fit, as.matrix(training[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F]), training[,vars_out,with=F])
2^get_rmse_from_caret(mc_rlm_fit, as.matrix(training_full_lasso[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F]), training_full_lasso$y)
2^get_rmse_from_caret(mc_rlm_fit, as.matrix(mc_rlm_fit$trainingData)[,-which(names(mc_rlm_fit$trainingData)==".outcome")], mc_rlm_fit$trainingData$.outcome)
2^get_rmse_from_nulllm( lm_fit, training[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F], training[,vars_out,with=F] )

2^get_rmse_from_caret(mc_rlm_fit, as.matrix(testing[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F]), testing[,vars_out,with=F])
2^get_rmse_from_nulllm( lm_fit, testing[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F], testing[,vars_out,with=F] )

rr <- bootstrap(x=training[,vars_out,with=F]$y, nboot=200, theta=function(x, object, xdata){2^get_rmse_from_caret(object, xdata,x)}, object=mc_rlm_fit, xdata=as.matrix(training[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F]))
quantile99 <- function(x) {return(quantile(x, c(0.005, 0.995)))}
quantile99(rr$thetastar)
STOP

mc_glmnet_fit <- cv.glmnet( x = as.matrix(training_full_lasso[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F])#[,1:300, with=F])
     , y = training_full_lasso$y
     , nfolds=10
     , parallel = T
     , penalty.factor = factor_penalties
)
coef(mc_glmnet_fit$glmnet_fit, s=mc_glmnet_fit$bestTune$alpha)
predict(mc_glmnet_fit$glmnet_fit, s=mc_glmnet_fit$bestTune, type="nonzero")
predict(mc_glmnet_fit$glmnet_fit, newx=testing, s=mc_glmnet_fit$bestTune, type="coefficients")

### using penalized packages
#Anova(mc_rlm_fit$finalModel)
#(mc_rlm_fit$finalModel@penalized[mc_rlm_fit$finalModel@penalized != 0])

mc_rlm_fit <- train( form = y ~ .  , data = training_r , method = "rpart1SE" , metric = "RMSE" , trControl = trainControl(method = "cv" , number = 2)) 
mc_rlm_fit
mc_rlm_fit$finalModel

mc_fittest <- train( x = training_full_lasso[,-which(names(training_full_lasso) == "y"),with=F] , y = log10(training_full_lasso$y+1) , method = "lm" , metric = "RMSE" , trControl = trainControl(method = "none" ))
mc_fittest <- train( x = training_full_lasso[,-which(names(training_full_lasso) == "y"),with=F] , y = training_full_lasso$y , method = "lm" , metric = "RMSE" , trControl = trainControl(method = "none" ))
mc_fittest <- train( x = training_full_lasso[,-which(names(training_full_lasso) == "y"),with=F] , y = training_full_lasso$y , method = "glmnet" , tuneLength=1, metric = "RMSE" , trControl = trainControl(method = "none" ), family='poisson')
plot(mc_fittest$finalModel)


pred = predict(mc_rlm_fit , testing)
RMSE.test = RMSE(obs = testing$srv_max_log , pred = pred)


# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", number=1, repeats=1, savePredictions=TRUE)
algorithmList <- c( 'lm', 'rlm', 'enet', 'glmnet', 'penalized', 'lasso', "C5.0", 'svmRadial', 'pls')
models <- caretList(srv_max_log~., data=training[1:5000], trControl=control, methodList=algorithmList, preProc = c("zv", "nzv"))
results <- resamples(models)
summary(results)
dotplot(results)



### ifttig fitting distiriubtions
require(vcd)
require(MASS)
library(fitdistrplus)
#fitdistr(spings[,.N,by=srv_addr]$N, "exponential")
#descdist(spings[,.N,by=srv_addr]$N, discrete = F)
#descdist(spings[,.N,by=srv_addr]$N, discrete = T)
descdist(training_full_lasso[!is.na(y) & y>0,y], discrete = F)
descdist(training_full_lasso[!is.na(y) & y>0,y], discrete = T)




### processing
mc_feat_select <- mc_rlm_fit$finalModel@penalized[mc_rlm_fit$finalModel@penalized != 0]
mc_feat_data <- as.data.table(mc_rlm_fit$trainingData)
mc_feat_notableexclude <- names(which(colSums(mc_feat_data[,names(mc_rlm_fit$finalModel@penalized[mc_rlm_fit$finalModel@penalized == 0]),with=F]) > 50))
tmpmcp <- names(mc_feat_select)[-1]
tmpmcf <- sub("^srvmax_", '', tmpmcp)
tmpmcfcursenames <- tmpmcf
tmpmcfcursenames[tmpmcfcursenames=='griefprevention'] <- 'grief-prevention'
tmpmcfcursenames[tmpmcfcursenames=='enjinminecraftplugin'] <- 'emp'
tmpmcfcursenames[tmpmcfcursenames=='permissionsbukkit'] <- 'permbukkit'
tmpmcfcursenames[tmpmcfcursenames=='vanishnopacket'] <- 'vanish'
setkey(coded_key_keywords, feat)
setkey(coded_key_keywords_2, feat)
setkey(friv_coded_key_keywords, feat)
mc_feat <- data.table(pred=tmpmcp, feat=tmpmcf, beta=as.numeric(mc_feat_select[tmpmcp]), i_srvmax=grepl("^srvmax_", tmpmcp), nobs=colSums(mc_feat_data[,tmpmcf]), friv=tmpmcf %in% friv_coded_key_keywords$feat)
tmpcursestring <- system(command=paste0('/Users/sfrey/anaconda/envs/mcscraper/bin/python  ', pathLocal, 'cursemodstats.py'), input=tmpmcf, intern=T )
mc_feat_curse <- as.data.table(read.csv(textConnection(tmpcursestring), colClasses=c("character", "character", 'integer', 'character', 'integer'), na.strings='0'))
mc_feat <- mc_feat_curse[!duplicated(mc_feat_curse)][mc_feat,on='feat']
mc_feat <- cbind(mc_feat, coded_key_keywords[tmpmcf,-1,with=F])
mc_feat <- cbind(mc_feat, coded_key_keywords_2[tmpmcf,-1,with=F])
write.csv(mc_feat, paste0(pathLocal, "mc_feat.csv"))
print(paste0(pathLocal, "mc_feat.csv"))
system(paste0('open -a Numbers ', pathLocal, "mc_feat.csv"))
                         

enet.edf( mc_feat_data, names(mc_feat_select), mc_rlm_fit$finalModel@lambda2)
glmnet.edf( mc_rlm_fit$trainingData, names(mc_rlm_fit$trainingData)[coef(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$alpha)@i], mc_rlm_fit$bestTune$lambda, mc_rlm_fit$bestTune$alpha)

### from python function def curseCategoryStats():
mod_cats <- c('fixes'= 899, 'economy'= 1058, 'developer-tools'= 674, 'anti-griefing-tools'= 1236, 'world-generators'= 154, 'website-administration'= 196, 'world-editing-and-management'= 731, 'role-playing'= 1766, 'informational'= 2099, 'teleportation'= 1132, 'chat-related'= 2356, 'admin-tools'= 6069, 'miscellaneous'= 870, 'mechanics'= 3867, 'general'= 2612, 'fun'= 6237)



### long past variable selection play
dim(training_full_lasso)

training_cols <- training[,-pred_vars,with=F]
training_cols <- training_cols[,-findLinearCombos(training_cols)$remove, with=F]
dim(training_cols)
training_cols <- training_cols[,-findCorrelation(cor(training_cols), cutoff=0.90), with=F]
dim(training_cols)
training_cols <- training_cols[,-(nearZeroVar(training_cols, freqCut=98/2, uniqueCut=5, allowParallel=F)), with=F] 
dim(training_cols)
training <- training[,c(pred_vars, names(training_cols)),with=F]


### to troubleshoot rfe: ((function(x,y){ lmFuncs$rank(lmFuncs$fit(x,y,T,F), x, y)})(training[,-pred_vars,with=F], training$srv_max_log ))
### fix with training <- training[,groupmanager:=NULL]   
###  but first, if it breaks, try runningit again. 
mc_rfe1 <- rfe(
                  x = training[,-pred_vars,with=F] 
                , y = training$srv_max_log 
                , sizes = 1:8*5
                , metric = "RMSE"
                , rfeControl = rfeControl(functions=lmFuncs, rerank=T)
                , trControl = trainControl(method = "cv", number = 2)
             )
mc_rfe1$optVariables
pred_not_na <- !is.na(training$y)
mc_rfe2 <- rfe(
                  x = training[pred_not_na,-pred_vars,with=F] 
                , y = training$y[pred_not_na] 
                , sizes = 1:8*5
                , metric = "RMSE"
                , rfeControl = rfeControl(functions=lmFuncs, rerank=T)
                , trControl = trainControl(method = "cv", number = 2)
                 
             )
mc_rfe2$optVariables
#training_r <- as.data.table(mc_rfe$fit$model)
training_r <- training[pred_not_na,c('y', 'srv_max_log', union(mc_rfe1$optVariables, mc_rfe2$optVariables)), with=F]


