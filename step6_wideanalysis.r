### initialize globals
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_step6_analysis.r"))

library(covTest)

### notes:
###  if there is lots of data 50/50 training/test is fine, and you shouldn't calculate full lasso paths (dfmax=50 or 100) and it's important to filter columns down before widening the matrix.  

mc <- readRDS(paste0(pathData, "step55_serversweeksplugins.rds"))
expect_true(mc[,length(unique(srv_addr))] == mc[,length(unique(post_uid))])
mc[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]
dim(mc)
### filtering for analysis
n_servers <- mc[,length(unique(srv_addr))]; n_servers 
feat_count_min <- max(2, as.integer(n_servers/5000))
mc <- filterDataSetDown(mc, cutUnrealistic=TRUE, cutNonVanilla=TRUE, cutNonPositiveDependent=TRUE, featureCountMin=feat_count_min, keepFeatTypes=c('plugin', 'property'), keepDataSource=c('reddit', 'omni', 'mcs_org'))
nsrv <- mc[,length(unique(srv_addr))]; nsrv 
dim(mc)


### define predictors
vars_non_model <- c(c("post_uid", "srv_addr", "srv_max", "srv_max_bak", "srv_repquery", "srv_repplug", "srv_repsample", "dataset_omni", "keyword_count", "tag_count", "sign_count", "date_ping_1st", "date_ping_lst", "weeks_up_total", "srv_votes", "y"))
vars_in_nonfeat <- c(c("srv_max_log", "date_ping_int", 'jubilees'), c("dataset_reddit", "dataset_mcs_org"))
vars_out <- c('ylog')
vars_in_feat_special <- c("log_plugin_count", "weeks_up_todate") ### these should be added explicitly, not implicitly
vars_server_level <- c(vars_non_model, vars_out, vars_in_nonfeat, vars_in_feat_special)

mc_w <- makeWideModelTable(mc, vars_server_level)
expect_equal(nrow(mc_w), nsrv) ### otherwise, vars_server_level has too many variables (and some variables that vary within server)

vars_in_feat <-  c(vars_in_feat_special, names(mc_w)[which(names(mc_w) %ni% vars_server_level)])
mc_interactions <- as.data.table(mc_w[,vars_in_feat,with=F][,apply(.SD, 2, function(x) x*mc_w$srv_max_log )])
vars_in_feat_xsrv <- paste("srvmax", vars_in_feat, sep='_')
names(mc_interactions) <- vars_in_feat_xsrv
mc_w <- cbind(mc_w, mc_interactions)
dim(mc_w)

mc_split <- splitDataTestTrain(mc_w, proportions=c(0.4, 0.3, 0.3), validation_set=TRUE)
training <- mc_split$train
validate <- mc_split$validate
testing <- mc_split$test

training_full_lasso <- training
#training_full_lasso <- training_full_lasso[!is.na(ylog)]
#setnames(training_full_lasso, (ncol(training)+1):ncol(training_full_lasso), paste("srvmax", names(training_full_lasso[,(ncol(training)+1):ncol(training_full_lasso), with=F]) , sep='_'))
#training <- training[!is.na(ylog)]
factor_counts <- colSums(training_full_lasso[,vars_in_feat,with=F]) + 0.00001
factor_penalties <- c(rep(1, length(vars_in_nonfeat)), 1+feat_count_min/factor_counts, 1+feat_count_min/factor_counts )
mc_rlm_fit <- train( x = as.matrix(training_full_lasso[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F])#[,1:300, with=F])
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
     , penalty.factor = factor_penalties
     #, nlambda = 30 
     , dfmax=200
     #, pmax=20
     #, lamdba=c(0.1, 1, 2,5, 10, 20, 50, 100, 200, 500, 1000)
     #, exclude = factors_exclude
)
mc_rlm_fit
coef_nonzero(mc_rlm_fit)

### merge with plugin codes
coefs <- coef_codable(mc_rlm_fit)
plugin_codes <- get_plugin_codes()
coefs$all[coefs$all %ni% plugin_codes$feat_code]
plugin_codes[feat_code %in% coefs$basic$positive, colSums(.SD[,4:ncol(.SD),with=F])] - plugin_codes[feat_code %in% coefs$basic$negative, colSums(.SD[,4:ncol(.SD),with=F])]
plugin_codes[feat_code %in% coefs$xsrv$positive, colSums(.SD[,4:ncol(.SD),with=F])] - plugin_codes[feat_code %in% coefs$xsrv$negative, colSums(.SD[,4:ncol(.SD),with=F])]

(mc_rlm_fit$finalModel$df)
lm_fit <- lm(training_full_lasso$y~1)
deviance(lm_fit)
logLik(lm_fit)
logLik_glmnet(mc_rlm_fit$finalModel, training_full_lasso$y)
deviance(mc_rlm_fit$finalModel)
glance(mc_rlm_fit$finalModel)
coef_nonzero(mc_rlm_fit)
coef(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$alpha)
coef(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$alpha)[which(coef(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$alpha) != 0)]
covTest(mc_rlm_fit)

dim(mc_w)
dim(training_full_lasso)
predict(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$lambda, type="nonzero")
predict(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$lambda, type="coefficients")
plot(mc_rlm_fit$finalModel, s=mc_rlm_fit$bestTune$lambda, xvar="lambda")

2^get_rmse_from_caret(mc_rlm_fit, as.matrix(training[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F]), training[,vars_out,with=F])
2^get_rmse_from_caret(mc_rlm_fit, as.matrix(training_full_lasso[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F]), training_full_lasso$y)
2^get_rmse_from_caret(mc_rlm_fit, as.matrix(mc_rlm_fit$trainingData)[,-which(names(mc_rlm_fit$trainingData)==".outcome")], mc_rlm_fit$trainingData$.outcome)
2^get_rmse_from_nulllm( lm_fit, training[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F], training[,vars_out,with=F] )

2^get_rmse_from_caret(mc_rlm_fit, as.matrix(validate[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F]), validate[,vars_out,with=F])
2^get_rmse_from_nulllm( lm_fit, validate[,c(vars_in_nonfeat, vars_in_feat, vars_in_feat_xsrv),with=F], validate[,vars_out,with=F] )

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
predict(mc_glmnet_fit$glmnet_fit, newx=validate, s=mc_glmnet_fit$bestTune, type="coefficients")

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


pred = predict(mc_rlm_fit , validate)
RMSE.test = RMSE(obs = validate$srv_max_log , pred = pred)


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


