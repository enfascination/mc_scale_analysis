### initialize globals
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_step6_analysis.r"))

### notes:
###  if there is lots of data 50/50 training/test is fine, and you shouldn't calculate full lasso paths (dfmax=50 or 100) and it's important to filter columns down before widening the matrix.  

mc <- readRDS(paste0(pathData, "step55_serversweeksplugins.rds"))
expect_true(mc[,length(unique(srv_addr))] == mc[,length(unique(post_uid))])
mc[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]
dim(mc)
### filtering for analysis
n_servers <- mc[,length(unique(srv_addr))]; n_servers 
feat_count_min <- max(2, as.integer(n_servers/5000))
mc <- filterDataSetDown(mc, cutUnrealistic=TRUE, cutNonVanilla=TRUE, cutNonPositiveDependent=TRUE, featureCountMin=feat_count_min, keepFeatTypes=c('plugin', 'tag', 'sign', 'property'), keepDataSource=c('reddit', 'omni', 'mcs_org'))
n_servers <- mc[,length(unique(srv_addr))]; n_servers 
dim(mc)

mc_w <- makeWideModelTable(mc)
dim(mc_w)

### define predictors
vars_non_model <- c(c("post_uid", "srv_addr", "srv_max", "dataset_reddit", "dataset_omni", "dataset_mcs_org", "keyword_count", "tag_count", 'y'))
vars_out <- c('ylog')
vars_in_nonfeat <- c(c("srv_max_log", "srv_repquery", "srv_repplug", "srv_repsample", "date_ping_int", "weeks_up_todate"), c("plugin_count", "sign_count"))
vars_in_feat <-  names(mc_w)[which(names(mc_w) %ni% c(vars_non_model, vars_out, vars_in_nonfeat))] ### added jubiliees (implicitly here) because I want to se interactions
mc_interactions <- as.data.table(mc_w[,vars_in_feat,with=F][,apply(.SD, 2, function(x) x*mc_w$srv_max_log )])
vars_in_feat_xsrv <- paste("srvmax", vars_in_feat, sep='_')
names(mc_interactions) <- vars_in_feat_xsrv
mc_w <- cbind(mc_w, mc_interactions)
dim(mc_w)

mc_split <- splitDataTestTrain(mc_w, proportions=c(0.5, 0.5))
training <- mc_split$train
testing <- mc_split$test
dim(training)

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
     , dfmax=100
     #, pmax=20
     #, lamdba=c(0.1, 1, 2,5, 10, 20, 50, 100, 200, 500, 1000)
     #, exclude = factors_exclude
)
mc_rlm_fit

(mc_rlm_fit$finalModel$df)
lm_fit <- lm(training_full_lasso$y~1)
deviance(lm_fit)
logLik(lm_fit)
logLik_glmnet <- function(model, y) {
    lm_fit <- lm(y~1)
    dev_null <- deviance(lm_fit)
    ll_null <- logLik(lm_fit)
    ll_sat <- 0.5*dev_null + ll_null
    dev_model <- tail(deviance(model),1)
    ll_model <- 0.5*(-dev_model+dev_null) + ll_null
    ll_model <- -0.5*dev_model + ll_sat
    dev_ratio <- tail(model$dev_ratio,1)
    dev_ratio <- 1 - dev_model/dev_null
    return(ll_model)
}
logLik_glmnet(mc_rlm_fit$finalModel, training_full_lasso$y)
deviance(mc_rlm_fit$finalModel)
mc_rlm_fit
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
get_rmse_from_nulllm <- function(fit, xdata, y) {
    comp <- cbind(predict(fit, newdata=xdata), y )
    mean(apply(comp, 1, function(x) (x[1] - x[2])^2))^0.5
}
get_rme_from_nulllm <- function(fit, xdata, y) {
    comp <- cbind(predict(fit, newdata=xdata), y )
    mean(apply(comp, 1, function(x) (x[1] - x[2])))
}
get_rmse_from_caret <- function(object, xdata, y) {
    #print(class(y))
    #print(class(object))
    #print(class(xdata))
    comp <- cbind(predict(object$finalModel, newx=xdata, s=object$bestTune$alpha, type="response"), y )
    #print(head(comp))
    mean(apply(comp, 1, function(x) (x[1] - x[2])^2))^0.5
    #return(head(comp))
}
get_rme_from_caret <- function(object, xdata, y) {
    comp <- cbind(predict(object$finalModel, newx=xdata, s=object$bestTune$alpha, type="response"), y )
    mean(apply(comp, 1, function(x) (x[1] - x[2])))
}
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
                         

### enet degrees of freedom from https://openaccess.leidenuniv.nl/bitstream/handle/1887/12096/04.pdf
enet.edf <- function(data, selected_variables, lambda2){
    tr <- function(m) sum(diag(m))
    A <- selected_variables
    X_A <- as.matrix(data[,A])
    evs <- eigen(t(X_A) %*% X_A, only.values=T)$values 
    
    #library(microbenchmark)
    #microbenchmark(
      #tr(X_A %*% solve(t(X_A) %*% X_A + lambda2 * diag(ncol(X_A))) %*% t(X_A))
    #, tr(t(X_A) %*% solve(t(t(X_A)) %*% t(X_A) + lambda2 * diag(nrow(X_A))) %*% X_A)
    #, sum(evs/(evs+lambda2))
    #)
    
    return(sum(evs/(evs+lambda2)))
}
enet.edf( mc_feat_data, names(mc_feat_select), mc_rlm_fit$finalModel@lambda2)
glmnet.edf <- function(data, selected_variables, lambda, alpha){
    ###alpha is the [0,1] mixing parameter
    lambda1 <-     alpha*lambda
    lambda2 <- (1-alpha)*lambda
    tr <- function(m) sum(diag(m))
    A <- selected_variables
    X_A <- as.matrix(data[,A])
    evs <- eigen(t(X_A) %*% X_A, only.values=T)$values 
    
    #library(microbenchmark)
    #microbenchmark(
      #tr(X_A %*% solve(t(X_A) %*% X_A + lambda2 * diag(ncol(X_A))) %*% t(X_A))
    #, tr(t(X_A) %*% solve(t(t(X_A)) %*% t(X_A) + lambda2 * diag(nrow(X_A))) %*% X_A)
    #, sum(evs/(evs+lambda2))
    #)
    
    return(sum(evs/(evs+lambda2)))
}
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


