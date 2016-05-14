#source('/Users/sfrey/projecto/research_projects/minecraft/server_surveyor/step5_plot_population_scaling.r')
library(ggplot2)
library(gridExtra)
library(ggthemes)
`%ni%` = Negate(`%in%`) ### help functions
options(gsubfn.engine = "R")
options(sqldf.driver= "SQLite")
library(sqldf)
library(data.table)
library(dplyr)
library(pls) ### pcr
library(stringr)
library(MASS)
library(testthat)
library(microbenchmark)

library(reshape2)
library(caret)
library(caretEnsemble)
library(mlbench)
library(randomForest)
library(doMC)
registerDoMC(cores = 8)
#library(pROC)
#library(sfsmisc)
library(rms)

### initialize globals
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_redditscrape.r"))
source(paste0(pathLocal,"plugin_classes.r"))

mc <- readRDS(paste0(pathData, "step5_serversweeksplugins.rds"))
expect_true(mc[,length(unique(srv_addr))] == mc[,length(unique(post_uid))])
mc[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_reptopic), sum, na.rm=T), by=dataset_source]


### preprocessing
#mc[srv_max > 5000,srv_max:=4999]
mc <- mc[srv_max <= 5000]
mc[,srv_max_bak:=srv_max]
mc[,srv_max:=log10(srv_max)]

mc_whitelist <- mc[feat %in% whitelist, unique(srv_addr)]
mc_forbid  <- mc[feat %in% word_forbid[,1], unique(srv_addr)]
mc_coded  <- mc[feat %in% coded_key_keywords_2[,feat], unique(srv_addr)]
mc <- mc[(srv_addr %ni% mc_forbid) ,]
#mc <- mc[(srv_addr %in% mc_coded) ,]

mc <- mc[srv_max >0]
mc[,xxx:=999] ### this is bs I don't want to have to deal with.  keeps an error from getting thrwon and wierd spuriousnesses from being hard to catch.

id_vars <- c("post_uid", "srv_addr")  ### these names should be the same as in the forumla below, and be updated all the time with every change, or exle everything will be badness and bad
pred_vars <- c("srv_max", "ncomm4visits")  ### these names should be the same as in the forumla below, and be updated all the time with every change, or exle everything will be badness and bad
mc_w <- as.data.table(dcast(formula=post_uid + srv_addr + srv_max + ncomm4visits ~ feat, data=mc, value.var="xxx", fun.aggregate = function(x) (length(x) > 0) + 0.0))
mc_w <- mc_w[,(c(id_vars, pred_vars, names(which(colSums(mc_w[,-(which(names(mc_w) %in% c(id_vars, pred_vars))), with=F]) > 9)))), with=F] ### get rid of features that occur only once  ### there are wierd interactions here with the nzv filtering below.  lower threshold (currently 5) raises the freqCut ratio and counterintuitively increases the number of columns that get censored out
mc_w[,':='(post_uid=NULL, srv_addr=NULL)]




setnames(mc_w, "ncomm4visits", "y")
pred_vars[2] <- "y"






set.seed(42)
#inTrain <- createDataPartition(y = mc_w$y, p = .6, list = FALSE)  ### can't handle NAs
inTrain <- sample(1:nrow(mc_w), 0.6*nrow(mc_w), replace=F)
training <- mc_w[ inTrain,]
testing <- mc_w[-inTrain,]
dim(training)

training_full_lasso <- cbind(training, training[,-pred_vars,with=F][,apply(.SD, 2, function(x) x*training$srv_max)])
training_full_lasso <- training_full_lasso[!is.na(y)]
setnames(training_full_lasso, (ncol(training)+1):ncol(training_full_lasso), paste("srvmax", names(training_full_lasso[,(ncol(training)+1):ncol(training_full_lasso), with=F]) , sep='_'))

training_cols <- training[,-pred_vars,with=F]
training_cols <- training_cols[,-findLinearCombos(training_cols)$remove, with=F]
dim(training_cols)
training_cols <- training_cols[,-findCorrelation(cor(training_cols), cutoff=0.90), with=F]
dim(training_cols)
training_cols <- training_cols[,-(nearZeroVar(training_cols, freqCut=98/2, uniqueCut=5, allowParallel=F)), with=F] 
dim(training_cols)
training <- training[,c(pred_vars, names(training_cols)),with=F]


### to troubleshoot rfe: ((function(x,y){ lmFuncs$rank(lmFuncs$fit(x,y,T,F), x, y)})(training[,-pred_vars,with=F], training$srv_max ))
### fix with training <- training[,groupmanager:=NULL]   
###  but first, if it breaks, try runningit again. 
mc_rfe1 <- rfe(
                  x = training[,-pred_vars,with=F] 
                , y = training$srv_max 
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
training_r <- training[pred_not_na,c('y', 'srv_max', union(mc_rfe1$optVariables, mc_rfe2$optVariables)), with=F]


#training <- training[!is.na(y)]

mc_rlm_fit <- train( x = training_full_lasso[,-which(names(training_full_lasso) == "y"),with=F]#[,1:300, with=F])
     , y = training_full_lasso$y
     #, y = log10(training_full_lasso$y+1)
     #form = y ~ .
     #, data = as.matrix(training_full)
     , method = "penalized",
     #, method = "enet",
     #, method = "lasso",
     #, method = "rlm"
     #, method = "rpart1SE"
     #, method = "rf"
     #, method = "lm"
     ## Center and scale the predictors for the training
     ## set and all future samples.
     #, preProc = c("zv", "nzv", "center", "scale")
     #, preProc = c("zv", "nzv")
     , metric = "RMSE"
     #, tuneLength =10
     #, verbose = T
     #, tuneGrid = expand.grid(fraction = c(0.01), lambda=0)
     #, tuneGrid = expand.grid(fraction = c(0.0001, 0.001, 0.01, (1:10)/10), lambda=0)
     , tuneGrid = expand.grid(lambda1 = c( 1, 2, 5, 10, 20, 50, 100, 200, 500), lambda2=c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500))
     #, tuneGrid = expand.grid(lambda1 = c( 0.01, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100), lambda2=0)
     #, tuneGrid = expand.grid(fraction = c( 0.01, 0.1, 0.5, 1, 2, 5, 10, 100, 1000))
     #, tuneGrid = function(len, data) {
                        #g = createGrid("lasso", len, data)
                        #g = expand.grid(.fraction=g$.fraction)
                        #return(g)
                                       #}
     #, trControl = trainControl(method = "none")
     , trControl = trainControl(method = "repeatedcv" # Use cross-validation
                              , number = 2, repeats = 5
                              #, preProcOptions=c(na.remove=T) # Use 5 folds for cross-validation
                              )
)
mc_rlm_fit
mc_rlm_fit$finalModel
Anova(mc_rlm_fit$finalModel)
(mc_rlm_fit$finalModel@penalized[mc_rlm_fit$finalModel@penalized != 0])

mc_rlm_fit <- train( form = y ~ .  , data = training_r , method = "rpart1SE" , metric = "RMSE" , trControl = trainControl(method = "cv" , number = 2)) 
mc_rlm_fit
mc_rlm_fit$finalModel

mc_fittest <- train( x = training_full_lasso[,-which(names(training_full_lasso) == "y"),with=F] , y = log10(training_full_lasso$y+1) , method = "lm" , metric = "RMSE" , trControl = trainControl(method = "none" ))
mc_fittest <- train( x = training_full_lasso[,-which(names(training_full_lasso) == "y"),with=F] , y = training_full_lasso$y , method = "lm" , metric = "RMSE" , trControl = trainControl(method = "none" ))
mc_fittest <- train( x = training_full_lasso[,-which(names(training_full_lasso) == "y"),with=F] , y = training_full_lasso$y , method = "glmnet" , tuneLength=1, metric = "RMSE" , trControl = trainControl(method = "none" ), family='poisson')
plot(mc_fittest$finalModel)


pred = predict(mc_rlm_fit , testing)
RMSE.test = RMSE(obs = testing$srv_max , pred = pred)


# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", number=1, repeats=1, savePredictions=TRUE)
algorithmList <- c( 'lm', 'rlm', 'penalized', 'lasso', "C5.0", 'svmRadial', 'pls')
models <- caretList(srv_max~., data=training, trControl=control, methodList=algorithmList, preProc = c("zv", "nzv"))
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

### from python function def curseCategoryStats():
mod_cats <- c('fixes'= 899, 'economy'= 1058, 'developer-tools'= 674, 'anti-griefing-tools'= 1236, 'world-generators'= 154, 'website-administration'= 196, 'world-editing-and-management'= 731, 'role-playing'= 1766, 'informational'= 2099, 'teleportation'= 1132, 'chat-related'= 2356, 'admin-tools'= 6069, 'miscellaneous'= 870, 'mechanics'= 3867, 'general'= 2612, 'fun'= 6237)

