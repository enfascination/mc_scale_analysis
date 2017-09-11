### this code (which doesn't work well so I'm not actually using it) detects undetected hacked servers by learning the features of known hacked servers.  I don't know unhacked serves, so it relies on one-class learning, which isn't any good. Dead end.

pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_redditscrape.r"))


### LEARN AHCKEDAPI IN UNCAUGHT CASES
library(e1071)
library(ROCR)
library(caret)
library(gdata)
library(fitdistrplus)

### HELPERS
forget.na <- function(DT, val) { 
    ### https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
  # or by number (slightly faster than by name) :
  for (j in seq_len(ncol(DT))) {
    set(DT,which(is.na(DT[[j]])),j,val)
    set(DT,which(is.infinite(DT[[j]])),j,val)
  }
}
heckafits <- function(v) { return( c(fitdistr(v, "normal")$estimate, fitdistr(v+1, "log-normal")$estimate, fitdistr(v, "poisson")$estimate, -fitdistr(v, "normal")$loglik, -fitdistr(v+1, "log-normal")$loglik, -fitdistr(v, "poisson")$loglik ) ) }

### load up data obje
spings <- readRDS(paste0(pathData, "step5_serversweeks.rds"))

### server level learnigns
### prep server level object
spings[srv_addr %in% spings[hackedapi == TRUE,unique(srv_addr)], hackedapi:=TRUE]
hh <- spings[, lapply(.SD, unique), by=.(srv_addr), .SDcols=c("dataset_source", "weeks_up_total", "srv_retired", "jubilees", "plugin_count", "keyword_count", "tag_count", "sign_count", "T1", "T2", "T3", "T4", "hackedapi")]
ff <- spings[, .(srv_details1=max(srv_details), srv_details2=min(srv_details), srv_maxn=length(srv_max), srv_max=max(srv_max), srv_repquery=max(srv_repquery), srv_repplug=max(srv_repplug), srv_repsample=max(srv_repsample), npings=.N, nmaxpoptotal=sum(nmaxpop, na.rm=T), nmaxpop2=min(nmaxpop, na.rm=T), ncomm30visitstotal=sum(ncomm30visits, na.rm=T), nuvisits12=sum(nuvisits12, na.rm=T), nvisitsobs12=sum(nvisitsobs12, na.rm=T),nuvisits52=sum(nuvisits52, na.rm=T), nvisitsobs52=sum(nvisitsobs52, na.rm=T), nvisitsunobstotal=sum(nvisitsunobs, na.rm=T), nvisitsunobs2=min(nvisitsunobs, na.rm=T), pctmaxpop=mean(pctmaxpop, na.rm=T), genivisits=mean(genivisits, na.rm=T), srv_v=srv_v[1], srv_votes=sum(srv_votes, na.rm=T), bghost=max(as.numeric(bghost))), by=.(srv_addr)]
nFits <- 8*3
gg <- dcast(cbind(spings[,ifelse(.N>3,list(c(heckafits(nvisitsunobs), heckafits(nmaxpop), heckafits(ncomm30visits))),list(rep(-1, nFits))), by=srv_addr], fit=1:nFits), srv_addr ~ fit, value.var="V1")### fits of visist patterns
setnames(gg, paste(1:24), paste("f", 1:24,sep=""))
ssrv <- merge( hh, ff, by="srv_addr", all=T)
ssrv <- merge( ssrv, gg , by="srv_addr", all=T)
#dim(ssrv); spings[,length(unique(srv_addr))]
### prep for learning
ssrv[,hackedapiLearn:=hackedapi]
ssrv$HackedClass[ssrv$hackedapi] <- "TRUE"
ssrv$HackedClass[!ssrv$hackedapi] <- "FALSE"
trainPositiveRows<- which(ssrv$HackedClass=="TRUE")
trainPositive<-ssrv[trainPositiveRows]
inTrain<-trainPositiveRows[createDataPartition(trainPositiveRows,p=0.6,list=FALSE)]
inValidation<-sample(1:nrow(ssrv), round(nrow(ssrv)*0.2))
inValidation<-inValidation[inValidation %ni% inTrain]
predictorsrv <- copy(ssrv)[,':='(dataset_source=as.numeric(factor(dataset_source)), srv_max=log2(srv_max+2), srv_v=as.numeric(factor(srv_v)), srv_retired=as.numeric(srv_retired), srv_addr_f1=as.numeric(grepl(":", srv_addr)), srv_addr_f2=as.numeric(grepl("[[:alpha:]]", srv_addr)), srv_addr=NULL, hackedapi=NULL, HackedClass=NULL, hackedapiLearn=NULL)]
#colSums(is.na(predictorsrv)) #str(predictorsrv)
forget.na(predictorsrv, -1)
trainpredictors<-predictorsrv[inTrain,]
testpredictors<-predictorsrv[,]
testLabels<-ssrv[,HackedClass]
if(0) {
    testpredictors<-predictorsrv[inValidation,] ### inValidation already expluedes inTrain in definition
    testLabels<-ssrv[inValidation,HackedClass] 
    for (k in c("radial", "polynomial", "sigmoid", "linear")) {
        print(k)
        if (k=="sigmoid") {
        nus = c(0.001, 0.002, 0.01, 0.02, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999)
        } else {
        nus = c(0.00001, 0.00002, 0.0001, 0.0002, 0.001, 0.002, 0.01, 0.02, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999)
        }
        for(i in nus) {
            print(i)
            svm.model<-svm(trainpredictors,y=NULL,
                           type='one-classification',
                           nu=i,
                           scale=TRUE,
                           kernel=k)
            svm.pred<-predict(svm.model,testpredictors)
            confusionMatrixTable<-table(Predicted=svm.pred,Reference=testLabels)
            confusionMatrix(confusionMatrixTable,positive='TRUE')
            print((confusionMatrix(confusionMatrixTable,positive='TRUE')$overall))
            print((confusionMatrix(confusionMatrixTable,positive='TRUE')$table))
            print("")
        }
    }
} 
ssrv[,hackedapiLearn:=svm.pred]


svm.roc <- prediction(as.numeric(svm.pred), as.numeric(as.logical(testLabels))) 
svm.auc <- performance(svm.roc, 'tpr', 'fpr') 
aucsvm <- performance(svm.roc, 'auc')
plot(svm.auc, col = 2, main="ROC curves comparing classification performance of three machine learning models")
plot(performance(svm.roc, 'fnr', 'fpr'), col = 2, main="DET")

PRINT("### DO A JOIN (i DIDN'T CHECK THIS CODE SO i ONLY THINK IT WORKS")
spings <- spings[ssrv[,.(srv_addr, hackedapiLearn)],]

#save everyting
saveRDS(spings, paste0(pathData, "step5_serversweeks.rds"))
