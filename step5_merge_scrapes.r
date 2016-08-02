pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_redditscrape.r"))
library(stringr)
library(mallet)
library(testthat)


### get sposts down to server/week or server/dataset
### start with server/week/plugin
splugins <- rbind(fread(paste0(pathData, 'step4_reddit_mcservers.csv')), fread(paste0(pathData, 'step4_omnimc_mcservers.csv')))
splugins[,':='(date_post=ymd(date_post))] ### get dates into the right format and columns in splugins
splugins[, feat:=tolower(feat)]
### careful here.  some servers were spotted under mutliple data sources.  prefer reddit over mcs over omni
g1 <- splugins[dataset_source=="omni",unique(srv_addr)]
g2 <- splugins[dataset_source=="mcs_org",unique(srv_addr)]
g3 <- splugins[dataset_source=="reddit",unique(srv_addr)]
splugins <- splugins[(dataset_source %in% c("mcs_org", "reddit")) | (dataset_source=="omni" & srv_addr %ni% intersect(g1, union(g2, g3)))]
splugins <- splugins[(dataset_source %in% c("omni", "reddit")) | (dataset_source=="mcs_org" & srv_addr %ni% intersect(g2, g3))]
#splugins <- splugins[srv_addr %ni% intersect(g1, union(g2, g3))]
### this'll hopefully change, but for now just count signs
splugins <- merge( splugins, splugins[feat_source=='plugin',list(plugin_count=.N),by=c("srv_addr")], by="srv_addr", all.x=T, all.y=F)
splugins <- merge( splugins, splugins[feat_source=='keyword',list(keyword_count=.N),by=c("srv_addr")], by="srv_addr", all.x=T, all.y=F)
splugins <- merge( splugins, splugins[feat_source=='tag',list(tag_count=.N),by=c("srv_addr")], by="srv_addr", all.x=T, all.y=F)
splugins <- merge( splugins, splugins[feat_source=='sign',list(sign_count=.N),by=c("srv_addr")], by="srv_addr", all.x=T, all.y=F)
splugins <- splugins[feat_source!='sign']
setkey(splugins, dataset_source, post_uid, date_post, srv_addr)

pluginstats <- as.data.table(read.csv(paste0(pathData, 'step45_curse_plugins_metadata_full.csv')))


#mc_forbid  <- splugins[feat %in% word_forbid[,1], unique(srv_addr)]
#head(mc_forbid)
### topic analysis.
###  then merge it into sserv
documents <- rbind(
                   read.table(paste0(pathData,"step4_reddit_texts.csv"), col.names=c("post_uid", "srv_addr", "dataset_source", "text"), colClasses=rep("character", 4), sep="\t", quote='"')
                 , read.table(paste0(pathData,"step4_mcs_org_texts.csv"), col.names=c("post_uid", "srv_addr", "dataset_source", "text"), colClasses=rep("character", 4), sep="\t", quote='"'))
#documents <- documents[documents$srv_addr %ni% mc_forbid,]
mallet.instances <- mallet.import(documents$srv_addr, documents$text, paste0(pathData,"step4_stopwords.txt"), token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
n_topics <- 4
topic.model <- MalletLDA(num.topics=n_topics)
topic.model$loadDocuments(mallet.instances)
#vocabulary <- topic.model$getVocabulary()
#word.freqs <- mallet.word.freqs(topic.model)
topic.model$setAlphaOptimization(20, 50)
#topic.model$train(1000)
topic.model$train(10)
topic.model$maximize(10)
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
topic.labels <- mallet.topic.labels(topic.model, topic.words, 3)
lapply(1:n_topics, function(x) mallet.top.words(topic.model, topic.words[x,]))
#documents[apply(doc.topics, 2, which.max),]
topic.labels <- mallet.topic.labels(topic.model, topic.words, 3)
#plot(mallet.topic.hclust(doc.topics, topic.words, 0.3))
#plot(mallet.topic.hclust(doc.topics, topic.words, 0.3), labels=topic.labels)
stopics <- data.table(documents[,1:3],doc.topics)
setnames(stopics, c("post_uid", "srv_addr", "dataset_source", paste0("T",1:n_topics)))
setkey(stopics, srv_addr)
stopics <- stopics[!srv_addr=='']


#get the datasets similar enough that they can be merged
### start with server/ping
spings <- readRDS(paste0(pathData, "step2_serversweeks.rds"))
setnames(spings, "server", "srv_addr")
spings[,':='(date_ping=yw(year, week))] ### get dates into the right format and columns in spings
spings[date_ping==max(date_ping), date_ping:=Sys.Date()]  ### for some reason, servers that go to-date read as going to far in the future
spings[,':='(ping_uid=str_c(srv_addr,format(date_ping, format='%Y%m%d'), sep='_'))]
### amount of time these servers were up, and their first and last weeks
spings[,':='(weeks_up=.N, date_ping_1st=min(date_ping), date_ping_lst=max(date_ping) ),by=c("srv_addr")]
spings[,':='(weeks_up_total=.N, weeks_up_todate=(date_ping-min(date_ping))/7, date_ping_1st=min(date_ping), date_ping_lst=max(date_ping) ),by=c("srv_addr")]

#require(vcd)
#require(MASS)
#library(fitdistrplus)
#fitdistr(spings[,.N,by=srv_addr]$N, "exponential")
#descdist(spings[,.N,by=srv_addr]$N, discrete = F)
#descdist(spings[,.N,by=srv_addr]$N, discrete = T)

### servers by week unique
sposts <- splugins[,lapply(.SD, unique),by=.(post_uid, date_post), .SDcols=c(grep("^srv_*", names(splugins)), which(names(splugins) %in% c("dataset_source")), grep("*_count$", names(splugins)))]
### I checked thoroughly, and this is the right place to measure for jubilees.  plugins has it, but only at the timescale of plugin scrapes
### this is an impoerfect measure of jubiles because lead could work as well as lag, and I can't figure out how to match the ping observation to this, and if I could, it's all so coarse that it probably wouldn't matter
setkey(sposts, srv_addr, date_post)
#sposts[,list(date_post, srv_v, shift(srv_v), jubilees=srv_v != shift(srv_v)),by=srv_addr]
sposts[, jubilees := (srv_v != shift(srv_v, fill=F)), by = srv_addr]

### do a rolling join, holy fucking hell
### this require explaining
spings[,':='(date_roll=date_ping)]
sposts[,':='(date_roll=date_post)]
sposts <- sposts[srv_repplug==T] ### this makes sure that the match below is to a server with plugin data, even if it is a bit further away than another measure without such data.  
setkey(spings, srv_addr, date_roll)
setkey(sposts, srv_addr, date_roll)
spings_m <- sposts[spings, roll="nearest"]
spings_m <- spings_m[,.(post_uid, ping_uid, date_post=date_post, date_ping=date_roll, srv_addr, srv_max=testnmaxquota, nmaxpop, pctmaxpop, nvisitsunobs=nvisits, nvisitsobs, nuvisits, genivisits, ncomm30visits, ncomm4visits, latency10ppl, latency20ppl, latency50pct, bestweek30visits, bestweek4visits, bghost, jubilees, srv_v, srv_max_bak=srv_max, srv_details, srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, dataset_source, weeks_up_total, weeks_up_todate, date_ping_1st, date_ping_lst, plugin_count, keyword_count, tag_count, sign_count)]
spings <- spings_m

### statistics and other NAs after this merge
spings[is.na(srv_repsample) & nvisitsobs==0,srv_repsample := FALSE ]
spings[is.na(srv_repsample) & nvisitsobs>0,srv_repsample := TRUE ]
spings[is.na(dataset_source), dataset_source:="omni"]
### and also ... this is obv ugly to have to even do, but data is data and it speaks truth
spings[!srv_repsample & nvisitsobs>0,srv_repsample := TRUE ]
spings[is.na(srv_repquery) & srv_repsample,srv_repquery := TRUE ]

### merge with topics
#spings <- spings[stopics, on=c("srv_addr")]
spings <- merge(spings, stopics[,!"post_uid",with=F], by=c('dataset_source','srv_addr'), all.x=T, all.y=F)
spings[, srv_reptopic:=ifelse(!is.na(T1), T, F)]

#get server/week down to server.  merge with posts, performance metrics, other things (topic data?)
### I'M COMMITTING HERE TO MY DEPENDEENT VARIABLE, so really everything from heredown should go ina  new file, since it isn't analysis agnostic
### keep developing spings a bit further
#setkey(spings, dataset_source, ping_uid, date_ping, srv_addr)
### reduce data down to one dependent type
sserv <- spings
sserv <- sserv[(bestweek4visits==T & bestweek30visits==T) | (is.na(bestweek4visits) & bestweek30visits==T)] ### get unique server rows, with a bias for the 4 visits measure over the 30visists measure
sserv[,':='(bestweek30visits=NULL, bestweek4visits=NULL, ncomm30visits=NULL, ncomm4visits=NULL)] ### erase these columsn to ge tthe ones from spings instead
sserv <- spings[bestweek4visits==T,.(srv_addr, ncomm4visits)][sserv,on=c("srv_addr")]
sserv <- sserv[!is.na(ncomm4visits)]
sserv <- sserv[srv_max >0]
sserv[,srv_max_log:=log10(srv_max)]

#spings <- spings[dataset_source=='reddit']
sserv[,lapply(list(srv_repstat, srv_repquery, srv_repplug, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]

### implictly, an important thing happening here is that null postuid means getting rid of pings that didn't match to plugin hauls
sfeat <- splugins[,list(post_uid, feat, feat_code, feat_type, feat_source, feat_trust, plugin_count, keyword_count, tag_count, sign_count)][sserv[!is.na(post_uid)], on=c("post_uid")]  ### CAREFUL!!!  the same IPs came from a few different data sources.  argg.
### sfeat[is.na(srv_repsample),srv_repsample:=i.srv_repsample]  ### be explicit about what gets mapped from splugins, or you'll get bad overrides
sfeat[,lapply(list(srv_repstat, srv_repquery, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source]
expect_true(sfeat[,length(unique(srv_addr))] == sfeat[,length(unique(srv_addr)), by=dataset_source][,sum(V1)])
expect_true(sfeat[,lapply(list(srv_repstat, srv_repquery, srv_repsample, srv_repsniff, srv_reptopic), sum, na.rm=T), by=dataset_source][3,V5] == 0)
### merge plugin properties into sfeat
#setnames(pluginstats, "build.y", "build")
sfeat <- merge(sfeat, pluginstats[build=='curse', list(feat, feat_source_coded = feat_source, feat_url = url, date_created, date_updated, dls_total, dls_recent, likes, cat_admintools, cat_antigrief, cat_chat, cat_devtools, cat_economy, cat_fixes, cat_fun, cat_general, cat_informational, cat_mechanics, cat_misc, cat_roleplay, cat_teleportation, cat_webadmin, cat_world, cat_worldgen)], all.x=T, all.y=F, by=c('feat'))
sfeat[,feat_count:=.N,by="feat_code"]


#save everyting
saveRDS(sserv, paste0(pathData, "step5_servers.rds"))
saveRDS(spings, paste0(pathData, "step5_serversweeks.rds"))
saveRDS(sfeat, paste0(pathData, "step5_serversweeksplugins.rds"))
#sfeat <- readRDS(paste0(pathData, "step5_serversweeksplugins.rds"))
#spings <- readRDS(paste0(pathData, "step5_serversweeks.rds"))
#sserv <- readRDS(paste0(pathData, "step5_servers.rds"))

### testing:
if(0){
    #ah hell.  first get number of unique serves in ping list: 61764
    spings[,length(unique(srv_addr))]
    #then u in reddit list: 1173
    sposts[,length(unique(srv_addr))]
    #then intersection: 295
    length(intersect(sposts[,unique(srv_addr)], spings[,unique(srv_addr)]))
    #then intersection of ping servers with community measuers: 171 or 291
    length(intersect(sposts[,unique(srv_addr)], spings[(ncomm4visits >= 0),unique(srv_addr)]))
    length(intersect(sposts[,unique(srv_addr)], spings[!(ncomm30visits == 0),unique(srv_addr)]))
    #the intersection of reddit servers with plugins reported: 288, well, actually, about 100
    length(intersect(sposts[!is.na(srv_repplug),unique(srv_addr)], spings[,unique(srv_addr)]))
    length(intersect(sposts[srv_repquery == T,unique(srv_addr)], spings[,unique(srv_addr)]))
    length(intersect(sposts[srv_repplug == T,unique(srv_addr)], spings[,unique(srv_addr)]))
    #then intersection of ping servers with community measuers and reddit servers with plugins reported.  That is what I should end up with.
    ### 44 or 80
    length(intersect(sposts[srv_repplug == T,unique(srv_addr)], spings[(ncomm4visits >= 0) ,unique(srv_addr)]))
    length(intersect(sposts[srv_repplug == T,unique(srv_addr)], spings[!(ncomm30visits == 0),unique(srv_addr)]))
}
