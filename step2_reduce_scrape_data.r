
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_db_redditscrape.r"))

library(lubridate)

make_db <- T
remake_visits_data <- T

tt <- timed()
### REDUCED DATASET: one row per server per week or per player per sever per week
### reset by week tables
if (make_db) {
    xx <- dbSendQuery(con, "DROP TABLE IF EXISTS playersweeks")
    xx <- dbSendQuery(con, "DROP TABLE IF EXISTS serversweeks")

    ### reduced dataset: one row per server per week
    ###   also initialize those columsn taht are cheap and easy to do with a simple group clause
    ###   FYI: nvisits is the number of visits this week, from players_online. it differs from nvisitsobs, which is the number of visits for which I have player ids
    server_by_week_query <- paste0("SELECT server, year, week, MAX(nquota) AS testnmaxquota, MIN(nquota) AS testnminquota, MAX(npop) AS nmaxpop, SUM(npop) AS nvisits, MAX(pctquota) AS pctmaxpop, SUM(CAST(hackedapi AS INTEGER)) > 0 AS hackedapi FROM serverspings GROUP BY server, year, week")
    #dbGetQuery(con, paste0(server_by_week_query, " ", "LIMIT 10"))
    xx <- dbSendQuery(con, paste0("CREATE TABLE serversweeks AS (", server_by_week_query,")"))
    xx <- dbSendQuery(con, "ALTER TABLE serversweeks ADD UNIQUE (server, year, week)")
    #dbGetQuery(con, "SELECT * from serverspings LIMIT 10")
    #dbGetQuery(con, "SELECT * from serverspings WHERE latn20 IS NOT NULL LIMIT 10")
    #dbGetQuery(con, "SELECT * from serversweeks LIMIT 10")
    #dbGetQuery(con, "SELECT COUNT(*) from serversweeks WHERE testnmaxquota != testnminquota")
    ### ADD COLUMNS at this scale
    ###  ***  median latency at 10 ppl
    ###  *** no sense getting this working without real values of latency.  when I get here, try this: http://blog.carbonfive.com/2013/01/17/postgresql-aggregates-medians-and-a-brief-command-reference/ or this https://wiki.postgresql.org/wiki/Aggregate_Median
    xx <- dbSendQuery(con, "ALTER TABLE serversweeks ADD COLUMN latency20ppl FLOAT DEFAULT NULL")
    xx <- dbSendQuery(con, paste0("WITH latencies AS 
                        (SELECT server, year, week, MAX(latn20) AS median_latency FROM 
                            ( SELECT server, year, week, npop, latn20, NTILE(2) OVER 
                                (PARTITION BY server, year, week ORDER BY latn20) AS two_buckets
                                FROM serverspings WHERE npop >= 20
                            ) AS inner1 WHERE two_buckets = 1 GROUP BY server, year, week
                        ) UPDATE serversweeks AS sw SET (latency20ppl) = (latencies.median_latency) 
                        FROM latencies WHERE sw.server = latencies.server AND sw.year = latencies.year AND sw.week = latencies.week"))
    xx <- dbSendQuery(con, "ALTER TABLE serversweeks ADD COLUMN latency10ppl FLOAT DEFAULT NULL")
    xx <- dbSendQuery(con, paste0("WITH latencies AS 
                        (SELECT server, year, week, MAX(latn10) AS median_latency FROM 
                            ( SELECT server, year, week, npop, latn10, NTILE(2) OVER 
                                (PARTITION BY server, year, week ORDER BY latn10) AS two_buckets
                                FROM serverspings WHERE npop >= 10
                            ) AS inner1 WHERE two_buckets = 1 GROUP BY server, year, week
                        ) UPDATE serversweeks AS sw SET (latency10ppl) = (latencies.median_latency) 
                        FROM latencies WHERE sw.server = latencies.server AND sw.year = latencies.year AND sw.week = latencies.week"))
    ###  ***   median latency at 75%
    xx <- dbSendQuery(con, "ALTER TABLE serversweeks ADD COLUMN latency50pct FLOAT DEFAULT NULL")
    xx <- dbSendQuery(con, paste0("WITH latencies AS 
                        (SELECT server, year, week, MAX(latpct50) AS median_latency FROM 
                            ( SELECT server, year, week, pctquota, latpct50, NTILE(2) OVER 
                                (PARTITION BY server, year, week ORDER BY latpct50) AS two_buckets
                                FROM serverspings WHERE pctquota >= 0.50
                            ) AS inner1 WHERE two_buckets = 1 GROUP BY server, year, week
                        ) UPDATE serversweeks AS sw SET (latency50pct) = (latencies.median_latency) 
                        FROM latencies WHERE sw.server = latencies.server AND sw.year = latencies.year AND sw.week = latencies.week"))

    ### THEN get server/player/ping data down to server/player/week
    ###   also initialize those columsn taht are cheap and easy to do with a simple group clause
    player_by_week_query <- paste0("SELECT server, uid, year, week, COUNT(*) AS nvisitsby FROM playerspings GROUP BY server, uid, year, week")
    xx <- dbGetQuery(con, paste0(player_by_week_query, " ", "LIMIT 10"))
    xx <- dbSendQuery(con, paste0("CREATE TABLE playersweeks AS (", player_by_week_query,")"))
    xx <- dbSendQuery(con, "ALTER TABLE playersweeks ADD UNIQUE (server, uid, year, week)")

    ### THEN start joining the server/player/week data into the server/week data
    #dbSendQuery(con, "ALTER TABLE serversweeks ADD COLUMN nvisits INTEGER DEFAULT 0")
    #dbSendQuery(con, "WITH pw AS (SELECT server, year, week, SUM(nvisits) AS nvisitsagg FROM playersweeks GROUP BY server, year, week)
                #UPDATE serversweeks AS sw SET nvisits = pw.nvisitsagg FROM pw WHERE sw.server = pw.server AND sw.year = pw.year AND sw.week = pw.week")
    ###    number of unique players who visited this week 
    #dbSendQuery(con, "WITH pw AS (SELECT server, year, week, COUNT(uid) AS nuvisitsagg FROM playersweeks GROUP BY server, year, week)
                #UPDATE serversweeks AS sw SET nuvisits = pw.nuvisitsagg FROM pw WHERE sw.server = pw.server AND sw.year = pw.year AND sw.week = pw.week")
    #dbGetQuery(con, "SELECT * FROM serversweeks LIMIT 10")
    ### calculate geni, and also the number of visits and the number of unique visists. this number of visits is different form the earlier one.  this is what i've measured and the other includes stuff I couldn't see at the player level (when servers reported players_online but an empty 'players' list.
    ### classic geni coefficient is a/a+b.  image a diagonal line and a curve sagging under it from its bottom up to it's top anchor.   b is the area under the sag and a is the area between the sag and the diagonal. In other words, total slack is total inequality, totally taught (on top of the diagonal) is total equality.  this changes a bit when you work with a discrete system, the diagonal becomes a set of stairs and you calculate a+b, the area under the stairs, as the area of a diagonal rest on the stairs minus the area divots of the stairs, which is 1*height of the stairs/2.  You calculate a as the sum of the cumulative sum of the number of visits, minus the number of visits.  That minus also also a discreteness correction that ensures the the minimum value of geni is zero.  that's because the staircase is shifted so that the steps up are as far to the right as possible, such that the cumulative sum never counts the person who made the most visists and thereby never reaches the max of the y axis, the total number of visits.  ### failure cases: 0 or 1 people on the server that week. NA for the first and the second.
    xx <- dbSendQuery(con, "ALTER TABLE serversweeks ADD COLUMN nvisitsobs INTEGER NOT NULL DEFAULT 0, ADD COLUMN nuvisits INTEGER NOT NULL DEFAULT 0, ADD COLUMN genivisits FLOAT DEFAULT NULL ")
    geni_calc_query <- "SELECT server, year, week, nvisitsto, nuvisitsto
                    , geninumerator::FLOAT/NULLIF((nvisitsto*nuvisitsto/2)-(nvisitsto/2), 0) AS genivisits
                    -- , geniwidth, geniheight, geninumerator, ((geniheight*geniwidth/2)-(geniheight/2)) AS genidenominator 
                    -- , ROUND(geninumerator::FLOAT/NULLIF((geniheight*geniwidth/2)-(geniheight/2), 0), 6) AS genivisits2
                    FROM (
                        SELECT server, year, week, SUM(nvisitsby) AS nvisitsto, COUNT(uid) AS nuvisitsto
                        , SUM(genicumvisits) - SUM(nvisitsby) AS geninumerator, COUNT(uid) AS geniwidth , MAX(genicumvisits) AS geniheight
                        FROM (
                                SELECT server, uid, year, week, nvisitsby, genivisitrank
                                , SUM(nvisitsby) OVER (PARTITION BY server, year, week ORDER BY genivisitrank ASC) AS genicumvisits 
                                FROM (
                                    SELECT server, uid, year, week, nvisitsby, 
                                    ROW_NUMBER() OVER (PARTITION BY server, year, week ORDER BY nvisitsby ASC) AS genivisitrank 
                                    FROM 
                                        playersweeks AS inner1
                                    ) AS inner2 ORDER BY server, year, week, genivisitrank ASC
                                ) AS inner3 GROUP BY server, year, week
                        ) AS inner4 ORDER BY server, year, week --LIMIT 30"
    xx <- dbSendQuery(con, paste0("WITH geni AS (   ", geni_calc_query ,"\n) UPDATE serversweeks AS sw SET (nvisitsobs, nuvisits, genivisits) = (geni.nvisitsto, geni.nuvisitsto, geni.genivisits) FROM geni WHERE sw.server = geni.server AND sw.year = geni.year AND sw.week = geni.week"))
}
#dbGetQuery(con, "SELECT * FROM serversweeks WHERE nvisitsobs>0 LIMIT 50")

if (remake_visits_data) {
    tt <- timed()
    ### now get Community
    ##      get the number of uplayers that showed up every week for one month server/week nComm 
            #comile list of all players in that week
            #take the intersections of the sets of players for a sliding window of four weeks over all data for all servers
            #output size of that set and thefirst week of that month
	     ### and in the event that a server was down for a week weeks and i didn't get data, just code that week as zero conservatively.
    pvisits <- dbGetQuery(con, "SELECT server, uid, year, week FROM playersweeks")
    pvisits <- data.table(pvisits)
    setkey(pvisits, server, year, week)
    pvisits <- pvisits[,.(uidLl=length(unique(uid)), uidList=list(unique(uid))), by=.(server, year, week)]
    pvisits[,':='(iL2=shift(.I, 1, type='lead'), iL3=shift(.I, 2, type='lead'), iL4=shift(.I, 3, type='lead')), by=.(server)]
    #pvisits[1:40,.(server, year, week, iL2, iL3, iL4, uidLl)]
    #pvisits[,regulars:=NULL]   ### this prevents annoying stuff about types and recasting column types which happens during development
    pvisits[,regulars:=NA_integer_]
    r_i <- which(names(pvisits) == "regulars")
    for (i in 1:nrow(pvisits)) {
        l1 <- pvisits[i,uidList[[1]] ]
        l2 <- pvisits[pvisits[i,iL2],uidList[[1]] ]
        l3 <- pvisits[pvisits[i,iL3],uidList[[1]] ]
        l4 <- pvisits[pvisits[i,iL4],uidList[[1]] ]
        if (!all(is.null(l4)) && !all(is.null(l3)) && !all(is.null(l2)) && !all(is.null(l1))) {
            set(pvisits, i, r_i, length(intersect(intersect(l4, l1), intersect(l2, l3))))
        }
    }
    tt <- timed()
    save(pvisits, file=paste0(pathData, "step2_pvisits.Rdata"))
} else {
    load(file=paste0(pathData, "step2_pvisits.Rdata"))
}

### fo some reason I have to refresh the conenction after this
#dbDisconnect(con)
#con <- dbConnect(drv, host='localhost', port='5432', dbname='mc')

    #dbSendQuery(con, "DROP TABLE serversweeks_community")
    xx <- dbSendQuery(con, "ALTER TABLE serversweeks DROP COLUMN IF EXISTS ncomm4visits")
    xx <- dbSendQuery(con, "ALTER TABLE serversweeks DROP COLUMN IF EXISTS ncomm30visits")
    xx <- dbSendQuery(con, "ALTER TABLE serversweeks DROP COLUMN IF EXISTS bestweek4visits")
    xx <- dbSendQuery(con, "ALTER TABLE serversweeks DROP COLUMN IF EXISTS bestweek30visits")
    xx <- dbSendQuery(con, "ALTER TABLE serversweeks DROP COLUMN IF EXISTS bghost")
    xx <- dbSendQuery(con, "DROP TABLE IF EXISTS servers")


### number of players who came mor than thirty times ina  month
xx <- dbSendQuery(con, "ALTER TABLE serversweeks ADD COLUMN ncomm30visits INTEGER NOT NULL DEFAULT 0, ADD COLUMN nvisits_month INTEGER NOT NULL DEFAULT 0, ADD COLUMN nuvisits_month INTEGER NOT NULL DEFAULT 0, ADD COLUMN bghost BOOLEAN DEFAULT TRUE")
xx <- dbSendQuery(con, paste0("WITH monthvisits AS 
                      (SELECT server, year, week, SUM(nvisitsbyinmonth) AS nvisitsbyinmonth, COUNT(nvisitsbyinmonth) AS nuvisitsbyinmonth, (COUNT(nvisitsbyinmonth)<=1) AS bghost, COUNT(nvisitsbyinmonth>=30) AS ncomm30visits FROM 
                          ( SELECT *, SUM(nvisitsby) OVER 
                              (PARTITION BY server, uid ORDER BY year, week ROWS BETWEEN CURRENT ROW AND 4 FOLLOWING) AS nvisitsbyinmonth 
                              FROM playersweeks
                          ) AS inner1 GROUP BY server, year, week
                      ) UPDATE serversweeks AS sw SET (ncomm30visits, nvisits_month, nuvisits_month, bghost) = (monthvisits.ncomm30visits, monthvisits.nvisitsbyinmonth, monthvisits.nuvisitsbyinmonth, monthvisits.bghost) 
                      FROM monthvisits WHERE sw.server = monthvisits.server AND sw.year = monthvisits.year AND sw.week = monthvisits.week"))

### get that back up and into postgres so I can do rolling window stuff next
xx <- dbSendQuery(con, "DROP TABLE IF EXISTS serversweeks_community")
xx <- dbWriteTable(con, "serversweeks_community", asdf(pvisits[,.(server, year, week, regulars)]))
xx <- dbSendQuery(con, "ALTER TABLE serversweeks ADD COLUMN ncomm4visits INTEGER DEFAULT NULL")
xx <- dbSendQuery(con, paste0(" UPDATE serversweeks AS sw SET (ncomm4visits) = (community.regulars) FROM serversweeks_community AS community
                         WHERE sw.server = community.server AND sw.year = community.year AND sw.week = community.week"))
xx <- dbSendQuery(con, "DROP TABLE IF EXISTS serversweeks_community")


### find the best week for each metric
### in the case that ncommmax IS NULL, mostly things are fine, but it happens sometime if a server was on and off over time, in which case its important to order values right so badness doesn't get randomized in to the data.
xx <- dbSendQuery(con, "ALTER TABLE serversweeks ADD COLUMN bestweek4visits BOOLEAN DEFAULT NULL")
xx <- dbSendQuery(con, paste0("WITH bestweek AS (SELECT DISTINCT ON (server) *, '4regvisits'::VARCHAR AS besttype, ncommmax = ncomm4visits AS bestweek 
                                           FROM ( SELECT *, MAX(ncomm4visits) OVER (PARTITION BY server ORDER BY year, week) AS ncommmax FROM serversweeks
                                           ) AS inner1 WHERE ncommmax = ncomm4visits OR ncommmax IS NULL ORDER BY server, ncommmax DESC NULLS LAST, RANDOM())
                        UPDATE serversweeks AS sw SET (bestweek4visits) = (bestweek.bestweek) FROM bestweek
                        WHERE sw.server = bestweek.server AND sw.year = bestweek.year AND sw.week = bestweek.week"))
xx <- dbSendQuery(con, "ALTER TABLE serversweeks ADD COLUMN bestweek30visits BOOLEAN DEFAULT FALSE")
xx <- dbSendQuery(con, paste0("WITH bestweek AS (SELECT DISTINCT ON (server) *, '30visits'::VARCHAR AS besttype, ncommmax = ncomm30visits AS bestweek 
                                           FROM ( SELECT *, MAX(ncomm30visits) OVER (PARTITION BY server ORDER BY year, week) AS ncommmax FROM serversweeks
                                           ) AS inner1 WHERE ncommmax = ncomm30visits OR ncommmax IS NULL ORDER BY server, ncommmax DESC NULLS LAST, RANDOM())
                        UPDATE serversweeks AS sw SET (bestweek30visits) = (bestweek.bestweek) FROM bestweek
                        WHERE sw.server = bestweek.server AND sw.year = bestweek.year AND sw.week = bestweek.week"))

### Before I thought I wanted to get down to server, but I ended up getting down to server/analysistype and now I want to get back up to server/week
### NOT DOING:  THEN get down from server/week to server
### NOT DOING: did at least one player show up every week for one month? server/week bComm #do previous and flip bit if set size is greater than one
### is this the week the first in the month in which the most players were regulars?  server/week maxComm
        ### take previous at the level of the server and flip bit for the highest week
xx <- dbSendQuery(con, paste0("CREATE TABLE servers AS SELECT DISTINCT ON (server) *
                        FROM ( SELECT *, MAX(ncomm4visits) OVER (PARTITION BY server ORDER BY year, week) AS ncommmax FROM serversweeks
                        ) AS inner1 WHERE ncommmax = ncomm4visits OR ncommmax IS NULL ORDER BY server, ncommmax DESC NULLS LAST, RANDOM()"))
#dbSendQuery(con, "ALTER TABLE servers ALTER COLUMN ncommmax SET NOT NULL")

### output the data to R and to file
servers <- data.table(dbGetQuery(con, "SELECT * FROM servers"))
saveRDS(servers, paste0(pathDataOut, "step2_servers_fortesting.rds"))
print(dim(servers))
rm(servers)
serversweeks <- data.table(dbGetQuery(con, "SELECT * FROM serversweeks"))
saveRDS(serversweeks, paste0(pathDataOut, "step2_serversweeks.rds"))
print(dim(serversweeks))
rm(serversweeks)
playersweeks <- data.table(dbGetQuery(con, "SELECT * FROM playersweeks"))
saveRDS(playersweeks, paste0(pathDataOut, "step2_playersweeks_fortesting.rds"))
print(dim(playersweeks))
rm(playersweeks)

#### prep servers files a bit more, for integration with reddit scrapes
### return date object from year and week integers
#yw <- function(year, week) {
    #return(ymd(paste0(year,"-01-01")) + dweeks(week))
#}
#reddit_datasets = c( "20141105" , "20150202" , "20150205" , "20150229" , "20150513" , "20150617" , "20150721" , "20150828" , "20150923" , "20151026" , "20151123" , "20151230")
### which reddit data is closest to the date of the ping?
#nn<- 5
#servers[sample(.N,nn), list(year, week, yw(year, week), reddit_datasets[apply(abs(matrix(rep(ymd(reddit_datasets), nn), nrow=nn, byrow=T)-as.integer(yw(year, week))), 1, which.min)])]
#servers[, data_date:=reddit_datasets[apply(abs(matrix(rep(ymd(reddit_datasets), .N), nrow=.N, byrow=T)-as.integer(yw(year, week))), 1, which.min)]]
#write.csv(servers[,], file.path(pathData, "step2_servers.csv"))





#SELECT COUNT(*) FROM (SELECT DISTINCT ON (server) *, '4regvisits'::VARCHAR AS besttype, ncommmax = ncomm4visits AS bestweek FROM ( SELECT *, MAX(ncomm4visits) OVER (PARTITION BY server ORDER BY year, week) AS ncommmax FROM serversweeks) AS inner1 ) AS inner2 WHERE ncommmax is NULL; 
#SELECT COUNT(*) FROM (SELECT DISTINCT ON (server) *, 'ncomm30visits'::VARCHAR AS besttype, ncommmax = ncomm30visits AS bestweek FROM ( SELECT *, MAX(ncomm30visits) OVER (PARTITION BY server ORDER BY year, week) AS ncommmax FROM serversweeks) AS inner1 ) AS inner2 WHERE ncommmax is NULL; 

dbDisconnect(con )
