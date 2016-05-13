
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_db_redditscrape.r"))

remake_db <- F
make_db <- T

tt <- timed()

if (make_db) {
    ### scrubbing out old data
    if (remake_db) {
        system("/Applications/Postgres.app/Contents/Versions/9.3/bin/psql -f step05_prep_postgres.sql")
        con <- dbConnect(drv, host='localhost', port='5432', dbname='mc')
    }
    if (0){ dbClearResult(dbListResults(con)[[1]]) }
    dbSendQuery(con, "DROP TABLE IF EXISTS serverspings")
    dbSendQuery(con, "DROP TABLE IF EXISTS playerspings")

    ### early prep of server/ping data
    dserv <- fread(paste0(pathData, "step0_server_visits.csv"))
    setnames(dserv, "timestamp", "thetimestamp")
    dserv <- unique(dserv, by=c("thetimestamp", "server"))
    dbWriteTable(con, "serverspings", asdf(dserv))
    dbSendQuery(con, "ALTER TABLE serverspings ALTER COLUMN thetimestamp SET DATA TYPE TIMESTAMP WITHOUT TIME ZONE USING thetimestamp::TIMESTAMP, ALTER COLUMN server SET NOT NULL, ALTER COLUMN nquota SET NOT NULL, ALTER COLUMN npop SET NOT NULL")
    dbSendQuery(con, "ALTER TABLE serverspings ADD UNIQUE (thetimestamp, server)")
    ###    week column
    dbSendQuery(con, "ALTER TABLE serverspings ADD COLUMN year INTEGER, ADD COLUMN week INTEGER")
    dbSendQuery(con, "UPDATE serverspings SET year = EXTRACT(YEAR FROM thetimestamp)")
    dbSendQuery(con, "UPDATE serverspings SET week = EXTRACT(WEEK FROM thetimestamp)")
    dbSendQuery(con, "ALTER TABLE serverspings ALTER COLUMN year SET NOT NULL, ALTER COLUMN week SET NOT NULL")
    ### early prep of player/ping data.  theignore it for a while
    dplay <- fread(paste0(pathData, "step0_player_visits.csv"))
    setnames(dplay, "timestamp", "thetimestamp")
    dplay <- unique(dplay, by=c("thetimestamp", "server", "uid"))
    dbWriteTable(con, "playerspings", asdf(dplay))
    dbSendQuery(con, "ALTER TABLE playerspings ALTER COLUMN thetimestamp SET DATA TYPE TIMESTAMP WITHOUT TIME ZONE USING thetimestamp::TIMESTAMP, ALTER COLUMN server SET NOT NULL, ALTER COLUMN uid SET NOT NULL")
    dbSendQuery(con, "ALTER TABLE playerspings ADD UNIQUE (thetimestamp, server, uid)")
    ###    week column
    dbSendQuery(con, "ALTER TABLE playerspings ADD COLUMN year INTEGER, ADD COLUMN week INTEGER")
    dbSendQuery(con, "UPDATE playerspings SET year = EXTRACT(YEAR FROM thetimestamp)")
    dbSendQuery(con, "UPDATE playerspings SET week = EXTRACT(WEEK FROM thetimestamp)")
    dbSendQuery(con, "ALTER TABLE playerspings ALTER COLUMN year SET NOT NULL, ALTER COLUMN week SET NOT NULL")
    rm(dserv, dplay)
}
tt <- timed()

### ADD columns to server data in prep for reduction
###    pctquota column
dbSendQuery(con, "ALTER TABLE serverspings ADD COLUMN pctquota FLOAT DEFAULT NULL")
dbSendQuery(con, "UPDATE serverspings SET pctquota = CAST(npop AS FLOAT)/nquota WHERE nquota > 0 ")
###    latency at 10 ppl
dbSendQuery(con, "ALTER TABLE serverspings ADD COLUMN latn20 FLOAT DEFAULT NULL")
dbSendQuery(con, "UPDATE serverspings SET latn20 = nlatency WHERE nlatency != -1 AND npop > 20")
dbSendQuery(con, "ALTER TABLE serverspings ADD COLUMN latn10 FLOAT DEFAULT NULL")
dbSendQuery(con, "UPDATE serverspings SET latn10 = nlatency WHERE nlatency != -1 AND npop > 10")
###    latency at 75%
dbSendQuery(con, "ALTER TABLE serverspings ADD COLUMN latpct50 FLOAT DEFAULT NULL")
dbSendQuery(con, "UPDATE serverspings SET latpct50 = nlatency WHERE nlatency != -1 AND pctquota > 0.5")

tt <- timed()
