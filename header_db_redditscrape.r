pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_redditscrape.r"))

library(RPostgreSQL)

#if(!isPostgresqlIdCurrent(con)) {
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='localhost', port='5432', dbname='mc')
#}
#dbDisconnect(con)


