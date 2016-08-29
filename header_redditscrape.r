options(stringsAsFactors = FALSE)  ### I can't stand factors when i didn't assign them explicitly
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"libs_mc_scrape.R"))

library(lazyeval)
library(lubridate)

### return date object from year and week integers
yw <- function(year, week) {
    return(ymd(paste0(year,"-01-01")) + dweeks(week))
}

word_forbid <- matrix(c("hub", "minigames", "mini games", "multiverse-core", "multiverse-inventories", "multiverse-netherportals", "robbit", "creative", "adventure", "playerheads", "mcmmo", "multiverse-portals", "pvp", "prison", "raiding", "kitpvp", "parkour", "skyblock", "survival games", "survivalgames", "skywars", "ftb", "pixelmon", "tekkit", "factions", "kits", "multiworld", "roleplay", "quests", "mobarena", "bedwars", "massivecore"), ncol=1, byrow=T)
neutrallacklist <- c( "citizens", "economy", "towny", "plotme", "buycraft", "whitelist", "hardcore", "uhc", "hard mode", "jobs")
whitelist <- c("smp", "pve", "vanilla", "semi-vanilla", "survival", "anarchy")
