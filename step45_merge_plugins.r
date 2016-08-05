pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'

#library(RPostgreSQL)
#library(sqldf)
source(paste0(pathLocal,"header_redditscrape.r"))
source(paste0(pathLocal,"plugin_classes.r"))
library(stringr)
library(assertthat)


plug <- fread(paste0(pathData, 'curse_plugins_metadata.csv'))
setkey(plug,name)
setnames(plug, c("name"), c("feat_humanname"))
plug[,dataset_source:='curse']
plug <- plug[feat_humanname!='name']
plug[,date_created:=as.integer(date_created)]
plug[,date_updated:=as.integer(date_updated)]
plug[,dls_total:=as.integer(dls_total)]
plug[,dls_recent:=as.integer(dls_recent)]
plug[,likes:=as.integer(likes)]
plug[category == "chat-related", category:="cat_chat"]
plug[category == "informational", category:="cat_informational"]
plug[category == "fun", category:="cat_fun"]
plug[category == "fixes", category:="cat_fixes"]
plug[category == "admin-tools", category:="cat_admintools"]
plug[category == "general", category:="cat_general"]
plug[category == "developer-tools", category:="cat_devtools"]
plug[category == "anti-griefing-tools", category:="cat_antigrief"]
plug[category == "mechanics", category:="cat_mechanics"]
plug[category == "world-generators", category:="cat_worldgen"]
plug[category == "miscellaneous", category:="cat_misc"]
plug[category == "teleportation", category:="cat_teleportation"]
plug[category == "economy", category:="cat_economy"]
plug[category == "role-playing", category:="cat_roleplay"]
plug[category == "world-editing-and-management", category:="cat_world"]
plug[category == "website-administration", category:="cat_webadmin"]
plug[category == "mc-addons", category:="cat_mod_addons"]
plug[category == "applied-energistics-2", category:="cat_mod_energistics"]
plug[category == "blood-magic", category:="cat_mod_bloodmagic"]
plug[category == "addons-buildcraft", category:="cat_mod_buildcraft"]
plug[category == "addons-forestry", category:="cat_mod_forestry"]
plug[category == "addons-industrialcraft", category:="cat_mod_industrialcraft"]
plug[category == "addons-thaumcraft", category:="cat_mod_thaumcraft"]
plug[category == "addons-thermalexpansion", category:="cat_mod_thermalexpansion"]
plug[category == "addons-tinkers-construct", category:="cat_mod_tinkers"]
plug[category == "adventure-rpg", category:="cat_mod_adventurerpg"]
plug[category == "library-api", category:="cat_mod_libraryapi"]
plug[category == "armor-weapons-tools", category:="cat_mod_armorweaponstools"]
plug[category == "cosmetic", category:="cat_mod_cosmetic"]
plug[category == "mc-food", category:="cat_mod_food"]
plug[category == "magic", category:="cat_mod_magic"]
plug[category == "map-information", category:="cat_mod_mapinformation"]
plug[category == "mc-miscellaneous", category:="cat_mod_misc"]
plug[category == "redstone", category:="cat_mod_redstone"]
plug[category == "server-utility", category:="cat_mod_serverutility"]
plug[category == "storage", category:="cat_mod_storage"]
plug[category == "technology", category:="cat_mod_technology"]
plug[category == "technology-energy", category:="cat_mod_techenergy"]
plug[category == "technology-item-fluid-energy-transport", category:="cat_mod_techthingtransport"]
plug[category == "technology-farming", category:="cat_mod_techfarming"]
plug[category == "technology-genetics", category:="cat_mod_techgenetics"]
plug[category == "technology-player-transport", category:="cat_mod_techplayertransport"]
plug[category == "technology-processing", category:="cat_mod_techprocessing"]
plug[category == "world-gen", category:="cat_mod_worldgen"]
plug[category == "world-biomes", category:="cat_mod_worldbiomes"]
plug[category == "world-mobs", category:="cat_mod_worldmobs"]
plug[category == "world-ores-resources", category:="cat_mod_worldresources"]
plug[category == "world-structures", category:="cat_mod_worldstructures"]
plug[, xxx:=999]
### widen into binary indicators of categoryXXX
#plug[,name_human:=feat_humanname]
plug[,feat_urlname:=str_extract(plug[,url], "[^/]*$")]
### manually fix one name collision:
plug[feat_urlname=='multiworld' & build=='forge', feat_urlname:='multiworld_forge']
#plug[,name_url_simple:=str_replace_all(plug[,feat_urlname], "-", '')]
plug_wide <- merge( as.data.table(dcast(formula=feat_urlname ~ category, data=plug, value.var="xxx", fun.aggregate = function(x) (length(x) > 0) + 0.0)), plug[!duplicated(feat_urlname), list(feat_humanname, feat_urlname, build, dataset_source, url, date_created, date_updated, dls_total, dls_recent, likes)], by="feat_urlname")
#### how much in each category?  plug_wide[,lapply(.SD[,na.omit(str_match(names(plug_wide), "^cat_.*")),with=F], sum ),]
coded_key_merge <- merge(coded_key[feat_source=='plugin'], plug_wide, by="feat_urlname", all.x=T, all.y=F)
### there remain a few installed plguisn without matches to curse or co: coded_key_merge[is.na(url), feat_urlname]
assert_that(0 == nrow(plug_wide[duplicated(feat_urlname)]))
plug_merge <- merge(coded_key[feat_source=='plugin'], plug_wide, by="feat_urlname", all.x=F, all.y=T)
plug_merge[is.na(feat),feat:=feat_urlname]
plug_properties <- plug_merge
#### during this merge, do some work here to make sure that things go OK with the names matching:
###   mc[is.na(cat_worldgen) & feat_source=='plugin',][, unique(feat)] and pluginstats$feat[grep("customjoin", pluginstats$feat)] (use mc after getting rid of rare plugins for a reasonable tload)
add_copied_feat <- function(input_dt, new, clone) {
    expect_true(length(new) == length(clone))
    pdt <- copy(input_dt)
    for (i in 1:length(new)){
        arow <- pdt[feat==clone[i]]
        #print(arow)}
        expect_true(nrow(arow) == 1)
        #if (nrow(arow) != 1){ print(arow); print(clone[i])}
        arow[,feat:=new[i]]
        pdt <- rbind(pdt, arow)
    }
    return(pdt)
}
missed_plugins <- c('chatcolor', 'commandnpc', 'cratereloaded', 'customjoinitems', 'customtab', 'deathmessages', 'fullvanish', 'hivejumppads', 'minigameslib', 'packetlistenerapi', 'perworldinventory', 'pingplayer', 'proreports', 'questioner', 'remotetoolkitplugin', 'simpleprefix', 'skywarsreloaded', 'survivalgames', 'townychat', 'uskyblock', 'wgcustomflags', 'whitelisted_server', 'helppages')
match_plugins <- c('chatcolors', 'commandnpcs', 'crate-reloaded', 'customjoinmessage', 'custom_text', 'deathmessagesprime', 'full-vanish', 'hive-jumppads', 'minigameslib-mobescape', 'galistener', 'perworldhomes', 'ping-player', 'reports', 'questions', 'remoteadmin', 'simpleprefixer', 'survivalgames-reloaded', 'survival-games', 'townychannel', 'askyblock', 'worldguard-custom-flags', 'whitelist', 'help-pages')
plug_properties <- add_copied_feat(plug_properties, missed_plugins, match_plugins)
write.csv(plug_properties, paste0(pathData, 'step45_curse_plugins_metadata_full.csv'))
### there are duplicate now on feat_urlname, but not on feat: plug_merge[duplicated(feat_urlname)]



# #### some early code I used to prepare hand coding of curse plugins automatically
###### processing
###mc_feat_select <- mc_rlm_fit$finalModel@penalized[mc_rlm_fit$finalModel@penalized != 0]
###mc_feat_data <- as.data.table(mc_rlm_fit$trainingData)
###mc_feat_notableexclude <- names(which(colSums(mc_feat_data[,names(mc_rlm_fit$finalModel@penalized[mc_rlm_fit$finalModel@penalized == 0]),with=F]) > 50))
###tmpmcp <- names(mc_feat_select)[-1]
###tmpmcf <- sub("^srvmax_", '', tmpmcp)
###tmpmcfcursenames <- tmpmcf
###tmpmcfcursenames[tmpmcfcursenames=='griefprevention'] <- 'grief-prevention'
###tmpmcfcursenames[tmpmcfcursenames=='enjinminecraftplugin'] <- 'emp'
###tmpmcfcursenames[tmpmcfcursenames=='permissionsbukkit'] <- 'permbukkit'
###tmpmcfcursenames[tmpmcfcursenames=='vanishnopacket'] <- 'vanish'
###setkey(coded_key_keywords, feat)
###setkey(coded_key_keywords_2, feat)
###setkey(friv_coded_key_keywords, feat)
###mc_feat <- data.table(pred=tmpmcp, feat=tmpmcf, beta=as.numeric(mc_feat_select[tmpmcp]), i_srvmax=grepl("^srvmax_", tmpmcp), nobs=colSums(mc_feat_data[,tmpmcf]), friv=tmpmcf %in% friv_coded_key_keywords$feat)
###tmpcursestring <- system(command=paste0('/Users/sfrey/anaconda/envs/mcscraper/bin/python  ', pathLocal, 'cursemodstats.py'), input=tmpmcf, intern=T )
###mc_feat_curse <- as.data.table(read.csv(textConnection(tmpcursestring), colClasses=c("character", "character", 'integer', 'character', 'integer'), na.strings='0'))
###mc_feat <- mc_feat_curse[!duplicated(mc_feat_curse)][mc_feat,on='feat']
###mc_feat <- cbind(mc_feat, coded_key_keywords[tmpmcf,-1,with=F])
###mc_feat <- cbind(mc_feat, coded_key_keywords_2[tmpmcf,-1,with=F])
###write.csv(mc_feat, paste0(pathLocal, "mc_feat.csv"))
###print(paste0(pathLocal, "mc_feat.csv"))
###system(paste0('open -a Numbers ', pathLocal, "mc_feat.csv"))
###
###### from python function def curseCategoryStats():
###mod_cats <- c('fixes'= 899, 'economy'= 1058, 'developer-tools'= 674, 'anti-griefing-tools'= 1236, 'world-generators'= 154, 'website-administration'= 196, 'world-editing-and-management'= 731, 'role-playing'= 1766, 'informational'= 2099, 'teleportation'= 1132, 'chat-related'= 2356, 'admin-tools'= 6069, 'miscellaneous'= 870, 'mechanics'= 3867, 'general'= 2612, 'fun'= 6237)
###
###
