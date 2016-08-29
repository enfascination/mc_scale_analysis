pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'

#library(RPostgreSQL)
#library(sqldf)
source(paste0(pathLocal,"header_redditscrape.r"))
source(paste0(pathLocal,"plugin_classes.r"))
library(stringr)
library(assertthat)
library(testthat)


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
        #print(arow)
        #print(clone[i])
        expect_true(nrow(arow) == 1)
        #if (nrow(arow) != 1){ print(arow); print(clone[i])}
        arow[,feat:=new[i]]
        pdt <- rbind(pdt, arow)
    }
    return(pdt)
}

### tis is to match as many plugins as possible to their category labels, or the labels of the closest curse plugin that does the same thing
match_plugins <- c(   'chatcolor', 'chatcolors', 
                          'commandnpc', 'commandnpcs', 
                       'cratereloaded', 'crate-reloaded', 
                       'customjoinitems', 'customjoinmessage', 
                       'customtab', 'custom_text', 
                       'deathmessages', 'deathmessagesprime', 
                       'fullvanish', 'full-vanish', 
                       'hivejumppads', 'hive-jumppads', 
                       'minigameslib',          'minigameslib-mobescape', 
                       'packetlistenerapi', 'galistener', 
                       'perworldinventory', 'perworldhomes', 
                       'pingplayer', 'ping-player', 
                       'proreports', 'reports'   , 
                       'questioner', 'questions', 
                       'remotetoolkitplugin', 'remoteadmin'         , 
                       'simpleprefix','simpleprefixer'  , 
                       'skywarsreloaded', 'survivalgames-reloaded', 
                       'survivalgames', 'survival-games', 
                       'townychat',  'townychannel', 
                       'uskyblock', 'askyblock', 
                       'wgcustomflags', 'worldguard-custom-flags', 
                       'whitelisted_server', 'whitelist', 
                       'helppages', 'help-pages', 
                       'questworld', 'quest-world', 
                       'customenchantments','mystic', 
                        'bukkitcompat', 'mcadmin-minecraft-admin', 
                        'placeholderapi', 'infoapi', 
                        'viaversion', 'version-ignore-rewritten', 
                        'cratesplus' ,'crates', 
                        'simplerename', 'simple-rename', 
                        'showcasestandalone', 'scs',
                        'buyregion', 'buy-regions',
                        'titlewelcomemessage', 'title-welcome-message',
                        'teleportsigns', 'warpsigns',
                        'townynameupdater', 'townynotifier',
                        'auctions', 'auctionsplus',
                        'bungeeportals', 'bungeeteleporter',
                        'featherboard', 'info-board',
                        'jsonapi', 'jsonapi-commands',
                        'actionhealth', 'action-bar-health',
                        'simplealias', 'xsimplealias',
                        'marriage', 'marriage-reloaded',
                        'permissions', 'permissionsex',
                        'aac', 'anticheatplus',
                        'holographicdisplayspatch', 'holographicdisplays',
                        'quickshop', 'quickshop-notlikeme', 
                        'simplevotelistener', 'simple-vote-listener', 
                        'globalmarket', 'global-market', 
                        'norain', 'no-rains',
                        'superspleef', 'super-spleef', 
                        'unioanticrash', 'crashprevention', 
                         'dicefurniture', 'mrcrayfish-furniture-mod', 
                         'furniturelib', 'mrcrayfish-furniture-mod', 
                        'letitrain', 'let-it-rain', 
                        'staffchat', 'easy-staff-chat',
                        'simplehelptickets', 'simple-help-tickets',
                        'infoboardreborn', 'info-board',
                        'terraincontrol', 'terrain-control',
                        'customcrates', 'crate-reloaded', 
                        'bettershops', 'better-shops', 
                        'epicspleef', 'super-spleef',
                        'touchscreenholograms', 'touchscreen-holograms', 
                        'luckyblock', 'luckyblocks', 
                        'moresounds', 'soundsreloaded', 
                        'noswear', 'a5h73y', 
                        'protectionlib', 'protectionslib',
                        'custombanplugin', 'custom-ban-plugin', 
                        'installer', 'plugin-installer', 
                        'minecraftmarket', 'minecraft-market-free-donation', 
                        'obsidiandestroyer', 'obsidianbreaker', 
                        'stargate', 'stargates',
                        'stats', 'lolmewnstats',
                        'ipwhitelist', 'whitelistip',
                        'trollingplus', 'trolling-plus',
                        'legendarymessages', 'joinmessages', 
                        'armorstandtools', 'armor-stand-edit', 
                        'gianttrees', 'giant-trees', 
                        'litebans', 'advanced-bans', 
                        'antispam', 'blacklist-chat', 
                        'combattagplus', 'combat-tag',
                        'combattag', 'combat-tag',
                        'sexymotd', 'sexy-motd',
                        'skinsmodule', 'simpleskins-reloaded', 
                        'administration_panel', 'administration-panel', 
                        'boseconomy', 'basiceconomy',
                        'bossbarapi', 'boss-bar-message',
                        'deluxechat', 'chatex',
                        'jumppads', 'adjustablejumppads', 
                        'chatreaction', 'chatex',
                        'clans', 'simpleclans',
                        'essentialskitmanager', 'ekitmgr',
                        'infinitedispensersanddroppers', 'infinite-dispensers-and-droppers',
                        'lightapi', 'infoapi', 
                        #"rootserver",
                        "autobroadcaster", 'auto-broadcast', 
                        "disguisecraft", 'libsdisguises', 
                        "holograms", "touchscreen-holograms", 
                          "joinmessageplus", "join-message-plus", 
                        "simpleticketmanager", "simple-ticket-manager", 
                        "wgextender", "worldguard", 
                        "actionannouncer", "autoannouncer", 
                          "enderpearlcooldown", "enderpearl-cooldown", 
                        "leaderheads", "heads-leaderboards", 
                        "logblockquestioner", "logblock", 
                        "voteparty", "voterewards",
                           "anvilcolors", "color-signz", 
                        "builder", "builder-citizens2",
                        "coloredanvils", "color-signz", 
                        "endlessenchant", "endless-enchant", 
                           "iconomy_recreation", "iconomy-7", 
                        "train_carts", "traincarts", 
                        "advancedportals", "advanced-portals", 
                        "alwaysday", "always-day",
                           "autobroadcast", "broadcastmc", 
"bloodparticles",  "blood-particles", 
"fe", "fe-economy", 
"limitedcreative",  "limited-creative", 
"minesecure", "securemyaccount",  ### https://www.spigotmc.org/resources/minesecure.699/
"particlelib", "particlemenu",  ### https://mods.curse.com/mc-mods/minecraft/233149-particlelib
"titlemotd", "motdmanager", 
"betterchairs", "chairsreloaded", 
"holographicscoreboard", "holographic-scoreboard", 
"noflyzone", "flycontrol", 
"pvpsoup", "soup",
"advancedachievements", "advanced-achievements", 
"armorstandeditor", "armor-stand-edit", 
"banknotes", "item_bank", ### https://www.spigotmc.org/resources/banknotes.3200/
"freeteleport", "simpleteleport", 
"wgregionevents", "worldguard-region-events",
"animatedframes" , "animated-menu", ### https://www.spigotmc.org/resources/animatedframes-1-8-1-9-1-10.5583/
"anticheat", "anticheatplus", 
"boots", "premiumboots", 
"epicworldgenerator", "biome-generator", 
"flyingcarpet", "magiccarpet", 
"hungergames", "cross-hunger-games", 
"ptweaks", "ptweaks-remove-all-server-lag", 
"neopaintingswitch", "paintingswitch", 
"powernbt", "nbteditor", 
"randomsk", 'skrambled', 
"announcement", "announcements", 
"arcaneenchants", "arcane-enchants", 
"buildmything", "build-my-thing",
"casinoslots", "casino-slots", 
"chatmanagerplus", "chatmanager", 
"frame_protector", "frame-protector", 
"gungame", "gungame-german-edition", 
"infernalmobs", "infernal-mobs", 
"nightvision", "nightvision-reloaded-1-8-x", 
"performancemonitor", "performance-monitor", 
"realping", "connection-ping",  ### https://mods.curse.com/ws-addons/wildstar/237914-realping
"skypvp", "pvp", 
"whitelistmessage", "custom-whitelist-message", 
"yt-boxrevolution" , "youtubertools",  ### https://www.spigotmc.org/resources/yt-boxrevolution-free-1-8.15113/
"animatednames", "c-t-s-n-c", 
"choptree2", "choptree", 
"joinmessage", "join-message-plus", 
"rpg_items", "rpgitem", 
"titleapi", 'titleedit', 
"exoticgarden", "exotic-garden", 
"extrassk", "skextras-skript-addon", 
"itemizer", "itemizer-tech", 
"killstats", "killstats-v1-0", 
"lobbys", "lobbyspawn", 
"noweather", "weatherrestrictions", 
"redstoneclockdetector", "redstone-clock-detector", 
"safecreeper", "safe-creeper", 
"seeplayerinventory", "seeinventory", 
"blockdisguise", "block-disguise", 
"bungeetablistplus", "coloredtablist", 
"communitybridge", "communitybridge-fm", 
"moreplayermodels2", "sit-and-lie-anywhere", 
"nick", "nniicckk", 
"orebfuscator4", "orebfuscator3", 
"signcolorz", "colorsignz", 
"tardischunkgenerator", "tardis", 
"varo", "mrvaro", 
"xrayinformer", "xray-informer", 
"bounty", "bounty-snorri", 
"herobrine", "herobrineunleashed", 
"rankup", "rank-up", 
"rename", "simple-rename", 
"statussign", "status-sign", 
"warp", "easy-warp", 
"welcomebookrecoded", "welcomebook", 
"azrank", "ranker", 
"creativecontrol", "creative-control", 
"customhelp", "custom-help", 
"phoenixanticheat", "anticheatplus", 
"thatonespawnplugin", "setspawn", 
"warpportalcommands", "warp-portal-commands", 
"1.8tags", "tagsplus", 
"antiwdl", "wdl-companion", 
"bookexploitfix", "bookmanager-reloaded", 
"bossbarmessage", "boss-bar-message", 
"cctv_camera", "cctv-camera", 
"disease", "byte-disease", 
"dtlcore", "dtltraders", 
"enchantsplus", "moreenchatsplus", 
"itag", "itags", 
"keepdeathdrop", "no-drop-death", 
"simplepets", "pets", 
"simplesit", "simple-sit", 
"spawners", "enhancedspawners",
"spawnershop", "enhancedspawners",
"tab", "fancytab", 
"actionbarapi", "actionbar", 
"advanced_item_effects", "advanced-item-effects", 
"backpacks-1.9", "backpacks_1-9", 
"backup", "minebackup", 
"checkmyinventory", "seeplayer-inventory", 
"farmlimiter", "nofarm", 
"itemrestrict", "item-restrict", 
"nordic", "banananordic", 
"perfectbackup", "minebackup", 
"pluginmanagerreloaded", "pm-pluginmanager", 
"premiumvanish", "supervanish", 
"shopguiplus", "gui-shop", 
"sohbet-gardiyani", "chatex", 
"tempban", "timeban", 
"wgblockrestricter", "worldguard-block-restricter", 
"betterenderchest", "enderchestviewer", 
"borderguard", "border-patrol", 
"bowspleef", "bow-spleef", 
"bungeesuiteportals", "bungeeteleporter", 
"commandblockerultimate", "cmdblockr", 
"enchanter", "xern-enchanter", 
"globalmute", "global-mute", 
"justwarp", "justwarp-1-0-simple-warps-superp", 
"oneinthebattle", "oitb", 
"rainbowarmour", "rainbow-armor-smooth", 
"scrollingsignwarp", "scrolling-sign-warp", 
"spectatordisabler", "spectator", 
#"system"
"trademe", "get-me-traded", 
"uralclans", "simpleclans", 
"advancedreport", "reporter", 
"creativecontrolbykubqoa", "control-creative-mode",
"customcommands", "customcommandbuilder", 
"elytraplus", "super-jetpack", 
"freesign", "free-signs", 
"homespawn", "homespawnplus", 
"itemcases", "itemcase", ### https://www.spigotmc.org/resources/itemcases.3744/
"jump_pads", "hive-jumppads", 
#"report"
"tpa", "credible-tpa" 
                           ) %>% matrix(ncol=2, byrow=TRUE, dimnames=list(NULL, c("missed", "match")))

plug_properties <- add_copied_feat(plug_properties, match_plugins[,1], match_plugins[,2])

### going through manually, there are a few plugins that belong in one of the categories I'm focusing on: admmintools, antigrief, chat, devtools, economy, informational, and webadmin, but that were in  other categories instead. these account for a little over 1% of all plugins.  the closest to being important is 'mechanics', which i'm excluding.  10% of its plugins have something to do with in-world resource management, like the three tree plugins, and accoutn for almost 1% of the data. I'm leaning against inclusion.  actually, i'm gonna go for it..  actually, no.
c("trading", "headdrops", "bounty", "freesign", "simplevipperks","wgregionevents", "wgcustomflags", "anticommandtab", "simplebackup", "simplevotelistener", "commandblocker", "autobroadcaster", "voteparty", "seeplayerinventory", "farmworld", "simple-autosave", "controltpa", "cctv_camera", "echopet", "nomorehunger", "oregenerator", "playervaults", "qwicktree", "serverminimap", "timber", "treeassist")

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
