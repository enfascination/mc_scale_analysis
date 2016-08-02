asdf <- data.frame
juicy_words = c('ban', 'banned', 'whitelisted', 'age', '18', '18+', 'owner', 'owners', 'mods', 'moderators', 'admins', 'admin', 'moderator', 'bans', 'faction', 'community', 'harass', 'threat', 'threatening', 'adult', 'friendly', 'fair', 'unfair', 'grief', 'griefs', 'griefing', 'respect', 'respectful', 'rollback', 'spam', 'spamming', 'swearing', 'swear', 'cheat', 'cheats' , "cheating" , "hack" , "hacks" , "hacking" , "steal" , "stealing" , "tnt", "fire" , "member" , "members" , "blacklist" , "greylist" , "rules" , "staff" , "abuse" , "abusers" , "abuser" , "forum" , "forums" , "wiki" , "website" , "subreddit" , "ddos" , "location" , "locale", "shop", "forms", "form", "application", "docs", "apply", "official", "site", "team", 'join', "voip")
#juicy_words = c('ban', 'banned', 'whitelisted', 'age', '18', '18+', 'owner', 'owners', 'mods', 'moderators', 'admins', 'admin', 'moderator', 'bans', 'faction', 'community', 'harass', 'threat', 'threatening', 'mature', 'adult', 'friendly', 'fair', 'unfair', 'grief', 'griefs', 'griefing', 'respect', 'respectful', 'rollback', 'spam', 'spamming', 'swearing', 'swear', 'cheat', 'cheats' , "cheating" , "hack" , "hacks" , "hacking" , "steal" , "stealing" , "tnt", "fire" , "member" , "members" , "whitelist" , "blacklist" , "greylist" , "rules" , "staff" , "abuse" , "abusers" , "abuser" , "forum" , "forums" , "wiki" , "website" , "subreddit" , "ddos" , "location" , "locale", "shop", "forms", "form", "application", "docs", "apply", "official", "site", "team", 'join', "voip", "skype", "mumble", "teamspeak")

### analysis framework connected to actual mc plugins
coded_key_plugins <- as.data.table(rbind( data.frame("feat"=character(), "rbasic"=numeric(), "rbiophys"=numeric(), "popvol"=numeric(), "popforce"=numeric(), "rprevent"=numeric(), "rexternal"=numeric(), "rhetero"=numeric(), "rincntv"=numeric(), "rinst"=numeric(), "rtech"=numeric(), "rspat"=numeric(), "rsoccap"=numeric(), "rleader"=numeric(), "rinfopred"=numeric())
 ,asdf(feat='essentials',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=1, rsoccap=1, rleader=1, rinfopred=1)
 ,asdf(feat='essentialschat',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=0, rinfopred=2)
 ,asdf(feat='essentialsspawn',rbasic=0, rbiophys=1, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='essentialsprotect',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='essentialsantibuild',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='essentialsgeoip',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=1, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='essentialsxmpp',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=0, rinfopred=2)
 ,asdf(feat='worldedit',rbasic=0, rbiophys=1, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='worldguard',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='dynmap-worldguard',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=1)
 ,asdf(feat='craftconomy',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='craftconomy3',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='dynmap',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=2)
 ,asdf(feat='groupmanager',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=0, rexternal=0, rhetero=1, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=1, rleader=0, rinfopred=0)
 ,asdf(feat='vault',rbasic=1, rbiophys=0, popvol=0, popforce=1, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=1, rleader=1, rinfopred=0)
 ,asdf(feat='coreprotect',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=1, rinfopred=1)
 ,asdf(feat='griefprevention',rbasic=0, rbiophys=1, popvol=0, popforce=1, rprevent=1, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=1, rsoccap=0, rleader=1, rinfopred=1)
 ,asdf(feat='permissionsex',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=0, rhetero=1, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='chatex',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=2)
 ,asdf(feat='worldborder',rbasic=0, rbiophys=1, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='chestshop',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='signshop',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='iconomy',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='modifyworld',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='dynmap-griefprevention',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=1)
 ,asdf(feat='votifier',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='banmanager',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='gogobanyourself',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='lwc',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=1, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='nocheatplus',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='galistener',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='enjinminecraftplugin',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='plotme',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=1, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='plotme-defaultgenerator',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=1, rspat=1, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='floauction',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=1, rspat=0, rsoccap=1, rleader=0, rinfopred=1)
 ,asdf(feat='shops',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='shopkeeper',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=1)
 ,asdf(feat='shopkeepers',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=1)
 ,asdf(feat='serversigns',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=1, rinfopred=1)
 ,asdf(feat='signedit',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=1, rinfopred=2)
 ,asdf(feat='clearlag',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='authme',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='logblock',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=2)
 ,asdf(feat='lockette',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='remotetoolkitplugin',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='zavautomessager',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=1)
 ,asdf(feat='treeassist',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='openinv',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='motdmanager',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=1)
 ,asdf(feat='automessage',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=1)
 ,asdf(feat='chestcommands',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='buycraft',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='buycraftx',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='clickwarp',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='vanishnopacket',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='supervanish',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='plugman',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='echopet',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='tablist',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=2)
 ,asdf(feat='autosaveworld',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='chatmanager',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=1, rinfopred=2)
 ,asdf(feat='chatcontrol',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=1, rinfopred=2)
 ,asdf(feat='clearchat',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=1, rinfopred=2)
 ,asdf(feat='frameprotect',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='areashop',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=1, rspat=1, rsoccap=1, rleader=0, rinfopred=0)
 ,asdf(feat='adminfun',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='maxbans',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='creeperheal',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='myhome',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='announcer',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=1)
 ,asdf(feat='commandspy',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='firstjoinplus',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=1, rinfopred=2)
 ,asdf(feat='herochat',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=2)
 ,asdf(feat='ontime',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=0, rinfopred=1)
 ,asdf(feat='serverlistplus',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=1, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='proreports',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=1, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='antibotultra',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='antijoinbot',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='simpleregionmarket',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='editablesign',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=2)
 ,asdf(feat='autorank',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=1, rleader=1, rinfopred=0)
 ,asdf(feat='bpermissions',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='customservermessages',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=1)
 ,asdf(feat='antixray',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='marriagemaster',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=1, rspat=0, rsoccap=1, rleader=0, rinfopred=0)
 ,asdf(feat='simplebroadcast',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=1)
 ,asdf(feat='bossbroadcast',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=1)
 ,asdf(feat='yenileyici',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='vipzero',rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=0, rhetero=1, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=1, rinfopred=0)
 ,asdf(feat='tce',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=1)
 ,asdf(feat='swearfilter',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='spamyasak',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='remotecontroller',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='pinger',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='ping',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='griefpreventionplus',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='chestshopnotifier',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=2)
 ,asdf(feat='chestprotect',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='biometp',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='playermarkers',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=2)
 ,asdf(feat='ram_monitor',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='invsee_v1.0.3',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='permissionsbukkit',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=1, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='anticreeper',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='orebfuscator3',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='farmworld',rbasic=0, rbiophys=1, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='simplehomes',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=1, rleader=0, rinfopred=0)
 ,asdf(feat='easyhome',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='sethome',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=1, rleader=0, rinfopred=0)
 ,asdf(feat='homespawnplus',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=1, rleader=1, rinfopred=0)
 ,asdf(feat='giveall',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ,asdf(feat='parachuteplus',rbasic=0, rbiophys=1, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='tablistprefix',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=0, rinfopred=1)
 ,asdf(feat='troll',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 #,asdf(feat='',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='whitelist',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='factions',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='towny',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=1, rsoccap=1, rleader=1, rinfopred=0)
,asdf(feat='citizens',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=1, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=1, rleader=1, rinfopred=0)
 ))
coded_key_plugins[,rnorms:=0]
coded_key_plugins[,feat_useful:=T]
coded_key_plugins[,feat_source:='plugin']

coded_key_tags <- as.data.table(rbind( data.frame("feat"=character(), "rbasic"=numeric(), "rbiophys"=numeric(), "popvol"=numeric(), "popforce"=numeric(), "rprevent"=numeric(), "rexternal"=numeric(), "rhetero"=numeric(), "rincntv"=numeric(), "rinst"=numeric(), "rtech"=numeric(), "rspat"=numeric(), "rsoccap"=numeric(), "rleader"=numeric(), "rinfopred"=numeric())
 ,asdf(feat='vanilla',  rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=-1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='whitelist',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='whitelisted',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='semi-vanilla',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=-1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='skype',    rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=1, rleader=0, rinfopred=2)
 ,asdf(feat='18+',      rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=1, rhetero=-1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='mumble',   rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=1, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=1, rleader=0, rinfopred=2)
 ,asdf(feat='usa',      rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=1, rhetero=-1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='survival',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='uhc',rbasic=1, rbiophys=1, popvol=1, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='hard mode',rbasic=1, rbiophys=1, popvol=1, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='mature',rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=1, rhetero=-1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='16+',      rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=1, rhetero=-1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='uk',      rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=1, rhetero=-1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='teamspeak',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=1, rleader=0, rinfopred=2)
 ,asdf(feat='small community',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=0, rinfopred=0)
 ,asdf(feat='economy',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='14+',      rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=1, rhetero=-1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='15+',      rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=1, rhetero=-1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='adult',      rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=1, rhetero=-1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='no whitelist',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='all ages',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='us',      rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=1, rhetero=-1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='greylist',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='eu',      rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=1, rhetero=-1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='shops',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='no griefing',rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='spigot',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='new map',rbasic=0, rbiophys=1, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='1.8-pre1',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='anarchy',rbasic=1, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=-1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='casual',rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=0, rinfopred=0)
 ,asdf(feat='conditional pvp',rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='anti-grief',rbasic=0, rbiophys=0, popvol=0, popforce=1, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
 ))
coded_key_tags[,rnorms:=1]
coded_key_tags[,feat_useful:=T]
coded_key_tags[,feat_source:='tag']

coded_key_keywords <- copy(coded_key_tags)
coded_key_keywords[,feat_source:='keyword']
coded_key_keywords <- rbind(coded_key_keywords, data.table(feat=juicy_words[juicy_words %ni% coded_key_keywords[,feat] ], feat_useful=T, feat_source='keyword', rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0, rnorms=1))

### more modeling, exploratory
coded_key_plugins_more <- as.data.table(rbind( 
  asdf("feat"=character(),"inst_type"=factor(), "inst_selection"=factor(), "boundary"=numeric(), "action_other_up"=numeric(), "action_other_down"=numeric(), "action_admin"=numeric(), "role_rank"=numeric(), "role_type"=numeric(), "economy"=numeric(), "communication"=numeric())
 ,asdf(feat='essentialsprotect',      inst_type=4, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=1, role_rank=1, role_type=1, economy=0, communication=0)
 ,asdf(feat='dynmap',                 inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='essentials',             inst_type=4, inst_selection=3, boundary=0, action_other_up=1, action_other_down=0, action_admin=1, role_rank=1, role_type=0, economy=1, communication=0)
 ,asdf(feat='essentialschat',         inst_type=1, inst_selection=2, boundary=0, action_other_up=1, action_other_down=0, action_admin=1, role_rank=0, role_type=0, economy=0, communication=1)
 ,asdf(feat='essentialsspawn',        inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=1, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='worldedit',              inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=1, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='worldguard',             inst_type=4, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=1, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='towny',                  inst_type=4, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=1, role_rank=1, role_type=0, economy=0, communication=0)
 ,asdf(feat='groupmanager',           inst_type=4, inst_selection=3, boundary=0, action_other_up=0, action_other_down=0, action_admin=1, role_rank=1, role_type=1, economy=0, communication=0)
 ,asdf(feat='vault',                  inst_type=4, inst_selection=3, boundary=0, action_other_up=0, action_other_down=0, action_admin=1, role_rank=1, role_type=1, economy=1, communication=0)
 ,asdf(feat='permissionsex',          inst_type=0, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=1, role_rank=1, role_type=1, economy=0, communication=0)
 ,asdf(feat='griefprevention',        inst_type=4, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='coreprotect',            inst_type=4, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='anti-grief',             inst_type=4, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=1, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='chestshop',              inst_type=0, inst_selection=0, boundary=0, action_other_up=1, action_other_down=0, action_admin=1, role_rank=0, role_type=0, economy=1, communication=0)
 ,asdf(feat='customservermessages',   inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=1, role_rank=0, role_type=0, economy=0, communication=1)
 ,asdf(feat='modifyworld',            inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=1, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='dynmap-griefprevention', inst_type=4, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=1, role_rank=1, role_type=0, economy=0, communication=0)
 ,asdf(feat='votifier',               inst_type=0, inst_selection=3, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='banmanager',             inst_type=4, inst_selection=3, boundary=1, action_other_up=0, action_other_down=0, action_admin=1, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='gogobanyourself',             inst_type=4, inst_selection=3, boundary=1, action_other_up=0, action_other_down=0, action_admin=1, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='plotme',                 inst_type=4, inst_selection=3, boundary=1, action_other_up=0, action_other_down=1, action_admin=0, role_rank=0, role_type=1, economy=0, communication=0)
 ,asdf(feat='jobs',                   inst_type=1, inst_selection=2, boundary=0, action_other_up=1, action_other_down=0, action_admin=0, role_rank=0, role_type=1, economy=0, communication=0)
 ,asdf(feat='floauction',             inst_type=0, inst_selection=0, boundary=0, action_other_up=1, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=1, communication=0)
,asdf(feat='whatisit'  ,             inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='citizens',               inst_type=4, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=1, role_rank=1, role_type=0, economy=0, communication=0)
 ,asdf(feat='protocollib',            inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='essentialsgeoip',        inst_type=0, inst_selection=0, boundary=1, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='shops',                  inst_type=0, inst_selection=0, boundary=0, action_other_up=1, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=1, communication=0)
 ,asdf(feat='holographicdisplays',    inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=1)
 ,asdf(feat='automessage',            inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=1, role_rank=0, role_type=0, economy=0, communication=1)
 ,asdf(feat='nocheatplus',            inst_type=0, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
,asdf(feat='worldborder',            inst_type=0, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=1, role_rank=0, role_type=0, economy=0, communication=0)
,asdf(feat='lwc',                    inst_type=4, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='galistener',             inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='essentialsantibuild',    inst_type=4, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=1, role_rank=0, role_type=1, economy=0, communication=0)
 ,asdf(feat='enjinminecraftplugin',   inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='hardcore',               inst_type=4, inst_selection=3, boundary=1, action_other_up=0, action_other_down=1, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
))
coded_key_plugins_more[,inst_type:=     factor(inst_type, levels=0:4, labels=c( "N/A", "Strategy", "Norm", "Rule", "No discretion"))]
coded_key_plugins_more[,inst_selection:=factor(inst_selection, levels=0:3, labels=c( "N/A", "Self-selection", "Manual", "Automatic"))]
#coded_key_plugins_more[,feat_useful:=T]
coded_key_plugins_more[,feat_source:='plugin']

coded_key_tags_more <- as.data.table(rbind( 
  asdf("feat"=character(),"inst_type"=factor(), "inst_selection"=factor(), "boundary"=numeric(), "action_other_up"=numeric(), "action_other_down"=numeric(), "action_admin"=numeric(), "role_rank"=numeric(), "role_type"=numeric(), "economy"=numeric(), "communication"=numeric())
 ,asdf(feat='smp',                    inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='vanilla',                inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='whitelist',              inst_type=3, inst_selection=2, boundary=1, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='pve',                    inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='1.7.10',                 inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='1.8',                    inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='1.8.1',                  inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='1.8.3',                  inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='1.8.8',                  inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='semi-vanilla',           inst_type=0, inst_selection=3, boundary=0, action_other_up=0, action_other_down=1, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='skype',                  inst_type=1, inst_selection=2, boundary=0, action_other_up=1, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=1)
 ,asdf(feat='18+',                    inst_type=3, inst_selection=1, boundary=1, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='mumble',                 inst_type=1, inst_selection=2, boundary=0, action_other_up=1, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=1)
 ,asdf(feat='snapshot',               inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='usa',                    inst_type=1, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='mature',                 inst_type=2, inst_selection=1, boundary=1, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='16+',                    inst_type=3, inst_selection=1, boundary=1, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='1.7.9',                  inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='uk',                     inst_type=1, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='teamspeak',              inst_type=1, inst_selection=2, boundary=0, action_other_up=1, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=1)
 ,asdf(feat='whitelisted',            inst_type=3, inst_selection=2, boundary=1, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='small community',        inst_type=2, inst_selection=1, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='economy',                inst_type=4, inst_selection=0, boundary=0, action_other_up=1, action_other_down=0, action_admin=1, role_rank=0, role_type=0, economy=1, communication=0)
 ,asdf(feat='14+',                    inst_type=3, inst_selection=1, boundary=1, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
,asdf(feat='dynmap',                 inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='version 1.8',            inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='adult',                  inst_type=3, inst_selection=1, boundary=1, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='15+',                    inst_type=3, inst_selection=1, boundary=1, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='custom plugins',         inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='no whitelist',           inst_type=1, inst_selection=1, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='all ages',               inst_type=1, inst_selection=1, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='us',                     inst_type=1, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='greylist',               inst_type=3, inst_selection=2, boundary=0, action_other_up=0, action_other_down=1, action_admin=0, role_rank=1, role_type=0, economy=0, communication=0)
 ,asdf(feat='eu',                     inst_type=1, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='no griefing',            inst_type=2, inst_selection=1, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='spigot',                 inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='new map',                inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='community',              inst_type=1, inst_selection=1, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='1.8-pre1',               inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='roleplay',               inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='hardcore',               inst_type=4, inst_selection=3, boundary=1, action_other_up=0, action_other_down=1, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='bukkitcompat',           inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='shops',                  inst_type=0, inst_selection=0, boundary=0, action_other_up=1, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=1, communication=0)
 ,asdf(feat='anarchy',                inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='casual',                 inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='survival',               inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='uhc',                    inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='hard mode',              inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='conditional pvp',        inst_type=3, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
 ,asdf(feat='snapshots',              inst_type=0, inst_selection=0, boundary=0, action_other_up=0, action_other_down=0, action_admin=0, role_rank=0, role_type=0, economy=0, communication=0)
))
#coded_key_tags_more[,feat_useful:=T]
coded_key_tags_more[,feat_source:='tag']

coded_key_keywords_more <- copy(coded_key_tags_more)
coded_key_keywords_more[,feat_source:='keyword']

### thigns that don't have to do with governance
coded_key_plugins_friv <- as.data.table(rbind(
  asdf("feat"=character(),  "rbasic"=numeric(), "rbiophys"=numeric(), "popvol"=numeric(), "popforce"=numeric(), "rprevent"=numeric(), "rexternal"=numeric(), "rhetero"=numeric(), "rincntv"=numeric(), "rinst"=numeric(), "rtech"=numeric(), "rspat"=numeric(), "rsoccap"=numeric(), "rleader"=numeric(), "rinfopred"=numeric())
,asdf(feat='bettercrops',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='commandsxl',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='deathmessagesprime',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='dueltag',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='setspawn',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='easyspawn',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='enchantplus',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='hideallplayers',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='minecartrevolution',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='hotbarpets',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='mineresetliteplus',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='nospawnchunks',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='quarterbukkit-plugin',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='randomlocationteleporter',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='redstonejukeboxtrig',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='superbuildbattle',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='superjump',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='tetris',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='plotsquared',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='tuxtwolib',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='mycommand',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
,asdf(feat='questioner',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=1, rleader=0, rinfopred=0)
,asdf(feat='barapi',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='tagapi',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='cs-corelib',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='mythicmobs',rbasic=0, rbiophys=1, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='easywarp',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='warpportals',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='titlemanager',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=1)
,asdf(feat='multiworld',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=1, rhetero=1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
,asdf(feat='playerheads',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='scoreboardstats',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=0, rinfopred=1)
,asdf(feat='lift',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='timtheenchanter',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='combatlog',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='skript',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
,asdf(feat='timber',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='multiverse-core',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=1, rhetero=1, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='multiverse-portals',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=1, rhetero=1, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='multiverse-netherportals',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=1, rhetero=1, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='multiverse-signportals',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=1, rhetero=1, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='multiverse-inventories',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=1, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='voxelsniper',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
,asdf(feat='craftbook',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='commandbook',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
,asdf(feat='commandsigns',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=1, rinfopred=0)
,asdf(feat='healthbar',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=1)
,asdf(feat='whatisit',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=1)
,asdf(feat='roleplay',rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=0, rinfopred=0)
,asdf(feat='hardcore',rbasic=1, rbiophys=1, popvol=1, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='protocollib',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='asynchworldedit',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='asyncworldedit',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='asynchworldeditinjector',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='asyncworldeditinjector',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='backpacks',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='backpack',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='mineresetlite',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='coloredtags',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='creativegates',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='compatnocheatplus',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=1, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='cleanroomgenerator',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='sentry',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='quests',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='askyblock',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='crackshot',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='paintball',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='nametagedit',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='skins',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='skinsrestorer',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='idisguise',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='libsdisguises',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='silkspawners',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='1vs1',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='mobarena',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='lobby',rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=1, rsoccap=0, rleader=0, rinfopred=1)
,asdf(feat='coloredsigns',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=1)
,asdf(feat='massivecore',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='jobs',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='chairs',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='chairstairs',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='bedwars',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='bedwarsrel',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
 ,asdf(feat='holographicdisplays',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=1)
 ))
coded_key_plugins_friv[,rnorms:=0]
coded_key_plugins_friv <- rbind(coded_key_plugins_friv, data.table(feat=c("fly", 'hidestream', 'bedhome', 'choptree', 'choptree3', 'nicknamer', 'wirelessredstone'),rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0, rnorms=1))
coded_key_plugins_friv[,feat_useful:=F]
coded_key_plugins_friv[,feat_source:='plugin']

coded_key_tags_friv <- as.data.table(rbind(
  asdf("feat"=character(),  "rbasic"=numeric(), "rbiophys"=numeric(), "popvol"=numeric(), "popforce"=numeric(), "rprevent"=numeric(), "rexternal"=numeric(), "rhetero"=numeric(), "rincntv"=numeric(), "rinst"=numeric(), "rtech"=numeric(), "rspat"=numeric(), "rsoccap"=numeric(), "rleader"=numeric(), "rinfopred"=numeric())
,asdf(feat='hardcore',rbasic=1, rbiophys=1, popvol=1, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='roleplay',rbasic=0, rbiophys=0, popvol=1, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=1, rleader=0, rinfopred=0)
,asdf(feat='bukkitcompat',rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='version 1.8',     rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='1.7.9',     rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='custom plugins',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='snapshot', rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='snapshots', rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='smp',      rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='pve',      rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='1.7.10',    rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='1.8',      rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='1.8.1',    rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='1.8.3',    rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='1.8.8',    rbasic=1, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='survivalgames',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='factions',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=1, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='jobs',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=1, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='mcmmo',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=1, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='amplified',rbasic=0, rbiophys=1, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=0, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
,asdf(feat='legitimate',rbasic=0, rbiophys=0, popvol=0, popforce=0, rprevent=0, rexternal=0, rhetero=0, rincntv=0, rinst=0, rtech=1, rspat=0, rsoccap=0, rleader=0, rinfopred=0)
))
coded_key_tags_friv[,rnorms:=1]
coded_key_tags_friv[,feat_useful:=F]
coded_key_tags_friv[,feat_source:='tag']


#merge plugin classes tables
#plugins are plguins
#tags are tags, usually  human enetered or selected butmachine readable
#keywords are pulled from unstructred human text
coded_key_plugins <- merge( rbind(coded_key_plugins, coded_key_plugins_friv), coded_key_plugins_more, by=c("feat", "feat_source"), all=T)
coded_key_tags <- merge( rbind(coded_key_tags, coded_key_tags_friv), coded_key_tags_more, by=c("feat", "feat_source"), all=T)
coded_key_keywords <- merge( coded_key_keywords, coded_key_keywords_more, by=c("feat", "feat_source"), all=T)
coded_key <- rbind(coded_key_plugins, coded_key_tags, coded_key_keywords)
coded_key[,feat:=as.character(feat)]
#coded_key[,dataset_source:='hand']
#coded_key[,build:='bukkit']
### now fix names to impove matching to raw curse scrape data
###   coded_key_merge[is.na(url), feat_urlname]
###   coded_key_merge[is.na(url), list(feat)][feat %in% plug[,feat_urlname], feat]
###    or coded_key_merge[is.na(url), list(feat)][feat %in% plug[,feat_urlname], list( plug[,feat_urlname][which(feat == plug[,feat_urlname])] ), by=feat]
###   coded_key_merge[is.na(url), list(feat)][feat %in% tolower(plug[,str_replace_all(plug[,feat], " ", '')]), feat]
###    or coded_key_merge[is.na(url), list(feat)][feat %in% tolower(plug[,str_replace_all(plug[,feat], " ", '')]), list( plug[,feat_urlname][which(feat == tolower(plug[,str_replace_all(plug[,feat], " ", '')]))] ), by=feat]$V1

#plug[,feat_urlname:=str_replace(plug[,feat_urlname], "-", '')]
###  plug_wide[!is.na(c(str_match(name_url, 'a-n-t-i-t-n-t'))), ]
#
setkey(coded_key, feat)
coded_key[,feat_urlname:=feat]
coded_key['1vs1',feat_urlname:='pvp-1vs1']
coded_key['announcer',feat_urlname:='deckerz-announcer']
coded_key['antibotultra',feat_urlname:='antibot-ultra']
coded_key['antijoinbot',feat_urlname:='easy-anti-join-bot-proxie']
coded_key['areashop',feat_urlname:='regionbuyandrent']
coded_key['askyblock',feat_urlname:='skyblock']
coded_key['asynchworldedit',feat_urlname:='worldedit']
coded_key['asynchworldeditinjector',feat_urlname:='worldedit']
coded_key['asyncworldedit',feat_urlname:='worldedit']
coded_key['asyncworldeditinjector',feat_urlname:='worldedit']
coded_key['authme',feat_urlname:='authme-reloaded']
coded_key['backpacks',feat_urlname:='backpack-item']
coded_key['banmanager',feat_urlname:='ban-management']
coded_key['bedwarsrel',feat_urlname:='bedwars-rel']
coded_key['bettercrops',feat_urlname:='better-crops']
coded_key['bossbroadcast',feat_urlname:='NA']
coded_key['buycraftx',feat_urlname:='buycraft']
coded_key['chatcontrol',feat_urlname:='wintereco-chatcontrol']
coded_key['chatmanager',feat_urlname:='chat-manager']
coded_key['chestcommands',feat_urlname:='chest-commands']
coded_key['chestprotect',feat_urlname:='chestprotect']
coded_key['chestshopnotifier',feat_urlname:='csn']
coded_key['choptree3',feat_urlname:='choptree']
coded_key['clearchat',feat_urlname:='clearmychat']
coded_key['clearlag',feat_urlname:='clearlagg']
coded_key['commandsxl',feat_urlname:='NA']
coded_key['compatnocheatplus',feat_urlname:='compatnocheatplus-cncp']
coded_key['craftconomy3',feat_urlname:='craftconomy']
coded_key['creeperheal',feat_urlname:='creeperheal-nitnelave']
coded_key['customservermessages',feat_urlname:='csm']
coded_key['deathmessagesprime',feat_urlname:='death-messages']
coded_key['dueltag',feat_urlname:='NA']
coded_key['easyspawn',feat_urlname:='easy-spawn']
coded_key['enjinminecraftplugin',feat_urlname:='emp']
coded_key['essentialsantibuild',feat_urlname:='antibuild']
coded_key['essentialschat',feat_urlname:='essentials']
coded_key['essentialsgeoip',feat_urlname:='essentials']
coded_key['essentialsprotect',feat_urlname:='essentials']
coded_key['essentialsspawn',feat_urlname:='essentials']
coded_key['essentialsxmpp',feat_urlname:='essentials']
coded_key['galistener',feat_urlname:='give-anything-listener']
coded_key['griefprevention',feat_urlname:='grief-prevention']
coded_key['griefpreventionplus',feat_urlname:='grief-prevention']
coded_key['groupmanager',feat_urlname:='essentials']
coded_key['holographicdisplays',feat_urlname:='holographic-displays']
coded_key['invsee_v1.0.3',feat_urlname:='invsee-plugin']
coded_key['libdisguises',feat_urlname:='']
coded_key['marriagemaster',feat_urlname:='marriage-master']
coded_key['massivecore',feat_urlname:='mcore']
coded_key['mineresetliteplus',feat_urlname:='mineresetlite']
coded_key['multiworld',feat_urlname:='multiworld-v-2-0']
coded_key['orebfuscator3',feat_urlname:='orebfuscator']
coded_key['paintball',feat_urlname:='paintball_pure_war']
coded_key['permissionsbukkit',feat_urlname:='permbukkit']
coded_key['playerheads',feat_urlname:='player-heads']
coded_key['playermarkers',feat_urlname:='mapmarkers']
coded_key['plotme-defaultgenerator',feat_urlname:='plotme']
coded_key['plotsquared',feat_urlname:='plotme']
coded_key['proreports',build:='NA']
coded_key['quarterbukkit-plugin',feat_urlname:='quarterbukkit']
coded_key['questioner',build:='NA']
coded_key['ram_monitor',feat_urlname:='server-monitor']
coded_key['remotecontroller',feat_urlname:='bukkit-remote-controller']
coded_key['remotetoolkitplugin',feat_urlname:='NA']
coded_key['sentry',feat_urlname:='sentry-citizens2']
coded_key['sethome',feat_urlname:='myhome']
coded_key['shopkeeper',feat_urlname:='shopkeepers']
coded_key['simpleregionmarket',feat_urlname:='region-market-0-8']
coded_key['skins',feat_urlname:='skins-downloader']
coded_key['skinsrestorer',feat_urlname:='skins-downloader']
coded_key['spamyasak',build:='NA']
coded_key['superbuildbattle',feat_urlname:='super-build-battle']
coded_key['tablist',feat_urlname:='mytablist']
coded_key['tablistprefix',feat_urlname:='mytablist']
coded_key['tagapi',feat_urlname:='tag']
coded_key['tetris',feat_urlname:='mc-tetris']
coded_key['timtheenchanter',feat_urlname:='enchanter']
coded_key['towny',feat_urlname:='chestshop-towny']
coded_key['vanishnopacket',feat_urlname:='vanish']
coded_key['warpportals',feat_urlname:='warp-portals']
coded_key['wirelessredstone',feat_urlname:='wireless-redstone']
coded_key['yenileyici',feat_urlname:='NA']
coded_key['anti-grief',feat_urlname:='grief']
coded_key['libsdisguises',feat_urlname:='disguisecraft']
#coded_key[duplicated(coded_key, by=c("feat", "feat_source")), feat]
coded_key[,build:=NULL]
