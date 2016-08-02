# -*- coding: utf-8 -*-
import ujson
import csv
import re
import subprocess
from pprint import pprint
from shutil import copyfile


#a few functions
def standardize_address (saddr): 
    saddr = saddr.lower() ### standardize case
    saddr = saddr.replace('\n', '') #### bc some addresses have a newline for some reason## some
    ### if there is no port, or only a default port, 
    ###  then report the name/ip of the server plain.  
    ###  if there is a special port, leave it in the name.  
    ###  that way an ip can serve multiple servers and I 
    ###  measure each of them separately.
    if saddr.partition(':')[2] == '' or  saddr.partition(':')[2] == '25565':  
        saddr = saddr.partition(':')[0]
    return(saddr)

def load_dirty_json(log_line):
    #if isinstance(log_line.
    ### sometimes the incoming json is two entries to a line, and sometimes it is just ugly newlines or something on a line
    log_line = log_line.strip()
    caught = False
    if log_line == '':
        ret = (0, '')
    else: 
        #print(log_line)
        try:
            dObs = ujson.loads(log_line)
            ### this can go wrong in a few ways, either plank line, 
            ### two json pings on one line, or other line crud
            ### json.loads accepts not only {} but also [] and 1.  it doesn't accept ''
            if isinstance(dObs, int) or isinstance(dObs, list):
                ret = (0, "")
            else: 
                ret = (1, dObs)
        except ValueError as err:  
            caught = True
        if caught:
            #print(log_line)
            if log_line[0] == "{":
                l_mult_obs = log_line.split("}{")
                if len(l_mult_obs) == 1:  l_mult_obs = log_line.split("} {") ### this means I used the wrong delimiter.  the only other way it could be one is if it hadn't thrown an error
                if len(l_mult_obs) == 2:
                    dObs1 =  ujson.loads(      l_mult_obs[0] + '}')
                    dObs2 =  ujson.loads('{' + l_mult_obs[1])
                    ret = (2, (dObs1, dObs2))
                elif len(l_mult_obs) == 3:
                    dObs1 =  ujson.loads(      l_mult_obs[0] + '}')
                    dObs2 =  ujson.loads('{' + l_mult_obs[1] + '}')
                    dObs3 =  ujson.loads('{' + l_mult_obs[2])
                    ret = (3, (dObs1, dObs2, dObs3))
                elif len(l_mult_obs) == 4:
                    dObs1 =  ujson.loads(      l_mult_obs[0] + '}')
                    dObs2 =  ujson.loads('{' + l_mult_obs[1] + '}')
                    dObs3 =  ujson.loads('{' + l_mult_obs[2] + '}')
                    dObs4 =  ujson.loads('{' + l_mult_obs[3])
                    ret = (3, (dObs1, dObs2, dObs3))
                else:
                    #print(log_line)
                    raise 
            else:
                ### this catches the messy (non salveabable) things that jsonloads throws errors on, like booleans
                ret = (0, "")
    return ret

def cat_mc_json_logs(infile_strings, dataid_strings, s_clean_outfile, s_full_outfile, dataset_tag):
    def write_plugins_json_row(rpost, io_servers, io_allservers, all_post_ids, dataset_tag):
        ### filter out posts for which I have no data
        rpost['dataset_date'] = dataset_date ### add the dataset id key to the row so I can ref it later
        rpost['dataset_source'] = dataset_tag 
        if 'mc_addr' in rpost.keys(): 
            rpost['mc_addr'] = standardize_address(rpost['mc_addr'])
            rpost['post_uid'] = rpost['mc_addr']+'_'+rpost['dataset_date']
            #rpost['reddit'] = True
            if not rpost['post_uid'] in all_post_ids:
                all_post_ids.append(rpost['post_uid'])
                io_servers.write(ujson.dumps(rpost))
                io_servers.write("\n")
                io_allservers.write(ujson.dumps(rpost))
                io_allservers.write("\n")
        else: ## don't care so much about duplicates in this other data.  
            ## This is what I'll use for learning topics that I apply to the resticted dataset
            io_allservers.write(ujson.dumps(rpost))
            io_allservers.write("\n")
        return all_post_ids
    ### first get everything into one big file, tagged by it's dataset date
    all_post_ids = [] 
    with open(s_clean_outfile, 'w') as io_servers:
        with open(s_full_outfile, 'w') as io_allservers:
            for dataset_date, dataset_path in zip(dataid_strings, infile_strings):
                print(dataset_date)
                ### read backwards for most recent edit of all individual posts
                for i, log_line in enumerate(subprocess.Popen(["tail","-r",dataset_path], stdout=subprocess.PIPE, universal_newlines=True).stdout):
                    k, rpost = load_dirty_json(log_line)
                    if k==1:
                        all_post_ids = write_plugins_json_row(rpost, io_servers, io_allservers, all_post_ids, dataset_tag)
                    elif k==2:
                        all_post_ids = write_plugins_json_row(rpost[0], io_servers, io_allservers, all_post_ids, dataset_tag)
                        all_post_ids = write_plugins_json_row(rpost[1], io_servers, io_allservers, all_post_ids, dataset_tag)
    print(len(all_post_ids))

def extract_features_from_mcjson_logs(s_infile, s_outfile):
    juicy_words = ('ban', 'banned', 'rules', 'whitelisted', 'age', '18', '18+', 'owner', 'owners', 'mods', 'moderators', 'admins', 'admin', 'moderator', 'bans', 'faction', 'community', 'harass', 'threat', 'threatening', 'mature', 'adult', 'friendly', 'fair', 'unfair', 'staff', 'grief', 'griefs', 'griefing', 'respect', 'respectful', 'rollback', 'spam', 'spamming', 'swearing', 'swear', 'cheat', 'cheats' , "cheating" , "hack" , "hacks" , "hacking" , "steal" , "stealing" , "tnt", "fire" , "member" , "members" , "whitelist" , "blacklist" , "greylist" , "rules" , "staff" , "abuse" , "abusers" , "abuser" , "forum" , "forums" , "wiki" , "website" , "subreddit" , "ddos" , "location" , "locale", "shop", "forms", "form", "application", "docs", "apply", "official", "site", "team", 'join', "voip", "skype", "mumble", "teamspeak", "bannable", "donation", "donations", "donate")
    juicy_word_beginnings = ('ban', 'rule', 'whitelist', 'age', '18', 'owner', 'moderat', 'admin', 'staff', 'faction', 'commun', 'harass', 'threat', 'matur', 'adult', 'friend', 'fair', 'unfair', 'grief', 'respect', 'rollback', 'spam', 'swear', 'cheat', "hack" , "steal" , "tnt", "fire" , "member" , "blacklist" , "greylist" , "abus" , "forum" , "wiki" , "website", "site", "subreddit" , "ddos" , "location" , "locale", "shop", "form", "appl", "official", "team", 'join', "voip", "skype", "mumble", "teamspeak", "donat", 'regist', 'apply', 'appli')
    keyword_keys = ( 'plugins_admin', 'plugins_antigrief', 'plugins_chat', 'plugins_developer', 'plugins_economy', 'plugins_fixes', 'plugins_fun', 'plugins_general', 'plugins_informational', 'plugins_mechanics', 'plugins_misc', 'plugins_roleplay', 'plugins_teleport', 'plugins_website', 'plugins_worldedit', 'plugins_worldgen', 'plugins_voip') 
    server_inventory = {0:0, 1:0, 2:0, 3:0, 4:0}
    with open(s_infile, 'r') as infile:
        with open(s_outfile, 'w') as outfile:
            mcdata_csv = csv.writer(outfile, delimiter='\t')
            header_csv = ("post_uid", "date_post", "srv_addr", "srv_v", "srv_max", "srv_details", 'feat', 'feat_code', 'feat_type', 'feat_source', 'feat_trust', "srv_repstat", "srv_repquery", "srv_repplug", "srv_repsample", "srv_repsniff", "dataset_source")
            row_edit_trigger = len(header_csv)### I'll be using this to make sure I always update my headers with my csv entries
            mcdata_csv.writerow(header_csv) #, "srv_repsample" ))
            for mcline in infile: 
                mc = ujson.loads(mcline)
                ### for data quality filtering and logging
                #print(mc)
                if not mc['reported_status']: continue
                #if mc.get('reported_plugins', False):
                if not 'reported_plugins' in mc.keys():
                    mc['reported_plugins'] = False
                #fix a mistake from higher up the pipeline in which vanilla servers reporting full queries get listed as not reporting plugins, as opposed to reporting no plugins
                if not mc['reported_plugins'] and ( mc.get('plugins_names', False) == [] ):
                    mc['reported_plugins'] = True  ### this is fixing a mistake i made
                    mc['plugins_names'] = ['VANILLA_SERVER',]
                if 'reported_sniff' not in mc:
                    mc['reported_sniff'] = False
                srv_details = mc['reported_status'] + mc['reported_query'] + mc['reported_plugins'] + mc['reported_sniff']
                #print("srv_details", srv_details )
                ### fixed to delete because they are getting fixed earlier int he pipeline but it's ok if i forget because they are benign
                if not mc.get('game_query', False):
                    mc['game_query'] = {}
                if not mc['game_query'].get('players_max', False): 
                    mc['game_query']['players_max'] = mc['players_max']
                if not mc['game_query'].get('players_online', False):
                    mc['game_query']['players_online'] = mc['players_online']
                ### sanity checks
                #if (srv_details >= 2) and (mc['motd'] != mc.get('motd2')): print('PROBLEM ASDF:', mc['motd'], mc['motd'])
                #print([mc['reported_status'] , mc['reported_query'] , mc.get('reported_plugins', False) ] )
                #print(mc['players_max'])
                #print(mc['game_query'])
                #print(mc['game_query']['players_max'])
                if (srv_details >= 2) and (mc['players_max'] != int(mc['game_query']['players_max'])): 
                    #print('PROBLEM DFASDKD max:', mc['players_max'], mc['game_query']['players_max'],type(mc['players_max']),type(mc['game_query']['players_max']) )
                    mc['players_max'] = int(mc['game_query']['players_max'])
                if (srv_details >= 2) and (mc['players_online'] != int(mc['game_query']['players_online'])): 
                    #print('PROBLEM DFASDKD online:', mc['players_online'], mc['game_query']['players_online'])
                    mc['players_online'] = int(mc['game_query']['players_online'])
                ### more prep and processing, but after filteringand ada data quality
                ### add rows by data type
                if 'server_version_number' not in mc and 'version' in mc: mc['server_version_number'] = mc['version']
                srv_ver = mc['server_version_number'].encode('ascii', 'ignore')
                if mc['reported_sniff']:
                    plugin_names_snf = []
                    if 'whitelist' in mc and mc['whitelist']:
                        plugin_names_snf.append('WHITELISTED_SERVER')
                    if 'snf_gamemode' in mc:
                        if mc['snf_gamemode'] == 0:
                            if 'snf_hardcore' in mc and mc['snf_hardcore']:
                                plugin_names_snf.append('gamemode_hardcore')
                            else:
                                plugin_names_snf.append('gamemode_survival')
                        elif mc['snf_gamemode'] == 1:
                            plugin_names_snf.append('gamemode_creative')
                        elif mc['snf_gamemode'] == 2:
                            plugin_names_snf.append('gamemode_adventure')
                    if 'snf_difficulty' in mc and mc['snf_difficulty'] is not None:
                        plugin_names_snf.append('difficulty_'+str(mc['snf_difficulty']))
                    if 'snf_level_type' in mc and mc['snf_level_type'] is not None:
                        plugin_names_snf.append('leveltype'+mc['snf_level_type'])
                    if 'text_short' in mc and \
                            'help_p1' in mc['text_short'] and \
                            mc['text_short']['help_p1'] is not None and \
                            len(mc['text_short']['help_p1']) > 0:
                        plugin_names_snf.append('helppages')
                    if len(plugin_names_snf) > 0: mc['plugins_names'] = plugin_names_snf
                ### start wrting to file
                if mc['dataset_source'] in ('omni', 'mcs_org', 'reddit'):  
                    server_inventory[srv_details] += 1
                    if ('location' in mc and mc['location']['country_code'] is not None)\
                            or ('country code' in mc and mc['country code'] != '')\
                            or ('country_code' in mc and mc['country_code'] != ''):
                        if 'location' in mc and 'country_code' not in mc:
                            mc['country_code'] = mc['location']['country_code']
                        elif 'country code' in mc and 'country_code' not in mc:
                            mc['country_code'] = mc['country code']
                            del mc['country code']
                        mc_cc = 'cc_'+mc['country_code'].lower()
                        row_csv = (mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), mc_cc, 'tag_'+mc_cc, '', 'tag', 1, mc['reported_status'], mc['reported_query'], mc['reported_plugins'], mc.get('reported_sample',u'NA'), mc['reported_sniff'], mc['dataset_source'])
                    elif 'country code' in mc: 
                        del mc['country code']
                        mcdata_csv.writerow(row_csv)
                    if (srv_details >= 3):
                        for t in mc['plugins_names']: 
                            row_csv = (mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), t.lower(), 'plugin_'+t.lower(), '', 'plugin', 1, mc['reported_status'], mc['reported_query'], mc['reported_plugins'], mc.get('reported_sample',u'NA'), mc['reported_sniff'], mc['dataset_source'])
                            if len(row_csv) != row_edit_trigger: raise NotImplementedError("PROBLEM GGSHJJJRRRJASDDGGD: update your headers")
                            mcdata_csv.writerow(row_csv)
                    if mc['dataset_source'] in ('mcs_org', 'reddit'):  ### not ideal way to handle reddit speific metadata.  better to refactor and make a redi specific function, but oh well
                        if mc.get('is_wanted'): continue  ### XXX this is reddit specific and not good here
                        ### handle an error
                        if mc.get('iffy', False) and ( ### this is reddit specific and not good here
                                len(mc['iffy']['server_ip_candidates']) == 0) and (
                                len(mc['iffy']['server_url_candidates']) == 0):
                            srv_details = 0
                            print("PROBLEM DFASDDDD: server slipped through bug around empty candidate list and had bad data")
                            continue
                        ### now process text
                        mc_tags = mc['primary_tags']
                        mc_tags.extend(mc.get('secondary_tags', []))
                        for t in mc_tags:
                            mc_tag = t.encode('ascii','ignore').lower().strip()
                            row_csv = (mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), mc_tag, 'tag_'+mc_tag, u'', 'tag', 1, mc['reported_status'], mc['reported_query'], mc['reported_plugins'], mc.get('reported_sample',u'NA'), mc['reported_sniff'], mc['dataset_source'])
                            if len(row_csv) != row_edit_trigger: raise NotImplementedError("PROBLEM GGSHJJJRRRJASD: update your headers")
                            mcdata_csv.writerow(row_csv)
                        mc_text = re.sub(r'[^a-zA-Z0-9=]', ' ', mc['selftext'].lower())
                        for t in juicy_word_beginnings:
                            #if re.search(r'\b'+t+r'\b', mc_text):
                            if re.search(r'\b'+t, mc_text):
                                mcdata_csv.writerow((mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), t.lower(), 'keyword_'+t.lower(), u'', 'keyword', 1, mc['reported_status'], mc['reported_query'], mc['reported_plugins'], mc.get('reported_sample',u'NA'), mc['reported_sniff'], mc['dataset_source']))
                    if mc['reported_sniff']:
                        if 'text_short' in mc:# and type(mc['text_short']) == type({}):
                            for text_short_type, text_short in mc['text_short'].items():
                                if text_short_type == 'signs':
                                    feat_type = 'sign'
                                    text_short = ' '.join(text_short)
                                    for i in range(mc['snf_signs_count']):
                                        mcdata_csv.writerow((mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), feat_type, feat_type+'_'+feat_type, u'', feat_type, 1, mc['reported_status'], mc['reported_query'], mc['reported_plugins'], mc.get('reported_sample',u'NA'), mc['reported_sniff'], mc['dataset_source']))
                                else: 
                                    feat_type = 'keyword'
                                ### wierd edge case i now filter earlier in the pipeline, so if I see this I can delete it
                                if 'motd' == text_short_type: 
                                    if type(mc['text_short']['motd']) == type({}):
                                        text_short = mc['motd']['text']
                                if text_short is None or len(text_short.strip()) == 0: continue
                                #print(text_short_type, text_short)
                                mc_text = re.sub(r'[^a-zA-Z0-9=]', ' ', text_short.lower())
                                for t in juicy_word_beginnings:
                                    #if re.search(r'\b'+t+r'\b', mc_text):
                                    if re.search(r'\b'+t, mc_text):
                                        mcdata_csv.writerow((mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), t, feat_type+'_'+t, u'', feat_type, 1, mc['reported_status'], mc['reported_query'], mc['reported_plugins'], mc.get('reported_sample',u'NA'), mc['reported_sniff'], mc['dataset_source']))
                        ### I got rid of this by getting rid of all its parts
                        ### if mc['dataset_source'] == 'reddit':
                            #for t in mc['secondary_tags']: 
                                #mcdata_csv.writerow((mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), t.lower(), u'', 'tag', 1, mc['reported_status'], mc['reported_query'], mc['reported_plugins'], mc.get('reported_sample',u'NA'), mc['reported_sniff'], mc['dataset_source']))
                            ### this attempted to pull plugin names fromteh description text to supplmment missing plugin info elsewhere.  that's to ugly for me now
                            ### if (srv_details >= 3):
                                ### for k in keyword_keys:
                                    ### key_trust = 0 if k != 'plugins_voip' else 1
                                    ### for t in mc['iffy'][k]: 
                                        ### mcdata_csv.writerow((mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), t.lower(), k, 'plugin', key_trust, mc['reported_status'], mc['reported_query'], mc['reported_plugins'], mc.get('reported_sample',u'NA'), mc['reported_sniff'], mc['dataset_source']))
                else: 
                    print("BIG PROBLEM DAFDSAJFDAJSDF")
            pprint(server_inventory)
