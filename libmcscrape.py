import json
import csv
import subprocess
from pprint import pprint

juicy_words = ('ban', 'banned', 'rules', 'whitelisted', 'age', '18', '18+', 'owner', 'owners', 'mods', 'moderators', 'admins', 'admin', 'moderator', 'bans', 'faction', 'community', 'harass', 'threat', 'threatening', 'mature', 'adult', 'friendly', 'fair', 'unfair', 'staff', 'grief', 'griefs', 'griefing', 'respect', 'respectful', 'rollback', 'spam', 'spamming', 'swearing', 'swear', 'cheat', 'cheats' , "cheating" , "hack" , "hacks" , "hacking" , "steal" , "stealing" , "tnt", "fire" , "member" , "members" , "whitelist" , "blacklist" , "greylist" , "rules" , "staff" , "abuse" , "abusers" , "abuser" , "forum" , "forums" , "wiki" , "website" , "subreddit" , "ddos" , "location" , "locale", "shop", "forms", "form", "application", "docs", "apply", "official", "site", "team", 'join', "voip", "skype", "mumble", "teamspeak")

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
        try:
            dObs = json.loads(log_line)
            ### this can go wrong in a few ways, either plank line, 
            ### two json pings on one line, or other line crud
            ### json.loads accepts not only {} but also [] and 1.  it doesn't accept ''
            if isinstance(dObs, int) or isinstance(dObs, list):
                ret = (0, "")
            else: 
                ret = (1, dObs)
        except json.decoder.JSONDecodeError as err:  
            caught = True
        if caught:
            #print(log_line)
            if log_line[0] == "{":
                l_mult_obs = log_line.split("}{")
                if len(l_mult_obs) == 1:  l_mult_obs = log_line.split("} {") ### this means I used the wrong delimiter.  the only other way it could be one is if it hadn't thrown an error
                if len(l_mult_obs) == 2:
                    dObs1 =  json.loads(      l_mult_obs[0] + '}')
                    dObs2 =  json.loads('{' + l_mult_obs[1])
                    ret = (2, (dObs1, dObs2))
                elif len(l_mult_obs) == 3:
                    dObs1 =  json.loads(      l_mult_obs[0] + '}')
                    dObs2 =  json.loads('{' + l_mult_obs[1] + '}')
                    dObs3 =  json.loads('{' + l_mult_obs[2])
                    ret = (3, (dObs1, dObs2, dObs3))
                elif len(l_mult_obs) == 4:
                    dObs1 =  json.loads(      l_mult_obs[0] + '}')
                    dObs2 =  json.loads('{' + l_mult_obs[1] + '}')
                    dObs3 =  json.loads('{' + l_mult_obs[2] + '}')
                    dObs4 =  json.loads('{' + l_mult_obs[3])
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
                json.dump(rpost, io_servers)
                io_servers.write("\n")
                json.dump(rpost, io_allservers)
                io_allservers.write("\n")
        else: ## don't care so much about duplicates in this other data.  
            ## This is what I'll use for learning topics that I apply to the resticted dataset
            json.dump(rpost, io_allservers)
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
    keyword_keys = ( 'plugins_admin', 'plugins_antigrief', 'plugins_chat', 'plugins_developer', 'plugins_economy', 'plugins_fixes', 'plugins_fun', 'plugins_general', 'plugins_informational', 'plugins_mechanics', 'plugins_misc', 'plugins_roleplay', 'plugins_teleport', 'plugins_website', 'plugins_worldedit', 'plugins_worldgen', 'plugins_voip') 
    server_inventory = {0:0, 1:0, 2:0, 3:0}
    with open(s_infile, 'r') as infile:
        with open(s_outfile, 'w') as outfile:
            mcdata_csv = csv.writer(outfile, delimiter='\t')
            header_csv = ("post_uid", "date_post", "srv_addr", "srv_v", "srv_max", "srv_details", 'feat', 'feat_type', 'feat_source', 'feat_trust', "srv_repstat", "srv_repquery", "srv_repplug", "srv_repsample", "dataset_source")
            row_edit_trigger = len(header_csv)### I'll be using this to make sure I always update my headers with my csv entries
            mcdata_csv.writerow(header_csv) #, "srv_repsample" ))
            for mcline in infile: 
                mc = json.loads(mcline)
                ### for data quality filtering and logging
                if not mc['reported_status']: continue
                srv_details = mc['reported_status'] + mc['reported_query'] + mc.get('reported_plugins', False) 
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
                    print('PROBLEM DFASDKD max:', mc['players_max'], mc['game_query']['players_max'],type(mc['players_max']),type(mc['game_query']['players_max']) )
                    mc['players_max'] = int(mc['game_query']['players_max'])
                if (srv_details >= 2) and (mc['players_online'] != int(mc['game_query']['players_online'])): 
                    print('PROBLEM DFASDKD online:', mc['players_online'], mc['game_query']['players_online'])
                    mc['players_online'] = int(mc['game_query']['players_online'])
                ### add rows by data type
                srv_ver = mc['server_version_number']
                if mc['dataset_source'] in ('omni', 'mcs_org', 'reddit'):  
                    server_inventory[srv_details] += 1
                    if (srv_details >= 3):
                        for t in mc['plugins_names']: 
                            row_csv = (mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), t.lower(), '', 'plugins', 1, mc['reported_status'], mc['reported_query'], mc.get('reported_plugins',u'NA'), mc.get('reported_sample',u'NA'), mc['dataset_source'])
                            if len(row_csv) != row_edit_trigger: raise NotImplementedError("PROBLEM GGSHJJJRRRJASDDGGD: update your headers")
                            mcdata_csv.writerow(row_csv)
                    if mc['dataset_source'] in ('mcs_org', 'reddit'):  ### not ideal way to handle reddit speific metadata.  better to refactor and make a redi specific function, but oh well
                        if mc.get('is_wanted'): continue  ### XXX this is reddit specific and not good here
                        for t in mc['primary_tags']: 
                            row_csv = (mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), t.lower(), u'', 'primary_tags', 1, mc['reported_status'], mc['reported_query'], mc.get('reported_plugins',u'NA'), mc.get('reported_sample',u'NA'), mc['dataset_source'])
                            if len(row_csv) != row_edit_trigger: raise NotImplementedError("PROBLEM GGSHJJJRRRJASD: update your headers")
                            mcdata_csv.writerow(row_csv)
                        for t in juicy_words:
                            if t in mc['selftext'].lower():
                                mcdata_csv.writerow((mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), t.lower(), u'', 'descriptor', 1, mc['reported_status'], mc['reported_query'], mc.get('reported_plugins',u'NA'), mc.get('reported_sample',u'NA'), mc['dataset_source']))
                        if mc['dataset_source'] == 'reddit':
                            ### this is reddit specific and not good here
                            if mc.get('iffy', False) and (
                                    len(mc['iffy']['server_ip_candidates']) == 0) and (
                                    len(mc['iffy']['server_url_candidates']) == 0):
                                srv_details = 0
                                print("PROBLEM DFASDDDD: server slipped through bug around empty candidate list and had bad data")
                                continue
                            for t in mc['secondary_tags']: 
                                mcdata_csv.writerow((mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), t.lower(), u'', 'secondary_tags', 1, mc['reported_status'], mc['reported_query'], mc.get('reported_plugins',u'NA'), mc.get('reported_sample',u'NA'), mc['dataset_source']))
                            if (srv_details >= 3):
                                for k in keyword_keys:
                                    key_trust = 0 if k != 'plugins_voip' else 1
                                for t in mc['iffy'][k]: 
                                    mcdata_csv.writerow((mc['post_uid'], mc['dataset_date'], mc['mc_addr'], srv_ver, int(mc['players_max']), int(srv_details), t.lower(), k, 'plugins', key_trust, mc['reported_status'], mc['reported_query'], mc.get('reported_plugins',u'NA'), mc.get('reported_sample',u'NA'), mc['dataset_source']))
                else: 
                    print("BIG PROBLEM DAFDSAJFDAJSDF")
            pprint(server_inventory)
