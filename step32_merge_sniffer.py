# -*- coding: utf-8 -*-

"""
this script doesn't just integrate sniffer data into omni and reddit, it also looks for and aggregates additional short snippets of text for coding
"""

from local_settings import pathData, pathDataInPlugins
from libmcscrape import cat_mc_json_logs, standardize_address, get_freshest_data_date
from shutil import copyfile
import ujson
import sys
import re
sys.setrecursionlimit(10000) # 10000 is an example, try with different values

def get_valid_json_with_halflines(line, start_char=0):
    #print(line[start_char:])
    i_second_open_bracket = line[start_char:].find('{')
    if i_second_open_bracket == -1: 
        row = False
    else:
        failed=False
        try:
            row = ujson.loads(line[(start_char + i_second_open_bracket):])
        except ValueError as err: ### includes json.decoder.JSONDecodeError, which is buggy apparently
            failed=True
            pass
        if failed:
            row = get_valid_json_with_halflines(line, (start_char + i_second_open_bracket + 1))
    return(row)


### load sniffer json into a giant dict indexed by theserver location
sniffer_dataset = get_freshest_data_date("lib_datasets_sniffer.txt")
d_mcsniff = {}
with open(pathData+'mcsniffer/'+sniffer_dataset+'/'+"out_servers.json", 'r') as f_mcdata_in:
    for line in f_mcdata_in:
        mc = get_valid_json_with_halflines(line.strip())
        if not mc: 
            continue
        mc['mc_addr'] = standardize_address(mc['host']+':'+str(mc['port']))
        d_mcsniff[mc['mc_addr']]=mc

### use that to structure merge into existing json
def merge_sniffer_into_core_json(d_mcsniff, s_in_file, s_out_file ):
    mc_addrs_sniff = d_mcsniff.keys()
    mc_json_out = []
    print(len(mc_addrs_sniff))
    counter = 0
    counter2 = 0
    with open(s_in_file, 'r') as infile:
        for line in infile:
            counter += 1
            #if counter % 100 != 0: continue
            mc = ujson.loads(line)
            interesting_text = {}
            if 'motd' in mc: 
                if type(mc['motd']) == type(''):
                    interesting_text['motd'] = mc['motd']
                elif type(mc['motd']) == type({}):
                    interesting_text['motd'] = mc['motd']['text']
            if 'website_url' in mc and len(mc['website_url']) > 0: interesting_text['website'] = 'website'
            alternative_dependents = {}
            if 'votes' in mc: alternative_dependents['votes'] = mc['votes']
            if 'updated' in mc: alternative_dependents['updated'] = mc['updated']
            if 'rank' in mc: alternative_dependents['rank'] = mc['rank']
            ### sniff fields are (from 20160712 data collected on 20160719)
            #{ 'host': {"<class 'str'>": 97267},
                    #'id': {"<class 'int'>": 97267},
                    #'online': {"<class 'bool'>": 97267},
                    #'port': {"<class 'int'>": 97267},
                    #'signs': {"<class 'list'>": 97267},
                    #'state': {"<class 'str'>": 97267},
                    #'description': {"<class 'NoneType'>": 257, "<class 'str'>": 97010},
                    #'protocol_version': {"<class 'NoneType'>": 267, "<class 'int'>": 97000},
                    #'version': {"<class 'NoneType'>": 267, "<class 'str'>": 97000},
                    #'max_player_count': {"<class 'NoneType'>": 276, "<class 'int'>": 96991},
                    #'player_count': {"<class 'NoneType'>": 276, "<class 'int'>": 96991},
                    #'error': {"<class 'NoneType'>": 9455, "<class 'str'>": 87812},
                    #'plugins_fml': {"<class 'NoneType'>": 69432, "<class 'list'>": 27835},
                    #'whitelist': {"<class 'NoneType'>": 78537, "<class 'bool'>": 18730},
                    #'difficulty': {"<class 'NoneType'>": 86782, "<class 'int'>": 10485}, 
                    #'gamemode': {"<class 'NoneType'>": 86782, "<class 'int'>": 10485},
                    #'hardcore': {"<class 'NoneType'>": 86782, "<class 'bool'>": 10485},
                    #'level_type': {"<class 'NoneType'>": 86782, "<class 'str'>": 10485},
                    #'brand': {"<class 'NoneType'>": 86797, "<class 'str'>": 10470},
                    #'players': {"<class 'NoneType'>": 87392, "<class 'list'>": 9875},
                    #'help_p1': {"<class 'NoneType'>": 91787, "<class 'str'>": 5480},
                    #'software': {"<class 'NoneType'>": 94398, "<class 'str'>": 2869},
                    #'plugins': {"<class 'NoneType'>": 94820, "<class 'str'>": 2447},
                    #'welcome': {"<class 'NoneType'>": 96224, "<class 'str'>": 1043}}
            if 'mc_addr' in mc and standardize_address(mc['mc_addr']) in mc_addrs_sniff:
                mc_snf = d_mcsniff[standardize_address(mc['mc_addr'])]
                counter2 += 1
                mc['reported_sniff'] = True
                mc['whitelist'] = mc_snf['whitelist']
                #if 'protocol_version' in mc_snf: mc['snf_protocol_v'] = mc_snf['protocol_version']
                if 'server_version_number' not in mc:
                    if 'version' in mc:
                        mc['server_version_number'] = mc['version']
                    elif 'game_query' in mc and 'version' in mc['game_query']:
                        mc['server_version_number'] = mc['game_query']['version']
                    elif 'game_query' in mc and ('server_mod_version' in mc['game_query'] or 'server_mod_name' in mc['game_query']):
                        mc['server_version_number'] = mc['game_query'].get('server_mod_name','')+'_'+mc['game_query'].get('server_mod_version','')
                    elif 'version' in mc_snf:
                        mc['server_version_number'] = mc_snf['version']
                if 'description' in mc_snf and 'description' not in mc: 
                    mc['description'] = mc_snf['description']
                if 'description' in mc: 
                    interesting_text['description'] = mc['description']
                if 'welcome' in mc_snf: 
                    interesting_text['welcome'] = mc_snf['welcome']
                if 'help_p1' in mc_snf: 
                    interesting_text['help_p1'] = mc_snf['help_p1']
                if 'difficulty' in mc_snf: mc['snf_difficulty'] = mc_snf['difficulty']
                if 'gamemode' in mc_snf: mc['snf_gamemode'] = mc_snf['gamemode']
                if 'hardcore' in mc_snf: mc['snf_hardcore'] = mc_snf['hardcore']
                if 'level_type' in mc_snf: mc['snf_level_type'] = mc_snf['level_type']
                if 'brand' in mc_snf: mc['snf_brand'] = mc_snf['brand']
                if 'software' in mc_snf: mc['snf_software'] = mc_snf['software']
                #
                ### this is probably wrong, and I probably don't want it, but 
                ###  if I get to wanting it, I'll do it here someway like this.
                #mc['plugins_names'].append(mc_snf['plugins_fml'])  
                ### this is complicated because its non default, and many 
                ###  people use it to say many things, and its sometimes empty, 
                ###  and I don't want to add to existing plugin_names list a 
                ###  plugin already listed
                plugins_text_sniff = mc_snf['plugins']
                if plugins_text_sniff is not None and plugins_text_sniff != '':
                    if re.match("^Plugins (\d+): ", plugins_text_sniff):
                        plugin_names_sniff = [plug_name.strip() for plug_name in plugins_text_sniff.partition(':')[2].split(',')]
                        plugin_names_omni = mc['plugins_names']
                        mc['plugins_names'] = set(plugin_names_omni + plugin_names_sniff)
                        mc['reported_plugins'] = True
                    else:
                        interesting_text['plugins_override'] = plugins_text_sniff
                
                ### now handle signs
                if 'signs' in mc_snf:
                    interesting_text['signs'] = []
                    num_signs = 0
                    for sign in mc_snf['signs']:
                        if len( ''.join(sign['lines'])) == 0:  ### empty sign
                            mc_signtext = ''
                            continue  ### decided that I don't want these
                        else:
                            mc_signtext = '\\\\'.join(sign['lines']).strip('\\\\')
                            num_signs += 1
                        interesting_text['signs'].append( mc_signtext )
                    if num_signs == 0: interesting_text.pop('signs', None) ### in case all signs were empty
                    mc['snf_signs_count'] = num_signs
                else: 
                    mc['snf_signs_count'] = None
            else:
                mc['reported_sniff'] = False
            mc['text_short'] = interesting_text
            mc_json_out.append(mc)
            #json.dump(mc, outfile)
            #outfile.write("\n")
    with open(s_out_file, 'w') as outfile:
        #json.dump(mc_json_out, outfile)
        for mc in mc_json_out:
            outfile.write(ujson.dumps(mc))
            outfile.write("\n")
    print('second number gives number of servers matched to sniff data')
    print(counter, counter2)
print("merge sniffer into concatenated omni logs, and to reddit and reddit_all logs")
merge_sniffer_into_core_json(d_mcsniff, pathData+"step3_scraped_reddit_posts"+".json"    , pathData+"step32_scraped_reddit_posts.json")
merge_sniffer_into_core_json(d_mcsniff, pathData+"step3_scraped_reddit_posts_all"+".json", pathData+"step32_scraped_reddit_posts_all.json")
#merge_sniffer_into_core_json(d_mcsniff, pathData+"step3_scraped_omnimc_posts"+".json", pathData+"step32_scraped_omnimc_posts"+".json" )
