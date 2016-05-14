# -*- coding: utf-8 -*-
""""
All servers at minecraftservers.org
"""

from collections import defaultdict  ### easier dictionaries
import pprint
pp = pprint.PrettyPrinter(indent=4)
import csv
csv.register_dialect('dboutput', quoting = csv.QUOTE_MINIMAL, escapechar = '\\')
import json
from datetime import datetime
from dateutil.parser import parse
from libmcscrape import standardize_address
from local_settings import pathData
#from mcstatus import MinecraftServer

#execfile('/Users/sfrey/projecto/research_projects/minecraft/server_surveyor/config_servers_survey001.py')
### initialize globals
data_path = '/Users/sfrey/projecto/research_projects/minecraft/mcsdotorg_server_surveyor/archive/'
#s_mcdata_in       = data_path+'servers_20151101.csv'
#s_mcdata_in_obs3  = data_path+'servers_20150513.csv'
#s_mcdata_in_obs2  = data_path+'servers_20150309.csv'
#s_mcdata_in_obs1  = data_path+'servers_20141126.csv'
#data_dates = ["20151101", "20150513", "20150309", "20141126"]
data_dates = [f.strip() for f in open("lib_datasets_players.txt",'r').readlines()]
s_mc_data = [data_path+'servers_'+date+'.csv' for date in data_dates]

### merge both csv files and their metadata into a json file
### first, get id's for all the first measurement
#l_ids_obs1 = getAttrFromDump(s_mcdata_in_obs1, "id")
#l_ids_obs2 = getAttrFromDump(s_mcdata_in_obs2, "id")
#l_ids_obs3 = getAttrFromDump(s_mcdata_in_obs3, "id")

### second, get everything for every second measurement
i_rowcount = 0
i_badrowcount = 0
total_tags = defaultdict(lambda: 0)
### servers attempted, servers reached, servers with details)
j = 0
k = 0
with open(pathData+'mcorgservers_omni_step1.txt', 'w') as f_mcdata_out:
    for thedate, thefile in zip(data_dates, s_mc_data):
        with open(thefile, 'rU') as csvfile:
            reader = csv.DictReader(csvfile, dialect='dboutput', fieldnames=["id", "title", "description", "tags", "ip", "port", "version", "banner", "created", "updated", "youtube_video", "website_url", "country_code", "votes", "rank", "uptime", "totaltime", "daily_uptime", "daily_totaltime"])
            for mc in reader:
                ### QC
                if len(mc) != 19: 
                    print("PROBLEM at {rc}, row length:{rlength}".format(rc=i_rowcount,rlength=len(mc)))
                    print('\n'.join(mc))
                for key, entry in iter(mc.items()):
                    if entry == 'N': mc[key]=None
                if mc['id'] == None: 
                    #i_badrowcount += 1
                    #print "bad row:", mc)
                    continue
                
                i_rowcount += 1
                #if i_rowcount > 100: break
                ### data formatting
                mc['id'] = int(mc['id'])
                mc['mc_addr'] = standardize_address(mc['ip']+':'+mc['port'])
                mc['port'] = int(mc['port']) 
                mc['dataset_date'] = thedate
                mc['dataset_source'] = 'mcs_org'
                mc['date_created'] = parse(mc['updated']).strftime('%Y%m%d')
                mc['post_uid'] = mc['mc_addr']+'_'+mc['dataset_date']
                mc['votes'] = int(mc['votes'])
                mc['rank'] = int(mc['rank'])
                mc['totaltime'] = int(mc['totaltime'])
                mc['daily_totaltime'] = int(mc['daily_totaltime'])
                mc['uptime'] = int(mc['uptime'])
                mc['daily_uptime'] = int(mc['daily_uptime'])
                mc['tags'] = mc['tags'].split(', ')
                ### new fields
                #mc['measure_one'] = str(mc['id']) in l_ids_obs1
                #mc['measure_two'] = str(mc['id']) in l_ids_obs2
                #mc['measure_three'] = str(mc['id']) in l_ids_obs3
                #mc['measure_four'] = True
                mc['is_wanted'] = False  ### is this a wanted ad by a player for a server?  no.  this tag si for reddit compatibility
                ### output
                for tag in mc['tags']: total_tags[tag] += 1
                json.dump(mc, f_mcdata_out)
                f_mcdata_out.write('\n')
        print("rows:", i_rowcount)
        pp.pprint(dict(total_tags))
        ### filter out irrelevant servers
        ### start pinging them to add metadata from query
