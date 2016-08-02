# -*- coding: utf-8 -*-
from local_settings import pathData, pathDataInPlugins, pathDataInMCSOrg
from libmcscrape import standardize_address, extract_features_from_mcjson_logs
import csv
import json
import subprocess
from pprint import pprint


def prep_topic_analysis(pathDataIn, pathDataOut, data_source_tag, onlyUnique=True):
    visits = set()
    #with open(pathDataIn, 'r') as infile: 
    with subprocess.Popen(["tail","-r",pathDataIn], stdout=subprocess.PIPE, universal_newlines=True).stdout as infile:
        with open(pathDataOut, 'w') as outfile:
            mcdata_csv = csv.writer(outfile, delimiter='\t')
            for mcline in infile: 
                mc = json.loads(mcline)
                if not mc.get('is_wanted',False) and mc.get('selftext', False) and len(mc['selftext']) > 0: 
                    visit = standardize_address(mc.get('mc_addr', ''))
                    if not onlyUnique or not visit in visits:
                        visits.add(visit)
                        try:
                            mcdata_csv.writerow([
                                mc.get('post_uid', visit)
                                , visit
                                , data_source_tag
                                , (mc['title']+' '+mc['selftext']).encode("UTF-8").replace('\n', ' ').replace('"', "'").replace('[[', ' ').replace(']]', ' ').replace('**', ' ').replace('[', ' ').replace(']', ' ').replace('*', ' ').replace('.', ' ').replace('/', ' ').replace('', '').replace('!', " ").replace('(', " ").replace(')', " ").replace(':', ' ')]) #, "srv_repsample" ))
                        except UnicodeEncodeError:
                            pprint(mc)
                            raise
    print(len(visits))

### prep reddit data
prep_topic_analysis(pathData+"step32_scraped_reddit_posts_all"+".json", pathData+"step4_reddit_texts.csv", "reddit")
### prep mcs_org data
prep_topic_analysis(pathData+"step32_scraped_omnimc_posts"+".json", pathData+"step4_mcs_org_texts.csv", "mcs_org")
