from local_settings import pathData, pathDataInPlugins, pathDataInMCSOrg
from libmcscrape import standardize_address, extract_features_from_mcjson_logs
import csv
import json
import subprocess

### now convert all those files to an r-friendly format
print("produce server/week/plugin rows from omni logs")
### pull the reddit scrapes together into a csn file for R to work with
extract_features_from_mcjson_logs(pathData+"step3_scraped_reddit_posts.json", pathData+'step4_reddit_mcservers.csv')
### pull the reddit scrapes together into a csn file for R to work with
extract_features_from_mcjson_logs(pathData+"step3_scraped_omnimc_posts.json", pathData+'step4_omnimc_mcservers.csv')

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
                        mcdata_csv.writerow([
                            mc.get('post_uid', visit)
                            , visit
                            , data_source_tag
                            , (mc['title']+' '+mc['selftext']).replace('\n', ' ').replace('"', "'").replace('[[', ' ').replace(']]', ' ').replace('**', ' ').replace('[', ' ').replace(']', ' ').replace('*', ' ').replace('.', ' ').replace('/', ' ').replace('', '').replace('!', " ").replace('(', " ").replace(')', " ").replace(':', ' ')]) #, "srv_repsample" ))
    print(len(visits))

### prep reddit data
prep_topic_analysis(pathData+"step3_scraped_reddit_posts_all"+".json", pathData+"step4_reddit_texts.csv", "reddit")
### prep mcs_org data
prep_topic_analysis(pathData+"step3_scraped_omnimc_posts"+".json", pathData+"step4_mcs_org_texts.csv", "mcs_org")
