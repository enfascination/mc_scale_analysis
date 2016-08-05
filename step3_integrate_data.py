# -*- coding: utf-8 -*-
from local_settings import pathData, pathDataInPlugins
from libmcscrape import cat_mc_json_logs, standardize_address
from shutil import copyfile
import ujson

### manually update.  left column is from  lib_datasets_plugins.txt and right is from lib_datasets_mcsorg.txt.  this tells me which datasets to match with which
map_omni_to_mcs_org = {
          "20150603" : "20150513"
        , "20150828" : "20150513"
        , "20150617" : "20150513"
        , "20150923" : "20150513"
        , "20151101" : "20151101"
        , "20151230" : "20151101"
        , "20160202" : "20151101"
        , "20160229" : "20151101"
        , "20160323" : "20151101" 
        , "20160406" : "20160331" 
        , "20160504" : "20160331" 
        , "20160602" : "20160331" 
        , "20160712" : "20160331" 
        }


### start with reddit data
#reddit_datasets = ( "20141105" , "20150205"  , "20150513" , "20150617" , "20150721" , "20150828" , "20150923" , "20151026" , "20151123" , "20151230", "20160202", "20160229", "20160323")
reddit_datasets = [f.strip() for f in open("lib_datasets_reddit.txt",'r').readlines()]
### gather disparate reddit scrapes into one big file
cat_mc_json_logs(
    [pathDataInPlugins+dataset_date+"/"+"mcservers_step3.txt" for dataset_date in reddit_datasets]
    , reddit_datasets
    , pathData+"step3_scraped_reddit_posts"+".json"
    , pathData+"step3_scraped_reddit_posts"+"_all"+".json"
    , "reddit")


### the do all master scrapes
#omni_data_dates = ( "20150603" , "20150603" , "20150828" , "20150617" , "20150923" , "20151101" , "20151230" , "20160202" , "20160229" , "20160323" )
omni_data_dates = [f.strip() for f in open("lib_datasets_plugins.txt",'r').readlines()]
omni_data_files = ( 
        "20150603_startedmerging/mcorgservers_step3.txt"
        , "20150603_startedmerging/mcservers_step3_merged.txt"
        , "20150828"+"/"+"mcservers_step3_master.txt"
        , "20150617/mcshodanservers_step3_20150617.json"
        , "20150923"+"/"+"mcservers_step3_master.txt"
        , "20151101/mcservers_step3_master_20151106_shodanandmcorg.txt"
        , "20151230"+"/"+"mcservers_step3_master.txt"
        , "20160202"+"/"+"mcservers_step3_master.txt"
        , "20160229"+"/"+"mcservers_step3_master.txt"
        , "20160323"+"/"+"mcservers_step3_master.txt"
        , "20160406"+"/"+"mcservers_step3_master.txt"
        , "20160504"+"/"+"mcservers_step3_master.txt"
        , "20160602"+"/"+"mcservers_step3_master.txt"
        , "20160712"+"/"+"mcservers_step3_master.txt"
        )
print("concatenate master logs")
cat_mc_json_logs(
    [pathDataInPlugins+dataset_file for dataset_file in omni_data_files]
    , omni_data_dates
    , pathData+"step3_scraped_omnimc_posts"+".json"
    , pathData+"step3_scraped_omnimc_posts"+"_all"+".json"
    , "omni")


### then begin merging mcs_org data into master/omni
print("load mcs.org logs")
### merge the mcorg data into the master (and change the tags of the matches from omni to mcs_org)
### prep mcs_org data
### from Nov 25, 2014 email from Simon Marti
### "PS: The csv data file has no header, so here are its columns: id, name, description, tags, ip, port, version, banner, created, updated, youtube video, website url, country code, votes, rank, uptime, totaltime, daily uptime, daily totaltime. All times are in seconds."
### possible tags (from former first line of 20150309 datafile
### \N,\N,\N,"Creative, Factions, PvE, Survival, Towny, Survival Games, Creative, Factions, Survival, Survival Games, PvP, Economy, Survival, Parkour, PvP, Raiding, Towny, PvP, Raiding, Creative, Economy, KitPvP, Raiding, Survival, Economy, Factions, PvP, Roleplay, Survival, Economy, Factions, Hardcore, McMMO, Prison, Economy, Factions, PvP, Raiding, Survival, Economy, Mini Games, Survival, Parkour, PvE, PvP, Survival, Vanilla, Creative, Factions, Mini Games, Parkour, Survival, Survival, Economy, McMMO, Roleplay, PvP, Economy, Factions, PvE, PvP, Raiding, Economy, Factions, McMMO, PvE, Survival, Creative, Parkour, Survival, KitPvP, Mini Games, Parkour, PvP, Raiding, Economy, Roleplay, Survival, Economy, McMMO, PvP, Survival, Factions, McMMO, PvP, PvE, PvP, Survival, Economy, McMMO, Mini Games, PvP, Survival, Factions, McMMO, PvP, Raiding, Factions, Mini Games, KitPvP, Parkour, PvP, KitPvP, PvP, Creative, KitPvP, Parkour, Survival, Survival Games, Economy, McMMO, PvE, Survival, Mini Games, PvE, Survival, Mini Games, Economy",\N,\N,\N,\N,\N,\N,\N,\N,\N,\N,\N,\N,\N,\N,\N
### load mcs_org into a json file
d_mcs_org = {}
with open(pathData+'mcorgservers_omni_step1.txt', 'r') as f_mcdata_in:
    for line in f_mcdata_in:
        mc = ujson.loads(line)
        d_mcs_org[mc['post_uid']]=mc
### use that to structure merge into existing json
print(len(d_mcs_org.keys()))
print("merge mcs.org into concatenated omni logs")
counter = 0
counter2 = 0
mc_json_out = []
copyfile(pathData+"step3_scraped_omnimc_posts"+".json", pathData+"tmp_step3_scraped_omnimc_posts"+".json")
with open(pathData+"tmp_step3_scraped_omnimc_posts"+".json", 'r') as infile:
    for line in infile:
        counter += 1
        mc = ujson.loads(line)
        ### merging of relevant fields, incl ["id", "title", "selftext", "primary_tags", "ip", "port", "version", "banner", "created", "updated", "youtube_video", "website_url", "country_code", "votes", "rank", "uptime", "totaltime", "daily_uptime", "daily_totaltime"]
        mco = False
        #print(mc['dataset_date'], map_omni_to_mcs_org.get(mc['dataset_date'], False), True if d_mcs_org.get(standardize_address(mc['mc_addr'])+'_'+map_omni_to_mcs_org[mc['dataset_date']], False) else False)
        if map_omni_to_mcs_org.get(mc['dataset_date'], False):
            if d_mcs_org.get(standardize_address(mc['mc_addr'])+'_'+map_omni_to_mcs_org[mc['dataset_date']], False):
                mco = d_mcs_org[ standardize_address(mc['mc_addr'])+'_'+map_omni_to_mcs_org[ mc['dataset_date'] ] ]
        if mco:
            counter2 += 1
            mc['dataset_source'] = 'mcs_org'
            mc['title'] = mco['title']
            mc['selftext'] = mco['description']
            mc['primary_tags'] = mco['tags']
            mc['ip'] = mco['ip'].rstrip()
            mc['port'] = mco['port']
            mc['server_version_number'] = mco['version']
            mc['banner'] = mco['banner']
            mc['created'] = mco['created']
            mc['updated'] = mco['updated']
            mc['youtube_video'] = mco['youtube_video']
            mc['website_url'] = mco['website_url']
            mc['country_code'] = mco['country_code']
            mc['votes'] = mco['votes']
            mc['rank'] = mco['rank']
            mc['uptime'] = mco['uptime']
            mc['totaltime'] = mco['totaltime']
            mc['daily_uptime'] = mco['daily_uptime']
            mc['daily_totaltime'] = mco['daily_totaltime']
        mc_json_out.append(mc)
with open(pathData+"step3_scraped_omnimc_posts"+".json", 'w') as outfile:
    for mc in mc_json_out:
        outfile.write(ujson.dumps(mc))
        outfile.write('\n')
print(counter, counter2)


