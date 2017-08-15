from local_settings import pathDataOut
from libmcscrape import load_dirty_json, standardize_address, get_freshest_data_date
import csv
import gzip
import hashlib
from pprint import pprint

players_dataset_date = get_freshest_data_date("lib_datasets_players.txt")
pathScrapeDataIn = '/Users/sfrey/projecto_staid/minecraft/' + players_dataset_date + '/' + "dump_players_online.json.gz"
#pathPlayerDataOut = "/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/data/player_visits.csv"
#pathServerDataOut = "/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/data/server_visits.csv"
pathServerDataOut = pathDataOut + "step0_server_visits.csv"
pathPlayerDataOut = pathDataOut + "step0_player_visits.csv"

def write_scrape_csv_row(dObs, playerWriter, serverWriter):
    dObs['mc_addr'] = standardize_address(dObs['mc_addr'])
    npopObs = len( dObs['players'] )
    for p in dObs['players']:
        playerWriter.writerow([dObs['timestamp'], dObs['mc_addr'], hashlib.md5(p.encode('utf-8')).hexdigest()])
    hackedAPI = (
        len( dObs['players'] ) == 0 or ### not sure why, but this is a reliable signal of a hacked API
        (len( dObs['players'] ) == 1 and dObs['players_online'] > 3) or ### must be equal
        (len( dObs['players'] ) > 1 and dObs['players_online'] - 10 > dObs['players_online']) or ### must be close
        (dObs['players_online'] > 0 and len( dObs['players'] ) - 9 > dObs['players_online']) or ### must be close, or, if players_online is zero, then player list is actually still trustworthy and mismatches are OK. This actually has some false positives, paticularly among big servers. the general rule remains that a mod to the API is disqualifying, unless I have guarantee for a spcial case (like zero ) taht API modifications are safe
        (len( dObs['players'] ) > 50 and len( dObs['players'] ) - 20 > dObs['players_online']) or ### and the tolerance is bigger for bigger servers, because more room for lag to affect synching of counts
        dObs['players_max'] + 2 < len( dObs['players'] ) or ### don't exceed max (plus/minus noise/lag)
        dObs['players_max'] + 2 < dObs['players_online'] or
        dObs['players_max'] <= 0 or ### negative and zero are impossible
        dObs['players_online'] < 0 or ### negative is impossible
        (len( dObs['players'] ) == 1 and len( dObs['players'][0]) < 3) or #### player array replace by int (18,14,10,1,or 0). When this happens, length of list is never greater than 1
        dObs['mc_addr'].lower() in (
"131.153.5.218", "alpa.playmcm.net", "playmcm.net", "pvp.originmc.org"
        )
    )
    #if dObs['players_online'] < len(dObs['players']):
    #if (dObs['players_online'] - 1000) > len(dObs['players']):
    #if len (dObs['players']) > 1 and len(dObs['players'][0]) < 3:
        #print("xxx", len(dObs['players']), dObs['players_online'], dObs['players_max'], dObs['players'])
    #if dObs['players_online'] + 10  < len(dObs['players']):
        #print("yyy", len(dObs['players']), dObs['players_online'], dObs['players_max'], dObs['players'])
        #if len (dObs['players']) > 1:
            #print( dObs['players'])
        #print(len(dObs['players']), dObs['players_online'], dObs['players_max'], dObs)
        #print()
    serverWriter.writerow([dObs['timestamp'], dObs['mc_addr']
                           , 1 if dObs['reported_status'] else 0
                           , 1 if dObs['reported_sample'] else 0
                           , 1 if dObs['reported_query'] else 0
                           , dObs['players_max']
                           , len(dObs['players'])
                           , dObs.get('latency', -1)
                           , hackedAPI
                          ])
    statistics = {}
    if True:
        #if len(dObs['players']) == 1 and len(dObs['players'][0]) < 10: print(dObs['players'])
        statistics['countInternal'] = 1
        ## always one or the other of these two:
        statistics['playerKeyInRow'] = 1 if 'players' in dObs else 0
        statistics['playerKeyNotInRow'] = 1 if not 'players' in dObs else 0
        ### always one or the other of these three
        ###  if list is empty, 10:1 chances that players_online is a lie. 
        ###     so I'm using emptiness as one flag of hacked APIS
        statistics['playerListEmpty'] = 1 if len( dObs['players'] ) == 0 else 0
        statistics['playerListLen1'] = 1 if len( dObs['players'] ) == 1  else 0
        statistics['playerListLenBig'] = 1 if len( dObs['players'] ) > 1 else 0
        #statistics['playerListLenCorrected'] = len( dObs['players'] )- (1 if  "00000000-0000-0000-0000-000000000000" in dObs['players']  else 0)
        #### this isn't a sign of badness, just a sign of a certain type of plugin installed, in which case it means the op is online
        statistics['playerListDummy'] = 1 if "00000000-0000-0000-0000-000000000000" in dObs['players'] else 0
        statistics['playerListDummyEmbedded'] = 1 if statistics['playerListDummy'] and statistics['playerListLenBig'] else 0
        statistics['playerListIntDummy'] = 1 if len(dObs['players']) > 0 and len(dObs['players'][0]) < 3 else 0
        ### always true
        statistics['playersReported'] = 1 if 'players_online' in dObs else 0
        ### usually true
        statistics['playersReportedEqualTruth'] = 1 if dObs['players_online'] == len( dObs['players'] ) else 0
        #### these are flags of a hacked API
        statistics['playersReportedOverTruth'] = 1 if dObs['players_online'] > len( dObs['players'] ) else 0
        statistics['playersReportedUnderTruth'] = 1 if dObs['players_online'] < len( dObs['players'] ) else 0
        ### always false
        statistics['playersReportedNull'] = 1 if dObs['players_online'] is None else 0
        statistics['playersReportedFalse'] = 1 if dObs['players_online'] is False else 0
        statistics['playersReported0'] = 1 if dObs['players_online'] == 0 else 0
        statistics['playersReported0Alone'] = 1 if dObs['players_online'] == 0 and len( dObs['players'] ) > 0 else 0
        ### true 9 times out of 10
        statistics['playersReportedNotEqualTruth0'] = 1 if dObs['players_online'] != 0 and len( dObs['players'] ) == 0 else 0
        statistics['playersReportedNegative'] = 1 if dObs['players_online'] < 0 else 0
        statistics['playersMaxNegative'] = 1 if dObs['players_max'] < 0 else 0
        ### this is never true
        statistics['apimod1'] = 1 if not dObs['reported_sample'] and len( dObs['players'] ) > 0 else 0
        ### this is most often true
        statistics['apimod2'] = 1 if not dObs['reported_query'] and len( dObs['players'] ) > 0 else 0
        ### these are rarely true, and sings of a hacked API
        statistics['apimod3'] = 1 if dObs['players_max'] < len( dObs['players'] ) else 0
        statistics['apimod4'] = 1 if dObs['players_max'] < dObs['players_online'] else 0
        statistics['apimod5'] = 1 if hackedAPI else 0
    return(statistics)


statTotal = {}
with open(pathPlayerDataOut, 'w') as ioPlayer:
    playerWriter = csv.writer(ioPlayer)
    playerWriter.writerow(['timestamp', "server", 'uid'])
    with open(pathServerDataOut, 'w') as ioServe:
        serverWriter = csv.writer(ioServe)
        serverWriter.writerow(['timestamp', "server", 'brstatus', 'brsample', 'brquery', 'nquota', 'npop', 'nlatency', 'hackedapi'])
        with gzip.open(pathScrapeDataIn, 'rt') as ioIn:
            for i, log_line in enumerate(ioIn):
                k, dObs = load_dirty_json(log_line)
                if k == 1:
                    stat = write_scrape_csv_row(dObs, playerWriter, serverWriter)
                    for k, v in stat.items():
                        statTotal[k] = statTotal.get(k,0) + v
                    statTotal['count'] = statTotal.get('count',0) + 1
                elif k == 2:
                    print (["AVERTED", "double entry json error","---->", i])
                    stat = write_scrape_csv_row( dObs[0], playerWriter, serverWriter)
                    for k, v in stat.items():
                        statTotal[k] = statTotal.get(k,0) + v
                    stat = write_scrape_csv_row( dObs[1], playerWriter, serverWriter)
                    for k, v in stat.items():
                        statTotal[k] = statTotal.get(k,0) + v
                    statTotal['count'] = statTotal.get('count',0) + 2
                elif k == 0:
                    print (["AVERTED2", "non dict entry json error","---->", i, log_line])

pprint(statTotal)
