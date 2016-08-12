from local_settings import pathDataOut
from libmcscrape import load_dirty_json, standardize_address, get_freshest_data_date
import csv
import gzip
import hashlib

players_dataset_date = get_freshest_data_date("lib_datasets_players.txt")
pathScrapeDataIn = '/Users/sfrey/projecto_staid/minecraft/' + players_dataset_date + '/' + "dump_players_online.json.gz"
#pathPlayerDataOut = "/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/data/player_visits.csv"
#pathServerDataOut = "/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/data/server_visits.csv"
pathServerDataOut = pathDataOut + "step0_server_visits.csv"
pathPlayerDataOut = pathDataOut + "step0_player_visits.csv"

def write_scrape_csv_row(dObs, playerWriter, serverWriter):
    dObs['mc_addr'] = standardize_address(dObs['mc_addr'])
    serverWriter.writerow([dObs['timestamp'], dObs['mc_addr']
        , 1 if dObs['reported_status'] else 0 , 1 if dObs['reported_sample'] else 0, 1 if dObs['reported_query'] else 0
        , dObs['players_max'], dObs['players_online'], dObs.get('latency', -1)])
    if dObs['reported_sample']:
        for p in dObs['players']:
            if p != "00000000-0000-0000-0000-000000000000":
                playerWriter.writerow([dObs['timestamp'], dObs['mc_addr'], hashlib.md5(p.encode('utf-8')).hexdigest()])

with open(pathPlayerDataOut, 'w') as ioPlayer:
  playerWriter = csv.writer(ioPlayer)
  playerWriter.writerow(['timestamp', "server", 'uid'])
  with open(pathServerDataOut, 'w') as ioServe:
    serverWriter = csv.writer(ioServe)
    serverWriter.writerow(['timestamp', "server", 'brstatus', 'brsample', 'brquery', 'nquota', 'npop', 'nlatency'])
    with gzip.open(pathScrapeDataIn, 'rt') as ioIn:
      for i, log_line in enumerate(ioIn):
        k, dObs = load_dirty_json(log_line)
        if k == 1:
          write_scrape_csv_row(dObs, playerWriter, serverWriter)
        elif k == 2:
          print (["AVERTED", "double entry json error","---->", i])
          write_scrape_csv_row( dObs[0], playerWriter, serverWriter)
          write_scrape_csv_row( dObs[1], playerWriter, serverWriter)
        elif k == 0:
          print (["AVERTED2", "non dict entry json error","---->", i, log_line])
