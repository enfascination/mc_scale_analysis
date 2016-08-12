from libmcscrape import get_freshest_data_date
players_dataset_date = get_freshest_data_date("lib_datasets_players.txt")

pathLocal = '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
#pathDataOut =  local_path+ 'data/'
pathDataOut =  'data/'
pathData =  'data/'
pathDataInPlayers =  '/Users/sfrey/projecto_staid/minecraft/'+players_dataset_date+'/'
pathDataInPlugins = '/Users/sfrey/projecto/research_projects/minecraft/server_surveyor/archive/'
pathDataInMCSOrg = "/mcsdotorg_server_surveyor/archive/mcorgservers_omni_step1.txt"

