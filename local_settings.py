pathLocal = '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
#pathDataOut =  local_path+ 'data/'
pathDataOut =  'data/'
pathData =  'data/'
pathDataInPlayers =  '/Users/sfrey/projecto_staid/minecraft/20160712/'
pathDataInPlugins = '/Users/sfrey/projecto/research_projects/minecraft/server_surveyor/archive/'
pathDataInMCSOrg = "/mcsdotorg_server_surveyor/archive/mcorgservers_omni_step1.txt"

import sys
sys.path.append("/Users/sfrey/projecto_git/")
import pygeoip
geoip = pygeoip.GeoIP(pathLocal+pathData+'GeoIP.dat')
