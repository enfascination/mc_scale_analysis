from local_settings import pathData
from bs4 import BeautifulSoup
from requests import get
import locale ### for http://stackoverflow.com/questions/1779288/how-do-i-use-python-to-convert-a-string-to-a-number-if-it-has-commas-in-it-as-th
locale.setlocale( locale.LC_ALL, 'en_US.UTF-8' ) 
import time
import csv
import sys 
import re
import fileinput
import random

#helper fucntions for curse html
def digitFromDCommaDSpaceText(txt):
    return(locale.atoi(re.split('\ ', txt)[0]))
### http://stackoverflow.com/questions/7241170/how-to-convert-current-date-to-epoch-timestamp
def epochFromCurseDateString(txt):
    pattern = '%d %b %Y'
    epoch = int(time.mktime(time.strptime(txt, pattern)))
    return(epoch)

categories_bukkit = (
            "admin-tools"
        , "anti-griefing-tools"
        , "chat-related"
        , "developer-tools"
        , "economy"
        , "fixes"
        , "fun"
        , "general"
        , "informational"
        , "mechanics"
        , "miscellaneous"
        , "role-playing"
        , "teleportation"
        , "website-administration"
        , "world-editing-and-management"
        , "world-generators"
        )
category_page_counts_bukkit_raw = (307, 63, 119, 35, 54, 46, 314, 134, 107, 195, 44, 89, 58, 10, 37, 8) ### by hand from http://mods.curse.com/bukkit-plugins/minecraft
category_page_counts_bukkit = dict(zip(categories_bukkit, category_page_counts_bukkit_raw))
### and again for forge, but maybe less verbose
category_page_counts_forge = {
          "mc-addons": 19
        , "applied-energistics-2": 1
        , "blood-magic": 1
        , "addons-buildcraft": 2
        , "addons-forestry": 1
        , "addons-industrialcraft": 3
        , "addons-thaumcraft": 4
        , "addons-thermalexpansion": 5
        , "addons-tinkers-construct": 2
        , "adventure-rpg": 30
        , "library-api": 18
        , "armor-weapons-tools": 64
        , "cosmetic": 42
        , "mc-food": 23
        , "magic": 21
        , "map-information": 16
        , "mc-miscellaneous": 37
        , "redstone": 2
        , "server-utility": 21
        , "storage": 11
        , "technology": 27
        , "technology-energy": 9
        , "technology-item-fluid-energy-transport": 7
        , "technology-farming": 8
        , "technology-genetics": 2
        , "technology-player-transport": 8
        , "technology-processing": 12
        , "world-gen": 1
        , "world-biomes": 10
        , "world-mobs": 27
        , "world-ores-resources": 48
        , "world-structures": 13
        }

def pluginMetaDataFromSoup(soup):
    dMod = {}
    dMod['feat_category'] = soup.find("a", class_="main-category")['title']
    dMod['feat_dl_total'] = digitFromDCommaDSpaceText(soup.find("li", class_="downloads").get_text())
    dMod['feat_dl_recent'] = digitFromDCommaDSpaceText(soup.find("li", class_="average-downloads").get_text())
    dMod['feat_fave'] = digitFromDCommaDSpaceText(soup.find("li", class_="favorited").get_text())
    dMod['feat_created'] = [j.find('abbr')['data-epoch'] for j in soup.find_all('li', class_="updated") if re.match('^Created', j.get_text())][0]
    dMod['feat_updated'] = [j.find('abbr')['data-epoch'] for j in soup.find_all('li', class_="updated") if re.match('^Updated', j.get_text())][0]
    return(dMod)

def curseCategoryStats(categories):
    dCategories = {}
    for category in categories:
        url = 'http://mods.curse.com/bukkit-plugins/minecraft/category/%(q)s'
        payload = {
                    'q': category,
                    }
        r = get(url % payload)
        #print(r.text)
        soup = BeautifulSoup(r.text, "html.parser")
        category_count = int(re.match('^(?P<cat_count>\d+) .*$', soup.find('span', class_='category-count').get_text()).group('cat_count'))
        print(category_count)
        dCategories[category] = category_count 
        time.sleep(0.1+random.random()/2)
    return(dCategories)

def curseBuildModsInCategoryPage(build, category, page):
    lPlugins = []
    url_base = 'http://mods.curse.com'
    if build == 'bukkit':
        url = url_base + '/bukkit-plugins/minecraft/category/%(q)s?page=%(p)s'
    elif build == 'forge':
        url = url_base + '/mc-mods/minecraft/category/%(q)s?page=%(p)s'
    else:
        print("REOIPHFDJKLDFDSLKFJ")
    payload = {
                'q': category,
                'p': str(page),
                }
    r = get(url % payload)
    soup = BeautifulSoup(r.text, "html.parser")
    ### past boilerplate, now results
    section = soup.find('div', id='addons-browse').find('ul', attrs={"class" : "listing-project"}).findAll('ul', attrs={"class" : "group"})
    for addon in section:
        lAddon = {}
        lAddon['name'] = addon.find('li', {'class':'title'}).h4.a.text
        lAddon['url'] = url_base + addon.find('li', {'class':'title'}).h4.a.get('href')
        lAddon['dls_total'] = digitFromDCommaDSpaceText(addon.find('li', {'class':'download-total'}).text)
        lAddon['dls_recent'] = digitFromDCommaDSpaceText(addon.find('li', {'class':'average-downloads'}).text)
        lAddon['date_created'] = epochFromCurseDateString(addon.find('li', {'class':'updated'}, text=re.compile('^Created')).text.partition(' ')[2] )
        lAddon['date_updated'] = epochFromCurseDateString(addon.find('li', {'class':'updated'}, text=re.compile('^Updated')).text.partition(' ')[2] )
        lAddon['category'] = category
        lAddon['build'] = build
        lAddon['likes'] = digitFromDCommaDSpaceText(addon.find('li', {'class':'grats'}).text)
        #print(addon)
        #print(lAddon)
        #return(lAddon)
        lPlugins.append(lAddon)
    return(lPlugins)

def curseBuildModsInCategory(build, category, category_page_counts):
    all_plugins = []
    pages = category_page_counts[category]
    for p in range(1, pages+1):
        result = curseBuildModsInCategoryPage(build, category, p)
        all_plugins.extend(result)
        time.sleep(0.5+random.random()/2)
    return(all_plugins)
def curseBukkitMods(category_page_counts):
    all_plugins = []
    for category, pages in category_page_counts.items():
        for p in range(1, pages+1):
            all_plugins.extend(curseBuildModsInCategoryPage(category, p))
            time.sleep(0.5+random.random()/2)
    return(all_plugins)

if __name__ == "__main__":
    #$lPlugins = (  "worldedit" , "worldguard" , "essentials" , "permissionsex" , "lwc" , "vault" , "nocheatplus" , "chairs" , "iconomy" , "chestshop" , "bukkitcompat" , "craftbook" , "clearlag" , "logblock" , "worldborder" , "coreprotect" , "coreprotect" , "zavautomessager" , "griefprevention" , "holographicdisplays" , "vanishnopacket" , "votifier" , "enjinminecraftplugin" , "hidestream" , "skinsrestorer" , "buycraft")

    csvout = csv.writer(sys.stdout)
    header = ('feat', 'feat_url', 'feat_category', 'feat_dl_total', 'feat_dl_recent', 'feat_created', 'feat_updated', 'feat_fave')
    csvout.writerow(header)  ### header
    lPlugins = [m.strip() for m in fileinput.input()]
    #with open(sys.argv[1],'w+') as out:
    for mod in sorted(lPlugins):
        #print(mod)
        if False:  ### get curse pages by plugin name
            url = 'http://mods.curse.com/bukkit-plugins/minecraft/%(q)s'
            payload = {
                        'q': mod,
                        }
            url = url % payload
        else:  ### get curse pages by url
            ### use awk -F, '{OFS=",";print $4}' data/curse_plugins_metadata.csv  | grep http | sort -u | python cursemodstats.py > data/curse_plugins_metadata_fine.csv
            url = mod
            mod = url.rpartition('/')[2]
        r = get(url)
        ### http://stackoverflow.com/questions/7406102/create-sane-safe-filename-from-any-unsafe-string
        r_file = open(pathData + 'curse_pages/' + "".join([c for c in url if c.isalpha() or c.isdigit() or c==' ']).rstrip() + '.html', 'w')
        r_file.write(r.text)
        r_file.close()
        if r.status_code >= 200 and r.status_code < 400:
            soup = BeautifulSoup(r.text, "html.parser")
            dMod = pluginMetaDataFromSoup(soup)
            dMod['feat'] = mod
            dMod['feat_url'] = url
            csvout.writerow([dMod[i] for i in header])
            time.sleep(5)

