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

lPlugins = [m.strip() for m in fileinput.input()]
#$lPlugins = (  "worldedit" , "worldguard" , "essentials" , "permissionsex" , "lwc" , "vault" , "nocheatplus" , "chairs" , "iconomy" , "chestshop" , "bukkitcompat" , "craftbook" , "clearlag" , "logblock" , "worldborder" , "coreprotect" , "coreprotect" , "zavautomessager" , "griefprevention" , "holographicdisplays" , "vanishnopacket" , "votifier" , "enjinminecraftplugin" , "hidestream" , "skinsrestorer" , "buycraft")

dMod = {}
if True:
    #with open(sys.argv[1],'w+') as out:
    csvout = csv.writer(sys.stdout)
    csvout.writerow(['feat', 'feat_url', 'feat_downloads', 'feat_category', 'feat_created'])  ### header
    for mod in sorted(lPlugins):
        #print(mod)
        url = 'http://mods.curse.com/bukkit-plugins/minecraft/%(q)s'
        payload = {
                    'q': mod,
                    }
        dMod[mod] = {}
        dMod[mod]['url'] = url % payload
        dMod[mod]['dl'] = ''
        dMod[mod]['category'] = ''
        dMod[mod]['iCreated'] = 0
        r = get(url % payload)
        if r.status_code >= 200 and r.status_code < 400:
            soup = BeautifulSoup(r.text, "html.parser")
            dMod[mod]['dl'] = locale.atoi(re.split('\ ', soup.find("li", class_="downloads").get_text())[0])
            dMod[mod]['category'] = soup.find("a", class_="main-category")['title']
            dMod[mod]['iCreated'] = [j.find('abbr')['data-epoch'] for j in soup.find_all('li', class_="updated") if re.match('^Created', j.get_text())][0]

        #time.strftime('%m/%d/%Y %H:%M:%S',  time.gmtime(1462916065))
        #print(soup.prettify()[1:1000])
        csvout.writerow([mod, dMod[mod]['url'], dMod[mod]['dl'], dMod[mod]['category'], dMod[mod]['iCreated'] ])
        time.sleep(2+random.random())

def curseCategoryStats():
    categories = (
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
    dCategories = {}
    for category in categories:
        url = 'http://mods.curse.com/bukkit-plugins/minecraft/category/%(q)s'
        payload = {
                    'q': category,
                    }
        r = get(url % payload)
        soup = BeautifulSoup(r.text, "html.parser")
        category_count = int(re.match('^(?P<cat_count>\d+) .*$', soup.find('span', class_='category-count').get_text()).group('cat_count'))
        dCategories[category] = category_count 
    print(dCategories)
