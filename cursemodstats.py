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
category_page_counts_raw = (307, 63, 119, 35, 54, 46, 314, 134, 107, 195, 44, 89, 58, 10, 37, 8) ### by hand from http://mods.curse.com/bukkit-plugins/minecraft
category_page_counts = dict(zip(categories, category_page_counts_raw))

def curseCategoryStats():
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

def curseBukkitModsInCategoryPage(category, page):
    lPlugins = []
    url_base = 'http://mods.curse.com'
    url = url_base + '/bukkit-plugins/minecraft/category/%(q)s?page=%(p)s'
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
        lAddon['build'] = 'curse'
        lAddon['likes'] = digitFromDCommaDSpaceText(addon.find('li', {'class':'grats'}).text)
        #print(addon)
        #print(lAddon)
        #return(lAddon)
        lPlugins.append(lAddon)
    return(lPlugins)

def curseBukkitModsInCategory(category, category_page_counts):
    all_plugins = []
    pages = category_page_counts[category]
    for p in range(1, pages):
        all_plugins.extend(curseBukkitModsInCategoryPage(category, p))
        time.sleep(0.5+random.random()/2)
    return(all_plugins)
def curseBukkitMods(category_page_counts):
    all_plugins = []
    for category, pages in category_page_counts.items():
        for p in range(1, pages):
            all_plugins.extend(curseBukkitModsInCategoryPage(category, p))
            time.sleep(0.5+random.random()/2)
    return(all_plugins)

if __name__ == "__main__":
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
                dMod[mod]['dl'] = digitFromDCommaDSpaceText(soup.find("li", class_="downloads").get_text())
                dMod[mod]['category'] = soup.find("a", class_="main-category")['title']
                dMod[mod]['iCreated'] = [j.find('abbr')['data-epoch'] for j in soup.find_all('li', class_="updated") if re.match('^Created', j.get_text())][0]

            #time.strftime('%m/%d/%Y %H:%M:%S',  time.gmtime(1462916065))
            #print(soup.prettify()[1:1000])
            csvout.writerow([mod, dMod[mod]['url'], dMod[mod]['dl'], dMod[mod]['category'], dMod[mod]['iCreated'] ])
            time.sleep(2+random.random())

