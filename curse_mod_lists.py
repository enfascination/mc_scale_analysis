from local_settings import pathData
import cursemodstats as curse
import csv
import time

#import importlib
#importlib.reload(curse)

#plugin_page = curse.curseBuildModsInCategory('admin-tools')
#all_plugins = curse.curseBukkitMods({'world-generators': 8})
#all_plugins = curse.curseBukkitMods(category_page_counts_bukkit)
#scrapeCurseBuildMods(s_file='/Users/sfrey/Desktop/tmpdump.csv', build='forge', category_page_counts={k: curse.category_page_counts_forge[k] for k in ('blood-magic', 'world-gen')})
def scrapeCurseBuildMods(s_file, build, category_page_counts):
    with open(s_file, 'a+') as csv_out:
        writer = csv.writer(csv_out)
        header = ('name', 'category', 'build', 'url', 'date_created', 'date_updated', 'dls_total', 'dls_recent', 'likes')
        writer.writerow(header)
        for category in category_page_counts.keys():
            print(category)
            all_plugins = curse.curseBuildModsInCategory(build, category, category_page_counts)
            for dd in all_plugins:
                writer.writerow([dd[key] for key in header ])
            time.sleep(5+random.random()*2)

if False:
    print("scraping curse for summary and category information on all bukkit plugins")
    scrapeCurseBuildMods(s_file=pathData+'curse_plugins_metadata.csv', build='bukkit', category_page_counts=curse.category_page_counts_bukkit)

if True:
    print("scraping curse for summary and category information on all forge plugins")
    scrapeCurseBuildMods(s_file=pathData+'curse_plugins_metadata.csv', build='forge', category_page_counts=curse.category_page_counts_forge)
