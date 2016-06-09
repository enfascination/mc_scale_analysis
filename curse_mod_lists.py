from local_settings import pathData
import cursemodstats as curse

#import importlib
#importlib.reload(curse)

#plugin_page = curse.curseBukkitModsInCategory('admin-tools')
#all_plugins = curse.curseBukkitMods({'world-generators': 8})
#all_plugins = curse.curseBukkitMods(category_page_counts)
print("scraping curse for summary and category information on all bukkit plugins")
with open(pathData+'curse_plugins_metadata.csv', 'a') as csv_out:
    writer = csv.writer(csv_out)
    header = ('name', 'category', 'build', 'url', 'date_created', 'date_updated', 'dls_total', 'dls_recent', 'likes')
    writer.writerow(header)
    for category in curse.category_page_counts.keys():
        print(category)
        all_plugins = curse.curseBukkitModsInCategory(category, category_page_counts)
        for dd in all_plugins:
            writer.writerow([dd[key] for key in header ])
