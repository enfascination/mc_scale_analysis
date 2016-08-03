#!/usr/bin/env python
# -*- coding: utf-8 -*-

""""
print out all 'institutional statements' from server descriptions in selftext

example use
python helper_reddit_institutional_statements.py >> ../mcsigns/mcselftext_full.csv
"""

import csv
import ujson
import sys
from local_settings import pathData
from pprint import pprint

### act like grep does, taking filename from second argument or streaming file
sFileArgument = False if len(sys.argv) == 1 else sys.argv[1]

### initialize objects
sCSVHeader = ('post_uid', 'mc_addr', 'dataset_date', 'statement')

#sFileInput = pathData+'step32_scraped_omnimc_posts.json'
sFileInput = pathData+'step32_scraped_reddit_posts.json'

mcdata_store = []
with open(sFileInput) as mcdata_in:
    for mcjson in mcdata_in.readlines():
        mc = ujson.loads(mcjson)
        if 'mc_addr' in mc and 'selftext' in mc and not mc['is_wanted'] and mc['reported_query']: 
            ### make text manageable, and find useful stuff in it
            #print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
            #print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
            #print(mc[ 'permalink'])
            #print(unicode(mc['title']).encode('utf8'))
            #print(unicode(mc['title']).encode('utf8'))
            #print(unicode(mc['selftext']).encode('utf8'))
            mcdata_entry_generic = [mc[k] for k in sCSVHeader[0:3]]
            mcdata_entry_title = list(mcdata_entry_generic)
            mcdata_entry_title.append(mc['title'])
            #print(mcdata_entry_title)
            mcdata_store.append( mcdata_entry_title )
            ### divide paragraph into institutional statements
            ### to debug this (right now there are too many linebreaks in the 
            ###  middle in unitary statements) use
            ###  grep minecraft4.com_20141105 data/step32_scraped_reddit_posts.json | head
            sStatements = [s.strip() for s in mc['selftext'].replace('**:',':'  ### first prevent linebreaks in the 
                                                                                    ### middle of institutional statemetns
                                                            ).replace('*:',':'
                                                            ).replace('. ','\n'  ### then linebreak instuttional statements
                                                            ).replace('*','\n'
                                                            ).replace('! ','\n'
                                                            ).replace('? ','\n'
                                                            ).replace('|','\n'
                                                            ).replace('–','\n'
                                                            ).replace('—','\n'
                                                            ).replace('---','\n'
                                                            ).replace('--','\n'
                                                            ).replace(' - ','\n'
                                                            ).replace(' -','\n'
                                                            ).replace('- ','\n'
                                                            ).replace('- ','\n' ### different from above, some wierd UTF
                                                            ).replace('•','\n'
                                                            ).split('\n')]
            for s in sStatements:
                if len(s) > 0:
                    mcdata_entry_statement =  list(mcdata_entry_generic)
                    mcdata_entry_statement.append(s) 
                    mcdata_store.append( mcdata_entry_statement )

            #print("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
            #print("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
            #print()
            #print()

#pprint(mcdata_store)

### now write
if not sFileArgument:
    f = sys.stdout
else:
    f = open(sFileArgument)
mcdata_out = csv.writer(f)
mcdata_out.writerow(sCSVHeader)
mcdata_out.writerows(mcdata_store)
#for r in mcdata_store:
    #mcdata_out.writerow(mcdata_store[r])
f.close()
