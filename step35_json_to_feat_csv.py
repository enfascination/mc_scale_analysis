# -*- coding: utf-8 -*-
from local_settings import pathData, pathDataInPlugins
from libmcscrape import cat_mc_json_logs, extract_features_from_mcjson_logs, standardize_address

### now convert all those files to an r-friendly format
print("produce server/week/plugin rows from omni logs")
### pull the reddit scrapes together into a csn file for R to work with
extract_features_from_mcjson_logs(pathData+"step32_scraped_reddit_posts.json", pathData+'step4_reddit_mcservers.csv')
### pull the reddit scrapes together into a csn file for R to work with
extract_features_from_mcjson_logs(pathData+"step32_scraped_omnimc_posts.json", pathData+'step4_omnimc_mcservers.csv')
