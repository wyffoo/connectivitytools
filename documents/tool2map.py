# clean all variables
for name in dir():
    if not name.startswith('_'):
        del globals()[name]

import pandas as pd
import numpy as np
from scipy.spatial.distance import pdist
from scipy.spatial.distance import squareform
import geopy.distance
import time
  
# Read school data from file
dataPath = 'E:\GIGA\Materials\Traning\Mapping demo\Am_Topology_RB_1(report).xlsx'

sheetName_res = 'Result'
sheetName_top = 'Topology'

data_res = pd.read_excel(dataPath, sheet_name = sheetName_res)
data_top = pd.read_excel(dataPath, sheet_name = sheetName_top)

start_ind = 8
num = data_top.shape[0]

data_obj = pd.DataFrame()

data_obj[['loc']] = data_res.iloc[start_ind:start_ind+num]['Unnamed: 1']
data_obj[['lon']] = data_res.iloc[start_ind:start_ind+num]['Unnamed: 4']
data_obj[['lat']] = data_res.iloc[start_ind:start_ind+num]['Unnamed: 5']
data_obj[['fb_dist']] = data_res.iloc[start_ind:start_ind+num]['Unnamed: 8']
data_obj[['users_num']] = data_res.iloc[start_ind:start_ind+num]['Unnamed: 9']

data_obj.to_csv('objects.csv')
# link_id
# loc_from
# lon_from
# lat_from
# loc_to
# lon_to
# lat_to
# tech
# npv

data_links_all = pd.DataFrame()

for i in range(num):
    loc_from = data_top.iloc[i]['Unnamed: 0']
    lon_from = data_res[data_res['Unnamed: 1'] == loc_from]['Unnamed: 4'].item()
    lat_from = data_res[data_res['Unnamed: 1'] == loc_from]['Unnamed: 5'].item()
    loc_to = data_top[data_top[loc_from] != '-']['Unnamed: 0']
    loc_to_size = loc_to.size
    tech_and_npv = data_top[data_top[loc_from] != '-'][loc_from]
    link_id = list(range(i, i+loc_to_size))
    data_links = pd.DataFrame()
    data_links['link_id'] =  link_id
    data_links['loc_from'] = [loc_from] * loc_to_size
    data_links['lon_from'] = [lon_from] * loc_to_size
    data_links['lat_from'] = [lat_from] * loc_to_size
    data_links['loc_to'] = loc_to.values
    lon_to = data_res[data_res['Unnamed: 1'].isin(loc_to.values)]['Unnamed: 4']
    lat_to = data_res[data_res['Unnamed: 1'].isin(loc_to.values)]['Unnamed: 5']
    data_links['lon_to'] = lon_to.values
    data_links['lat_to'] = lat_to.values
    data_links['tech_and_npv'] = tech_and_npv.values
    data_links_all = data_links_all.append(data_links)

data_links_all.reset_index()
    
data_links_all.to_csv('links.csv')