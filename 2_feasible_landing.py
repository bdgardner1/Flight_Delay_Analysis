# -*- coding: utf-8 -*-
"""
Created on Wed May  6 11:29:20 2020

@author: GardneB
"""
#Checks Monte Carlo simuations to determine whether they conform to feasible landing patters.
#No more than 8 flights can land in a 15 minute window.
#No more than 12 flights can land in a 30 minute window. 

import pandas as pd
from datetime import timedelta
#Loads in MC file
#Enter file path
mc = pd.read_csv("")
#Convert arrival time to datetime format
mc['Arrival_Sim'] = pd.to_datetime(mc['Arrival_Sim'])

#Define function to create 15 minute interval period time based on flight arrival time 
def period_15(row):
    ET = row['Arrival_Sim']
    interval = ET - timedelta(minutes=ET.minute % 15,
                             seconds=ET.second,
                             microseconds=ET.microsecond)
    return interval
#Define function to create 30 minute interval period time based on flight arrival time 
def period_30(row):
    ET = row['Arrival_Sim']
    interval = ET - timedelta(minutes=ET.minute % 30,
                             seconds=ET.second,
                             microseconds=ET.microsecond)
    return interval
#create new column of 15 min interval period times
mc['period_15'] = mc.apply(period_15,axis=1)
#create new column of 30 min interval period times
mc['period_30'] = mc.apply(period_30,axis=1)
#Groups MC df by scenario number and 15 minute period. Counts the number in each period. 
mc_15 = mc.groupby(['SC_NO', 'period_15'])['IATA'].count()
mc_15 = mc_15.reset_index()
#Groups MC df by scenario number and 30 minute period. Counts the number in each period. 
mc_30 = mc.groupby(['SC_NO', 'period_30'])['IATA'].count()
mc_30 = mc_30.reset_index()
#Creates new dfs listing periods with more than feasible number of landings
v15 = mc_15.loc[mc_15['IATA']>8]
v30 = mc_30.loc[mc_30['IATA']>12]

print(v15, v30)

#Below code checks that flights do not land within 30 minutes of each other at the same gate. 
#Not currently used. 

mcg = mc.loc[mc['Gate.Stand']!= '/']
gates = list(mcg['Gate.Stand'].unique())

mcf = mc
mcgc = pd.DataFrame([])
for i in mcf['SC_NO'].unique():
    mci = mcf.loc[mcf['SC_NO']==i]    
    for g in gates:
        mcg = mci.loc[mci['Gate.Stand']==g]
        mcg = mcg.sort_values(by=['Arrival_Sim'])
        mcg['gates'] = g
        mcg['scenario'] =i
        mcg['prev_flight'] = mcg['Arrival_Sim'].shift(periods=1)
        mcg['flight_gap'] = mcg['Arrival_Sim'] - mcg['prev_flight']
        mcgc = mcgc.append(mcg)

mcgx = mcgc.loc[mcgc['flight_gap']<timedelta(minutes=30)]

print(mcgx.loc[mcgx['scenario']==10])

mcgx.count()

mcgc.count()

mcgc.head()
