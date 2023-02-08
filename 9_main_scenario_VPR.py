# -*- coding: utf-8 -*-
"""
Created on Thu 14 May 2020

@author: GardneB
"""
#Agent-based simulation of airport terminal. Models flight arrival, PAX disembarkment, joining/
#nonEEA Queue, queue processing and caluculate of wait times. 
#Variable processing times model

import numpy as np
import agentframework_main2
import pandas as pd
import random
random.seed(3141)

from datetime import datetime, timedelta

#Cycles simulation model through the required dates
airport_list = ['LHRT2FULL', 'LHRT3FULL', 'LHRT4FULL', 'LHRT5FULL', 'STNFULL', 'MANT1FULL', 'LGWSFULL', 'LGWNFULL']
#airport_list = ['LHRT5FULL']
for airport in airport_list:
    airporttext = airport
    ds = datetime(2019,11,1)
    df = datetime(2020,1,31)
    
    Desks = pd.read_csv("Desks/Calculated_Random/" + airporttext + "_desks_cal.csv")
    Desks['Time']=pd.to_datetime(Desks['Time'], dayfirst = True)
    
    day_start = ds + timedelta(hours = 4)    #creates a start datetime of 04:00 for the date. 
    day_end = df + timedelta(hours=28)       #creates a end datetime of 04:00 for the following day
    MC_data=pd.read_csv("FlightData/" + airporttext + "_fd.csv")    #load in Monte Carlo simulation file
    rate_file = pd.read_csv("Rate_File_Amend.csv") #Load in nationality processing rates
    MC_data['PCP_SIM'] = pd.to_datetime(MC_data['PCP_SIM'], dayfirst=True)  #convert MC date value to correct format
    #Function to highlight flights landing outside of our required window, marking them as cancelled. 
    
    int_datetime = []   #List for datetime periods
    #function to generate time deltas
    def perdelta(start, end, delta):
        curr = start
        while curr < end:
            yield curr
            curr += delta
    #Create list of datetime values of a fixed interval for specific period. 
    for result in perdelta(day_start, day_end, timedelta(seconds=30)):
        int_datetime.append(result)
        
    pass_det = []   #List for passenger wait times
    
    #Run simulation
    
    print("Scenario " + airporttext + ": Working")
    fd = pd.merge(MC_data, rate_file, on='Origin') #Merge with rate file
    #Create list of queues
    queue = []
    #create list for pax data
    passengers = []
    #set total pax number as 0
    pass_num = 0
    #Set number of flights
    f_num = len(fd.index)
    #Create queue length list
    queue_length = []
    failed_list = []
    
    #Create a queue instance using the Queue class. Feed in queue details
    queue.append(agentframework_main2.Queues(passengers))
    fl = []
    passenger_list = []
    
    print("Creating Flights")
    #Generate flight agents using the Flight class
    for j in fd.index:  #Iterate through the total number of flights
        AT = fd.loc[j,'PCP_SIM']  #Store arrival time
        PAX = fd.loc[j,'TOTAL_PAX'] #Store total pax number
        Non_EEA = fd.loc[j,'Non_EEA'] #Store NonEEA pax number
        origin = fd.loc[j, 'Origin'] #Store flight origin
        share = fd.loc[j, 'Share']
        rate = fd.loc[j, 'Rate'] #Store pass process rate
        nation = fd.loc[j,'Nationality']
        #calculate the rate at which pax will enter pcp
        Non_EEA_r = int(round((Non_EEA/PAX) *10 ))
        #If the above calculates to zero then set as 1 (min rate)
        if Non_EEA_r ==0:
            Non_EEA_r =1             
        #create each agent using above variables. We are only concerned in Non_EEA pax    
        fl.append(agentframework_main2.Flights(fl, AT,Non_EEA, Non_EEA_r, 
                                              origin, nation, rate, share, queue,passengers, pass_num))
    print("Iterating through days")
    #Below code takes number of desks from historic data and randomly applies reductions.
    a = 120
    red_time = 0
    desk_red = 0
    red_gates = 0
    failed = 0
    total_failed = 0
    desk_open_list = []
    for t in int_datetime: #Run the below code for each datetime value for the period we are analysing.
        a += 1
        interval = t - timedelta(minutes=t.minute % 15,
                        seconds=t.second,
                        microseconds=t.microsecond)
        open_desks = int(Desks['act_desks'].loc[Desks['Time']==interval])
        if (random.random() < 0.002) and (red_time == 0):
            red_time = random.randint(10,240)
            desk_red = random.randint(5,10)
        if red_time != 0:
            open_desks -= desk_red
            open_desks = max(1, open_desks)
            red_time -=1
        desk_open_list.append([t,open_desks,red_time,desk_red])
        fdf = fd.loc[(fd["PCP_SIM"]<=t) & (fd["PCP_SIM"] > t - timedelta(hours=2))]
        for j in fdf.index:   #Model activity for each flight
            if fl[j].status == 0:
                if fl[j].AT == t:    #checks if current time is equal to the arrival of first pasengers in PCP ('land')
                    fl[j].status = 1      #sets that the plane has 'landed'    #Checks if flight has 'landed' at terminal
                    fl[j].disembark(t,queue,pass_num) #Models EG pax disembarkment from flight into PCP
            elif fl[j].status == 1:
                fl[j].disembark(t,queue,pass_num) #Models EG pax disembarkment from flight into PCP  
        queue[0].reduce(t, open_desks) #Model the processing of pax and queue reduction
        if (a % 2880 == 0) & (a>0):
            for passenger in passengers:
                if passenger.ET == "none":
                    failed += 1
                    total_failed += 1
                    next
                else:
                    AT = passenger.AT     #Arrival at PCP datetime
                    ET = passenger.ET
                    origin = passenger.origin
                    nation = passenger.nation
                    pdet = [AT,ET,origin, nation]
                    pass_det.append(pdet)
            passengers.clear()
            failed_det = [t,failed]
            failed_list.append(failed_det)
            failed = 0
            print(str(t) + " Complete")
    print("Saving Passenger Data")
    #Iterate through pax 
    #If pax exited system, store enter and exit times. 
    
    print("Scenario " + airporttext + ": Complete")
    #Create dataframe `from list data
    pa = np.array(pass_det)
    ps = pd.DataFrame(pa, columns=['Arrival_Time', 'Exit_Time', 'Origin','Nationality'])
    
    desk_open_array = np.array(desk_open_list)
    desk_open_df = pd.DataFrame(desk_open_array)
    desk_open_df.to_csv("Results/regular_random/" + airporttext + "_open_desks.csv")
    failed_array = np.array(failed_list)
    failed_df = pd.DataFrame(failed_array, columns=['Date', 'Number Failed'])
    failed_df.to_csv("Results/regular_random2/" + airporttext + "_failed.csv")
    #Add column of passenger wait time
    ps['Wait_Time'] = (ps['Exit_Time'] - ps['Arrival_Time'])
    #Define function that converts wait time into minutes
    def waittime(row):
        wait_time_mins = (row['Wait_Time'].seconds)/60
        return wait_time_mins
    #Create new wait time minutes column
    ps['WaitTime_Mins'] = ps.apply(waittime,axis=1)
    #Save file
    print("_Saving Output")
    ps.to_csv("Results/regular_random2/" + airporttext + "_res.csv")
    print("Output Saved Successfully")
    print(str(total_failed) + " Passengers failed to exit the system")
