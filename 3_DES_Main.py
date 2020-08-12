# -*- coding: utf-8 -*-
"""
Created on Thu 14 May 2020

@author: GardneB
"""
#Agent-based simulation of airport terminal. Models flight arrival, PAX disembarkment, joining/
#nonEEA Queue, queue processing and caluculate of wait times. 
#Fifth model: Cycles through all required dates for 6-15 desks for 500 scenarios

import numpy as np
import agentframework_mm5
import pandas as pd

from datetime import datetime, timedelta
#Sets the number of scenarios to model
tot_scn = 500
#Cycles simulation model through the required dates
#Enter Required datetimes
for d in ([datetime(),datetime(),datetime()]):
    day_start = d + timedelta(hours=4)      #creates a start datetime of 04:00 for the date. 
    day_end = d + timedelta(hours=28)       #creates a end datetime of 04:00 for the following day
    file_date = day_start.strftime("%d%b%y")    #gets file date in correct format for up/downloading
    flight_data = pd.read_csv("schedules/" + file_date + ".csv")    #Load in flight schedule
    MC_data=pd.read_csv("MCS/MCS_" + file_date + ".csv")    #load in Monte Carlo simulation file
    MC_data['PCP_SIM'] = pd.to_datetime(MC_data['PCP_SIM'], dayfirst=True)  #convert MC date value to correct format
    #Function to highlight flights landing outside of our required window, marking them as cancelled. 
    def cancelled(row):
        if row['PCP_SIM'] > day_end:
            return "C"
        elif row['PCP_SIM'] < day_start:
            return "C"
        else:
            return "L"
    #create new column of interval period times using period function
    MC_data['Cancelled'] = MC_data.apply(cancelled,axis=1)  
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
   
    #Run simulations for d number of operational desks for the entire period
    for d in range(6,16):
        open_desks = d
        pass_det = []   #List for passenger wait times
        #Run simulation for each MC scenario
        for i in range(1,tot_scn+1):
            print("Scenario " + str(i) + ": Working")
            MCs = MC_data.loc[MC_data['SC_NO']==i]  #Lock MC data to required scenario
            fd = pd.merge(flight_data,MCs, on='IATA')   #Merge flight and MC data
            fd = fd.loc[fd['Cancelled'] == 'L'] #Filter out cancelled flights
            #Create list of queues
            queue = []
            #create list for pax data
            passengers = []
            #set total pax number as 0
            pass_num = 0
            #Reset failed passenger total (those that do not leave the system in the window)
            exit_failed = 0
            #Set number of flights
            f_num = len(fd.index)
            #Create queue length list
            queue_length = []
            
            #Create a queue instance using the Queue class. Feed in queue details
            queue.append(agentframework_mm5.Queues(passengers))
            fl = []
            #Generate flight agents using the Flight class
            for j in fd.index:  #Iterate through the total number of flights
                AT = fd.loc[j,'PCP_SIM']  #Store arrival time
                PAX = fd.loc[j,'TOTAL_PAX'] #Store total pax number
                EEA = fd.loc[j,'EEA']   #Store EEA pax number
                Non_EEA = fd.loc[j,'Non_EEA'] #Store NonEEA pax number
                #calculate the rate at which pax will enter pcp
                EEA_r = int(round(EEA/PAX * 15))
                Non_EEA_r = int(round((Non_EEA/PAX) *15 ))
                #If the above calculates to zero then set as 1 (min rate)
                if EEA_r ==0:
                    EEA_r =1
                if Non_EEA_r ==0:
                    Non_EEA_r =1             
                #create each agent using above variables. We are only concerned in Non_EEA pax    
                fl.append(agentframework_mm5.Flights(fl, AT,Non_EEA, Non_EEA_r, queue,passengers, pass_num))
            
            for t in int_datetime: #Run the below code for each datetime value for the period we are analysing.
        
                for j in range(f_num):   #Model activity for each flight
                          fl[j].land(t)     #Checks if flight has 'landed' at terminal
                          fl[j].disembark(t,queue,pass_num) #Models EG pax disembarkment from flight into PCP
                
                queue[0].reduce(t, open_desks) #Model the processing of pax and queue reduction
                   
                queue_length.append([t, queue[0].size]) #Creates a list of queue size for each time period
            #Generate dataframe of queue lengths throughout the day   
            ql = np.array(queue_length)
            ql = pd.DataFrame(ql, columns=['Time', 'Size'])  
            #Iterate through pax 
            for passenger in passengers:
                #If pax failed to exit system, increase exit failed total by 1
                if passenger.ET == "none":
                    exit_failed = exit_failed+1 
                else:
                #If pax exited system, store enter and exit times. 
                    AT = passenger.AT     #Arrival at PCP datetime
                    ET = passenger.ET
                    Scenario = i
                    pdet = [i,AT,ET]
                    pass_det.append(pdet)
            print("Scenario " + str(i) + ": Complete")
        #Create dataframe from list data
        pa = np.array(pass_det)
        ps = pd.DataFrame(pa, columns=['Scenario','Arrival_Time', 'Exit_Time'])
        #Add column of passenger wait time
        ps['Wait_Time'] = (ps['Exit_Time'] - ps['Arrival_Time'])
        #Define function that converts wait time into minutes
        def waittime(row):
            wait_time_mins = (row['Wait_Time'].seconds)/60
            return wait_time_mins
        #Create new wait time minutes column
        ps['WaitTime_Mins'] = ps.apply(waittime,axis=1)
        #Save file
        print(str(d) + " Desks: Saving Output")
        ps.to_csv(file_date + "/full_results_"  + file_date + "_" + str(d) +"_" + str(tot_scn) +  ".csv")
        print("Output Saved Successfully")