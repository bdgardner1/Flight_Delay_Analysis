# -*- coding: utf-8 -*-
"""
Created on Tue Mar 19 14:23:17 2019

@author: mip18bdg
"""
#Model 5. 
#This section creates the classes for use in the agent based model
#Flight class creates flights with required info
class Flights():
    def __init__ (self,fl, AT,Non_EEA, Non_EEA_r, queue,passengers, pass_num):
    
        self.AT = AT #First pax arrival at PCP
        self.Non_EEA = Non_EEA  #Number of nonEEA pax 
        self.Non_EEA_r = Non_EEA_r #nonEEA pax disembarkment rate
        self.fl = fl   #list of other flights
        self.landed = False    #sets that the plane is not ready to disembark
        self.queue = queue    #list of queues
        self.passengers = passengers #list of pax
    #checks whether plane is ready to disembark
    def land(self,t):
        if self.AT == t:    #checks if current time is equal to the arrival of first pasengers in PCP ('land')
            self.landed = True      #sets that the plane has 'landed'
    #simulates the flow of pax from plance to nonEEA queue
    def disembark(self,t,queue,pass_num):
        #sets self time to current time
        self.t = t
        #checks to see if plance has 'landed' and whether there is a positive number of nonEEA pax still 'onboard'
        if self.landed == True and self.Non_EEA > 0:
            #sets the number of nonEEA paassengers available to add to gate queue.     
            if self.Non_EEA > self.Non_EEA_r:  #If there are more nonEEA pax than nonEEA pax rate then select nonEEA pax rate
                nonEEAPass = int(self.Non_EEA_r)
            else:
                nonEEAPass = int(self.Non_EEA)   #sets nonEEA pax number as remaining nonEEA px
            #Create required additional pax using Passengers class       
            for i in range(nonEEAPass):
                self.passengers.append(Passengers(self.t,self.queue))
            #reduce the total number of nonEEA pax on flight by number that have entered queue
            self.Non_EEA -= nonEEAPass
            #add total number of pax to queue size
            self.queue[0].add(nonEEAPass)
#Manages queues agents with required attributes
#In this model only one queue agent is used.             
class Queues():
    def __init__(self,passengers):
        self.size = 0   #Queue size initially zero
        self.passengers = passengers    #Adds pax list

#Models the queue size increasing   
    def add(self, q_add):
        self.size += q_add  #Increase queue size 
#Models the queue size decreasing       
    def reduce(self,t,open_desks):
        if self.size > 0:
            nonEEA_Op = (open_desks/3)*(1)  #Sets the rate at which pax are processed
            if self.size >= nonEEA_Op:  #Checks whether the queue is bigger then the process rate
                q_red = nonEEA_Op      #If yes, then sets the reduction size to process rate
            else:
                q_red = self.size   #If no, sets the reduction size to the size of the queue
            #cycle through pax list and if they are in the queue, reduce their queue number with regard to the queue reduction
            for passenger in self.passengers:   
                if passenger.inqueue == True:
                    passenger.move_queue(t,q_red)    
            #Reduces the queue size by the reduction figure    
            self.size -= q_red
#Manages passenger agents
class Passengers():
    def __init__(self,t, queue):
        self.AT = t     #set arrival time in pcp as current time
        self.ET = "none"    #initialises exit time
        self.queue = queue  #includes queue agents
        self.q_num = self.queue[0].size + 1     #sets pax's queue number dependning on existing size
        self.inqueue = True     #sets that pax is in the queue
    #models how pax move through queue
    def move_queue(self,t,q_red):
        self.q_num -= q_red     #pax queue number updated to reflect reduced queue size
        if self.q_num <1:   #checks in pax is still in queue
            self.exit_queue(t) #runs exit queue function if no longer in queue
    #models pax exiting queue
    def exit_queue(self,t):
        self.ET = t     # sets pax queue exit time as current time
        self.inqueue = False    #sets that pax is no longer in queue
        
        
    
