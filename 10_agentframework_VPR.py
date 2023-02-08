# -*- coding: utf-8 -*-
"""
Created on Tue Mar 19 14:23:17 2019

@author: mip18bdg
"""
#Variable Processing Rate Model
#This section creates the classes for use in the agent based model
#Flight class creates flights with required info


class Flights():
    def __init__ (self,fl, AT,eGate, eGate_r, 
                                          origin, queue,passengers, pass_num):
        self.AT = AT #First pax arrival at PCP
        self.eGate = eGate  #Number of eGate pax 
        self.eGate_r = eGate_r #eGate pax disembarkment rate
        self.fl = fl   #list of other flights
        self.status = 0    #sets that the plane is not ready to disembark
        self.queue = queue    #list of queues
        self.passengers = passengers #list of pax
        self.origin = origin
    #checks whether plane is ready to disembark

    #simulates the flow of pax from plance to eGate queue
    def disembark(self,t,queue,pass_num):
        #sets self time to current time
        self.t = t
        #sets the number of eGate paassengers available to add to gate queue.     
        if self.eGate > self.eGate_r:  #If there are more eGate pax than eGate pax rate then select eGate pax rate
            eGatePass = int(self.eGate_r)
        else:
            eGatePass = int(self.eGate)   #sets eGate pax number as remaining eGate px
        #Create required additional pax using Passengers class       
        for i in range(eGatePass):
            self.queue[0].size += 1 #increase the queue size by 1
            self.passengers.append(Passengers(self.t,self.queue, self.origin))
        #reduce the total number of eGate pax on flight by number that have entered queue
        self.eGate -= eGatePass
        if self.eGate == 0:
            self.status = 2

#Manages queues agents with required attributes
#In this model only one queue agent is used.             
class Queues():
    def __init__(self,passengers):
        self.size = 0   #Queue size initially zero
        self.passengers = passengers    #Adds pax list


#Models the queue size decreasing       
    def reduce(self,t,open_desks):
        if self.size > 0:
            eGate_Op = open_desks  #Sets the rate at which pax are processed
            if self.size >= eGate_Op:  #Checks whether the queue is bigger then the process rate
                q_red = eGate_Op      #If yes, then sets the reduction size to process rate
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
    def __init__(self,t, queue,origin):
        self.origin = origin
        self.AT = t     #set arrival time in pcp as current time
        self.ET = "none"    #initialises exit time
        self.queue = queue  #includes queue agents
        self.q_num = self.queue[0].size     #sets pax's queue number dependning on existing size
        self.inqueue = True     #sets that pax is in the queue
    #models how pax move through queue
    def move_queue(self,t,q_red):
        self.q_num -= q_red     #pax queue number updated to reflect reduced queue size
        if self.q_num <=0:   #checks in pax is still in queue
            self.exit_queue(t) #runs exit queue function if no longer in queue
    #models pax exiting queue
    def exit_queue(self,t):
        self.ET = t     # sets pax queue exit time as current time
        self.inqueue = False    #sets that pax is no longer in queue
        
        
    
