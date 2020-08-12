#Code generates Monte Carlo scenarios for use in Python DES model.
#Imports flight arrival times for airport terminal for 2019
#Generates distribution of flight delays
#Sampled and applied to the required day's schedule to create scenarios

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(viridis)
library(xts)
library(ggthemes)
#Enter local drive
Local_Drive = "//Poise.Homeoffice.Local/Home/TMS7/Users/GardneB/My Documents/Main_Project/GitHubFiles"

setwd(Local_Drive)

#Load in filght arrival data and filter
ALL = read.csv("APT_2019.csv",quote="", na.strings = "", stringsAsFactors = FALSE)

#Drop unnecessary columns
ALL = ALL[1:25]

#Convert scheduled date to correct format
ALL$Scheduled.Date <- as.Date(ALL$Scheduled.Date, 
                              format = "%d/%m/%Y")
#Remove duplicates
ALL = unique(ALL)
#Remove cancelled and forecast flights
ALL = filter(ALL, !Status %in% 
               c("Cancelled","Canceled", "Port Forecast",
                 "ACL Forecast"))

#Flight arrival delay analysis 

#Replace missing arrival data with estimated arrival or scheduled time
ALL$Act.Arrival = ifelse(is.na(ALL$Est.Arrival),ALL$Scheduled.Time, ALL$Est.Arrival)
#Create scheduled arrival datetime
ALL$Sch.Arr = as.POSIXct(with(ALL, ymd(Scheduled.Date) + hm(Scheduled.Time)))
#create actual arrival datetime
ALL$Act.Arr = as.POSIXct(with(ALL, ymd(Scheduled.Date) + hm(Act.Arrival)))
#Create pcp arrival datetime
ALL$PCP.Act.Arrival = as.POSIXct(with(ALL, ymd(Scheduled.Date) + hm(Est.PCP) ))
#Create new delay column
ALL$delay = as.numeric(ALL$Act.Arr - ALL$Sch.Arr)
#filter out erronous delays
ALL = filter(ALL, delay> -14400)
#Calcualte walk times from gate to pcp
ALL$walk_time = as.numeric(ALL$PCP.Act.Arrival - ALL$Act.Arr)
#Create scheduled pcp arrival time
ALL$PCP.Sch.Arrival = ALL$Sch.Arr + ALL$walk_time*60
#create list of all delay times
delay_list <- ALL$delay
mean(ALL$delay)/60
#Generate a chart to display the flight delay distribution
ALL %>% 
  filter((delay > -14400) & (delay < 14400)) %>%
  ggplot() + 
  aes(x = delay/60)+
  xlim(-100, 200) +
  geom_density() +
  geom_vline(xintercept = median(ALL$delay)/60, linetype = "dashed", color = 'blue') +
  annotate(x = 50, y = 0.022, geom= 'text', label = "Median = -11 minutes")  + 
  labs(x = "Arrival Delay (Mins)", 
       y = "Density") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=8), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))+
  theme_hc()
ggsave("Terminal_Delays.png")
length(delay_list)

#Replace missing API values with either historic flight or terminal averages
ALL$Non.EEA_est = ifelse(is.na(ALL$API.Non.EEA),ALL$Historical.Non.EEA, ALL$API.Non.EEA)
ALL$Non.EEA_est = ifelse(is.na(ALL$Non.EEA_est),ALL$Terminal.Average.Non.EEA, ALL$Non.EEA_est)

#Analysis on nonEEA totals

ALLf = filter(ALL, Non.EEA_est >=0)

#Create dataframe of nonEEA totals for each day
nEEA_tot = aggregate(Non.EEA_est ~ Scheduled.Date, ALLf, sum)
#graph of nonEEA total throught the 12 month period
ggplot(data = nEEA_tot, aes(x = Scheduled.Date, y = Non.EEA_est)) +
  geom_line()

#Filter out the period before b5jSSK change
nEEA_b5j = filter(nEEA_tot, Scheduled.Date > "2019-05-31")
#Graph of nonEEA totals without b5jssk period
ggplot(data = nEEA_b5j, aes(x = Scheduled.Date, y = Non.EEA_est)) +
  geom_line()
#Summary statistics for above dataframe
summary(nEEA_b5j$Non.EEA_est)
#Show the dates that are closest to the Q1, Q2 and Q3 of nonEEA totals for the year
#You need to enter totals first
subset(nEEA_b5j, Non.EEA_est > "" & Non.EEA_est < "")
subset(nEEA_b5j, Non.EEA_est > "" & Non.EEA_est <"")
subset(nEEA_b5j, Non.EEA_est > "" & Non.EEA_est <"")
#Create full graph for the above
nEEA_b5j %>% 
  ggplot() +
  aes(x = Scheduled.Date, y = Non.EEA_est) +
  geom_line() + 
  labs( title = "Passengers Using Non-EEA Queues at Target Airport",
        x = "Date (2019)",
        y = "Total Passengers") +
  theme(plot.title = element_text(face='bold', size = 12, colour = 'skyblue4'),
        axis.title.y = element_text(margin = margin(t=0, r=10,b=0,l=0)))
#View top 5 busiest dates for nonEEA Pax
nEEA_b5j %>%  top_n(5)

#Monte Carlo scenario generation
#Set required datetime values for MC scenario generation using nonEEA analysis
Day_Start = "2019/01/01 04:00"
Day_End = "2019/02/01 04:00"
req_date = "2019/01/01"

#Create star and end datetime for required date
start = as.POSIXct(Day_Start)
end = as.POSIXct(Day_End)
Required_Date = as.Date(req_date)

#filter arrivals for just the required date to create a scheduled df
j2 = ALL %>% filter(Scheduled.Date == Required_Date)

#Copy to new df for use in loop
MC = j2
#Create null value delay column (all flights arrive on schedule)
MC$delay_sim <- 0
#Create arrival column as scheduled time
MC$Arrival_Sim = j2$Sch.Arr
#Create PCP arrival column as pcp scheduled time
MC$PCP_sim <- j2$PCP.Sch.Arrival
#Filter for required columns
MC = select(MC,c(1,4,6,27,32,33,34,35))
#Set the scenario as '1'
MC$SC_NO = 1

#Code repeats the above process to create a df for the actual landing times
MCa = j2
MCa$delay_sim = MCa$delay
MCa$PCP_sim <- j2$PCP.Act.Arrival
MCa$Arrival_Sim = MCa$Act.Arr
MCa = select(MCa,c(1,4,6,27,32,33,34,35))
MCa$SC_NO = 2

#Combine previous two dfs
MC = rbind(MC, MCa)

#Generate 498 random flight arrival schedules using the delay distribution.  
set.seed(31416)
for (i in 3:500) {
  print(i)
  delay_sample = sample(delay_list,nrow(j2))
  j2$delay_sim <- delay_sample
  j2$Arrival_Sim <- j2$Sch.Arr + j2$delay_sim
  j2$PCP_sim <- j2$PCP.Sch.Arrival + j2$delay_sim
  MSC = select(j2,c(1,4,6,27,32,33,34,35))
  MSC$SC_NO = i
  MC = rbind(MC,MSC)
}

#save file
RD_format = toString(format(Required_Date, "%d-%b-%y"))
file_name = paste("MCS_",RD_format, ".csv", sep="")
write.csv(MC,file_name)

#Analysis for validation and data exploraiton
#Graph of flight delay distributions for MC scenarios
MC %>% 
  filter((delay_sim > -14400) & (delay_sim < 14400)) %>%
  ggplot() + 
  aes(x = delay_sim/60)+
  xlim(-100, 200) +
  geom_density() +
  geom_vline(xintercept = median(MC$delay_sim)/60, linetype = "dashed", color = 'blue') +
  annotate(x = 50, y = 0.022, geom= 'text', label = paste("Median =", round(median(MC$delay_sim)/60), "minutes"))  + 
  labs(x = "Arrival Delay (Mins)", 
       y = "Density",
       title = "Distribution of Delays for Target Terminal in 2019",
       subtitle = "Extreme values removed") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6))
ggsave("_Delays.png")


#Scenario Analysis

#create breaks and labels for use in hourly graphs
t_breaks = seq(as.POSIXct(Day_Start), as.POSIXct(Day_End), "4 hours")
labels = c("04:00","08:00","12:00","16:00","20:00","00:00", "04:00")
#filter out the ineligible flight arrival scheudled (obtained from 3_feasible_landings.py )
MC = filter(MC, !(SC_NO %in% c()))

#Graph the arrival pattern for each flight by number of non-EEA pax
MC %>% 
  ggplot() + 
  aes(x = Arrival_Sim, y = Non.EEA_est, color = IATA) +
  geom_line() +
  scale_color_viridis(discrete = TRUE, option = "A")+
  scale_x_datetime(breaks = t_breaks, labels = labels)  +
  theme(legend.title = element_blank(),
        legend.position = "none") + 
  labs(x = "", 
     y = "Passengers") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6))
ggsave("flight_arrivals_size.png")

#Same graph as above for removing flights with <25 nonEEA PAX
MC %>% 
  filter(Non.EEA_est > 25) %>% 
  ggplot() + 
  aes(x = Arrival_Sim, y = Non.EEA_est, color = IATA) +
  geom_line() +
  scale_color_viridis(discrete = TRUE, option = "A")+
  theme(legend.title = element_blank(),
        legend.position = "none")
#Same graph showing only flights with <=25 PAX
MC %>% 
  filter(Non.EEA_est <= 25) %>% 
  ggplot() + 
  aes(x = Arrival_Sim, y = Non.EEA_est, color = IATA) +
  geom_line()
  scale_color_viridis(discrete = TRUE, option = "A")+
  theme(legend.title = element_blank(),
        legend.position = "none")
  
#Passenger Flow Analysis

#Create new column for the hour in which the flight arrives
MC$arr_hour = align.time(MC$Arrival_Sim - ((60*60)-1), n = 60*60)  

#Using results for wait time analysis of DES model
#Graph the total passenger arrivals for each hour of the day for the 'best' and 'worst' scenario
MC %>% 
  filter(SC_NO %in% c(76,62)) %>% 
  group_by(SC_NO, arr_hour) %>% 
  summarise(arrival_tot = sum(Non.EEA_est)) %>% 
  ggplot() + 
  aes(x = arr_hour, y = arrival_tot, fill = factor(SC_NO)) + 
  geom_col(position = 'dodge') + 
  scale_x_datetime(breaks = t_breaks, labels = labels)  +
  labs(x = "Time", 
       y = "Passengers",
       fill = "Scenario") +
  scale_fill_discrete(name = "Scenario", labels = c("Best (62)","Worst (76)"))+
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6))
ggsave("best_worst.png")

#Rolling Window Passenger Flow Analysis
#Code cycles through each scenario to find the maximum number of arriving passengers by specific time windows.
#Time windows are 15 minute intervals between 60 and 150 minutes.

#create a list of datetimes for 1 minute intervals for the 24 hour period. 
periods = as.POSIXct(seq.POSIXt(start,end, by=60))
#create list of time windows
gap = c(60, 75, 90, 105, 135, 150)
for (g in gap){      #For each time window
  roll_all = list()
  for (i in 1:500) {    #For each scenario
      runtext = paste(g,i)
      print(runtext)
      res_roll =list()
      ind = 0
      for (p in periods){    #For each datetime period
        roll = MC %>%
          filter(SC_NO == i) %>% 
          filter(Arrival_Sim >= p & Arrival_Sim <= p+ (60*g)) 
        ta = sum(roll$Non.EEA_est)
        ind = ind + 1
        res_roll[[ind]] = c(p,ta)
      }
      resroll = do.call(rbind, res_roll)
      colnames(resroll) = c("period","total")
      resroll = as.data.frame(resroll)
      max_roll = as.numeric(max(resroll$total))
      roll_all[[i]] = c(i,max_roll)
  }
  resAll = do.call(rbind, roll_all)
  cname =paste("Roll",g,"_Max",sep="")
  colnames(resAll) = c("Scenario",cname)
  resAll = as.data.frame(resAll)
  results_all = left_join(results_all, resAll, by="Scenario")  
}
#Save results  
write.csv(results_all,"All_Results_.csv")

