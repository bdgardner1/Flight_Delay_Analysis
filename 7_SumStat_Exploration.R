#Script uses the summary results from "RCode_Stat_Processing.R" to perform data exploration
#Required dates need to be added for the summary results. Dataframes should be updated for required day. 
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(xts)
library(plyr)
library(ggthemes)

setwd("")
#set required dates
req_dates = c()
#loops through required dates
for (d in req_dates){
  #loops through open desk numbers
  for(i in 6:15) {
    i = toString(i)
    file_name = paste(d, "/", d,"_",i, "_500.csv",sep="")
    file_load = read.csv(file_name)
    file_load$Date = d
    file_load$Period = as.POSIXct(file_load$Period)
    desk_name = paste("desks_",i,sep="")
    assign(desk_name,file_load)
  }
  #create list of all desk dfs
  desk_list = list(desks_6,desks_7, desks_8, desks_9, desks_10, desks_11, desks_12, desks_13, desks_14, desks_15)
  #merge all desk dfs into single df for the date
  assign(paste("desksAll_",d,sep=""),ldply(desk_list))

}

#combine all dates into one df
desksALL = rbind(desksAll_, desksAll_, desksAll_)
#create summary statistics for probability of SLA breach and average wait for each desk opening and date
desk_comp = unique(select(desksALL, c("Date", "desk_no", "prob_breachSLA", "avg_wait")))

#Create comparison of SLA breach probability for all days by open desk
desk_comp %>% 
  ggplot() +
  aes(x = desk_no, y = prob_breachSLA, color = Date) +
  geom_line() + 
  scale_x_continuous(breaks = seq(6,15,1)) +
  scale_color_manual(values = c('green','orange','red'),limits = c("07dec19","03sep19","11sep19"),name = "Passenger Levels", labels = c("Low","Medium","High")) +
  labs(x = "Desks Open",
       y = "Probability 95th Percentile > 45 Mins",
       title = "The risk of SLA breach is highly sensitive to open desks") +
  theme_hc() +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10))
ggsave("Analysis/Publish/SLAProb_ALL.png")

#create comparison of average wait time for all open desks and all days
desk_comp %>% 
  ggplot() +
  aes(x = desk_no, y = avg_wait, color = Date) +
  geom_line() + 
  scale_x_continuous(breaks = seq(6,15,1)) +
  scale_color_manual(values = c('green','orange','red'),limits = c("day1","day2","day3"),name = "Passenger Levels", labels = c("Low","Medium","High")) +
  labs(x = "Desks Open",
       y = "Wait Time (Mins)",
       title = "Average Simulated Passenger Queue Wait Times",
       subtitle = "Comparison by passenger volumes over all scenarios") +
  theme_hc() +
  theme(plot.title = element_text(size = 12),
        legend.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10))

ggsave("Analysis/Publish/AvgWait_All.png")

write.csv(desk_comp,"Desk_Comparison.csv")

#comparison of scheduled wait time and worst case scenario
desksAll_ %>% 
  filter(desk_no %in% c(10)) %>% 
  ggplot() +
  aes(x= Period) +
  geom_line(aes(y = Worst_Case_Avg, color = 'Worst Case'))+
  geom_line(aes(y = Scheduled, color = 'Scheduled')) + 
  theme_hc() +
  labs(x = "", 
       y = "Average Wait Time (Mins)",
       title = "Comparison of Scheduled and Worst Case Scenarios",
       subtitle = "LHRT3 10 Desks") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10))
ggsave("Analysis/sch-worstComp_11sep.png")

#Average wait times for day_ by open desk number
desksAll_ %>% 
  ggplot() +
  aes(x= Period, y =Scheduled, color = factor(desk_no)) +
  geom_line() + 
  theme_hc() +
  labs(x = "", 
       y = "Wait Time (Mins)",
       title = "Comparison of Average Wait Time by Operational Desks",
       color = "Open Desks") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6))

ggsave("Analysis/DeskAvgComp_.png")

#Worst case scenario wait time for all open desks numbers
desksAll_ %>% 
  ggplot() +
  aes(x= Period, y =Worst_Case_Avg, color = factor(desk_no)) +
  geom_line() + 
  theme_hc() +
  labs(x = "", 
       y = "Wait Time (Mins)",
       title = "Comparison of Worst Case Wait Time by Operational Desks",
       color = "Open Desks") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10),        
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6))

ggsave("Analysis/DeskWorstComp_.png")

#convert date to date format
desksALL$Date = as.Date(desksALL$Date, format = "%d%b%y")

#The difference in total wait time for the overall average and scheduled time for each day
desksALL %>% 
  dplyr::group_by(Date,desk_no) %>% 
  dplyr::summarise(difference = sum(Overall_Avg) - sum(Scheduled)) %>% 
  ggplot()+
  aes(x = desk_no, y = difference) +
  scale_x_continuous(breaks = seq(6,15,1))+
  labs(x = "Open Desks",
       y = "Average Wait Time Difference (mins)", 
         title = "Difference in total average scenario wait time compared to scheduled") + 
  geom_col() + 
  theme_hc() +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10)) + 
  facet_wrap(~Date)

  ggsave("Analysis/WaitDiff_ALL.png")

  #Comparison of scheduled and overall wait times for day_ for 6 desks open
desksAll_ %>% 
  filter(desk_no %in% c(10)) %>% 
  ggplot() +
  aes(x= Period) +
  geom_line(aes(y = Overall_Avg, color = 'Scenario Average'))+
  geom_line(aes(y = Scheduled, color = 'Scheduled')) + 
  theme_hc() +
  labs(x = "", 
       y = "Average Wait Time (Mins)",
       title = "Comparison of Scheduled and Scenario Average",
       subtitle = "LHRT3 10 Desks") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10))

ggsave("Analysis/sch-avgComp_10_.png")

t_breaks = seq(as.POSIXct(""), as.POSIXct(""), "4 hours")
labels = c("04:00","08:00","12:00","16:00","20:00","00:00")
#Comparison of scheduled and overall wait times for medium for 9 desks open
desksAll_ %>% 
  filter(desk_no == 10) %>% 
  ggplot() +
  aes(x= Period) +
  geom_line(aes(y = Overall_Avg, color = 'Scenario Average'))+
  geom_line(aes(y = Scheduled, color = 'Scheduled')) + 
  scale_x_datetime(breaks = t_breaks, labels = labels) + 
  theme_hc() +
  labs(x = "", 
       y = "Average Wait Time (Mins)",
       title = "") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10))

ggsave("Analysis/Publish/sch-avgComp_10_high.png")

#Same for 8 desks
desksAll_ %>% 
  filter(desk_no %in% c(8)) %>% 
  ggplot() +
  aes(x= Period) +
  geom_line(aes(y = Overall_Avg, color = 'Scenario Average'))+
  geom_line(aes(y = Scheduled, color = 'Scheduled')) + 
  theme_hc() +
  labs(x = "", 
       y = "Average Wait Time (Mins)",
       title = "Comparison of Scheduled and Scenario Average",
       subtitle = "LHRT3 10 Desks") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10))
ggsave("Analysis/sch-avgComp_10_.png")

#Probability that wait times exceed 45 and 90 minutes for day_ for 7,8,9 and 10 open desks
desksAll_ %>% 
  filter(desk_no %in% c(7:10)) %>% 
  ggplot() +
  aes(x= Period) +
  geom_line(aes(y = Over45, color = '> 45 Mins'))+
  geom_line(aes(y = Over90, color = '> 90 Mins')) + 
  scale_x_datetime(breaks = t_breaks, labels = labels) + 
  theme_hc() +
  labs(x = "", 
       y = "Probability") +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 8), 
        axis.text.y = element_text(size=8), axis.title.x = element_text(size = 4), 
        axis.text.x = element_text(size = 5), axis.title.y = element_text(size = 10),
         strip.text.x = element_text(size = 8, color = 'gray44')) + 
  facet_wrap(~desk_no)

ggsave("Analysis/Publish/prob_7-10_high.png")



