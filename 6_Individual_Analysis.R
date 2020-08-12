#Use to peform analysis of full results for a specific day/number of desks
#Change date as required

install.packages("ggthemes")
library(lubridate)
library(ggplot2)
library(xts)
library(dplyr)
library(ggthemes)

#Enter Required date
start_date = ""
end_date = ""
date_text = ""
#Exclude non-feasible scenarios (from feasiable landing scenario analysis)
exclude = c()


setwd()
results = list()

#Summary Statistic Analysis for different number of open desks for each scenario

for (i in 6:15){ #Cycle through open desks
  path = paste(date_text, "/full_results_", date_text,"_",i,"_500.csv", sep="") 
  ps = read.csv(path,quote="", na.strings = "", stringsAsFactors = FALSE) # load file
  #Cycle through scenarios
  for (n in 1:500){
    if(n %in% exclude) next #skip if non-feasible scenario
    ps_filtered = filter(ps, Scenario==n)
    ps_filtered$Arrival_Time =  as.POSIXct(ps_filtered$Arrival_Time)
    avg_wait = mean(ps_filtered$WaitTime_Mins) # average wait time
    p95 = quantile(ps_filtered$WaitTime_Mins, .95) # 95th percentile
    varT = var(ps_filtered$Arrival_Time) #variance
    results[[n + ((i-6)*500)]] = c(i, n, avg_wait, p95, varT)
  }
}
#Create dataframe of above results
res_df = do.call(rbind, results)
colnames(res_df) = c('Desks','Scenarios', 'Average_Wait',"Per95","Variance")
res_df = as.data.frame(res_df)

#Save File
write.csv(res_df,"Results_.csv")


#Time window passenger flow analysis 

#load time window flow results from scenario analysis
res_ALL = read.csv("ALL_Results_.csv")

#Combine results df with flow analysis results for fixed number of desks
comAll = res_df %>% 
  filter(Desks ==10) %>% 
  left_join(res_ALL, by = c("Scenarios" = "Scenario"))

#Scatter graph of max pax flow by 95th percentile wait time
comAll%>% 
  ggplot()+
  aes(x = Roll105_Max, y= Per95)+
  geom_point() + geom_smooth(method = 'lm') + 
  labs(x = "Maximum Pax Total for 105 minute Window",
       y = "95th Percentile Wait Time",
       title = "Comparison of Scenario Arrival Peaks with SLA target",
       subtitle = "Busy Day") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10))+
  theme_hc() 
ggsave("Analysis/rolling-sla_cor11sep.png")

#Correlation of max pax flow and 95th percentile wait time
cor(comAll$Roll105_Max, comAll$Per95)
#SLR modelling of max pax flow and 95th percentile wait time
summary(lm(Per95~Roll105_Max,comAll))

#Calculate top and bottom 3 scenarios in terms of 95th percentile for x desks
require(dplyr)
res_df %>% 
  filter(Desks == 10) %>% 
  top_n(-3, Per95)

res_df %>% 
  filter(Desks == 10) %>% 
  top_n(3, Per95)

res_10 = filter(res_df, Desks == 10)

#Wait Time Distribution Analysis

#Box plot distributions of Average wait times for all scenarios for each desk total
res_df %>% 
  group_by(Desks) %>% 
  mutate(SLA= ifelse(mean(Per95)>45,"Breach","Within")) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = Desks, y = Average_Wait, group = Desks, color = SLA) +
  scale_x_continuous(breaks = seq(6,15,1)) +
  labs(x = "Desks Open",
       y = "PAX Wait Time (mins)") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10))+
  theme_hc() +
  geom_boxplot()
ggsave("Analysis/Publish/WaitDis_ALL_.png")


#Boxplot graph as above but filtering for average 95th within 45 mins
res_df %>% 
  group_by(Desks) %>% 
  mutate(SLA= ifelse(mean(Per95)>45,"Breach","Within")) %>% 
  ungroup() %>% 
  filter(SLA == "Within") %>% 
  ggplot() +
  aes(x = Desks, y = Average_Wait, group = Desks) +
  scale_x_continuous(breaks = seq(6,15,1)) +
  labs(x = "Desks Open",
       y = "Wait Time (mins)") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10))+
  theme_hc() +
  geom_boxplot(color = 'turquoise')
ggsave("Analysis/Publish/WaitDis_SLA_High.png")

#Boxplot graph as above but for 95th Percentiles
res_df %>% 
  ggplot() +
  aes(x = Desks, y = Per95, group = Desks) +
  scale_x_continuous(breaks = seq(6,15,1)) +
  labs(x = "Desks Open",
       y = "Wait Time (mins)") +
  geom_hline(yintercept=45, linetype = 'dashed', color = 'red')+
  theme_hc() +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10))+
  annotate(geom="text", x = 16, y = 40, label = "SLA", color = 'red') +
  geom_boxplot(color = 'gray44')
ggsave("Analysis/Publish/Dis95th_ALL_.png")


#Line graph of 95th percentile wait time for scheduled arrival scenario
res_df %>% 
  filter(Scenarios == 1) %>% 
  ggplot()+
  aes(x = Desks, y = Per95)+
  geom_line() + 
  scale_x_continuous(breaks = seq(6,15,1)) +
  labs(x = "Desks Open",
       y = "Wait Time (mins)") +
  geom_hline(yintercept=45, linetype = 'dashed', color = 'red')+
  theme_hc() +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10))
ggsave("Analysis/Publish/Sch_P95.png")

#Analysis to calcuate total number of scenarios that meet criteria
#>45 min 95th percentile with 10 desks
res_df %>% 
  filter(Desks == 10) %>% 
  filter(Per95>45) %>% 
  nrow()
#>45 min 95th percentile with 12 desks
res_df %>% 
  filter(Desks == 12) %>% 
  filter(Per95>45) %>% 
  nrow()
#>45 min 95th percentile with 8 desks
res_df %>% 
  filter(Desks ==8) %>% 
  nrow()


#MCS average wait time vs Schedule wait time analysis

#load comparison file (created manually in excel)
res_comp = read.csv("Avg_Sch.csv")
colnames(res_comp)[1] = "Passenger_Volume"
#set volumes as factors
res_comp$Volumes = factor(res_comp$Passenger_Volume, levels = c("Low","Medium", "High"))

#Graph of difference in mcs average and scheduled wait times
res_comp %>% 
  ggplot() +
  aes(x = Desks, y = Difference*100) +
  scale_x_continuous(breaks = seq(6,15,1)) +
  labs(x = "Desks Open",
       y = "Difference in Wait Time (%)") + 
  theme_hc() +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10), 
        axis.text.y = element_text(size=10), axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 10))+ 
  geom_col()+
  facet_wrap(~ Volumes)
ggsave("Analysis/Publish/Avg_Sch.png")



