library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(xts)
library(plyr)

#Enter datetimes/text for required date

start_date = ""
end_date = ""
date_text = ""

setwd("")

#Convergence Analysis

#set which results you are analysing (number of gates)
i = "10"
#load file
path = paste(date_text, "/full_results_", date_text,"_",i,"_500.csv", sep="") 
ps = read.csv(path,quote="", na.strings = "", stringsAsFactors = FALSE)

#Generate the mean, variance and maximum for wait times for the addition of each scenario
results = list()
for (n in 1:500){
  ps_filtered = filter(ps, Scenario<=n)      #Filter out scenarios less than n
  avg_wait = mean(ps_filtered$WaitTime_Mins)
  variance = var(ps_filtered$WaitTime_Mins)
  max = max(ps_filtered$WaitTime_Mins)
  results[[n]] = c(n, avg_wait, max, variance)
}
#create dataffram of results
res_df = do.call(rbind, results)
colnames(res_df) = c('Scenarios', 'Average_Wait','Maximum', 'Variance')
res_df = as.data.frame(res_df)

#Graph of result for mean wait times for 25+ scenarios
res_df %>% 
  filter(Scenarios>24) %>% 
  ggplot() +
  aes(x = Scenarios, y = Average_Wait) + 
  geom_line() + 
  labs(x = "Total Scenarios Included (Starts at 25)",
       y = "PAX Wait Time (mins)",
       title = "Wait Time Variance Convergence",
       subtitle = "") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme_hc()
ggsave("Analysis/Converg_mean.png")

#Graph of result for wait times variance for 25+ scenarios
res_df %>% 
  filter(Scenarios>24) %>% 
  ggplot() +
  aes(x = Scenarios, y = Variance) + 
  geom_line() + 
  labs(x = "Total Scenarios Included (Starts at 25)",
       y = "PAX Wait Time (mins)",
       title = "Wait Time Variance Convergence",
       subtitle = "") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme_hc()
ggsave("Analysis/Converg_var_10.png")

#Graph of result for maximum wait times for 25+ scenarios
res_df %>% 
  filter(Scenarios>24) %>% 
  ggplot() +
  aes(x = Scenarios, y = Maximum) + 
  geom_line() + 
  labs(x = "Total Scenarios Included (Starts at 25)",
       y = "PAX Wait Time (mins)",
       title = "Wait Time Variance Convergence",
       subtitle = "_10 Desks") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size=10), 
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme_hc()
ggsave("Analysis/Converg_var_11.png")




