if (!require("pacman")) install.packages("pacman")

#Check if you really need to change the working directory. Skip this if you want
setwd("~/R/covid_trends_each_country")

pacman::p_load(datasets, pacman, tidyverse, rio, dplyr, ggplot2, magrittr)

#IMPORTING ONLINE DATA
df <- import("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/data.csv")
df <- as_tibble(df)

#DECLARING TODAY'S DATE
today.date = Sys.Date()

#THIS "ID" IS THE SELECTOR, CHANGE IT TO ANY 2 LETTER COUNTRY CODE, 
df_id <- df %>% filter(str_detect(geoId, "US"))
df_id <- as_tibble(df_id)

df_id %<>% 
  filter(cases > 0) #get rid of the date where the cases is 0, meaning the covid has not started
df_id$date <- as.Date(df_id$dateRep, format = "%d/%m/%y") #SETTING UP THE dateRep AS DATE

df_id %<>%   
  filter(date <= today.date) %>% #this to delete every date after today
  arrange(date)

#TIDYING UP DATA TO GET ONLY DATE, CASES, DEATHS, CUMULATIVE CASES AND DEATHS
df_id %<>% 
  as_tibble() %>%
  mutate(total_cases = cumsum(cases)) %>%
  mutate(total_deaths = cumsum(deaths)) %>%
  select(date, cases, deaths, total_cases, total_deaths) %>%
  mutate_if(is.integer, as.character) %>%
  mutate_if(is.character, as.numeric) %>%
  print()

#DECLARING THE 7 DAYS MOVING AVEERAGE OF CASES AND DEATH
df_id %<>% add_column(ave7 = df_id$cases, death7 = df_id$deaths)

#COMPUTING THE 7 DAYS MOVING AVERAGE OF CASES AND DEATH
for(i in 1:(nrow(df_id))){
  if (i < 7){                   #THIS WILL RETURN OWN VALUE BECAUSE NOT ENOUGH DATA
    df_id[i,6] = df_id[i,2]      
    df_id[i,7] = df_id[i,3]
  }
  else{  
    df_id[i,6] = colMeans(df_id[(i-6):i,6])
    df_id[i,7] = colMeans(df_id[(i-6):i,7])}
}


#PLOTTING
#BELOW, PLOTTING OF DATE VS DEATHS and ITS WEEKLY MOVING AVERAGE
df_id %>%
  ggplot(aes(x = date)) %>%
  + geom_col(aes(y = deaths, color = "red"), size = 1, fill = "white") %>%
  + geom_line(aes(y = death7, color = "blue"), size = 3, group=1) %>%
  #Change the title if you want
  + ggtitle(paste('U.S.A COVID-19 trends on', today.date, sep = " ")) %>%
  + theme(
    axis.line = element_line(size = 1),
    axis.text=element_text(size=17),
    axis.title=element_text(size=17,face="bold"),
    legend.text=element_text(size=17),
    legend.justification=c(1,1),
    legend.position=c(0.5,0.95),
    legend.title= element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50")) %>%
  + scale_color_manual(values = c("blue", "red"),
                       labels = c("Weekly Average","Daily Death Cases")) %>%
  + xlab("Date") %>%
  + ylab("Cases") %>%
  + ggsave(filename = paste0("death_", format(Sys.time(), "%Y-%m-%d_%H-%M", tz = "Europe/London"),".jpg"), width = 6.3 , height = 8.49, dpi = 300, units = "in")

#BELOW, PLOTTING OF DATE VS CASES and ITS WEEKLY MOVING AVERAGE
df_id %>%
  ggplot(aes(x = date)) %>%
  + geom_col(aes(y = cases, color = "darkorange"), size = 1, fill = "white") %>%
  + geom_line(aes(y = ave7, color = "blue"), size = 3, group=1) %>%
  #Change the title if you want
  + ggtitle(paste('U.S.A COVID-19 trends on', today.date, sep = " ")) %>%
  + theme(
    axis.line = element_line(size = 1),
    axis.text=element_text(size=17),
    axis.title=element_text(size=17,face="bold"),
    legend.text=element_text(size=17),
    legend.justification=c(1,1),
    legend.position=c(0.5,0.95),
    legend.title= element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50")) %>%
  + scale_color_manual(values = c("blue", "darkorange"),
                       labels = c("Weekly Average","Daily Cases")) %>%
  + xlab("Date") %>%
  + ylab("Cases") %>%
  + ggsave(filename = paste0("cases_", format(Sys.time(), "%Y-%m-%d_%H-%M", tz = "Europe/London"),".jpg"), width = 6.3 , height = 8.49, dpi = 300, units = "in")



rm(list = ls())

#ENJOY!!!
