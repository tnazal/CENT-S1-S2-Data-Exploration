
library(dplyr)

#read in data
data <- read.csv("CENT_showlevel.csv") %>% select(-X)


#keep only S1 & S2 shows that were used in clustering
data <- data %>% group_by(Network, Show_Name) %>%
  filter(all(c(1, 2) %in% Season_num)) %>% 
  ungroup() %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  filter(max(Episode_num) >= 3) %>% 
  ungroup() %>% 
  group_by(Network, Show_Name) %>% 
  filter(all(c(1, 2) %in% Season_num)) %>% 
  arrange(Show_Name, Season_num, Episode_num) %>% 
  filter(Season_num == 1 | Season_num == 2)

data <- data[data$Show_Name != "RHOM", ]


#add best clusters
s1_shows_best_clusters <- read.csv("s1_shows_best_clusters.csv") %>% select(-X)


#edit date format for program start, rounding to nearest half hour for grouping
#and add variable for program duration in minutes
data$Start_Time <- as.POSIXlt(round(as.double(strptime(as.character(data$Start_Time), 
                                                       "%H:%M:%S"))/(30*60))*(30*60), 
                              origin = (as.POSIXlt('1970-01-01')))

data$End_Time <- as.POSIXlt(round(as.double(strptime(as.character(data$End_Time), 
                                                     "%H:%M:%S"))/(30*60))*(30*60), 
                            origin = (as.POSIXlt('1970-01-01')))

data$Program_Duration <- data$End_Time - data$Start_Time

data$Program_Duration[data$Program_Duration == -1410] <- 30
data$Program_Duration[data$Program_Duration == -1380] <- 60
data$Program_Duration[data$Program_Duration == -1320] <- 120

data$Start_Time <- format(data$Start_Time, "%H:%M")
data$End_Time <- format(data$End_Time, "%H:%M")


#create variable for cumulative episode count over S1-S2
data <- data %>% group_by(Show_Name) %>% 
  mutate(cum_ep = row_number())


#get unique days of week and start time overall and per season
data <- data %>% 
  group_by(Show_Name) %>% 
  mutate(unique_DOWs_total = length(unique(DOW)), 
         unique_starts_total = length(unique(Start_Time)))

data <- data %>% 
  group_by(Show_Name, Season_num) %>% 
  mutate(unique_DOWs_season = length(unique(DOW)), 
         unique_starts_season = length(unique(Start_Time)))


#get changes in day of week and start time overall and per season
data <- data %>% 
  mutate(change_DOW_overall = ifelse(unique_DOWs_total == 1, "No", "Yes"), 
         change_start_overall = ifelse(unique_starts_total == 1, "No", "Yes"), 
         change_DOW_season = ifelse(unique_DOWs_season == 1, "No", "Yes"), 
         change_start_season = ifelse(unique_starts_season == 1, "No", "Yes"))


#add cluster information
data <- merge(data, s1_shows_best_clusters)

write.csv(data, "s1_s2_data.csv")



#S1 and S2 Overall Statistics

#get correlations between C3 and LS
correlations <- data %>% 
  group_by(Show_Name, Season_num) %>% 
  summarise(correlation = cor(C3_Impressions, LS_Impressions, method = "pearson"))


#get season averages
season_averages <- data %>% 
  group_by(Show_Name, Season_num) %>% 
  summarise(avg_C3 = mean(C3_Impressions), 
            avg_LS = mean(LS_Impressions))

stats_by_season <- merge(season_averages, correlations)

stats_by_season$higher_imps <- ifelse(stats_by_season$avg_C3 > stats_by_season$avg_LS, "C3", "LS")


#total unique days of week/start times in each season
season_changes <- data %>% group_by(Show_Name, Season_num) %>% 
  summarise(unique_DOWs_total = mean(unique_DOWs_total), 
            unique_starts_total = mean(unique_starts_total), 
            unique_DOWs_season = mean(unique_DOWs_season), 
            unique_starts_season = mean(unique_starts_season))

stats_by_season <- merge(stats_by_season, season_changes)


#get overall changes for each season
stats_by_season <- stats_by_season %>% 
  mutate(change_DOW_overall = ifelse(unique_DOWs_total == 1, "No", "Yes"), 
         change_start_overall = ifelse(unique_starts_total == 1, "No", "Yes"), 
         change_DOW_season = ifelse(unique_DOWs_season == 1, "No", "Yes"), 
         change_start_season = ifelse(unique_starts_season == 1, "No", "Yes"))


#add cluster information
stats_by_season <- merge(stats_by_season, s1_shows_best_clusters)

write.csv(stats_by_season, "s1_s2_stats_by_season.csv")
