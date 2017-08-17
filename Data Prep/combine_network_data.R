
library(plyr)
library(dplyr)

#set directory and read in minut-by-minute network data from Bravo, E!, Syfy, and USA
directory <- ("C:/Users/206535096/Dropbox (NBCUniversal)/MSI_DSPractica/SoS_trends/data")

brvo <- read.csv(file.path(directory, "CENT_18_49_V8_4_BRVO_non_agg.csv"))
e <- read.csv(file.path(directory, "CENT_18_49_V8_3_ENT_non_agg.csv"))
syfy <- read.csv(file.path(directory, "CENT_18_49_V8_4_SYFY_non_agg.csv"))
usa <- read.csv(file.path(directory, "CENT_18_49_V8_3_USA_non_agg.csv"))

#aggregate data at the episode level for each show and each stream (C3 and LS)
brvo_showlevel <- plyr::ddply(subset(brvo, Source == "Nielsen" & LF_Season == 1), 
                              c("Network", "Broadcast_Year", "Cable_Qtr", "DOW", "Date", "Show_Name", 
                                "Episode_num", "Season_num", "Start_Time", "End_Time", "Genre"), 
                              summarize, 
                              C3_Duration = sum(SC3_C_Dur),
                              C3_Impressions = sum(SC3_Impressions * SC3_C_Dur)/C3_Duration, 
                              LS_Duration = sum(LS_Dur),
                              LS_Impressions = sum(LS_Imps * LS_Dur)/LS_Duration)

e_showlevel <- plyr::ddply(subset(e, Source == "Nielsen" & LF_Season == 1), 
                           c("Network", "Broadcast_Year", "Cable_Qtr", "DOW", "Date", "Show_Name", 
                             "Episode_num", "Season_num", "Start_Time", "End_Time", "Genre"), 
                           summarize, 
                           C3_Duration = sum(SC3_C_Dur),
                           C3_Impressions = sum(SC3_Impressions * SC3_C_Dur)/C3_Duration, 
                           LS_Duration = sum(LS_Dur),
                           LS_Impressions = sum(LS_Imps * LS_Dur)/LS_Duration)

syfy_showlevel <- plyr::ddply(subset(syfy, Source == "Nielsen" & LF_Season == 1), 
                              c("Network", "Broadcast_Year", "Cable_Qtr", "DOW", "Date", "Show_Name", 
                                "Episode_num", "Season_num", "Start_Time", "End_Time", "Genre"), 
                              summarize, 
                              C3_Duration = sum(SC3_C_Dur),
                              C3_Impressions = sum(SC3_Impressions * SC3_C_Dur)/C3_Duration, 
                              LS_Duration = sum(LS_Dur),
                              LS_Impressions = sum(LS_Imps * LS_Dur)/LS_Duration)

usa_showlevel <- plyr::ddply(subset(usa, Source == "Nielsen" & LF_Season == 1), 
                             c("Network", "Broadcast_Year", "Cable_Qtr", "DOW", "Date", "Show_Name", 
                               "Episode_num", "Season_num", "Start_Time", "End_Time", "Genre"), 
                             summarize, 
                             C3_Duration = sum(SC3_C_Dur),
                             C3_Impressions = sum(SC3_Impressions * SC3_C_Dur)/C3_Duration, 
                             LS_Duration = sum(LS_Dur),
                             LS_Impressions = sum(LS_Imps * LS_Dur)/LS_Duration)

#reformat date variables
brvo_showlevel$Date <- as.Date(brvo_showlevel$Date)
e_showlevel$Date <- as.Date(e_showlevel$Date)
syfy_showlevel$Date <- as.Date(syfy_showlevel$Date)
usa_showlevel$Date <- as.Date(usa_showlevel$Date, format = "%m/%d/%Y")

#combine all networks
showlevel_data <- rbind(brvo_showlevel, e_showlevel, syfy_showlevel, usa_showlevel) %>% 
  arrange(Show_Name, Season_num, Episode_num)

#create rebase (to 1st episode of season) and growth variables
showlevel_data <- showlevel_data %>% group_by(Show_Name, Season_num) %>% 
  mutate(C3_Rebase = C3_Impressions - C3_Impressions[1],
         C3_Growth = lag(C3_Impressions, 1) / C3_Impressions,
         LS_Rebase = LS_Impressions - LS_Impressions[1],
         LS_Growth = lag(LS_Impressions, 1) / LS_Impressions)

#create variables for C3 & LS differences and averages
showlevel_data$diff_C3_LS <- showlevel_data$C3_Impressions - showlevel_data$LS_Impressions
showlevel_data$higher_imps <- ifelse(showlevel_data$C3_Impressions > showlevel_data$LS_Impressions, "C3", "LS")
showlevel_data$avg_C3_LS <- (showlevel_data$C3_Impressions + showlevel_data$LS_Impressions) / 2

#create rebase (to 1st episode of season) and growth for C3 & LS transformed variables
showlevel_data$diff_C3_LS_rebase <- showlevel_data$C3_Rebase - showlevel_data$LS_Rebase
showlevel_data$diff_C3_LS_growth <- showlevel_data$C3_Growth - showlevel_data$LS_Growth
showlevel_data$avg_C3_LS_rebase <- (showlevel_data$C3_Rebase + showlevel_data$LS_Rebase) / 2
showlevel_data$avg_C3_LS_growth <- (showlevel_data$C3_Growth + showlevel_data$LS_Growth) / 2

write.csv(showlevel_data, "CENT_showlevel.csv")
