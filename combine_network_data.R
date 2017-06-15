
library(plyr)
library(dplyr)

#set directory
directory <- ("C:/Users/206535096/Dropbox (NBCUniversal)/MSI_DSPractica/SoS_trends/data")

#read network data
brvo <- read.csv(file.path(directory, "CENT_18_49_V8_4_BRVO_non_agg.csv"))
syfy <- read.csv(file.path(directory, "CENT_18_49_V8_4_SYFY_non_agg.csv"))
usa <- read.csv(file.path(directory, "CENT_18_49_V8_3_USA_non_agg.csv"))
e <- read.csv(file.path(directory, "CENT_18_49_V8_3_ENT_non_agg.csv"))

# aggregate data at show level

brvo_showlevel <- plyr::ddply(subset(brvo, Source == "Nielsen" & LF_Season == 1), 
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

e_showlevel <- plyr::ddply(subset(e, Source == "Nielsen" & LF_Season == 1), 
                           c("Network", "Broadcast_Year", "Cable_Qtr", "DOW", "Date", "Show_Name", 
                             "Episode_num", "Season_num", "Start_Time", "End_Time", "Genre"), 
                           summarize, 
                           C3_Duration = sum(SC3_C_Dur),
                           C3_Impressions = sum(SC3_Impressions * SC3_C_Dur)/C3_Duration, 
                           LS_Duration = sum(LS_Dur),
                           LS_Impressions = sum(LS_Imps * LS_Dur)/LS_Duration)

# Create Rebase (to 1st Episode of Season) and Growth variables
brvo_showlevel <- brvo_showlevel %>% group_by(Show_Name, Season_num) %>% 
  mutate(C3_Rebase = C3_Impressions - C3_Impressions[1],
         C3_Growth = lag(C3_Impressions, 1) / C3_Impressions) %>% 
  mutate(LS_Rebase = LS_Impressions - LS_Impressions[1],
         LS_Growth = lag(LS_Impressions, 1) / LS_Impressions)

syfy_showlevel <- syfy_showlevel %>% group_by(Show_Name, Season_num) %>% 
  mutate(C3_Rebase = C3_Impressions - C3_Impressions[1],
         C3_Growth = lag(C3_Impressions, 1) / C3_Impressions) %>% 
  mutate(LS_Rebase = LS_Impressions - LS_Impressions[1],
         LS_Growth = lag(LS_Impressions, 1) / LS_Impressions)

usa_showlevel <- usa_showlevel %>% group_by(Show_Name, Season_num) %>% 
  mutate(C3_Rebase = C3_Impressions - C3_Impressions[1],
         C3_Growth = lag(C3_Impressions, 1) / C3_Impressions) %>% 
  mutate(LS_Rebase = LS_Impressions - LS_Impressions[1],
         LS_Growth = lag(LS_Impressions, 1) / LS_Impressions)

e_showlevel <- e_showlevel %>% group_by(Show_Name, Season_num) %>% 
  mutate(C3_Rebase = C3_Impressions - C3_Impressions[1],
         C3_Growth = lag(C3_Impressions, 1) / C3_Impressions) %>% 
  mutate(LS_Rebase = LS_Impressions - LS_Impressions[1],
         LS_Growth = lag(LS_Impressions, 1) / LS_Impressions)

#combine
showlevel_data <- rbind(brvo_showlevel, syfy_showlevel, usa_showlevel, e_showlevel)

#write
write.csv(showlevel_data, "CENT_showlevel.csv")


