
library(plyr)
library(dplyr)

#set directory
directory <- ("C:/Users/206535096/Dropbox (NBCUniversal)/V8_2")

#read network data
brvo <- read.csv(file.path(directory, "CENT_18_49_V8_2_BRVO.csv"))
syfy <- read.csv(file.path(directory, "CENT_18_49_V8_2_SYFY.csv"))
usa <- read.csv(file.path(directory, "CENT_18_49_V8_2_USA.csv"))
e <- read.csv(file.path(directory, "CENT_18_49_V8_2_ENT.csv"))

# aggregate data at show level
brvo_showlevel <- plyr::ddply(subset(brvo, Source == "Nielsen" & LF_Season == 1), 
                              c("Network", "Broadcast_Year", "Cable_Qtr", "DOW", "Date", "Show_Name", 
                                "Episode_num", "Season_num", "Start_Time", "End_Time", "Genre"), 
                              summarize, 
                              C3_Dur = sum(SC3_C_Dur),
                              C3_Imps = sum(SC3_Impressions * SC3_C_Dur)/C3_Dur, 
                              LS_Dur = sum(LS_Dur), 
                              LS_Imps = sum(LS_Imps * LS_Dur)/LS_Dur)

syfy_showlevel <- plyr::ddply(subset(syfy, Source == "Nielsen" & LF_Season == 1), 
                              c("Network", "Broadcast_Year", "Cable_Qtr", "DOW", "Date", "Show_Name", 
                                "Episode_num", "Season_num", "Start_Time", "End_Time", "Genre"), 
                              summarize, 
                              C3_Dur = sum(SC3_C_Dur),
                              C3_Imps = sum(SC3_Impressions * SC3_C_Dur)/C3_Dur, 
                              LS_Dur = sum(LS_Dur), 
                              LS_Imps = sum(LS_Imps * LS_Dur)/LS_Dur)

usa_showlevel <- plyr::ddply(subset(usa, Source == "Nielsen" & LF_Season == 1), 
                             c("Network", "Broadcast_Year", "Cable_Qtr", "DOW", "Date", "Show_Name", 
                               "Episode_num", "Season_num", "Start_Time", "End_Time", "Genre"), 
                             summarize, 
                             C3_Dur = sum(SC3_C_Dur),
                             C3_Imps = sum(SC3_Impressions * SC3_C_Dur)/C3_Dur, 
                             LS_Dur = sum(LS_Dur), 
                             LS_Imps = sum(LS_Imps * LS_Dur)/LS_Dur)

e_showlevel <- plyr::ddply(subset(e, Source == "Nielsen" & LF_Season == 1), 
                           c("Network", "Broadcast_Year", "Cable_Qtr", "DOW", "Date", "Show_Name", 
                             "Episode_num", "Season_num", "Start_Time", "End_Time", "Genre"), 
                           summarize, 
                           C3_Dur = sum(SC3_C_Dur),
                           C3_Imps = sum(SC3_Impressions * SC3_C_Dur)/C3_Dur, 
                           LS_Dur = sum(LS_Dur), 
                           LS_Imps = sum(LS_Imps * LS_Dur)/LS_Dur)

# Create Rebase (to 1st Episode of Season) and Growth variables
brvo_showlevel <- brvo_showlevel %>% group_by(Show_Name, Season_num) %>% 
  mutate(rebase = C3_Imps - C3_Imps[1],
         growth = lag(C3_Imps, 1) / C3_Imps) %>% 
  mutate(LS_rebase = LS_Imps - LS_Imps[1],
         LS_growth = lag(LS_Imps, 1) / LS_Imps)

syfy_showlevel <- syfy_showlevel %>% group_by(Show_Name, Season_num) %>% 
  mutate(rebase = C3_Imps - C3_Imps[1],
         growth = lag(C3_Imps, 1) / C3_Imps) %>% 
  mutate(LS_rebase = LS_Imps - LS_Imps[1],
         LS_growth = lag(LS_Imps, 1) / LS_Imps)

usa_showlevel <- usa_showlevel %>% group_by(Show_Name, Season_num) %>% 
  mutate(rebase = C3_Imps - C3_Imps[1],
         growth = lag(C3_Imps, 1) / C3_Imps) %>% 
  mutate(LS_rebase = LS_Imps - LS_Imps[1],
         LS_growth = lag(LS_Imps, 1) / LS_Imps)

e_showlevel <- e_showlevel %>% group_by(Show_Name, Season_num) %>% 
  mutate(rebase = C3_Imps - C3_Imps[1],
         growth = lag(C3_Imps, 1) / C3_Imps) %>% 
  mutate(LS_rebase = LS_Imps - LS_Imps[1],
         LS_growth = lag(LS_Imps, 1) / LS_Imps)

#combine
showlevel_data <- rbind(brvo_showlevel, syfy_showlevel, usa_showlevel, e_showlevel)

#write
write.csv(showlevel_data, "CENT_showlevel.csv")


