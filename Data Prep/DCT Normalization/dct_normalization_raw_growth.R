
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(syuzhet)
library(purrr)
library(tidyr)

#read data
df <- read.csv("CENT_showlevel.csv") %>% select(-X)

#keep only shows that have both a first and second season in the data
#with 4 or more episodes
df <- df %>% group_by(Network, Show_Name) %>%
  filter(all(c(1, 2) %in% Season_num)) %>% 
  ungroup() %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  filter(max(Episode_num) >= 4) %>% 
  ungroup() %>% 
  group_by(Network, Show_Name) %>%
  filter(all(c(1, 2) %in% Season_num)) 

#remove show with single episode starting at episode 6
df <- df[df$Show_Name != "RHOM", ]


#transformation function
dct <- function(x){
  get_dct_transform(
    x, 
    low_pass_size = 3, 
    x_reverse_len = 100,
    scale_vals = FALSE,
    scale_range = TRUE
  )
}


#C3 Raw and Growth 
df_curves_C3_raw <- df %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(imps = list(C3_Impressions))

df_curves_C3_growth <- df %>%
  filter(!is.na(C3_Growth)) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(growth = list(C3_Growth))

df_with_dct_C3_raw <- df_curves_C3_raw %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number())

df_with_dct_C3_growth <- df_curves_C3_growth %>%
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number() + 100)

dct_C3_raw_growth <- rbind(df_with_dct_C3_raw, 
                           df_with_dct_C3_growth) %>% 
  arrange(Show_Name, Season_num, period)

dct_spread_C3_raw_growth <- dct_C3_raw_growth %>% 
  spread(key = period, value = dct_values)

write.csv(dct_C3_raw_growth, "dct_C3_raw_growth.csv")
write.csv(dct_spread_C3_raw_growth, "dct_spread_C3_raw_growth.csv")


#LS Raw and Growth
df_curves_LS_raw <- df %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(imps = list(LS_Impressions))

df_curves_LS_growth <- df %>%
  filter(!is.na(LS_Growth)) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(growth = list(LS_Growth))

df_with_dct_LS_raw <- df_curves_LS_raw %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number())

df_with_dct_LS_growth <- df_curves_LS_growth %>%
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number() + 100)

dct_LS_raw_growth <- rbind(df_with_dct_LS_raw, 
                           df_with_dct_LS_growth) %>% 
  arrange(Show_Name, Season_num, period)

dct_spread_LS_raw_growth <- dct_LS_raw_growth %>% 
  spread(key = period, value = dct_values)

write.csv(dct_LS_raw_growth, "dct_LS_raw_growth.csv")
write.csv(dct_spread_LS_raw_growth, "dct_spread_LS_raw_growth.csv")



#S1-S2 CONTINUOUS

#C3
dct_cont_C3_raw <- df %>%
  filter(Season_num == 1 | Season_num == 2) %>%
  group_by(Network, Show_Name) %>% 
  summarise(imps = list(C3_Impressions)) %>%
  ungroup() %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number())

dct_cont_C3_growth <- df %>%
  filter(Season_num == 1 | Season_num == 2) %>%
  filter(!is.na(C3_Growth)) %>% 
  group_by(Network, Show_Name) %>% 
  summarise(growth = list(C3_Growth)) %>%
  ungroup() %>% 
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number() + 100)

dct_C3_raw_growth <- rbind(dct_cont_C3_raw, 
                           dct_cont_C3_growth) %>% 
  arrange(Show_Name, period)

dct_spread_C3_raw_growth <- dct_C3_raw_growth %>% 
  spread(key = period, value = dct_values)

write.csv(dct_C3_raw_growth, "dct_C3_raw_growth_cont.csv")
write.csv(dct_spread_C3_raw_growth, "dct_spread_C3_raw_growth_cont.csv")

#LS
dct_cont_LS_raw <- df %>%
  filter(Season_num == 1 | Season_num == 2) %>%
  group_by(Network, Show_Name) %>% 
  summarise(imps = list(LS_Impressions)) %>%
  ungroup() %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number())

dct_cont_LS_growth <- df %>%
  filter(Season_num == 1 | Season_num == 2) %>%
  filter(!is.na(LS_Growth)) %>% 
  group_by(Network, Show_Name) %>% 
  summarise(growth = list(LS_Growth)) %>%
  ungroup() %>% 
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number() + 100)

dct_LS_raw_growth <- rbind(dct_cont_LS_raw, 
                           dct_cont_LS_growth) %>% 
  arrange(Show_Name, period)

dct_spread_LS_raw_growth <- dct_LS_raw_growth %>% 
  spread(key = period, value = dct_values)

write.csv(dct_LS_raw_growth, "dct_LS_raw_growth_cont.csv")
write.csv(dct_spread_LS_raw_growth, "dct_spread_LS_raw_growth_cont.csv")
