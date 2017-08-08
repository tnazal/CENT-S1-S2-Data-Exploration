
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
#with 3 or more episodes
df <- df %>% group_by(Network, Show_Name) %>%
  filter(all(c(1, 2) %in% Season_num)) %>% 
  ungroup() %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  filter(max(Episode_num) >= 3) %>% 
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


#to cluster C3 & LS appended
df_curves_C3 <- df %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(imps = list(C3_Impressions))

df_curves_LS <- df %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(imps = list(LS_Impressions))

df_with_dct_C3 <- df_curves_C3 %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number())

df_with_dct_LS <- df_curves_LS %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number() + 100)

dct_C3_LS_raw <- rbind(df_with_dct_C3, df_with_dct_LS) %>% 
  arrange(Show_Name, Season_num, period)

dct_spread_C3_LS_raw <- dct_C3_LS %>% 
  spread(key = period, value = dct_values)

write.csv(dct_C3_LS_raw, "dct_C3_LS_raw.csv")
write.csv(dct_spread_C3_LS_raw, "dct_spread_C3_LS_raw.csv")


#to cluster on differences
C3_LS_diff_curves <- df %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(imps = list(diff_C3_LS))

dct_C3_LS_diff <- C3_LS_diff_curves %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number())

dct_spread_C3_LS_diff <- dct_C3_LS_diff %>% 
  spread(key = period, value = dct_values)

write.csv(dct_C3_LS_diff, "dct_C3_LS_diff.csv")
write.csv(dct_spread_C3_LS_diff, "dct_spread_C3_LS_diff.csv")


#to cluster on averages
C3_LS_avg_curves <- df %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(imps = list(avg_C3_LS))

dct_C3_LS_avg <- C3_LS_avg_curves %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number())

dct_spread_C3_LS_avg <- dct_C3_LS_avg %>% 
  spread(key = period, value = dct_values)

write.csv(dct_C3_LS_avg, "dct_C3_LS_avg.csv")
write.csv(dct_spread_C3_LS_avg, "dct_spread_C3_LS_avg.csv")



#S1-S2 CONTINUOUS

#appended
dct_cont_C3 <- df %>%
  group_by(Network, Show_Name) %>% 
  filter(Season_num == 1 | Season_num == 2) %>% 
  summarise(imps = list(C3_Impressions)) %>%
  ungroup() %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number())

dct_cont_LS <- df %>%
  group_by(Network, Show_Name) %>% 
  filter(Season_num == 1 | Season_num == 2) %>% 
  summarise(imps = list(LS_Impressions)) %>%
  ungroup() %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number() + 100) 

dct_cont_C3_LS <- rbind(dct_cont_C3, dct_cont_LS) %>% 
  arrange(Show_Name, period)

dct_spread_cont_C3_LS <- dct_cont_C3_LS %>% 
  spread(key = period, value = dct_values)

write.csv(dct_cont_C3_LS, "dct_C3_LS_raw_cont.csv")
write.csv(dct_spread_cont_C3_LS, "dct_spread_C3_LS_raw_cont.csv")

#differences
dct_cont_diff <- df %>%
  group_by(Network, Show_Name) %>% 
  filter(Season_num == 1 | Season_num == 2) %>% 
  summarise(imps = list(diff_C3_LS)) %>%
  ungroup() %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number()) 

dct_spread_cont_diff <- dct_cont_diff %>% 
  spread(key = period, value = dct_values)

write.csv(dct_cont_diff, "dct_C3_LS_diff_cont.csv")
write.csv(dct_spread_cont_diff, "dct_spread_C3_LS_diff_cont.csv")

#averages
dct_cont_avg <- df %>%
  group_by(Network, Show_Name) %>% 
  filter(Season_num == 1 | Season_num == 2) %>% 
  summarise(imps = list(avg_C3_LS)) %>%
  ungroup() %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number()) 

dct_spread_cont_avg <- dct_cont_avg %>% 
  spread(key = period, value = dct_values)

write.csv(dct_cont_avg, "dct_C3_LS_avg_cont.csv")
write.csv(dct_spread_cont_avg, "dct_spread_C3_LS_avg_cont.csv")
