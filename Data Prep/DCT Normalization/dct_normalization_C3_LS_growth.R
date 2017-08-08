
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


#to cluster C3 & LS appended
df_curves_C3_growth <- df %>%
  filter(!is.na(C3_Growth)) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(growth = list(C3_Growth))

df_curves_LS_growth <- df %>%
   filter(!is.na(LS_Growth)) %>% 
   group_by(Network, Show_Name, Season_num) %>% 
   summarise(growth = list(LS_Growth))

df_with_dct_C3_growth <- df_curves_C3_growth %>% 
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number())

df_with_dct_LS_growth <- df_curves_LS_growth %>% 
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number() + 100)

dct_C3_LS_growth <- rbind(df_with_dct_C3_growth, df_with_dct_LS_growth) %>% 
  arrange(Show_Name, Season_num, period)

dct_spread_C3_LS_growth <- dct_C3_LS_growth %>% 
  spread(key = period, value = dct_values)

write.csv(dct_C3_LS_growth, "dct_C3_LS_growth.csv")
write.csv(dct_spread_C3_LS_growth, "dct_spread_C3_LS_growth.csv")


#to cluster on differences 
df_curves_C3_LS_diff_growth <- df %>% 
  filter(!is.na(diff_C3_LS_growth)) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(growth = list(diff_C3_LS_growth))

df_with_dct_C3_LS_diff_growth <- df_curves_C3_LS_diff_growth %>%
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number()) 

df_final_diff <- df_with_dct_C3_LS_diff_growth %>% 
  spread(key = period, value = dct_values)

write.csv(df_with_dct_C3_LS_diff_growth, "dct_C3_LS_diff_growth.csv")
write.csv(df_final_diff, "dct_spread_C3_LS_diff_growth.csv")


#to cluster on averages
df_curves_C3_LS_avg_growth <- df %>% 
  filter(!is.na(avg_C3_LS_growth)) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(growth = list(avg_C3_LS_growth))

df_with_dct_C3_LS_avg_growth <- df_curves_C3_LS_avg_growth %>%
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number()) 

df_final_avg <- df_with_dct_C3_LS_avg_growth %>% 
  spread(key = period, value = dct_values)

write.csv(df_with_dct_C3_LS_avg_growth, "dct_C3_LS_avg_growth.csv")
write.csv(df_final_avg, "dct_spread_C3_LS_avg_growth.csv")



#S1-S2 CONTINUOUS

#appended
dct_cont_C3 <- df %>%
  group_by(Network, Show_Name) %>% 
  filter(Season_num == 1 | Season_num == 2) %>%
  filter(!is.na(C3_Growth)) %>% 
  summarise(growth = list(C3_Growth)) %>%
  ungroup() %>% 
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number()) 

dct_cont_LS <- df %>%
  group_by(Network, Show_Name) %>% 
  filter(Season_num == 1 | Season_num == 2) %>%
  filter(!is.na(LS_Growth)) %>%  
  summarise(growth = list(LS_Growth)) %>%
  ungroup() %>% 
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number() + 100) 

dct_cont_C3_LS <- rbind(dct_cont_C3, dct_cont_LS) %>% 
  arrange(Show_Name, period)

dct_spread_cont_C3_LS <- dct_cont_C3_LS %>% 
  spread(key = period, value = dct_values)

write.csv(dct_cont_C3_LS, "dct_C3_LS_growth_cont.csv")
write.csv(dct_spread_cont_C3_LS, "dct_spread_C3_LS_growth_cont.csv")

#differences
dct_cont_diff <- df %>%
  group_by(Network, Show_Name) %>% 
  filter(Season_num == 1 | Season_num == 2) %>%
  filter(!is.na(diff_C3_LS_growth)) %>%  
  summarise(growth = list(diff_C3_LS_growth)) %>%
  ungroup() %>% 
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number()) 

dct_spread_cont_diff <- dct_cont_diff %>% 
  spread(key = period, value = dct_values)

write.csv(dct_cont_diff, "dct_C3_LS_diff_growth_cont.csv")
write.csv(dct_spread_cont_diff, "dct_spread_C3_LS_diff_growth_cont.csv")

#averages
dct_cont_avg <- df %>%
  group_by(Network, Show_Name) %>% 
  filter(Season_num == 1 | Season_num == 2) %>%
  filter(!is.na(avg_C3_LS_growth)) %>%  
  summarise(growth = list(avg_C3_LS_growth)) %>%
  ungroup() %>% 
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number()) 

dct_spread_cont_avg <- dct_cont_avg %>% 
  spread(key = period, value = dct_values)

write.csv(dct_cont_avg, "dct_C3_LS_avg_growth_cont.csv")
write.csv(dct_spread_cont_avg, "dct_spread_C3_LS_avg_growth_cont.csv")