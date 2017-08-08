
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


df_curves <- df %>%
  filter(!is.na(LS_Growth)) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(LS_growth = list(LS_Growth))

df_with_dct <- df_curves %>%
  mutate(dct_values = map(LS_growth, dct)) %>% 
  select(-LS_growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number()) 

df_final <- df_with_dct %>% 
  spread(key = period, value = dct_values)

write.csv(df_with_dct, "dct_LS_growth.csv")
write.csv(df_final, "dct_spread_LS_growth.csv")


#S1 & S2 appended
dct_s1_s2 <- df_curves %>% 
  mutate(dct_values = map(LS_growth, dct)) %>% 
  select(-LS_growth) %>% 
  unnest(dct_values) %>% 
  filter(Season_num == 1 | Season_num == 2) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number()) 

write.csv(dct_s1_s2, "dct_LS_growth_s1_s2.csv")


#S1 & S2 continuous normalization
dct_cont <- df %>%
  filter(Season_num == 1 | Season_num == 2) %>%
  filter(!is.na(LS_Growth)) %>% 
  group_by(Network, Show_Name) %>% 
  summarise(LS_growth = list(LS_Growth)) %>%
  ungroup() %>% 
  mutate(dct_values = map(LS_growth, dct)) %>% 
  select(-LS_growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number())  

dct_spread_cont <- dct_cont  %>% 
  spread(key = period, value = dct_values)

write.csv(dct_cont, "dct_LS_growth_cont.csv")
write.csv(dct_spread_cont, "dct_spread_LS_growth_cont.csv")
