
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(syuzhet)
library(purrr)
library(tidyr)

# read data
df <- read.csv("CENT_showlevel.csv")
df <- select(df, -X)

# keep only shows that have both a first and second season in the data
# with 3 or more episodes
df <- df %>% group_by(Network, Show_Name) %>%
  filter(all(c(1, 2) %in% Season_num)) %>% 
  ungroup() %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  filter(max(Episode_num) >= 3) %>% 
  ungroup() %>% 
  group_by(Network, Show_Name) %>% 
  filter(all(c(1, 2) %in% Season_num))

df <- df[df$Show_Name != "RHOM", ]

df_curves <- df %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(imps = list(C3_Impressions))

dct <- function(x){
  get_dct_transform(
    x, 
    low_pass_size = 3, 
    x_reverse_len = 10,
    scale_vals = FALSE,
    scale_range = TRUE
  )
}

df_with_dct <- df_curves %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number()) 

df_final <- df_with_dct %>% 
  spread(key = period, value = dct_values)

write.csv(df_with_dct, "CENT_dct_10p.csv")
write.csv(df_final, "CENT_dct_10p_spread.csv")

# for matching with clusters on S1 & S2
dct_s1_s2 <- df_curves %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  filter(Season_num == 1 | Season_num == 2) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number())

write.csv(dct_s1_s2, "CENT_dct_10p_s1_s2.csv")

# S1 & S2 continuous normalization
dct_cont <- df %>%
  group_by(Network, Show_Name) %>% 
  filter(Season_num == 1 | Season_num == 2) %>% 
  summarise(imps = list(C3_Impressions)) %>%
  ungroup() %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number()) 

dct_spread_cont <- dct_cont  %>% 
  spread(key = period, value = dct_values)

write.csv(dct_cont, "CENT_dct_10p_cont.csv")
write.csv(dct_spread_cont, "CENT_dct_10p_spread_cont.csv")

