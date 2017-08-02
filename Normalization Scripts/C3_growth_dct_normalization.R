
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
  filter(max(Episode_num) >= 4) %>% 
  ungroup() %>% 
  group_by(Network, Show_Name) %>%
  filter(all(c(1, 2) %in% Season_num)) 

df <- df[df$Show_Name != "RHOM", ]

df_growth_curves <- df %>%
  filter(is.na(C3_Growth) == F) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(growth = list(C3_Growth))

dct <- function(x){
  get_dct_transform(
    x, 
    low_pass_size = 3, 
    x_reverse_len = 100,
    scale_vals = FALSE,
    scale_range = TRUE
  )
}

df_growth_with_dct <- df_growth_curves %>%
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number()) 

df_growth_final <- df_growth_with_dct %>% 
  spread(key = period, value = dct_values)

write.csv(df_growth_with_dct, "CENT_growth_dct.csv")
write.csv(df_growth_final, "CENT_growth_dct_spread.csv")

# for matching with clusters on s1 & s2
dct_s1_s2 <- df_growth_curves %>% 
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  filter(Season_num == 1 | Season_num == 2) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number()) 

write.csv(dct_s1_s2, "CENT_growth_dct_s1_s2.csv")

# S1 & S2 continuous normalization
dct_cont <- df %>%
  filter(Season_num == 1 | Season_num == 2) %>%
  filter(is.na(C3_Growth) == F) %>% 
  group_by(Network, Show_Name) %>% 
  summarise(growth = list(C3_Growth)) %>%
  ungroup() %>% 
  mutate(dct_values = map(growth, dct)) %>% 
  select(-growth) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number())  

dct_spread_cont <- dct_cont  %>% 
  spread(key = period, value = dct_values)

write.csv(dct_cont, "CENT_dct_cont_growth.csv")
write.csv(dct_spread_cont, "CENT_dct_spread_cont_growth.csv")
