
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(syuzhet)
library(purrr)
library(tidyr)

# read data
df <- read.csv("CENT_showlevel.csv") %>% select(-X)

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

# remove show with single episode starting at episode 6
df <- df[df$Show_Name != "RHOM", ]


# transformation function
dct <- function(x){
  get_dct_transform(
    x, 
    low_pass_size = 3, 
    x_reverse_len = 100,
    scale_vals = FALSE,
    scale_range = TRUE
  )
}


LS_df_curves <- df %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(imps = list(LS_Impressions))

LS_df_with_dct <- LS_df_curves %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number()) 

LS_df_final <- LS_df_with_dct %>% 
  spread(key = period, value = dct_values)

write.csv(LS_df_with_dct, "CENT_LS_dct.csv")
write.csv(LS_df_final, "CENT_LS_dct_spread.csv")


# S1 & S2 appended
LS_dct_s1_s2 <- LS_df_curves %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  filter(Season_num == 1 | Season_num == 2) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number())

write.csv(LS_dct_s1_s2, "CENT_LS_dct_s1_s2.csv")


# S1 & S2 continuous normalization
LS_dct_cont <- df %>%
  group_by(Network, Show_Name) %>% 
  filter(Season_num == 1 | Season_num == 2) %>%  
  summarise(imps = list(LS_Impressions)) %>%
  ungroup() %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number()) 

LS_dct_spread_cont <- LS_dct_cont  %>% 
  spread(key = period, value = dct_values)

write.csv(LS_dct_cont, "CENT_LS_dct_cont.csv")
write.csv(LS_dct_spread_cont, "CENT_LS_dct_spread_cont.csv")

