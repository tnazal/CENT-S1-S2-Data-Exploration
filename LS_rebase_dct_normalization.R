
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

df_LS_rebase_curves <- df %>%
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(LS_rebase = list(LS_Rebase))

dct <- function(x){
  get_dct_transform(
    x, 
    low_pass_size = 3, 
    x_reverse_len = 100,
    scale_vals = FALSE,
    scale_range = TRUE
  )
}

df_LS_rebase_with_dct <- df_LS_rebase_curves %>%
  mutate(dct_values = map(LS_rebase, dct)) %>% 
  select(-LS_rebase) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number()) 

df_LS_rebase_final <- df_LS_rebase_with_dct %>% 
  spread(key = period, value = dct_values)

write.csv(df_LS_rebase_with_dct, "CENT_LS_rebase_dct.csv")
write.csv(df_LS_rebase_final, "CENT_LS_rebase_dct_spread.csv")

# for matching with clusters on s1 & s2
dct_s1_s2 <- df_LS_rebase_curves %>% 
  mutate(dct_values = map(LS_rebase, dct)) %>% 
  select(-LS_rebase) %>% 
  unnest(dct_values) %>% 
  filter(Season_num == 1 | Season_num == 2) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number()) 

write.csv(dct_s1_s2, "CENT_LS_rebase_dct_s1_s2.csv")

# S1 & S2 continuous normalization
dct_cont <- df %>%
  filter(Season_num == 1 | Season_num == 2) %>% 
  group_by(Network, Show_Name) %>% 
  summarise(LS_rebase = list(LS_Rebase)) %>%
  ungroup() %>% 
  mutate(dct_values = map(LS_rebase, dct)) %>% 
  select(-LS_rebase) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number())  

dct_spread_cont <- dct_cont  %>% 
  spread(key = period, value = dct_values)

write.csv(dct_cont, "CENT_LS_rebase_dct_cont.csv")
write.csv(dct_spread_cont, "CENT_LS_rebase_dct_spread_cont.csv")

