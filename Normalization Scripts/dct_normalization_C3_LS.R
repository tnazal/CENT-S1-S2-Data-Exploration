
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


# get differences between and averages for C3 & LS
df$diff_C3_LS <- df$C3_Impressions - df$LS_Impressions
df$avg_C3_LS <- (df$C3_Impressions + df$LS_Impressions) / 2


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


# to cluster C3 & LS appended
C3_curves <- df %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(imps = list(C3_Impressions))

LS_curves <- df %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(imps = list(LS_Impressions))

dct_C3 <- C3_curves %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number())

dct_LS <- LS_curves %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number() + 100)

dct_C3_LS <- rbind(dct_C3, dct_LS) %>% 
  arrange(Show_Name, Season_num, period)

dct_spread_C3_LS <- dct_C3_LS %>% 
  spread(key = period, value = dct_values)

write.csv(dct_C3_LS, "CENT_dct_C3_LS.csv")
write.csv(dct_spread_C3_LS, "CENT_dct_spread_C3_LS.csv")


# to cluster on differences
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

write.csv(dct_C3_LS_diff, "CENT_dct_C3_LS_diff.csv")
write.csv(dct_spread_C3_LS_diff, "CENT_dct_spread_C3_LS_diff.csv")


# to cluster on averages
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

write.csv(dct_C3_LS_avg, "CENT_dct_C3_LS_avg.csv")
write.csv(dct_spread_C3_LS_avg, "CENT_dct_spread_C3_LS_avg.csv")
