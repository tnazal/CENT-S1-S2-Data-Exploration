
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
#with 3 or more episodes (otherwise DCT normalization on raw data will not work)
df <- df %>% group_by(Network, Show_Name) %>%
  filter(all(c(1, 2) %in% Season_num)) %>% 
  ungroup() %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  filter(max(Episode_num) >= 3) %>% 
  ungroup() %>% 
  group_by(Network, Show_Name) %>% 
  filter(all(c(1, 2) %in% Season_num))

#remove show with single episode in season starting at episode 6
df <- df[df$Show_Name != "RHOM", ]


#Discrete Cosine Transformation function
#see PDFs on GitHub for literature on DCT
dct <- function(x){
  get_dct_transform(
    x, 
    low_pass_size = 3, 
    x_reverse_len = 100,
    scale_vals = FALSE,
    scale_range = TRUE
  )
}


#create list of episode impressions for each show & season 
#to create DCT values from impressions on each episode
df_curves <- df %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  summarise(imps = list(C3_Impressions))

#map impressions as DCT values between -1 and 1 over 100 periods
df_with_dct <- df_curves %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  group_by(Network, Show_Name, Season_num) %>% 
  mutate(period = row_number()) 

#convert data to wide format
df_final <- df_with_dct %>% 
  spread(key = period, value = dct_values)

write.csv(df_with_dct, "dct_C3_raw.csv")
write.csv(df_final, "dct_spread_C3_raw.csv")


#S1 & S2 normalized separately and appended
dct_s1_s2 <- df_curves %>% 
  mutate(dct_values = map(imps, dct)) %>% 
  select(-imps) %>% 
  unnest(dct_values) %>% 
  filter(Season_num == 1 | Season_num == 2) %>% 
  group_by(Network, Show_Name) %>% 
  mutate(period = row_number())

write.csv(dct_s1_s2, "dct_C3_raw_s1_s2.csv")


#S1 to S2 continuously normalized together
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

dct_spread_cont <- dct_cont %>% 
  spread(key = period, value = dct_values)

write.csv(dct_cont, "dct_C3_raw_cont.csv")
write.csv(dct_spread_cont, "dct_spread_C3_raw_cont.csv")

