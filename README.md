# CENT-S1-S2-Data-Exploration

# Context

A difficult task in forecasting shows season-over-season change is identifying the correct level of change from the first to the second season. We need to investigate seasonal dynamics to identify potential indicators that may narrow down the range of change rates between S1 and S2 for shows.

#Task

* Explore data for first airings of series in CENT networks (USA. SYFY, BRAVO, E!) to determine whether there are some patterns that would allow series to be grouped. Of particular interest would be episode-level patterns in Season 1 that could help reduce the range where S2 would trend towards. Suggested metrics to look at:
* raw C3 impressions
* growth ratios for impressions
* rebase to first episode of the season
* test whether any clustering algorithm helps early classification of series. Suggested algorithms to look into:
  * clustering (PCA, k-means)
  * time-series similarity
  * dynamic time-warping (see example here )

# Outputs

* initial visualizations of seasonal patterns (aggregate season averages / episode trends)
* initial outputs of clustering algorithms
* initial insights on viable clusters

# next steps
* Data and documentation
* Data for CENT networks:
  * BRAVO here
  * USA here
  * SYFY here
  * E! here
  
You can also consult some (outdated but informative) basic documentation of the data here

# A few things to keep in mind to filter the right information for this exercise:

`LF_Season == 1` selects only shows/seasons that correspond to seasons as defined for forecasting (drops specials, repeats, sports, anything else that's not needed)  
`Source == "Nielsen"` selects only historical data  
A few variables to keep an eye out for:  
`Show_Name` is a short version of the show, but allows you to group by a unique id  
`Season_num` and `Episode_num` give you the corrected season and episode numbers  
`SC3_Impressions` gives you the C3 impressions by half hour  
`SC3_Dur` gives you the duration in seconds of commercials for that half hour  
A quick snippet of code to correctly aggregate the data at the show level:  
```
df_showlevel <- ddply(subset(yourdata,  Source == "Nielsen" & LF_Season == 1),  
                     c("Network", "Broadcast_Year", "Cable_Qtr", "Date",  
                       "Show_Name", "Episode_num", "Season_num", "Start_Time", "End_time"),  
                     summarize,   
                     C3_Dur = sum(SC3_C_Dur),  
                     C3_Imps = sum(SC3_Impressions * SC3_C_Dur)/C3_Dur)                     
```
