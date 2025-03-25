# Project: WSU Snowshoe Hare and PCT Project
# Subproject: GPS processing
# Script: 05 - Track metrics
# Author: Nathan D. Hooven, Graduate Research Assistant
# Email: nathan.hooven@wsu.edu / nathan.d.hooven@gmail.com
# Date began: 24 Mar 2025
# Date completed: 
# Date last modified: 24 Mar 2025
# R version: 4.4.3

#_______________________________________________________________________
# 1. Load in packages ----
#_______________________________________________________________________

library(tidyverse)            # data cleaning and manipulation
library(lubridate)
library(amt)                  # track processing

#_______________________________________________________________________
# 2. Directories ----
#_______________________________________________________________________

dir.pre <- "E:/Hare project/Data analysis/GPS processing/Derived data/Cleaned data 2/PRE/"
dir.dur <- "E:/Hare project/Data analysis/GPS processing/Derived data/Cleaned data 2/DUR/"
dir.post <- "E:/Hare project/Data analysis/GPS processing/Derived data/Cleaned data 2/POST/"

#_______________________________________________________________________
# 3. Define function ----
#_______________________________________________________________________

track_metrics <- function(directory) {
  
  
  # list files
  file.list <- list.files(directory)
  
  # loop through all files
  all.metrics <- data.frame()
  
  for (i in 1:length(file.list)) {
    
    # read file
    hare.data <- read.csv(paste0(directory,
                                 file.list[i]))
    
    # convert to track
    hare.track <- hare.data %>%
      
      # keep only columns we need
      dplyr::select(timestamp,
                    longitude,
                    latitude,
                    days.cap:trt) %>%
      
      # make sure that anything sampled at midnight is accounted for 
      mutate(timestamp = ifelse(nchar(timestamp) == 10,
                                paste0(timestamp, "00:00:00"),
                                timestamp)) %>%
      
      # correct time zone
      mutate(timestamp = ymd_hms(timestamp, 
                                 tz = "America/Los_Angeles")) %>%
      
      # make a track
      make_track(.x = longitude,
                 .y = latitude,
                 .t = timestamp,
                 crs = "EPSG:4326",
                 all_cols = TRUE) %>%
      
      # convert to UTM
      transform_coords(crs_to = "EPSG:32611")
    
    # calculate and save track metrics
      # date of first and last relocation
      from.to <- as.Date(from_to(hare.track))
      
      # total duration (in days)
      duration <- as.numeric(difftime(to(hare.track), 
                                      from(hare.track),
                                      units = "days"))
      
      # sampling rate
      samp.rate <- summarize_sampling_rate(hare.track)
      
      # straight line speeds (convert from m/s to km/day)
      track.speeds <- amt::speed(hare.track) * 86.4
      
      
      # pack into df
      focal.metrics <- data.frame(order = hare.track$order[1],
                                  first = from.to[1],
                                  last = from.to [2],
                                  duration = duration,
                                  rate.min = samp.rate[1],
                                  rate.med = samp.rate[3],
                                  rate.mean = samp.rate[4],
                                  rate.max = samp.rate[6],
                                  rate.sd = samp.rate[7],
                                  speed.med = median(track.speeds, na.rm = T),
                                  speed.mean = mean(track.speeds, na.rm = T),
                                  speed.sd = sd(track.speeds, na.rm = T))
      
      # bind in
      all.metrics <- rbind(all.metrics, focal.metrics)
    
  }
  
  # return
  return(all.metrics)
  
}

#_______________________________________________________________________
# 4. Use function ----
#_______________________________________________________________________

track.metrics.pre <- track_metrics(dir.pre)
track.metrics.dur <- track_metrics(dir.dur)
track.metrics.post <- track_metrics(dir.post)

#_______________________________________________________________________
# 5. Copy to paste ----
#_______________________________________________________________________

write.table(track.metrics.pre,
            "clipboard",
            sep = "\t")

write.table(track.metrics.dur,
            "clipboard",
            sep = "\t")

write.table(track.metrics.post,
            "clipboard",
            sep = "\t")
