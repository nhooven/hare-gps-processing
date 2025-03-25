# Project: WSU Snowshoe Hare and PCT Project
# Subproject: GPS processing
# Script: 03 - Add in season and treatment delineations
# Author: Nathan D. Hooven, Graduate Research Assistant
# Email: nathan.hooven@wsu.edu / nathan.d.hooven@gmail.com
# Date began: 21 Mar 2025
# Date completed: 21 Mar 2025
# Date last modified: 24 Mar 2025
# R version: 4.4.3

#_______________________________________________________________________
# 1. Load in packages ----
#_______________________________________________________________________

library(tidyverse)            # data cleaning and manipulation
library(lubridate)            # work with dates

#_______________________________________________________________________
# 2. Directories ----
#_______________________________________________________________________

dir.pre <- "E:/Hare project/Data analysis/GPS processing/Derived data/Cleaned data/PRE/"
dir.dur <- "E:/Hare project/Data analysis/GPS processing/Derived data/Cleaned data/DUR/"
dir.post <- "E:/Hare project/Data analysis/GPS processing/Derived data/Cleaned data/POST/"

# file lists
list.pre <- list.files(dir.pre)
list.dur <- list.files(dir.dur)
list.post <- list.files(dir.post)

#_______________________________________________________________________
# 3. Define seasonal cutoffs ----
#_______________________________________________________________________

# 2022-2023
# both high and low-elevation had the same dates
on.1.both <- c(as.Date(mdy("10-06-2022", tz = "America/Los_Angeles")),
               as.Date(mdy("04-30-2023", tz = "America/Los_Angeles")))

# 2023-2024
# same start date, different end date
on.2.hi <- c(as.Date(mdy("10-25-2023", tz = "America/Los_Angeles")),
             as.Date(mdy("04-24-2024", tz = "America/Los_Angeles")))

on.2.lo <- c(as.Date(mdy("10-25-2023", tz = "America/Los_Angeles")),
             as.Date(mdy("04-12-2024", tz = "America/Los_Angeles")))

# 2024-2025
# different start (and likely end) dates
on.3.hi <- c(as.Date(mdy("10-31-2024", tz = "America/Los_Angeles")),
             NA)

on.3.lo <- c(as.Date(mdy("11-02-2024", tz = "America/Los_Angeles")),
             NA)

#_______________________________________________________________________
# 4. Define treatment dates ----
#_______________________________________________________________________

thin.1A <- as.Date(mdy("10-12-2023", tz = "America/Los_Angeles"))
thin.1B <- as.Date(mdy("10-11-2023", tz = "America/Los_Angeles"))

thin.2A <- as.Date(mdy("10-07-2023", tz = "America/Los_Angeles"))
thin.2B <- as.Date(mdy("10-06-2023", tz = "America/Los_Angeles"))

thin.3A <- as.Date(mdy("10-04-2023", tz = "America/Los_Angeles"))
thin.3B <- as.Date(mdy("10-05-2023", tz = "America/Los_Angeles"))

thin.4A <- as.Date(mdy("10-10-2023", tz = "America/Los_Angeles"))
thin.4B <- as.Date(mdy("10-09-2023", tz = "America/Los_Angeles"))

#_______________________________________________________________________
# 5. Function to assign all relocations a season and treatment ----
#_______________________________________________________________________

assign_season <- function(file.list,
                          period) {
  
  # loop through
  for (i in 1:length(file.list)) {
    
    # extract correct name
    focal.file <- file.list[i]
    
    # which period
    if (period == "PRE") {
      
      focal.data <- read.csv(paste0(dir.pre, focal.file))
      
    } else {
      
      if (period == "DUR") {
        
        focal.data <- read.csv(paste0(dir.dur, focal.file))
        
      } else {
        
        focal.data <- read.csv(paste0(dir.post, focal.file))
        
      }
      
    }
    
    # convert numeric time to POSIXct
    focal.data.1 <- focal.data %>%
      
      # convert numeric time to POSIXct
      mutate(timestamp = as.POSIXct(timestamp,
                                    tz = "America/Los_Angeles"))
    
    # which cluster
    if (focal.data.1$site[1] %in% c("4A", "4B", "4C")) {
      
      focal.data.2 <- focal.data.1 %>%
        
      # season
      mutate(
        
        season = case_when(
        
        # snow-off
        timestamp < on.1.both[2] |
        (timestamp > on.1.both[2] & timestamp < on.2.lo[1]) |
        (timestamp > on.2.lo[2] & timestamp < on.3.lo[1]) ~ "off",
        
        # snow-on
        (timestamp >= on.1.both[1] & timestamp <= on.1.both[2]) |
        (timestamp >= on.2.lo[1] & timestamp <= on.2.lo[2]) |
        (timestamp >= on.3.lo[1]) ~ "on")
        
        )
      
    } else {
      
      focal.data.2 <- focal.data.1 %>%
        
        # season
        mutate(
          
          season = case_when(
          
            # snow-off
            timestamp < on.1.both[2] |
              (timestamp > on.1.both[2] & timestamp < on.2.hi[1]) |
              (timestamp > on.2.hi[2] & timestamp < on.3.hi[1]) ~ "off",
            
            # snow-on
            (timestamp >= on.1.both[1] & timestamp <= on.1.both[2]) |
              (timestamp >= on.2.hi[1] & timestamp <= on.2.hi[2]) |
              (timestamp >= on.3.hi[1]) ~ "on")
          
          )
      
    }
    
    # add in treatment
    focal.data.3 <- focal.data.2 %>%
      
      mutate(
        
        trt = case_when(
          
          site[1] %in% c("1C", "2C", "3C", "4C") ~ "unthinned",
          site[1] == "1A" & timestamp < thin.1A ~ "unthinned",
          site[1] == "1B" & timestamp < thin.1B ~ "unthinned",
          site[1] == "2A" & timestamp < thin.2A ~ "unthinned",
          site[1] == "2B" & timestamp < thin.2B ~ "unthinned",
          site[1] == "3A" & timestamp < thin.3A ~ "unthinned",
          site[1] == "3B" & timestamp < thin.3B ~ "unthinned",
          site[1] == "4A" & timestamp < thin.4A ~ "unthinned",
          site[1] == "4B" & timestamp < thin.4B ~ "unthinned",
          site[1] == "1A" & timestamp >= thin.1A ~ "retention",
          site[1] == "1B" & timestamp >= thin.1B ~ "piling",
          site[1] == "2A" & timestamp >= thin.2A ~ "piling",
          site[1] == "2B" & timestamp >= thin.2B ~ "retention",
          site[1] == "3A" & timestamp >= thin.3A ~ "piling",
          site[1] == "3B" & timestamp >= thin.3B ~ "retention",
          site[1] == "4A" & timestamp >= thin.4A ~ "retention",
          site[1] == "4B" & timestamp >= thin.4B ~ "piling")
          
      )
    
    # write to file
    if (period == "PRE") {
      
      write.csv(focal.data.3, 
                paste0("Derived data/Cleaned data 2/PRE/", focal.file))
      
    } else {
      
      if (period == "DUR") {
        
        write.csv(focal.data.3, 
                  paste0("Derived data/Cleaned data 2/DUR/", focal.file))
        
      } else {
        
        write.csv(focal.data.3, 
                  paste0("Derived data/Cleaned data 2/POST/", focal.file))
        
      }
      
    }
    
  }
  
}

#_______________________________________________________________________
# 6. Use function ----
#_______________________________________________________________________

assign_season(list.pre, "PRE")
assign_season(list.dur, "DUR")
assign_season(list.post, "POST")
