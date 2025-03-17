# Project: WSU Snowshoe Hare and PCT Project
# Subproject: GPS processing
# Script: 02 - Read and clean GPS data
# Author: Nathan D. Hooven, Graduate Research Assistant
# Email: nathan.hooven@wsu.edu / nathan.d.hooven@gmail.com
# Date began: 17 Mar 2025
# Date completed: 17 Mar 2025
# Date last modified: 17 Mar 2025
# R version: 4.4.3

#_______________________________________________________________________
# 0. Progress ----
#_______________________________________________________________________

# 17 Mar 2025 - PRE 055

#_______________________________________________________________________
# 1. Load in packages ----
#_______________________________________________________________________

library(tidyverse)            # data cleaning and manipulation
library(lubridate)            # work with dates
library(ctmm)                 # CTSP movement modeling
library(amt)                  # work with animal movement tracks
library(sf)                   # spatial operations
library(cowplot)              # multiple plots

#_______________________________________________________________________
# 2. Read in data ----
#_______________________________________________________________________

# error model
load("Derived data/error_model.RData")

# chosen thresholds (from 11 Nov 2024):
# speed (m/s): 1.5
# VAR.xy (circular variance): 23,047

# hare identifiers
id.group <- "PRE"
id.order <- "055"
id.site <- "3C"
id.sex <- "M"
id.indiv <- 510
id.deploy <- 1

# read in .csv 
hare.data <- read.csv(paste0("E:/Hare project/GPS data/alldata/",
                             id.group,
                             "/",
                             id.order,
                             "_",
                             id.indiv,
                             "_",
                             id.deploy,
                             ".csv"), 
                      sep = "",           # required to separate columns
                      fill = TRUE)        # required to separate columns

# examine
summary(hare.data)

#_______________________________________________________________________
# 3. Cleaning ----
#_______________________________________________________________________
# 3a. Rename columns and keep variables ----
#_______________________________________________________________________

if ("Timestamp" %in% names(hare.data)) {
  
  names(hare.data) <- c("TagID",
                        "Date",
                        "Time",
                        "X",
                        "Y",
                        "Z",
                        "Activity",
                        "location.lat",
                        "location.lon",
                        "height.msl",
                        "ground.speed",
                        "satellites",
                        "hdop",
                        "signal.strength",
                        "Battery",
                        "X.V.")
  
  # cleaning step
  hare.data.1 <- hare.data %>%
    
    # keep only lat-long entries
    mutate(location.lon = as.numeric(location.lon),
           location.lat = as.numeric(location.lat)) %>%
    
    # keep locations within reasonable window
    filter(location.lon > -121 & location.lon < -117 &
           location.lat > 47 & location.lat < 50) %>%
    
    # create timestamp and coerce to POSIXct
    mutate(timestamp = dmy_hms(paste0(Date, " ", Time),
                               tz = "UTC")) %>%
    
    # coerce to correct timezone
    mutate(timestamp = with_tz(timestamp,
                               tzone = "America/Los_Angeles")) %>%
    
    # keep columns we want
    dplyr::select(location.lon, 
                  location.lat,
                  timestamp,
                  height.msl,
                  satellites,
                  hdop) %>%
    
    # coerce required columns
    mutate_at(.vars = c("location.lon",
                        "location.lat",
                        "height.msl",
                        "hdop"),
              .funs = as.numeric) %>%
    
    # drop any NAs in the key columns
    drop_na(location.lon,
            location.lat,
            timestamp)

} else {
  
  # cleaning step
  hare.data.1 <- hare.data %>%
    
    # keep only lat-long entries
    mutate(location.lon = as.numeric(location.lon),
           location.lat = as.numeric(location.lat)) %>%
    
    # keep locations within reasonable window
    filter(location.lon > -121 & location.lon < -117 &
             location.lat > 47 & location.lat < 50) %>%
    
    # create timestamp and coerce to POSIXct
    mutate(timestamp = dmy_hms(paste0(Date, " ", Time),
                               tz = "UTC")) %>%
    
    # coerce to correct timezone
    mutate(timestamp = with_tz(timestamp,
                               tzone = "America/Los_Angeles")) %>%
    
    # keep columns we want
    dplyr::select(location.lon, 
                  location.lat,
                  timestamp,
                  height.msl,
                  satellites,
                  hdop) %>%
    
    # coerce required columns
    mutate_at(.vars = c("location.lon",
                        "location.lat",
                        "height.msl",
                        "hdop"),
              .funs = as.numeric) %>%
    
    # drop any NAs in the key columns
    drop_na(location.lon,
            location.lat,
            timestamp)
  
}

#_______________________________________________________________________
# 3b. Examine obviously erroneous relocations ----
#_______________________________________________________________________

# coordinates
ggplot(data = hare.data.1,
       aes(x = location.lon,
           y = location.lat,
           color = timestamp)) +
  
  theme_bw() +
  
  geom_point() +
  
  scale_color_datetime(low = "lightgreen",
                       high = "darkblue") +
  
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) -> coord.plot

# elevation and hdop
ggplot(data = hare.data.1,
       aes(x = hdop,
           y = height.msl,
           color = timestamp)) +
  
  theme_bw() +
  
  geom_point() +
  
  scale_color_datetime(low = "lightgreen",
                       high = "darkblue") +
  
  theme(legend.position = "top") -> elev.plot

# plot both
plot_grid(coord.plot, elev.plot)

#_______________________________________________________________________
# 3c. Drop obviously erroneous relocations ----
#_______________________________________________________________________

hare.data.2 <- hare.data.1 %>%
  
  filter(height.msl > 1525 &
           height.msl < 1650)

#_______________________________________________________________________
# 3d. Examine again ----
#_______________________________________________________________________

# coordinates
ggplot(data = hare.data.2,
       aes(x = location.lon,
           y = location.lat,
           color = timestamp)) +
  
  theme_bw() +
  
  geom_point() +
  
  scale_color_datetime(low = "lightgreen",
                       high = "darkblue") +
  
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) -> coord.plot.2

# elevation and hdop
ggplot(data = hare.data.2,
       aes(x = hdop,
           y = height.msl,
           color = timestamp)) +
  
  theme_bw() +
  
  geom_point() +
  
  scale_color_datetime(low = "lightgreen",
                       high = "darkblue") +
  
  theme(legend.position = "top") -> elev.plot.2

# plot both
plot_grid(coord.plot.2, elev.plot.2)

#_______________________________________________________________________
# 4. Create telemetry object ----
#_______________________________________________________________________

# convert to Movebank format
hare.movebank <- data.frame("timestamp" = hare.data.2$timestamp,
                            "location.lat" = hare.data.2$location.lat,
                            "location.long" = hare.data.2$location.lon,
                            "height above mean sea level" = hare.data.2$height.msl,
                            "GPS satellite count" = hare.data.2$satellites,
                            "GPS HDOP" = hare.data.2$hdop)

# convert to telemetry object
hare.telem <- as.telemetry(object = hare.movebank,
                           timeformat = "auto",
                           timezone = "America/Los_Angeles",
                           keep = TRUE)

# add a "class" variable for error model
hare.telem$class <- as.factor(ifelse(hare.telem$GPS.satellite.count > 3,
                                     "3D",
                                     "2D"))

# add in error model
uere(hare.telem) <- uere.HDOP.class

# plot 
plot(hare.telem)

#_______________________________________________________________________
# 5. Examine outliers and remove ----
#_______________________________________________________________________

hare.outliers <- outlie(hare.telem)

# speed and distance plot
ggplot(data = hare.outliers,
      aes(x = distance,
          y = speed,
          color = VAR.speed,
          size = VAR.speed)) +
  
  # white background
  theme_bw() +
  
  # points
  geom_point(alpha = 0.25) +
  
  # viridis color scale
  scale_color_viridis_c()

# remove outliers (if necessary)
out.rules <- hare.outliers$speed >= 0.04

hare.telem.1 <- hare.telem[!out.rules, ]

# if not
hare.telem.1 <- hare.telem

#_______________________________________________________________________
# 6. Classify all relocations based upon capture and mortality dates ----
#_______________________________________________________________________

# days since capture variable
# cap.date
cap.date <- as.Date(mdy("01-23-2024", tz = "America/Los_Angeles"))

hare.telem.1$days.cap <- as.numeric(as.Date(hare.telem.1$timestamp) - cap.date)

# remove relocations on:

  # 1) the same day as mortality
  # 2) the same day as the collar was retrieved in the afternoon

mort.date <- as.Date(mdy("02-22-2024", tz = "America/Los_Angeles"))

retr.date <- as.Date(mdy("01-31-2024", tz = "America/Los_Angeles"))

# if collar has a mort date, remove all relocations that day
if (is.na(mort.date) == FALSE) {
  
  hare.telem.2 <- hare.telem.1[which(hare.telem.1$timestamp < mort.date), ]
  
  # if collar was removed, discard any relocations after noon Pacific on retrieval day
} else {
  
  retr.cutoff <- ymd_hms(paste0(retr.date, 
                                " ",
                                "12:00:00"),
                         tz = "America/Los_Angeles")
  
  hare.telem.2 <- hare.telem.1[which(hare.telem.1$timestamp < retr.cutoff), ]
  
}

#_______________________________________________________________________
# 7. Add in identifier information and write to .csv  ----

# we'll just keep all columns to not mess up the ctmm procedure

#_______________________________________________________________________

# coerce telemetry object to data.frame
hare.telem.df <- as.data.frame(matrix(unlist(hare.telem.2@.Data),
                                      ncol = length(hare.telem.2@.Data),
                                      byrow = FALSE))

names(hare.telem.df) <- names(hare.telem.2)

hare.data.3 <- hare.telem.df %>%
  
  # add identifiers
  mutate(group = id.group,
         order = id.order,
         site = id.site,
         sex = id.sex,
         indiv = id.indiv,
         deploy = id.deploy)

# write to .csv
file.name <- paste0(id.order, "_", id.indiv, "_", id.deploy)

if (id.group == "PRE") {
  
  write.csv(hare.data.3, paste0("Derived data/Cleaned data/PRE/", file.name, ".csv"))
  
} else {
  
  if (id.group == "DUR") {
    
    write.csv(hare.data.3, paste0("Derived data/Cleaned data/DUR/", file.name, ".csv"))
    
  } else {
    
    write.csv(hare.data.3, paste0("Derived data/Cleaned data/POST/", file.name, ".csv"))
    
  }
  
}
