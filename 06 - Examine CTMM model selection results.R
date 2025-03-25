# Project: WSU Snowshoe Hare and PCT Project
# Subproject: GPS processing
# Script: 06 - Examine CTMM model selection results
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

#_______________________________________________________________________
# 2. Read in data ----
#_______________________________________________________________________

model.results <- read.csv("Raw data/GPS_data_03_24_2025.csv")

#_______________________________________________________________________
# 3. Clean data ----
#_______________________________________________________________________

model.results.1 <- model.results %>%
  
  # drop NAs
  drop_na(All.obs) %>%
  
  # transform to hours
  mutate(autocorr.pos = Tau.1 / 3600,
         autocorr.vel = Tau.2 / 3600)

#_______________________________________________________________________
# 4. Plots ----
#_______________________________________________________________________
# 4a. Tau 1 ----

# this is the positional autocorrelation timescale in hours

#_______________________________________________________________________

# all pooled
ggplot(data = model.results.1,
       aes(x = autocorr.pos)) +
  
  theme_bw() +
  
  geom_density(fill = "lightgray",
               linewidth = 0.85) +
  
  xlab("Positional autocorrelation timescale (h)") +
  
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

# by movement model type
ggplot(data = model.results.1,
       aes(x = autocorr.pos,
           color = Model,
           fill = Model)) +
  
  theme_bw() +
  
  geom_density(linewidth = 0.85,
               alpha = 0.25) +
  
  xlab("Positional autocorrelation timescale (h)") +
  
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.7, 0.7)) +
  
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  
  coord_cartesian(xlim = c(1.3, 15))

#_______________________________________________________________________
# 4b. Tau 2 ----

# this is the velocity autocorrelation timescale in hours

#_______________________________________________________________________

# all pooled
ggplot(data = model.results.1,
       aes(x = autocorr.vel)) +
  
  theme_bw() +
  
  geom_density(fill = "lightgray",
               linewidth = 0.85) +
  
  xlab("Velocity autocorrelation timescale (h)") +
  
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

# by movement model type
ggplot(data = model.results.1,
       aes(x = autocorr.vel,
           color = Model,
           fill = Model)) +
  
  theme_bw() +
  
  geom_density(linewidth = 0.85,
               alpha = 0.25) +
  
  xlab("Positional autocorrelation timescale (h)") +
  
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.7, 0.7)) +
  
  scale_color_viridis_d() +
  scale_fill_viridis_d()

#_______________________________________________________________________
# 4c. Sigma - bivariate density ----
#_______________________________________________________________________

# all pooled
ggplot(data = model.results.1,
       aes(x = Sigma.major,
           y = Sigma.minor)) +
  
  theme_bw() +
  
  geom_density_2d_filled() +
  
  xlab("Sigma (major axis)") +
  ylab("Sigma (minor axis)") +
  
  theme(panel.grid = element_blank(),
        legend.position = "none")

# by movement model type
ggplot(data = model.results.1,
       aes(x = Sigma.major,
           y = Sigma.minor)) +
  
  facet_wrap(~ Model,
             scales = "free") +
  
  theme_bw() +
  
  geom_density_2d_filled() +
  
  xlab("Sigma (major axis)") +
  ylab("Sigma (minor axis)") +
  
  theme(panel.grid = element_blank(),
        legend.position = "none")

#_______________________________________________________________________
# 4d. Sigma - scatterplot ----
#_______________________________________________________________________

# all pooled
ggplot(data = model.results.1,
       aes(x = Sigma.major,
           y = Sigma.minor)) +
  
  theme_bw() +
  
  geom_point() +
  
  xlab("Sigma (major axis)") +
  ylab("Sigma (minor axis)") +
  
  theme(panel.grid = element_blank(),
        legend.position = "none")

# by movement model type
ggplot(data = model.results.1,
       aes(x = Sigma.major,
           y = Sigma.minor)) +
  
  facet_wrap(~ Model) +
  
  theme_bw() +
  
  geom_point() +
  
  xlab("Sigma (major axis)") +
  ylab("Sigma (minor axis)") +
  
  theme(panel.grid = element_blank(),
        legend.position = "none")

#_______________________________________________________________________
# 4e. Correlation between sigma (major) and pos. autocorr ----

# are these two quantities related?

#_______________________________________________________________________

# all pooled
ggplot(data = model.results.1,
       aes(x = Sigma.major,
           y = autocorr.pos)) +
  
  theme_bw() +
  
  geom_smooth(method = "lm") +
  
  geom_point() +
  
  xlab("Sigma (major axis)") +
  ylab("Positional autocorrelation timescale") +
  
  theme(panel.grid = element_blank(),
        legend.position = "none")

# by model type
ggplot(data = model.results.1,
       aes(x = Sigma.major,
           y = autocorr.pos)) +
  
  facet_wrap(~ Model) +
  
  theme_bw() +
  
  geom_smooth(method = "lm") +
  
  geom_point() +
  
  xlab("Sigma (major axis)") +
  ylab("Positional autocorrelation timescale") +
  
  theme(panel.grid = element_blank(),
        legend.position = "none")

# seems independent

#_______________________________________________________________________
# 4f. Model type and data quality ----

# Does the amount of data correlate to the best fit model?

#_______________________________________________________________________

ggplot(data = model.results.1,
       aes(x = Rm.mort.retrieve,
           color = Model,
           fill = Model)) +
  
  theme_bw() +
  
  geom_density(alpha = 0.25,
               linewidth = 0.85) +
  
  xlab("Relocations") +
  
  theme(panel.grid = element_blank(),
        legend.position = c(0.75, 0.75)) +
  
  coord_cartesian(xlim = c(0, 1000)) +
  
  scale_color_viridis_d() +
  scale_fill_viridis_d()

#_______________________________________________________________________
# 5. Descriptive statistics ----
#_______________________________________________________________________

model.summary <- model.results.1 %>%
  
  group_by(Model) %>%
  
  summarize(n = n(),
            mean.tau1 = mean(autocorr.pos),
            sd.tau1 = sd(autocorr.pos),
            mean.tau2 = mean(autocorr.vel),
            sd.tau2 = sd(autocorr.vel))

model.summary
