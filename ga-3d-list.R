# Script to format data for machine learning algorithms
# Matt Kmiecik - Started 17 Nov 2019

# Loading libraries
library(eegkitdata)
library(tidyverse)

data(eegdata) # loads in data

# Subject-wise grand averages
ga_ss <- 
  eegdata %>%
  group_by(subject, channel, time) %>%
  summarise(m = mean(voltage)) %>%
  ungroup()

# Code for forming a list
subs  <- unique(ga_ss$subject)  # Subjects
chans <- unique(ga_ss$channel)  # Channels
times <- unique(ga_ss$time)     # Times

ga_array <- vector(mode = "list", length = length(subs)) # Initializes list

# Spreads df to have time along row and columns are channels
# subjects will be on third dimension
ga_ss_spread <- 
  ga_ss %>% 
  spread(channel, m) %>% 
  mutate(group = substr(subject, 4, 4))

# splits subjects along 3rd dimension
for(i in 1:length(subs)){
  
  this_sub <- subs[i] # channel selected for this loop
  this_slice <- ga_ss_spread %>% filter(subject %in% this_sub) %>% select(-subject)
  ga_array[[i]] <- this_slice
  
}

# Saves out data
save(ga_array, file = "ga-array.RData")