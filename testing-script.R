# Loading libraries
library(eegkitdata)
library(tidyverse)

data(eegdata) # loads in data

# Data set information
# unique(eegdata$channel)
# unique(eegdata$condition)
# unique(eegdata$trial)

# Subject-wise grand averages
ga_ss <- 
  eegdata %>%
  group_by(subject, channel, time) %>%
  summarise(m = mean(voltage)) %>%
  ungroup()

# Subject-wise grand average plots
# ggplot(ga_ss, aes(time, m, group = subject)) +
#   geom_line() +
#   facet_wrap(~channel)

# grand average (all)
ga <- 
  ga_ss %>%
  group_by(time, channel) %>%
  summarise(M = mean(m), sd = sd(m), n = n(), sem = sd/sqrt(n)) %>%
  ungroup()

# Grand average plot
# ggplot(ga, aes(time, M, group = channel)) +
#   geom_line()

# grand average (groups)
ga_groups <- 
  ga_ss %>%
  mutate(group = substr(subject, 4, 4)) %>%
  group_by(group, channel, time) %>%
  summarise(M = mean(m), sd = sd(m), n = n(), sem = sd/sqrt(n)) %>%
  ungroup()

midline <- c("FPZ", "AFZ", "FZ", "FCZ", "CZ", "CPZ", "PZ", "POZ", "OZ")

ggplot(
  ga_groups %>% filter(channel %in% midline), 
  aes(time, M, group = group, color = group)
  ) +
  geom_line() +
  facet_wrap(~channel)

ga_groups_midline <-
  ga_groups %>%
  filter(channel %in% midline) %>%
  mutate(channel = fct_relevel(channel, midline), time_2 = time*(1000/256)) 

ggplot(
  ga_groups_midline, 
  aes(time_2, M, group = group, color = group)
  ) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  facet_wrap(~channel)

# Getting data ready for ML
head(eegdata)

channel_key <- 
  tibble(
    channel = unique(eegdata$channel), 
    chan_key = 1:length(unique(eegdata$channel))
    )

eegdata_ml <-
  eegdata %>%
  left_join(., channel_key, by = "channel") %>%
  mutate(
    subject = substr(subject, 5, 11),
    group = ifelse(group == "a", 1, 2) # alcoholic = 1, control = 2
    ) %>%
  select(-condition, -channel)

# 3d array
subs <- unique(ga_ss$subject)
chans <- unique(ga_ss$channel)
times <- unique(ga_ss$time)

ga_array <- vector(mode = "list", length = length(subs))

ga_ss_spread <- 
  ga_ss %>% 
  spread(channel, m) %>% 
  mutate(group = substr(subject, 4, 4))

for(i in 1:length(subs)){
  
  this_sub <- subs[i] # channel selected for this loop
  this_slice <- ga_ss_spread %>% filter(subject %in% this_sub) %>% select(-subject)
  ga_array[[i]] <- this_slice
  
}



