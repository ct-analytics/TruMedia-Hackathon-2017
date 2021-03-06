library(readr)
library(dplyr)
library(lubridate)
d <- rbind(read_csv("../../2016.csv") %>% filter(pitcher=="Jon Lester"),
           read_csv("../../2015.csv") %>% filter(pitcher=="Jon Lester"),
           read_csv("../../2014.csv") %>% filter(pitcher=="Jon Lester"))

# Get a glimpse of the data
glimpse(d)

# Create additional features
d <- d %>%
  mutate(battingTeam = ifelse(side=="T", visitor, home),
         date = date(ymd_hms(gameDate)))

# Create a pitchID for ordering pitches
d <- d %>% 
  group_by(gameString,pitcherId) %>%
  mutate(pitchID = row_number()) %>%
  ungroup()
