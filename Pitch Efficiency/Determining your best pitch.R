library(dplyr)
library(ggplot2)

ggplot(data=d %>%
         filter(pitcher=="Jon Lester",
                gameString=="gid_2016_10_25_chnmlb_clemlb_1"),
       aes(x=pitchID,y=probCalledStrike,color=pitchType)) + 
  geom_point() +
  facet_grid(pitchType~.)

d %>%
  filter(pitcher=="Jon Lester") %>%
  group_by(pitcher, pitchType) %>%
  summarise(numPitches= n(),
            probCalledStrikeMean = mean(probCalledStrike),
            probCalledStrikeSd = sd(probCalledStrike)) %>%
  arrange(desc(probCalledStrikeMean))

ggplot(d %>%
         filter(pitcher=="Jon Lester"),
       aes(x=probCalledStrike)) +
  geom_histogram(binwidth = .05) +
  facet_grid(~pitchType)
