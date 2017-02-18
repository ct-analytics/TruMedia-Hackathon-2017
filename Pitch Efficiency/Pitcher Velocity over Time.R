if (!exists("d")) source("read in data.R")

df <- d %>%
  filter(pitcher=="Jon Lester") %>%
  group_by(gameString,pitcherId,pitcher,pitchType) %>%
  mutate(pitchNumber=row_number())


library(ggplot2)

ggplot(data=df,aes(x=pitchNumber,y=releaseVelocity,color=pitchType)) + 
  geom_line()
