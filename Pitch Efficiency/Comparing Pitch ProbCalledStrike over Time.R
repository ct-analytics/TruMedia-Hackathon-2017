library(dtw)
library(dplyr)
game.1 <- df %>% ungroup() %>%
  filter(gameString=="gid_2016_10_25_chnmlb_clemlb_1") %>%
  select(probCalledStrike)

game.2 <- df %>% ungroup() %>%
  filter(gameString=="gid_2016_10_30_clemlb_chnmlb_1") %>%
  select(probCalledStrike)

lester.dtw <- dtw(game.1,game.2)

plot(lester.dtw)
