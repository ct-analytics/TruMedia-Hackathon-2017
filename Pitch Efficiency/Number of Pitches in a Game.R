library(dplyr)
if (!exists("d")) source("read in data.R")

df <- d %>%
  group_by(gameString, date, pitcher, pitcherId, battingTeam) %>%
  summarize(numPitches=n(),
            inningStart=first(inning)) %>%
  filter(inningStart==1) %>%
  mutate(lastNumPitches=lag(numPitches,order_by=date)) 

# attach(df)

library(survival)
surv <- survfit(Surv(numPitches)~strata(battingTeam), data = df)
surv.summary <- summary(surv)

plot(surv, xlab="Number of Pitches", ylab="Survival Probability")

results <- do.call(data.frame,lapply(c(2:11), function(x) surv.summary[x]))

library(ggplot2)

ggplot(results %>% filter(strata=="strata(battingTeam)=BOS"),
       aes(x=time, y=surv)) +
  geom_line() +
  ylab("Survival Probability") +
  xlab("Number of Pitches") +
  ggtitle("Estimated Number of Pitches",subtitle="For Starting Pitchers Against the Boston Red Sox")
