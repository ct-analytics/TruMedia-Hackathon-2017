library(dplyr)

df <- d %>% 
  filter(pitchType!="UN",
         pitchType!="PO",
         pitcher=="Jon Lester") %>%
  # filter(pitcher=="Jon Lester",
         # gameString=="gid_2016_10_25_chnmlb_clemlb_1") %>%
  select(pitcher,pitchID,pitchType,probCalledStrike,pitchResult,batterHand,umpireId,
         balls, strikes, outs, manOnFirst, manOnSecond, manOnThird, inning, timesFaced) %>%
  mutate(lastPitchType=lag(pitchType),
         lastprobCallStrike=lag(probCalledStrike),
         lastPitchResult = lag(pitchResult),
         lastBatterHand = lag(batterHand)) %>%
  # left_join(d %>%
  #             filter(pitchType!="UN",
  #                    pitchType!="PO",
  #                    pitcher=="Jon Lester") %>%
  #             select(pitcher,gameString,pitchID,pitchType,probCalledStrike) %>%
  #             arrange(pitcher,gameString,pitchID) %>%
  #             group_by(pitcher,pitchType) %>%
  #             mutate(lastPitchTypeProbCalledStrike = lag(probCalledStrike))
  #             ) %>%
  filter(pitchID>1) %>%
         # !is.na(lastPitchTypeProbCalledStrike)) %>%
  select(-pitchResult,-pitchID,-probCalledStrike,-pitcher,-gameString) 

df$pitchType <- factor(df$pitchType)
# df$pitcher <- factor(df$pitcher)
df$lastPitchType <- factor(df$lastPitchType)
df$lastPitchResult <- factor(df$lastPitchResult)
df$batterHand <- factor(df$batterHand)
df$lastBatterHand <- factor(df$lastBatterHand)
df$umpireId <- factor(df$umpireId)

#todo: add feature on last pitch of that type  

library(caret)

split.data <- createDataPartition(y=df$pitchType, 
                                  p = 0.6, 
                                  list=F)

df.train <- df[split.data,]
df.test <- df[-split.data,]
control <- trainControl(method="repeatedcv", 
                        number=10, 
                        repeats=3, 
                        savePredictions="final",
                        index=createResample(df.train$pitchType, 10),
                        classProbs=TRUE)

rf <- train(pitchType~., data=df, method="rf", trControl=control)
print(rf)
plot(rf)

nnet <- train(pitchType~., data=df, method="nnet", trControl=control)
print(nnet)

rf.test <- predict(rf, newdata = df.test)
confusionMatrix(rf)

confusionMatrix(rf.test)

library(caretEnsemble)

models <- caretList(pitchType~., 
                    data=df.train,
                    trControl=control,
                    methodList=c("rf", "nnet", "svmRadial")
                    )

# Doesn't work for multi class problems :()
modelEnsemble <- caretEnsemble(models)

results <- resamples(models)
summary(results)
modelCor(results)
dotplot(results)
splom(results)
models.preds <- as.data.frame(predict(models, newdata = df.test))

