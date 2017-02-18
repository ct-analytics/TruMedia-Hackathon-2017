library(dplyr)

df <- d %>% 
  filter(pitchType!="UN",
         pitchType!="PO",
         pitchType!="AB") %>%
  dplyr::select(pitcher, pitchID, pitchType, probCalledStrike, pitchResult, batterHand, umpireId,
         balls, strikes, outs, manOnFirst, manOnSecond, manOnThird, inning, timesFaced) %>%
  mutate(lastPitchType=lag(pitchType),
         lastprobCallStrike=lag(probCalledStrike),
         lastPitchResult = lag(pitchResult),
         lastBatterHand = lag(batterHand)
         ) %>%
  filter(pitchID>1,
         !is.na(umpireId),
         !is.na(lastprobCallStrike)) %>%
  dplyr::select(-pitchResult,-pitchID,-probCalledStrike) %>%
  mutate_at(c("pitchType","pitcher","lastPitchType","lastPitchResult",
              "batterHand","lastBatterHand","umpireId","manOnFirst",
              "manOnSecond","manOnThird"),factor)

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

library(doMC)
registerDoMC(cores = 3)
# model <- train(pitchType~., data=df.train, method="nnet", trControl=control)
# model <- train(pitchType~., data=df.train, method="rf", trControl=control)
# model <- train(pitchType~., data=df.train, method="nb", trControl=control)
# model <- train(pitchType~., data=df.train, method="rpart", trControl=control)
# model <- train(pitchType~., data=df.train, method="knn", trControl=control)
# model <- train(pitchType~., data=df.train, method="ranger", trControl=control)
# model <- train(pitchType~., data=df.train, method="gbm", trControl=control)
# model <- train(pitchType~., data=df.train, method="dnn", trControl=control)
model <- train(pitchType~., data=df.train, method="multinom", trControl=control)
print(model)

confusionMatrix(model)

# nnet <- train(pitchType~., data=df, method="nnet", trControl=control)
# print(nnet)

model.test <- predict(model, newdata = df.test,type="prob")
model.test$pred <- predict(model, newdata = df.test)
model.test$obs <- df.test$pitchType
confusionMatrix(model.test$pred,model.test$obs)

library(caretEnsemble)

models <- caretList(pitchType~., 
                    data=df.train,
                    trControl=control,
                    methodList=c("rf", "nnet", "nb", "rpart", "knn", "ranger", "gbm", "dnn", "multinom")
                    )

# Doesn't work for multi class problems :()
modelEnsemble <- caretEnsemble(models)
modelStack <- caretStack(models)

results <- resamples(models)
summary(results)
modelCor(results)
dotplot(results)
splom(results)
models.preds <- as.data.frame(predict(models, newdata = df.test, verbose = T))

