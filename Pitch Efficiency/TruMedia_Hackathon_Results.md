# TruMedia Hackathon 2017: Predicting Pitch Types
Christopher Teixeira, chris@feasibleanalytics.com  

This document represents the work of Christopher Teixeira for the 2017 TruMedia Hackathon. 

# Understanding pitches 

For illustrative purposes, I wanted to understand this new field _probCalledStrike_ to see how well it might be used to determine a pitcher's best pitch. I'm doing this work on a macbook air, with limited access to other more powerful resources. To that end, I'll read in three years worth of data and stick to analyzing Jon Lester's set of pitches in that time frame. The raw data had a lot of data, but I found two variables of interest that I wanted to add:

1. battingTeam: the team facing the pitcher
2. pitchID: an identifier that would allow me to order pitches in a game


```r
library(doMC)
registerDoMC(cores = 3)

library(readr)
library(dplyr)
library(lubridate)
d <- rbind(read_csv("../../2016.csv") %>% filter(pitcher=="Jon Lester"),
           read_csv("../../2015.csv") %>% filter(pitcher=="Jon Lester"),
           read_csv("../../2014.csv") %>% filter(pitcher=="Jon Lester"))

# Get a glimpse of the data
glimpse(d)
```

```
## Observations: 10,685
## Variables: 58
## $ seasonYear              <int> 2016, 2016, 2016, 2016, 2016, 2016, 20...
## $ gameString              <chr> "gid_2016_04_05_chnmlb_anamlb_1", "gid...
## $ gameDate                <dttm> 2016-04-05 22:05:00, 2016-04-05 22:05...
## $ gameType                <chr> "REG", "REG", "REG", "REG", "REG", "RE...
## $ visitor                 <chr> "CHC", "CHC", "CHC", "CHC", "CHC", "CH...
## $ home                    <chr> "LAA", "LAA", "LAA", "LAA", "LAA", "LA...
## $ visitingTeamFinalRuns   <int> 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,...
## $ homeTeamFinalRuns       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ inning                  <int> 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,...
## $ side                    <chr> "B", "B", "B", "B", "B", "B", "B", "B"...
## $ batterId                <int> 488862, 488862, 488862, 502226, 502226...
## $ batter                  <chr> "Yunel Escobar", "Yunel Escobar", "Yun...
## $ batterHand              <chr> "R", "R", "R", "R", "R", "R", "R", "R"...
## $ pitcherId               <int> 452657, 452657, 452657, 452657, 452657...
## $ pitcher                 <chr> "Jon Lester", "Jon Lester", "Jon Leste...
## $ pitcherHand             <chr> "L", "L", "L", "L", "L", "L", "L", "L"...
## $ catcherId               <int> 424325, 424325, 424325, 424325, 424325...
## $ catcher                 <chr> "David Ross", "David Ross", "David Ros...
## $ umpireId                <int> 427220, 427220, 427220, 427220, 427220...
## $ umpire                  <chr> "Angel Hernandez", "Angel Hernandez", ...
## $ timesFaced              <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ batterPosition          <chr> "3B", "3B", "3B", "LF", "LF", "CF", "D...
## $ balls                   <int> 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1,...
## $ strikes                 <int> 0, 0, 1, 0, 1, 0, 0, 1, 2, 2, 2, 0, 0,...
## $ outs                    <int> 0, 0, 0, 1, 1, 2, 0, 0, 0, 0, 0, 1, 1,...
## $ manOnFirst              <chr> "false", "false", "false", "false", "f...
## $ manOnSecond             <chr> "false", "false", "false", "false", "f...
## $ manOnThird              <chr> "false", "false", "false", "false", "f...
## $ endManOnFirst           <chr> "false", "false", "false", "false", "f...
## $ endManOnSecond          <chr> "false", "false", "false", "false", "f...
## $ endManOnThird           <chr> "false", "false", "false", "false", "f...
## $ visitingTeamCurrentRuns <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ homeTeamCurrentRuns     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ pitchResult             <chr> "B", "F", "IP", "SL", "IP", "IP", "SL"...
## $ pitchType               <chr> "SI", "FF", "SI", "FF", "SI", "FF", "F...
## $ releaseVelocity         <dbl> 92.3, 90.3, 92.2, 93.3, 93.3, 94.0, 92...
## $ spinRate                <dbl> 2069.06, 2504.76, 2041.99, NA, 2040.51...
## $ spinDir                 <dbl> 117.503, 192.522, 107.649, NA, 125.498...
## $ px                      <dbl> 1.060, -0.434, 0.607, 0.782, 0.666, 0....
## $ pz                      <dbl> 2.052, 2.105, 2.429, 2.401, 1.979, 2.1...
## $ szt                     <dbl> 3.45, 3.40, 3.40, 3.46, 3.46, 3.57, 3....
## $ szb                     <dbl> 1.57, 1.57, 1.57, 1.55, 1.55, 1.64, 1....
## $ x0                      <dbl> 2.764, 2.616, 2.743, 2.680, 2.843, 2.6...
## $ y0                      <int> 50, 50, 50, 50, 50, 50, 50, 50, 50, 50...
## $ z0                      <dbl> 5.502, 5.568, 5.515, 5.488, 5.325, 5.4...
## $ vx0                     <dbl> -8.074, -8.156, -9.421, -6.830, -9.111...
## $ vy0                     <dbl> -135.059, -132.054, -134.809, -136.584...
## $ vz0                     <dbl> -5.187, -5.095, -3.495, -6.025, -5.436...
## $ ax                      <dbl> 18.833, 0.975, 19.863, 9.268, 17.490, ...
## $ ay                      <dbl> 28.893, 26.722, 27.608, 31.333, 29.568...
## $ az                      <dbl> -21.450, -20.658, -25.303, -12.303, -1...
## $ probCalledStrike        <dbl> 0.269, 0.987, 0.969, 0.841, 0.884, 0.9...
## $ paResult                <chr> NA, NA, "IP_OUT", NA, "IP_OUT", "IP_OU...
## $ runsHome                <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
## $ battedBallType          <chr> NA, NA, "GB", NA, "GB", "LD", NA, NA, ...
## $ battedBallAngle         <dbl> NA, NA, 14.79, NA, -23.97, 38.36, NA, ...
## $ battedBallDistance      <dbl> NA, NA, 117.06, NA, 117.37, 227.83, NA...
## $ atbatDesc               <chr> NA, NA, "Yunel Escobar grounds out, se...
```

```r
# Create additional features
d <- d %>%
  mutate(battingTeam = ifelse(side=="T", visitor, home),
         date = date(ymd_hms(gameDate)))

# Create a pitchID for ordering pitches
d <- d %>% 
  group_by(gameString,pitcherId) %>%
  mutate(pitchID = row_number()) %>%
  ungroup()
```

# A pitcher's best pitch


```r
bp <- d %>%
  filter(pitcher=="Jon Lester",
         pitchType!="PO",
         pitchType!="UN") %>%
  group_by(pitcher, pitchType) %>%
  summarise(numPitches= n(),
            probCalledStrikeMean = mean(probCalledStrike),
            probCalledStrikeSd = sd(probCalledStrike)) %>%
  arrange(desc(probCalledStrikeMean))

knitr::kable(bp)
```



pitcher      pitchType    numPitches   probCalledStrikeMean   probCalledStrikeSd
-----------  ----------  -----------  ---------------------  -------------------
Jon Lester   FC                 2377              0.4584699            0.4214171
Jon Lester   SI                 1268              0.4451964            0.4086514
Jon Lester   CU                 1565              0.3200946            0.4040301
Jon Lester   CH                  425              0.3144729            0.3961353
Jon Lester   FF                 5025                     NA                   NA

```r
library(ggplot2)
ggplot(data=d %>% filter(pitcher=="Jon Lester",pitchType!="PO",pitchType!="UN"),
       aes(x=pitchType,y=probCalledStrike)) +
  geom_boxplot() +
  geom_jitter(width = 0.2)
```

![](TruMedia_Hackathon_Results_files/figure-html/bestPitch-1.png)<!-- -->

We can see that for Jon Lester, he throws the cutter the most, and gets a higher average _probCalledStrike_. This matches up against [Brooks Baseball's Jon Lester card](http://www.brooksbaseball.net/landing.php?player=452657) which provides a nice validation for this metric. However, the boxplot shows that the median is actually higher for the fourseam fastball. 

My next question was, "could you use _probCalledStrike_ as a way to determine whether he will throw that type of pitch again?" This gets into a bit of a sophisticated solution. Using previous pitch information, we can try and see if it helps to determine whether he feels confident in throwing that pitch. 

# Feature engineering

In starting to look at this, I'm taking a very few amount of features. Here's a quick description of the features and their calculations.


```r
library(dplyr)

df <- d %>% 
  filter(pitchType!="UN",
         pitchType!="PO",
         pitchType!="AB") %>%
  dplyr::select(seasonYear,pitchID, pitchType, probCalledStrike, pitchResult, batterHand,
         balls, strikes, outs, manOnFirst, manOnSecond, manOnThird, inning, timesFaced) %>%
  mutate(lastPitchType=lag(pitchType),
         lastprobCallStrike=lag(probCalledStrike),
         lastPitchResult = lag(pitchResult),
         lastBatterHand = lag(batterHand)
         ) %>%
  filter(pitchID>1,
         !is.na(lastprobCallStrike)) %>%
  dplyr::select(-pitchResult,-pitchID,-probCalledStrike) %>%
  mutate_at(c("pitchType","lastPitchType","lastPitchResult",
              "batterHand","lastBatterHand","manOnFirst",
              "manOnSecond","manOnThird","seasonYear","balls",
              "strikes","outs"),factor) 

df$noise <- runif(nrow(df),0,1)

knitr::kable(data.frame(Features=names(df),
                        Description=c("Year the pitch took place",
                                      "The pitch type being predicted",
                                      "The batter's hand",
                                      "The number of balls for the at bat before the pitch",
                                      "The number of strikes for the at bat before the pitch",
                                      "The number of outs before the pitch",
                                      "Boolean for a runner on first",
                                      "Boolean for a runner on second",
                                      "Boolean for a runner on third",
                                      "Inning",
                                      "Number of times batter faced this pitcher within this game",
                                      "The last pitch type thrown",
                                      "The last pitch's probability of called strike",
                                      "The last pitch result",
                                      "The batter's hand against the last pitch",
                                      "Random noise")))
```



Features             Description                                                
-------------------  -----------------------------------------------------------
seasonYear           Year the pitch took place                                  
pitchType            The pitch type being predicted                             
batterHand           The batter's hand                                          
balls                The number of balls for the at bat before the pitch        
strikes              The number of strikes for the at bat before the pitch      
outs                 The number of outs before the pitch                        
manOnFirst           Boolean for a runner on first                              
manOnSecond          Boolean for a runner on second                             
manOnThird           Boolean for a runner on third                              
inning               Inning                                                     
timesFaced           Number of times batter faced this pitcher within this game 
lastPitchType        The last pitch type thrown                                 
lastprobCallStrike   The last pitch's probability of called strike              
lastPitchResult      The last pitch result                                      
lastBatterHand       The batter's hand against the last pitch                   
noise                Random noise                                               

# Building a model

Now that we have a set of features to work with, let's get into modeling. For this exercise, I chose a multinomial logistic regression model. I chose to use the [caret](http://topepo.github.io/caret/index.html) library for the ability to switch models later on just in case it is needed. 

First up, let's split the data into train and test data sets. This will allow us better control to assess the performance of the model. I'll also take the time to create the train control that will be used in the train function. 


```r
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
```

Now let's actually train the model and print out the some information on its performance. As you can see in the results below, this isn't exactly a great model. The first print function gives information on the model. It describes the classes we're predicting and the accuracy for different tuning attempts. 

We then print out the coefficients to see how they might be different across the pitch types and the various inputs to describe the pitching situation. In theory, these estimates would indicate whether each variable has a positive or negative influence on the type of pitch to be thrown.

The confusion matrix gives us an idea on where the predictions might be failing. In other algorithms, the confusion matrix shows that those algorithms only choose the fourseam fastball while maintaining similar accuracy. In this particular effort, we can see how one pitch might be mistaken for another. 

Finally, let's print out the variable importance to see exactly where _probCalledStrike_ fell in the list of variables. I include a noise variable in order to measure which variables are better than random noise. As it turns out most variables are, but the _probCalledStrike_ strike for the previous pitch didn't have a large impact on the final result.


```r
model <- train(pitchType~., data=df.train, method="multinom", trControl=control, verbose=F)
```

```
## Loading required package: nnet
```

```
## # weights:  160 (124 variable)
## initial  value 10195.789175 
## iter  10 value 8611.298975
## iter  20 value 8361.971170
## iter  30 value 8127.472836
## iter  40 value 8064.170870
## iter  50 value 8041.784992
## iter  60 value 8036.749165
## iter  70 value 8034.120077
## iter  80 value 8032.800858
## iter  90 value 8032.082761
## iter 100 value 8031.983115
## final  value 8031.983115 
## stopped after 100 iterations
```

```r
print(model)
```

```
## Penalized Multinomial Regression 
## 
## 6335 samples
##   15 predictor
##    5 classes: 'CH', 'CU', 'FC', 'FF', 'SI' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 3 times) 
## Summary of sample sizes: 6335, 6335, 6335, 6335, 6335, 6335, ... 
## Resampling results across tuning parameters:
## 
##   decay  Accuracy   Kappa     
##   0e+00  0.4663982  0.07066388
##   1e-04  0.4663982  0.07066388
##   1e-01  0.4666997  0.07049878
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was decay = 0.1.
```

```r
summary(model)
```

```
## Call:
## multinom(formula = .outcome ~ ., data = dat, decay = param$decay, 
##     verbose = ..1)
## 
## Coefficients:
##    (Intercept) seasonYear2015 seasonYear2016 batterHandR      balls1
## CU    1.296773     -0.7135083     -0.8014459   -1.136575 -0.11273011
## FC    3.225152     -0.8248108     -1.0301182   -2.030207  0.02160746
## FF    4.535374     -0.6298446     -0.3276806   -1.557145 -0.01409377
## SI    3.201216     -0.5352801     -0.1552216   -1.311984  0.05085453
##        balls2     balls3    strikes1   strikes2        outs1      outs2
## CU -0.4205782 -1.0820315 -0.08116832  1.0242800  0.054380957 -0.1018166
## FC  0.2598481  0.9737080  0.18291571  0.3974565 -0.129867708 -0.4277661
## FF  0.4105134  1.0785906 -0.64614404 -0.6438565 -0.141378621 -0.6022626
## SI  0.3842730  0.8627385 -0.53531972 -1.1318137 -0.004537398 -0.4789314
##    manOnFirsttrue manOnSecondtrue manOnThirdtrue     inning timesFaced
## CU      0.5387534       0.9533738     0.23466816 0.14658144 -0.4176445
## FC      0.7441631       1.0274568     0.51367972 0.13224617 -0.5311008
## FF      0.9428205       0.8235630     0.19215950 0.07091614 -0.6438904
## SI      1.1493030       0.5125843     0.02525606 0.07231911 -0.4357077
##    lastPitchTypeCU lastPitchTypeFC lastPitchTypeFF lastPitchTypeSI
## CU       0.4355868       0.9632397       0.7107673       0.6271696
## FC       0.4395288       0.5398632       0.3937799       0.4016352
## FF       0.3399400       0.4312791       0.2789197       0.1276010
## SI       0.1745919       0.2158255      -0.1553934       0.2442378
##    lastprobCallStrike lastPitchResultBID lastPitchResultF
## CU         0.10579613          0.3964467        0.3921844
## FC        -0.11052168         -0.4957858        0.1886392
## FF        -0.14497334         -0.2889971        0.5975800
## SI        -0.01916037         -0.6658150        0.3289603
##    lastPitchResultFT lastPitchResultHBP lastPitchResultIP
## CU         0.2149000          1.1814334         0.8131514
## FC        -0.2893520          1.0479296         0.4042014
## FF         0.2307791          0.1691733         0.6119929
## SI        -0.1178226         -1.6292771         0.1703695
##    lastPitchResultMB lastPitchResultSL lastPitchResultSS lastBatterHandR
## CU        -0.3323003        0.16296659         0.6903547      0.43500051
## FC         0.7365004       -0.17285860         0.4232164      0.53786382
## FF         0.7238262        0.05137188         1.0114818      0.28929014
## SI         0.3727095       -0.35912901         0.3029239      0.03254488
##          noise
## CU  0.02950930
## FC  0.13210802
## FF  0.15567208
## SI -0.03225021
## 
## Std. Errors:
##    (Intercept) seasonYear2015 seasonYear2016 batterHandR    balls1
## CU   0.5526809      0.1854576      0.1884617   0.3353625 0.2220321
## FC   0.5254466      0.1796982      0.1829744   0.3294666 0.2183871
## FF   0.5022950      0.1750663      0.1761127   0.3185281 0.2105911
## SI   0.5497395      0.1933770      0.1924058   0.3341228 0.2365162
##       balls2    balls3  strikes1  strikes2     outs1     outs2
## CU 0.2697345 0.4171217 0.2263521 0.2519972 0.1831830 0.1798019
## FC 0.2599103 0.3701585 0.2102525 0.2415030 0.1761894 0.1732458
## FF 0.2512620 0.3631036 0.1992740 0.2315711 0.1691007 0.1664069
## SI 0.2802771 0.3989180 0.2168025 0.2612420 0.1853651 0.1851462
##    manOnFirsttrue manOnSecondtrue manOnThirdtrue     inning timesFaced
## CU      0.2163976       0.2649742      0.3229064 0.06877406  0.1616057
## FC      0.2100775       0.2589681      0.3097696 0.06672844  0.1568794
## FF      0.2041175       0.2544986      0.3037385 0.06442116  0.1515028
## SI      0.2154573       0.2738821      0.3320016 0.07079532  0.1665193
##    lastPitchTypeCU lastPitchTypeFC lastPitchTypeFF lastPitchTypeSI
## CU       0.3546688       0.3528316       0.3268418       0.3713512
## FC       0.3310093       0.3321588       0.3043393       0.3470660
## FF       0.3090740       0.3109451       0.2821376       0.3241933
## SI       0.3441782       0.3441792       0.3147790       0.3550703
##    lastprobCallStrike lastPitchResultBID lastPitchResultF
## CU          0.2622774          0.6962150        0.3050585
## FC          0.2575052          0.6799385        0.2956716
## FF          0.2466949          0.6584085        0.2865849
## SI          0.2727190          0.7227722        0.3200958
##    lastPitchResultFT lastPitchResultHBP lastPitchResultIP
## CU         0.8311834           2.648996         0.3790299
## FC         0.8369253           2.607403         0.3696320
## FF         0.7841232           2.574804         0.3518665
## SI         0.8837320           3.083454         0.3833092
##    lastPitchResultMB lastPitchResultSL lastPitchResultSS lastBatterHandR
## CU          2.098565         0.3130967         0.3338067       0.2941138
## FC          1.937349         0.3027841         0.3257614       0.2902742
## FF          1.920129         0.2915970         0.3132639       0.2756486
## SI          2.143224         0.3273245         0.3481055       0.2922142
##        noise
## CU 0.2505485
## FC 0.2420285
## FF 0.2325111
## SI 0.2568414
## 
## Residual Deviance: 16063.97 
## AIC: 16311.97
```

```r
confusionMatrix(model)
```

```
## Cross-Validated (10 fold, repeated 3 times) Confusion Matrix 
## 
## (entries are percentual average cell counts across resamples)
##  
##           Reference
## Prediction   CH   CU   FC   FF   SI
##         CH  0.0  0.0  0.0  0.0  0.0
##         CU  0.3  2.1  1.5  2.1  0.3
##         FC  0.3  1.5  2.9  3.1  0.9
##         FF  3.3 11.2 18.0 41.6 10.6
##         SI  0.0  0.0  0.0  0.1  0.0
##                             
##  Accuracy (average) : 0.4667
```

```r
varImp(model)
```

```
## multinom variable importance
## 
##   only 20 most important variables shown (out of 30)
## 
##                    Overall
## batterHandR         100.00
## lastPitchResultHBP   65.59
## balls3               65.07
## manOnFirsttrue       54.41
## manOnSecondtrue      53.42
## strikes2             51.37
## seasonYear2015       42.90
## lastPitchResultSS    38.18
## seasonYear2016       36.24
## lastPitchResultMB    33.68
## lastPitchTypeFC      33.43
## timesFaced           31.34
## lastPitchResultIP    30.85
## lastPitchResultBID   28.23
## outs2                24.18
## lastPitchTypeFF      22.95
## lastPitchResultF     22.41
## balls2               21.86
## strikes1             21.35
## lastPitchTypeSI      20.58
```

```r
plot(varImp(model))
```

![](TruMedia_Hackathon_Results_files/figure-html/model-1.png)<!-- -->

Given the original model wasn't great, I was hesitant to take this further. However, let's use the test dataset to see how well this model performed. The confusion matrix indicates a similar performance to the model, which is somewhat good and somewhat bad. The model performs poorly, but consistently poorly. 


```r
model.test <- predict(model, newdata = df.test,type="prob")
model.test$pred <- predict(model, newdata = df.test)
model.test$obs <- df.test$pitchType
confusionMatrix(model.test$pred,model.test$obs)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   CH   CU   FC   FF   SI
##         CH    0    1    0    0    1
##         CU   11   86   52   99    9
##         FC    9   72  103  111   34
##         FF  150  466  793 1763  458
##         SI    0    0    1    0    1
## 
## Overall Statistics
##                                          
##                Accuracy : 0.4628         
##                  95% CI : (0.4477, 0.478)
##     No Information Rate : 0.4675         
##     P-Value [Acc > NIR] : 0.7364         
##                                          
##                   Kappa : 0.0595         
##  Mcnemar's Test P-Value : <2e-16         
## 
## Statistics by Class:
## 
##                      Class: CH Class: CU Class: FC Class: FF Class: SI
## Sensitivity          0.0000000   0.13760   0.10854    0.8936 0.0019881
## Specificity          0.9995062   0.95243   0.93091    0.1691 0.9997310
## Pos Pred Value       0.0000000   0.33463   0.31307    0.4857 0.5000000
## Neg Pred Value       0.9596965   0.86399   0.78258    0.6441 0.8809862
## Prevalence           0.0402844   0.14810   0.22488    0.4675 0.1191943
## Detection Rate       0.0000000   0.02038   0.02441    0.4178 0.0002370
## Detection Prevalence 0.0004739   0.06090   0.07796    0.8602 0.0004739
## Balanced Accuracy    0.4997531   0.54502   0.51972    0.5313 0.5008595
```

# Building a better model

My next thought was, why not try other algorithms. We can use the [caretEnsemble package](https://cran.r-project.org/web/packages/caretEnsemble/) to run several models at once. It doesn't support multiclass predictions but we can at least get the results out in a minimal amount of coding. 

The correlation and dotplot give us information about how each model performed. In general, it wasn't much better than our original model but perhaps we can find away to ensemble them together as future work.


```r
library(caretEnsemble)

models <- caretList(pitchType~., 
                    data=df.train,
                    trControl=control,
                    methodList=c("rf", "nnet", "multinom")
                    )
```

```
## # weights:  113
## initial  value 11026.136266 
## iter  10 value 8444.615828
## iter  20 value 8239.298555
## iter  30 value 8131.550211
## iter  40 value 8073.145864
## iter  50 value 8048.939169
## iter  60 value 8041.075168
## iter  70 value 8038.564973
## iter  80 value 8036.733702
## iter  90 value 8035.233480
## iter 100 value 8034.722843
## final  value 8034.722843 
## stopped after 100 iterations
## # weights:  160 (124 variable)
## initial  value 10195.789175 
## iter  10 value 8611.298975
## iter  20 value 8361.971170
## iter  30 value 8127.472836
## iter  40 value 8064.170870
## iter  50 value 8041.784992
## iter  60 value 8036.749165
## iter  70 value 8034.120077
## iter  80 value 8032.800858
## iter  90 value 8032.082761
## iter 100 value 8031.983115
## final  value 8031.983115 
## stopped after 100 iterations
```

```r
results <- resamples(models)

summary(results)
```

```
## 
## Call:
## summary.resamples(object = results)
## 
## Models: rf, nnet, multinom 
## Number of resamples: 10 
## 
## Accuracy 
##            Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
## rf       0.4605  0.4659 0.4694 0.4688  0.4722 0.4761    0
## nnet     0.4596  0.4659 0.4717 0.4691  0.4726 0.4764    0
## multinom 0.4485  0.4638 0.4690 0.4667  0.4702 0.4766    0
## 
## Kappa 
##                Min. 1st Qu.    Median      Mean  3rd Qu.     Max. NA's
## rf       -0.0008324 0.00000 0.0002156 0.0005866 0.001363 0.002425    0
## nnet      0.0096460 0.04000 0.0635700 0.0565200 0.071960 0.088470    0
## multinom  0.0432100 0.06106 0.0716700 0.0704100 0.076440 0.092820    0
```

```r
modelCor(results)
```

```
##                 rf      nnet  multinom
## rf       1.0000000 0.8501164 0.7015046
## nnet     0.8501164 1.0000000 0.6944573
## multinom 0.7015046 0.6944573 1.0000000
```

```r
dotplot(results)
```

![](TruMedia_Hackathon_Results_files/figure-html/ensemble-1.png)<!-- -->

# Next Steps

Well as you can tell, this didn't turn out too great. I'd like to create a custom ensemble model just to see how much it might improve on the individual models. In addition, there are additional features that I would like to create:

* Batter Performance against every pitch type
* Ballpark factors on pitches
* Influences by an umpire

Please reach out to [chris@feasibleanalytics.com](mailto:chris@feasibleanalytics.com) or @ct_analytics on twitter if you have any questions about this analysis.
