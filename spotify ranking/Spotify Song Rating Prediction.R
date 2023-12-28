setwd("/Users/tingyiho/Downloads/")
songs = read.csv('analysisData.csv')

library(caret)
library(forcats)
library(qdap)
library(stringr)

nearZeroVar(songs,saveMetrics = T)
table(songs$time_signature)
fct_count(songs$genre)

songs4 <- songs
songs4$genre <- gsub("[|]|'|,","",songs4$genre)
songs4$genre <- strsplit(songs4$genre,split)
songs4$genre <- gsub("\\[|\\]","",songs4$genre)


library(dplyr)
songs5<-songs4%>%
  mutate(dummy_pop= if_else(str_detect(genre, "pop"), 1, 0))%>%
  mutate(dummy_rock= if_else(str_detect(genre, "rock"), 1, 0))%>%
  mutate(dummy_dance= if_else(str_detect(genre, "dance"), 1, 0))%>%
  mutate(dummy_rap= if_else(str_detect(genre, "rap"), 1, 0))
songs5

library(mice)
songs6 = mice::complete(mice(songs5,seed = 617))

sapply(songs6, function(x) sum(is.na(x)))

library(dplyr)
songs6<-songs6%>%
  select(-id,-performer,-song,-genre)

library(caret)
set.seed(1731)
split = createDataPartition(y = songs6$rating, p = 0.75, list = F,groups = 40)
train5 = songs6[split,]
test5 = songs6[-split,]

scoringData = read.csv('scoringData.csv')

scoringData4 <- scoringData%>%
  mutate(dummy_pop= if_else(str_detect(genre, "pop"), 1, 0))%>%
  mutate(dummy_rock= if_else(str_detect(genre, "rock"), 1, 0))%>%
  mutate(dummy_dance= if_else(str_detect(genre, "dance"), 1, 0))%>%
  mutate(dummy_rap= if_else(str_detect(genre, "rap"), 1, 0))

scoringData4$genre <- gsub("[|]|'|,","",scoringData4$genre)
scoringData4$genre <- strsplit(scoringData4$genre,split)
scoringData4$genre <- gsub("\\[|\\]","",scoringData4$genre)

scoringData4<-scoringData4%>%
  select(-performer,-song,-genre)

scoringData4 = mice::complete(mice(scoringData4,seed = 617))
sapply(scoringData4, function(x) sum(is.na(x)))


library(ranger)
set.seed(1731)
forest_ranger = ranger(rating~.,
                       data = train5, 
                       num.trees = 1000)
pred_train5 = predict(forest_ranger, data = train5, num.trees = 1000)
rmse_train_forest_ranger5 = sqrt(mean((pred_train5$predictions - train5$rating)^2))
rmse_train_forest_ranger5

pred5 = predict(forest_ranger, data = scoringData4, num.trees = 1000)


head(submissionFile)

submissionFile = data.frame(id = scoringData4$id, rating = pred5$predictions)
write.csv(submissionFile, 'sample_submission_1118_05.csv',row.names = F)


