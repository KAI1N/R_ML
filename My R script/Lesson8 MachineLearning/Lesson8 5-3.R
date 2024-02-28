library(titanic)  
library(caret)
library(tidyverse)
library(rpart)
options(digits = 3)
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
set.seed(42, sample.kind = "Rounding") 
test_index=createDataPartition(titanic_clean$Survived,times=1,p=0.2,list=FALSE)
test=titanic_clean[test_index,]
train=titanic_clean[-test_index,]
nrow(test)
nrow(train)
mean(train$Survived=="1")

set.seed(3, sample.kind = "Rounding")
guess <- sample(c(0,1), nrow(test), replace = TRUE)
mean(guess == test$Survived)

train %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "female") %>%
  pull(Survived)
train %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male") %>%
  pull(Survived)

sex_model <- ifelse(test$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test$Survived)

sapply(c(1,2,3),function(i){
    train%>%
    group_by(Pclass) %>%
    summarize(Survived = mean(Survived == 1)) %>%
    filter(Pclass==i) %>%
    pull(Survived)
})
Pclass_prediction=ifelse(test$Pclass==1, 1, 0) # predict Survived=1 if female, 0 if male
mean(Pclass_prediction==test$Survived)

train %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Survived > 0.5)

both_prediction=ifelse(test$Sex=='female'&test$Pclass!=3,1,0)
mean(both_prediction==test$Survived)

confusionMatrix(data = factor(sex_model), reference = factor(test$Survived))
confusionMatrix(data = factor(Pclass_prediction), reference = factor(test$Survived))
confusionMatrix(data = factor(both_prediction), reference = factor(test$Survived))

F_meas(data = factor(sex_model), reference = factor(test$Survived))
F_meas(data = factor(Pclass_prediction), reference = factor(test$Survived))
F_meas(data = factor(both_prediction), reference = factor(test$Survived))

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_lda <- train(Survived ~ Fare, method = "lda", data = train)
lda_preds <- predict(train_lda, test)
mean(lda_preds == test$Survived)
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_qda <- train(Survived ~ Fare, method = "qda", data = train)
qda_preds <- predict(train_qda, test)
mean(qda_preds == test$Survived)

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_glm1 <- train(Survived ~ Age, method = "glm", data = train)
glm_preds1 <- predict(train_glm1, test)
mean(glm_preds1 == test$Survived)
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_glm4 <- train(Survived ~ Age+Sex+Pclass+Fare, method = "glm", data = train)
glm_preds4 <- predict(train_glm4, test)
mean(glm_preds4 == test$Survived)
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_glm8 <- train(Survived ~ Age+Sex+Pclass+Fare+SibSp+Parch+FamilySize+Embarked, method = "glm", data = train)
glm_preds8 <- predict(train_glm8, test)
mean(glm_preds8 == test$Survived)

set.seed(6, sample.kind = "Rounding") # if using R 3.6 or later
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune

knn_preds<-predict(train_knn, test)
mean(knn_preds==test$Survived)

set.seed(8, sample.kind = "Rounding")    # simulate R 3.5
train_knn_cv <- train(Survived ~ .,
                      method = "knn",
                      data = train,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn_cv$bestTune
knn_cv_preds <- predict(train_knn_cv, test)
mean(knn_cv_preds == test$Survived)

set.seed(10, sample.kind = "Rounding")    # simulate R 3.5
train_rpart <- train(Survived ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train)
train_rpart$bestTune
rpart_preds <- predict(train_rpart, test)
mean(rpart_preds == test$Survived)
plot(train_rpart)

train_rpart$finalModel # inspect final model
# make plot of decision tree
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel)

set.seed(14, sample.kind = "Rounding")    # simulate R 3.5
train_rf <- train(Survived ~ .,
                  data = train,
                  method = "rf",
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:7)))
train_rf$bestTune
rf_preds <- predict(train_rf, test)
mean(rf_preds == test$Survived)
varImp(train_rf) 
