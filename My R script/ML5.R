library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
titanic_train
index=createDataPartition(titanic_clean$Survived,times=1,p=0.2,list=FALSE)
test_set=titanic_clean[index,]
train_set=titanic_clean[-index,]
nrow(train_set)
nrow(test_set)
mean(train_set$Survived == 1)
guess=sample(c(0,1),nrow(train_set),replace=TRUE)
mean(train_set$Survived==guess)
train_set_female=train_set%>% #
  filter(Sex=='female')
mean(train_set_female$Survived==1)
train_set_male=train_set%>% #
  filter(Sex=='male')
mean(train_set_male==1)

#train_set %>%
# group_by(Sex) %>%
# summarize(Survived = mean(Survived == 1)) %>%
# filter(Sex == "female") %>%
# pull(Survived)

sex_prediction=ifelse(test_set$Sex=='female',1,0)
mean(test_set$Survived==sex_prediction)

train_set%>%
  group_by(Pclass)%>%
  summarize(Survived=mean(Survived==1))

class_prediction=ifelse(test_set$Pclass==1,1,0)
mean(test_set$Survived==class_prediction)

train_set%>%
  group_by(Sex,Pclass)%>%
  summarize(Survived=mean(Survived==1))

sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
mean(sex_class_model == test_set$Survived)

confusionMatrix(data = factor(sex_prediction), reference = factor(test_set$Survived))

train_lda=train(Survived~Fare,method='lda',data=train_set)
lda_preds <- predict(train_lda, test_set)
mean(lda_preds == test_set$Survived)
train_qda=train(Survived~Fare,method='qda',data=train_set)
qda_preds <- predict(train_qda, test_set)
mean(qda_preds == test_set$Survived)

train_glm=train(Survived~Age,method='glm',data=train_set)
glm_preds=predict(train_glm,test_set)
mean(glm_preds==test_set$Survived)

train_glm_many=train(Survived~Age+Sex+Pclass+Fare,method='glm',data=train_set)
glm_many_preds=predict(train_glm_many,test_set)
mean(glm_many_preds==test_set$Survived)

train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_all_preds <- predict(train_glm_all, test_set)
mean(glm_all_preds == test_set$Survived)

train_knn=train(Survived~.,method='knn',data=train_set,tuneGrid = data.frame(k = seq(3, 51, 2)))
knn_preds=predict(train_knn,test_set)
train_knn$bestTune
ggplot(train_knn)
mean(knn_preds==test_set$Survived)

train_knn_cv <- train(Survived ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn_cv$bestTune
knn_cv_preds <- predict(train_knn_cv, test_set)
mean(knn_cv_preds == test_set$Survived)

train_rpart=train(Survived~.,
                  method='rpart',
                  data=train_set,
                  tuneGrid=data.frame(cp = seq(0, 0.05, 0.002)))
train_rpart$bestTune
rpart_preds=predict(train_rpart,test_set)
mean(rpart_preds==test_set$Survived)
ggplot(train_rpart)
plot(train_rpart$finalModel)
text(train_rpart$finalModel)

train_rf=train(Survived~.,
                  method='rf',
                  data=train_set,
                  tuneGrid=data.frame(mtry = seq(1:7)),
                  ntree=100
               )
train_rf$bestTune
rf_preds=predict(train_rf,test_set)
mean(rf_preds==test_set$Survived)
varImp(train_rf)




