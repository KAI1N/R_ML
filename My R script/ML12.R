set.seed(1)
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]
mean(train_y=='B')
mean(test_y=='B')
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}
set.seed(3)
k=kmeans(train_x,2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)
confusionMatrix(data=as.factor(kmeans_preds),reference=test_y)

set.seed(1)
train_glm <- train(train_x, train_y, method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)

set.seed(1)
train_lda=train(train_x,train_y,method='lda')
lda_preds=predict(train_lda,test_x)
mean(lda_preds==test_y)

set.seed(1)
train_qda=train(train_x,train_y,method='qda')
qda_preds=predict(train_qda,test_x)
mean(qda_preds==test_y)

set.seed(5)
library(caret)
library(gam)
train_loess=train(train_x,train_y,method='gamLoess')
loess_preds=predict(train_loess,test_x)
mean(loess_preds==test_y)

set.seed(7)
train_knn=train(train_x,train_y,method='knn',tuneGrid=data.frame(k = seq(3,21,2)),)
train_knn$bestTune
knn_preds=predict(train_knn,test_x)
mean(knn_preds==test_y)

set.seed(9)
train_rf=train(train_x,train_y,method='rf',tuneGrid=data.frame(mtry=c(3,5,7,9)),importance=TRUE)
train_rf$bestTune
rf_preds=predict(train_rf,test_x)
mean(rf_preds==test_y)
varImp(train_rf)

ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)




models <- c("glm", "lda", "knn", "gamLoess","qda", "rf")
fits <- lapply(models, function(model){ 
  print(model)
  train(train_x,train_y,method = model)
})
names(fits) <- models
pred <- sapply(fits, function(object){ 
  predict(object,newdata=test_x)
})
dim(pred)
acc <- colMeans(pred==test_y)
acc

