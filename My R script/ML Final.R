##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dslabs)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#only mu
head(edx)
mu_hat <- mean(edx$rating)
naive_rmse <- RMSE(final_holdout_test$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
rmse_results

#using movieId
mu=mean(edx$rating)
movieId_avg=edx%>%
  group_by(movieId)%>%
  summarise(b_i=mean(rating-mu))
Y1=mu+final_holdout_test%>% 
  left_join(movieId_avg, by='movieId') %>%
  .$b_i
model1_rmse <- RMSE(Y1,final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model1_rmse ))

#using userId
userId_avg=edx%>%
  left_join(movieId_avg,by='movieId')%>%
  group_by(userId)%>%
  summarise(b_u=mean(rating-mu-b_i))
Y2=final_holdout_test%>%
  left_join(movieId_avg,by='movieId')%>%
  left_join(userId_avg, by='userId') %>%
  mutate(pred=mu+b_i+b_u)%>%
  .$pred
model2_rmse <- RMSE(Y2,final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="User Effect Model",
                                     RMSE = model2_rmse ))
ns=seq(0,5000,10)
ls=seq(0, 10, 0.25)
find_bias=sapply(ns,function(a){
  find_bias_movie=edx%>%
  group_by(movieId)%>%
  summarise(n=n(),b_i=mean(rating-mu))%>%
  filter(n>10)
  Yn=mu+final_holdout_test%>% 
    left_join(find_bias_movie, by='movieId') %>%
    .$b_i
  bias_movie=edx%>%
    group_by(movieId)%>%
    summarise(n=n(),s=sum(rating-mu))%>%
    filter(n<10)
  rmses=sapply(ls,function(l){
    predicted_ratings <- final_holdout_test %>% 
      left_join(bias_movie, by='movieId') %>% 
      mutate(b_i = s/(n+l)) %>%
      mutate(pred = mu + b_i) %>%
      .$pred
    Yn[is.na(Yn)]=predicted_ratings[!is.na(predicted_ratings)]
    biased_rmse=RMSE(Yn,final_holdout_test$rating)
    return(c(l,biased_rmse))
 })
  rmses[,rmses[2,]==min(rmses[2,])]
})
find_bias=sapply(seq(0,501,1),function(a){find_bias[[a]]})
  


lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})


edx


edx %>% group_by(movieId, title) %>%
  summarize(count = n(),avg_rating=mean(rating)) %>%
  arrange(desc(count))
edx %>% group_by(movieId, title) %>%
  summarize(count = n(),avg_rating=mean(rating)) %>%
  arrange(desc(-count))
under100rating=edx%>%group_by(movieId, title)%>%
  summarize(count=n(),avg_rating=mean(rating))%>%
  filter(count<100)%>%
  arrange(desc(count))
hist(under100rating$count)
hist(under100rating$avg_rating)
under100rating%>%
  ggplot(aes(count,avg_rating))+
  geom_point()+
  geom_smooth(method='lm')
cor(under100rating$count,under100rating$avg_rating)
findmaxmean=function(countnum){
  mean(edx%>%group_by(movieId, title)%>%
    summarize(count=n(),avg_rating=mean(rating))%>%
    filter(count>countnum)%>%
      .$avg_rating)
}
n1=seq(10000,30000,100)
n1result<-sapply(n1,findmaxmean)
plot(n1result)
max(n1result)
topcountrating=edx%>%group_by(movieId, title)%>%
  summarize(count=n(),avg_rating=mean(rating))%>%
  filter(count>23000)%>%
  arrange(desc(count))
hist(topcountrating$avg_rating)
hist(topcountrating$count)
mean(topcountrating$avg_rating)
mean(under100rating$avg_rating)

