if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dplyr)
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
#making RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#First model, only using mu
mu_hat <- mean(edx$rating)
naive_rmse <- RMSE(final_holdout_test$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + final_holdout_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
#Second Model, using mu and user_avgs
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))

#extracting year 
edx=edx%>%
  mutate(years=str_extract(.$title,'(?<=\\()[0-9-]+(?=\\))'))
edx$years=as.numeric(edx$years)
sort(edx$years)
#there is error in extracting year data so looking what is wrong
edx%>%
  filter(years==6)
#the title included()so there was error extracting year
#all was 1994 so replaced 6 to 1994
edx$years=replace(edx$years,edx$years==6,1994)
#check 6 is replaced to 1994
sort(edx$years)
#assume that the older the movie get, the rating get higher ratings
edx%>%
  group_by(years)%>%
  summarize(mean=mean(rating))%>%
  ggplot(aes(years,mean))+
  geom_point()
#looks like after 1980 has less rating
mean(edx%>%
  filter(years<1980)%>%
  .$rating)
mean(edx%>%
    filter(years>=1980)%>%
    .$rating)
#making dummy if the movie was released after 1980 as 1,and others as 0
edx=edx%>%
  mutate(dummy=1)
edx$dummy=replace(edx$dummy,edx$years<1980,0)
#Third Model, using year
year_avgs <- edx %>% 
  group_by(years) %>% 
  summarize(b_y = mean(rating - mu))
#forget to make new colum, year, in final_hold_out set
final_holdout_test=final_holdout_test%>%
  mutate(years=str_extract(.$title,'(?<=\\()[0-9-]+(?=\\))'))
final_holdout_test$years=as.numeric(final_holdout_test$years)
final_holdout_test$years=replace(final_holdout_test$years,final_holdout_test$years==6,1994)

predicted_ratings <- mu + final_holdout_test %>% 
  left_join(year_avgs, by='years') %>%
  .$b_y
model_3_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Year Effect Model",
                                     RMSE = model_3_rmse ))

#Forth Model, using dummy
dummy_avgs=edx%>%
  group_by(dummy)%>%
  summarize(b_d=mean(rating-mu))
#forget to make colum dummy
final_holdout_test=final_holdout_test%>%
  mutate(dummy=1)
final_holdout_test$dummy=replace(final_holdout_test$dummy,final_holdout_test$years<1980,0)

predicted_ratings <- mu + final_holdout_test %>% 
  left_join(dummy_avgs, by='dummy') %>%
  .$b_d
model_4_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Dummy Effect Model",
                                     RMSE = model_4_rmse ))
#Fifth Model, using year,dummy
dummy_avgs=edx%>%
  left_join(year_avgs, by='years') %>%
  group_by(dummy)%>%
  summarize(b_d=mean(rating-mu-b_y))
predicted_ratings <- final_holdout_test %>% 
  left_join(year_avgs, by='years') %>%
  left_join(dummy_avgs, by='dummy') %>%
  mutate(pred = mu + b_y + b_d) %>%
  .$pred
model_5_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Year Regularized Effect Model",
                                     RMSE = model_5_rmse ))


#Sixth Model, using year,dummy, movieId
movie_avgs <- edx %>% 
  left_join(year_avgs, by='years') %>%
  left_join(dummy_avgs, by='dummy') %>%
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu - b_y - b_d))
predicted_ratings <- mu + final_holdout_test %>% 
  left_join(year_avgs, by='years') %>%
  left_join(dummy_avgs, by='dummy') %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_6_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Year Regularized + Movie  Effect Model",
                                     RMSE = model_6_rmse ))
#Seventh Model, using year,dummy, movieId, userId
user_avgs <- edx %>% 
  left_join(year_avgs, by='years') %>%
  left_join(dummy_avgs, by='dummy') %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i- b_y - b_d))
predicted_ratings <- final_holdout_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='years') %>%
  left_join(dummy_avgs, by='dummy') %>%
  mutate(pred = mu + b_i + b_u + b_y+b_d) %>%
  .$pred
model_7_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Year Regularized + Movie + User Effects Model",  
                                     RMSE = model_7_rmse ))

#find the min of Year Regularized,Regularized Movie,User Effect Model
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_y <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(years) %>%
    summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+l))
  b_d <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by="years") %>%
    group_by(dummy) %>%
    summarize(b_d = sum(rating - b_i - b_u - b_y - mu)/(n()+l))
  predicted_ratings <- 
    final_holdout_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by="years") %>%
    left_join(b_d, by="dummy") %>%
    mutate(pred = mu + b_i + b_u + b_y + b_d) %>%
    .$pred
  return(RMSE(predicted_ratings, final_holdout_test$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Year Regularized + Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_y <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(years) %>%
    summarize(b_y = mean(rating - b_i - b_u - mu))
  b_d <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by="years") %>%
    group_by(dummy) %>%
    summarize(b_d = mean(rating - b_i - b_u - b_y - mu))
  predicted_ratings <- 
    final_holdout_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by="years") %>%
    left_join(b_d, by="dummy") %>%
    mutate(pred = mu + b_i + b_u + b_y + b_d) %>%
    .$pred
  return(RMSE(predicted_ratings, final_holdout_test$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Year Regularized + Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))

library(lubridate)
edx=edx%>%
  mutate(rated_date=as_datetime(timestamp))%>%
  mutate(rated_year=year(rated_date))
edx%>%
  group_by(rated_year)%>%
  summarize(mean=mean(rating),n=n())%>%
  ggplot(aes(rated_year,mean))+
  geom_point()
edx%>%
  group_by(rated_year)%>%
  summarize(mean=mean(rating),n=n())
