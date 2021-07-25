######### Harvardx's Data Sicience Professional Certificate Capstone ########
######################## Movielen Project###################################
########################### R Script########################################



# Install all needed libraries if it is not present
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(tidyr)) install.packages("tidyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(xgboost)) install.packages("Xgboost")
if(!require(data.table)) install.packages("data.table")

# load libraries
library(tidyverse)
library(stringr)
library(ggplot2)
library(tidyr)
library(data.table)
library(lubridate)
library(xgboost)

# I. Download Movielens data : copy the code provided by edx

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret",
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table",
                                          repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t",
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(
  unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
##################################
#End edx code:
##################################

# Save validation to the final step

fwrite(validation, "validation.csv")

rm(dl, ratings, movies, test_index, temp, movielens, removed,
   validation) # we save validation,  remove it  and load it in the final step
gc()

# II . Data Exploration :

## 1. ## Initial exploration :
### The structure of the dataset
str(edx)

### Let's take a look on the first six rows of Edx data :
head(edx)

### Number of observations and variables :
dim(edx)

### Look for missing values :
sapply(edx, function(i){
  sum(is.na(i))
}) 

### Number of movies and users :
edx %>%
  summarize(users = n_distinct(userId),
            movies = n_distinct(movieId))


## 2. Variable Exploration :


## a. movies distribution:

### There are 10677 different movies in the edx set,
###some of them are rated more than others :
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies distribution") + theme_bw() +
  xlab("number of ratings")

### First six most rated movies :
edx %>% dplyr::count(movieId) %>% arrange(desc(n)) %>% head()

### 90% of movies are rated at least 10 times:
edx %>% dplyr::count(movieId) %>% summarise(
  "movies which are rated at least 10 times" = mean(n>=10))

### Summary of rating per movies:
edx %>% group_by(movieId) %>% summarise(number_of_ratings = n()) %>% 
  ungroup() %>% pull(number_of_ratings) %>% summary()


## b. users

### Six users that rated the most :
edx %>% dplyr::count(userId) %>% arrange(desc(n)) %>% head()

### There are 69878 different users in the edx set :
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("User distribution") +
  xlab("number of ratings") + theme_bw()

### Summary rating per users: 
edx %>% group_by(userId) %>% summarise(number_of_ratings = n()) %>% 
  ungroup() %>% pull(number_of_ratings) %>% summary()


### 95 % of the users rated more than 20 movies
edx %>% dplyr::count(userId) %>% summarise(
  "users who rated more than 20 movies" = mean(n>=20))

## c. Rating 

### The number of ratings in the dataset is :
nrow(edx)

### The rating range is between 0.5 and 5 : 10 possible choices.
length(edx$rating)
unique(edx$rating)

### Total number for each rating :
edx %>% dplyr::count(rating) %>% arrange(desc(n))

### Most rating get rounded value :
edx %>% ggplot(aes(rating)) + geom_bar() + theme_bw() +
  ggtitle("Distribution of rating") 

### Distribution of the average rating per user
edx %>% group_by(userId) %>% summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) + 
  geom_histogram(color = "black")

### Summary of average rating per user :
edx %>% group_by(userId) %>% summarise(avg_rating = mean(rating)) %>%
  pull(avg_rating) %>% summary()

### Distribution of the average rating per movie :
edx %>% group_by(movieId) %>% summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) + 
  geom_histogram(color = "black")
## Summary of average rating per movie :
edx %>% group_by(movieId) %>% summarise(avg_rating = mean(rating)) %>%
  pull(avg_rating) %>% summary()


## d. Date_time :

### d.1 Timestamp : Rating time :
####  convert timestamp  to year of rating
edx <- edx %>% mutate(
  rating_year = as.integer(year(date(as_datetime(timestamp,
                                                 origin = "1970-01-01"))))) %>% 
  select(-timestamp)


#### Summary of date of rating
#### The rating start in 1995, ends in 2009 and last more than 14 years.
range(edx$rating_year)
diff(range(edx$rating_year))

#### Number of rating per year of rating :
#### The ten years with most number of ratings
edx %>% group_by(rating_year) %>% summarise(n=n()) %>% head() %>%
  arrange(desc(n))

#### Number of rating per year of rating plot :
edx %>% ggplot(aes(rating_year)) + geom_histogram() + theme_bw() +
  ggtitle("Distribution per year of rating") + ylab("number of ratings")


### d.2 Year of release :

#### extract year of release 
edx <- edx %>% mutate(title = str_trim(title)) %>% 
  extract(title, c("title_1", "release"),
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>% 
  mutate(release = if_else(str_length(release) > 4,
                           as.integer(str_split(release, "-",
                                                simplify = T)[1]),
                           as.integer(release))) %>% 
  mutate(title = if_else(is.na(title_1), title,
                         title_1)) %>% select(-title_1)

#### This is how our data looks like now :
head(edx) 

### Summary year of relaese
range(edx$release)
diff(range(edx$release))

#### Distribution of ratings per year of release

edx %>% group_by(release) %>% summarise(n = n()) %>%
  ggplot(aes(release, n)) + geom_line(color = "blue") + 
  theme_bw() + ggtitle("Distribution per year of release") + 
  ylab("number of ratings")

#### The 10 years of release with the most rated movies
edx %>% group_by(release) %>% summarise(n = n()) %>% arrange(desc(n)) %>% 
  head(10)

#### Number of released movies per year :
edx %>% filter(!duplicated(movieId)) %>% dplyr::count(release) %>%
  arrange(desc(n)) %>% head()

#### Distribution :
edx %>% filter(!duplicated(movieId)) %>% dplyr::count(release) %>%
  ggplot(aes(release, n)) +
  geom_line(color = "blue") + theme_bw() +
  ggtitle("distribution of released movies per year") +
  ylab("number of movies released")

## e. Genre :

### Number of rating per genre

edx <- edx %>% select(-title) # to release some memory

### Number of rating per genre
edx %>% dplyr::count(genres) %>% arrange(desc(n)) %>% head()
unique(edx$genres) %>% length()

gc()

### Split the genre to have one per each row(this can take some time to run!)
 
edx <- edx %>% separate_rows(genres, sep = "\\|")
gc()

### This is how our data looks now
head(edx) 

### Number of genres in our data :
edx %>% dplyr::count(genres) %>% arrange(desc(n))

### Number of rating per each genre is :
edx %>% dplyr::count(genres) %>% arrange(desc(n))

### Distribution of rating per genres :
edx %>% mutate(genres = factor(genres,
                               levels = names(sort(table(edx$genres),
                                                  decreasing = TRUE)))) %>%
  ggplot(aes(genres)) +  geom_bar(fill = "blue") + theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("number of movie rated per genres")

### Summary of rating per genres
edx %>% group_by(genres) %>% 
  summarise(avg_rating = mean(rating), med = median(rating),
            min = min(rating), max = max(rating))

# III. Data analysis
library(caret)
options( digits = 4)

## making the evaluation function :
RMSE <- function(actual , prediction){
  sqrt(mean((actual - prediction)^2))
}
## Convert genres to integer to release memory and make calculations fast : 
edx <- edx %>% mutate(genres = as.integer(as.factor(genres)))

## Create train and test set
set.seed(1, sample.kind = "Rounding")

test_index <- createDataPartition(edx$rating, 
                                  times = 1, p = 0.2, list = FALSE)

train <- edx %>% dplyr::slice(-test_index)
temp <- edx %>% dplyr::slice(test_index)
test <- temp %>% semi_join(train, by = "movieId") %>% 
  semi_join(train, by = "userId")
removed <- anti_join(temp, test)
train <- rbind(train, removed)
rm(temp, test_index, removed, edx)
gc()

## 1. first model : just the mean

### Predict rating as the average :
mean_rating <- mean(train$rating)

# RMSE :
just_avg <- RMSE(test$rating, mean_rating)

results <- data.frame("method" = "just the avg", "rmse" = just_avg)
results %>% knitr::kable()

## 2. Add movie effect :

### Model :
movies_avgs <- train %>%
  group_by(movieId) %>%
  summarize(movie_eff = mean(rating - mean_rating))

### Predictions :
movie_eff <- test %>% left_join(movies_avgs, by = "movieId") %>% 
  mutate(pred = mean_rating + movie_eff)

### RMSE :
movie_eff_rmse <- RMSE(test$rating, movie_eff$pred)

results <- results %>% add_row(method = "movie_eff", 
                               rmse = movie_eff_rmse)
results %>% knitr::kable()
rm(movie_eff)
gc()
## 3. Add user effect :

### Model :
users_avgs <- train %>%
  left_join(movies_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(user_eff = mean(rating - mean_rating - movie_eff))

### Predictions :
movie_user_eff <- test %>% 
  left_join(movies_avgs, by = 'movieId') %>%
  left_join(users_avgs , by = 'userId') %>% 
  mutate(pred = mean_rating + movie_eff + user_eff) 
### RMSE :
movie_user_rmse <- RMSE(test$rating ,  movie_user_eff$pred)
results <- results %>% add_row(method = "movie_user_eff", 
                               rmse = movie_user_rmse)
results %>% knitr::kable()
rm(movie_user_eff)
gc()
## 4. Add genres effect :

### Model :
genres_avgs <- train %>% select(-c(rating_year, release)) %>%
  left_join(movies_avgs, by='movieId') %>%
  left_join(users_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarize(genre_eff = mean(rating - mean_rating - movie_eff - user_eff))

### Predictions :
movie_user_g_eff <- test %>% 
  left_join(movies_avgs, by = 'movieId') %>%
  left_join(users_avgs , by = 'userId') %>%
  left_join(genres_avgs, by ="genres") %>%
  mutate(pred = mean_rating + movie_eff + user_eff + genre_eff) 

### RMSE :
movie_user_g_rmse <- RMSE(test$rating ,  movie_user_g_eff$pred)
results <- results %>% add_row(method = "movie_user_g_eff", 
                               rmse = movie_user_g_rmse)
results %>% knitr::kable()
rm(movie_user_g_eff)
gc()

## 5. Add release_year effect :

### Modeling :
release_avgs <- train %>%
  left_join(movies_avgs, by='movieId') %>% 
  left_join(users_avgs, by="userId") %>%
  left_join(genres_avgs, by = "genres") %>% 
  group_by(release) %>%
  summarize(release_eff = 
         mean(rating - mean_rating - movie_eff - user_eff - genre_eff))

### Getting predictions :
release_eff <- test %>% 
  left_join(movies_avgs, by = 'movieId') %>%
  left_join(users_avgs , by = 'userId') %>%
  left_join(genres_avgs, by = "genres")%>% 
  left_join(release_avgs, by="release") %>%
  mutate(pred = mean_rating + movie_eff + user_eff + genre_eff + release_eff)

### Getting RMSE :
movie_user_release_rmse <- RMSE(test$rating ,  release_eff$pred)
results <- results %>% add_row(method = "movie_user_gre_rel_eff", 
                               rmse = movie_user_release_rmse)
results %>% knitr::kable()

rm(release_eff)
gc()


## 6. Add rating year effect :

### Making our model :
rat_year_avgs <- train %>%
  left_join(movies_avgs, by='movieId') %>% 
  left_join(users_avgs, by="userId") %>%
    left_join(genres_avgs, by = "genres") %>% 
  left_join(release_avgs, by="release")  %>% group_by(rating_year) %>%
  summarize(rat_year_eff = 
  mean(rating - mean_rating - movie_eff - user_eff - genre_eff - release_eff))

### Applying it to the test set : Predictions
rat_year_eff <- test %>% 
  left_join(movies_avgs, by = 'movieId') %>%
  left_join(users_avgs , by = 'userId') %>% 
  left_join(genres_avgs, by = "genres") %>% 
  left_join(release_avgs, by="release") %>%
  left_join(rat_year_avgs, by="rating_year") %>% 
  mutate(pred = 
  mean_rating + movie_eff + user_eff + genre_eff + release_eff + rat_year_eff)


### Getting RMSE :
movie_user_year_rmse <- RMSE(test$rating ,  rat_year_eff$pred)
results <- results %>% add_row(method = "movie_user_year_eff", 
                               rmse = movie_user_year_rmse)
results %>% knitr::kable()
rm(rat_year_eff, genres_avgs, movies_avgs, rat_year_avgs,
   release_avgs, users_avgs) 
gc()


# 7. The Gradiant boosting method : Xgboost 

#### We will make "digits = 2" while preparing the train data in order to 
#### not exhaust the memory but will set "digits =4" for RMSE calcilation

## Data processing : add average rating per important predictors to our data

### train_set
movie_avg <- train %>% group_by(movieId) %>%
  summarise(mov_avg = mean(rating))

user_avg <- train %>% group_by(userId) %>%
  summarise(user_avg = mean(rating))

genre_avg <- train %>% group_by(genres) %>% 
  summarise(genre_avg = mean(rating))

train <- train %>% left_join(movie_avg) %>% left_join(user_avg) %>% 
  left_join(genre_avg) %>% as.data.frame()

rm(genre_avg, user_avg, movie_avg)
gc()
head(train)

### test_set
movie_avg <- test %>% group_by(movieId) %>%
  summarise(mov_avg = mean(rating))

user_avg <- test %>% group_by(userId) %>%
  summarise(user_avg = mean(rating))

genre_avg <- test %>% group_by(genres) %>% 
  summarise(genre_avg = mean(rating))

test <- test %>% left_join(movie_avg) %>% left_join(user_avg) %>% 
  left_join(genre_avg) %>% as.data.frame()

rm(genre_avg, user_avg, movie_avg)
gc()
head(test)

#define predictor and response variables in training set
train_x = data.matrix(train[, -3])
train_y = train[,3]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -3])
test_y = test[, 3] 

# free some memory
rm(train, test)
gc()

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
set.seed(1, sample.kind = "Rounding")

model = xgb.train(data = xgb_train, max.depth = 10,
                  watchlist=watchlist, nrounds = 20)
#define final model
final = xgboost(data = xgb_train, max.depth = 10, nrounds = 15, 
                verbose = TRUE)
# get predictions
y_pred <- predict(final, data.matrix(test_x))

rmse_xgboost <- RMSE(test_y, y_pred)

results <- results %>% add_row(method = "xgboost", 
                               rmse = rmse_xgboost)
results %>% knitr::kable()

rm(test_x, test_y)
gc()

### apply the winning model for on validation set

#### load validation set :
validation <- fread("raw_data/validation.csv")

#### validation preprocessing

#### rating year
validation <- validation %>% mutate(
  rating_year = as.integer(year(date(
    as_datetime(timestamp, origin = "1970-01-01"))))) %>% 
  select(-timestamp)

#### extract year of release 
validation <- validation %>% mutate(title = str_trim(title)) %>% 
  extract(title, c("title_1", "release"),
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>% 
  mutate(release = if_else(str_length(release) > 4,
                           as.integer(str_split(release, "-",
                                                simplify = T)[1]),
                           as.integer(release))) %>% 
  mutate(title = if_else(is.na(title_1), title,
                         title_1)) %>% select(-title_1, -title)

#### genres
validation <- validation %>% separate_rows(genres, sep = "\\|")

validation <- validation %>% 
  mutate(genres = as.integer(as.factor(genres))) %>%
  as.data.frame()

### making validation ready for our model
options(digits = 2)

movie_avg <- validation %>% group_by(movieId) %>%
  summarise(mov_avg = mean(rating))

user_avg <- validation %>% group_by(userId) %>%
  summarise(user_avg = mean(rating))

genre_avg <-  validation %>% group_by(genres) %>% 
  summarise(genre_avg = mean(rating))

validation <- validation %>% left_join(movie_avg) %>%
  left_join(user_avg) %>% 
  left_join(genre_avg)

rm(genre_avg, user_avg, movie_avg)
gc()

valid_x = data.matrix(validation[, -3])
valid_y = validation[, 3]

### validation final results
options(digits = 4)
rat_pred <- predict(final, data.matrix(valid_x))

### get predictions
rmse_xgboost <- RMSE(valid_y, rat_pred)

paste(c("The MRSE of Xgboost applied on validation set is", rmse_xgboost))




