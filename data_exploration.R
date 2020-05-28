#' Functions for exploring and visualizing data.

library("caret")
library("tidyverse")
source("data_utils.R")
data <- edx_load_data()

# Some basic checks that the data looks correct.
stopifnot(nrow(edx) == 9000055, ncol(edx) == 6)

# Look at user ratings vs time.
user_v_time <- edx %>% group_by(userId) %>%
  mutate(period = (timestamp - min(timestamp)) / (60*60*24*7)) %>% ungroup()
         
avg_movie_rating <- mean(edx$rating)
small <- user_v_time %>% sample_n(nrow(edx)*0.1)

tab <- data.frame(x=small$period, y=small$rating)
fit <- mgcv::gam(y ~ s(x, bs = "cs"), data=tab)
y_hat <- predict(fit, tab)
tab_hat <- data.frame(x=tab$x, y=y_hat)

small %>%
  ggplot(aes(period, rating)) +
  #geom_point(alpha=0.01) +
  #geom_jitter(width=0.4, alpha=0.01) +
  geom_smooth() +
  geom_line(aes(tab_hat$x, tab_hat$y), color="green") #+
  #coord_cartesian(xlim=c(0,200))

# Movie ratings vs time.
movie_v_time <- edx %>% group_by(movieId) %>%
  mutate(period = (timestamp - min(timestamp)) / (60*60*24*7)) %>% ungroup()
small_movie <- movie_v_time %>% sample_n(nrow(edx)*0.1)

movie_fit <- mgcv::gam(rating ~ s(period, bs = "cs"), data = small_movie)
y_hat_movie <- predict(movie_fit, small_movie)
tab_hat_movie <- data.frame(x=small_movie$period, y=y_hat_movie)

small_movie %>%
  ggplot(aes(period, rating)) +
  geom_point(alpha=0.01) +
  geom_jitter(width=0.4, alpha=0.01) +
  geom_smooth() +
  geom_line(aes(tab_hat_movie$x, tab_hat_movie$y), color="green") #+
  #coord_cartesian(xlim=c(0,200))

# Split into above and below average movies.
small_movie %>% 
  filter(rating >= avg_movie_rating) %>%
  ggplot(aes(period, rating)) +
  geom_point(alpha=0.01) +
  geom_jitter(width=0.4, alpha=0.01) +
  geom_smooth()
  
small_movie %>% 
  filter(rating < avg_movie_rating) %>%
  ggplot(aes(period, rating)) +
  geom_point(alpha=0.01) +
  geom_jitter(width=0.4, alpha=0.01) +
  geom_smooth()

# Histogram of number of review per movie.
small_movie %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(binwidth = 200)

# Scatter plot of avg rating per movie, vs number of reviews of the movie.
small_movie %>% group_by(movieId) %>%
  summarise(avg_rating=mean(rating), n=n()) %>%
  ggplot(aes(n, avg_rating)) +
  geom_point(alpha=0.2)

# Fit loess model for movie rating over time.
#movie_effect_t_fit <- small_movie %>% group_by(movieId) %>%
#  loess(rating ~ period, degree=1, span=0.2, data=.)
small_movie %>% group_by(movieId) %>%
  summarise(n=n()) %>% arrange(desc(n)) %>% top_n(10)


movie_ids <- unique(small_movie$movieId)
fits <- sapply(movie_ids, function(id) {
  data <- small_movie %>% filter(movieId == id)
  ifelse(nrow(data) > 50,
         mgcv::gam(rating ~ s(period, bs = "cs"), data=data),
         NA)
})

#fits <- sapply(movie_ids, function(id) {
#  data <- small_movie %>% filter(movieId == id)
#  ifelse(nrow(data) > 25,
#         loess(rating ~ period, degree=1, span=0.2, data=data),
#         NA)
#})

# Create model of rating vs time since first rating. Too few samples.
train_knn <- small_movie %>% filter(movieId == 200) %>%
  train(rating ~ period, method = "knn", data = .,
        tuneGrid = data.frame(k = seq(5, 51, 2)))
ggplot(train_knn, highlight = TRUE)

# movieId = 200 has 25 ratings, not enough to form a model.
length(small_movie$movieId[small_movie$movieId == 200])
small_movie %>% filter(movieId == 200) %>%
  ggplot(aes(period, rating)) +
  geom_smooth(method="gam") +
  geom_point()

small_movie %>% group_by(movieId) %>% summarise(n=n()) %>%
  filter(n <= 500) %>% top_n(5)

small_movie %>% group_by(movieId) %>% summarise(n=n()) %>%
  top_n(5)
  
# movie 511 has 100 ratings.
small_movie %>% filter(movieId == 511) %>%
  ggplot(aes(x=period, y=rating)) +
  geom_point() + geom_smooth(method="gam")#, formula ~ rating ~ s(period, bs = "cs"))
  #geom_point() + geom_smooth(span=0.2, degree=1)

# movie 2672 has 200 ratings.
small_movie %>% filter(movieId == 2672) %>%
  ggplot(aes(period, rating)) +
  geom_point() + geom_smooth(method="gam")

# movie 2003 has 498 ratings.
small_movie %>% filter(movieId == 2003) %>%
  ggplot(aes(period, rating)) +
  #geom_point() + geom_smooth(method="gam")
  geom_point() + geom_smooth(method="loess")

# movie 296 has 3168 ratings.
small_movie %>% filter(movieId == 296) %>%
  ggplot(aes(period, rating)) +
  geom_point() + geom_smooth(method="gam")
  #geom_point() + geom_smooth(method="loess")

train_knn <- small_movie %>% filter(movieId == 296) %>%
  train(rating ~ period, method = "knn", data = .,
        #tuneGrid = data.frame(k = seq(5, 501, 50)))
        tuneGrid = data.frame(k = seq(5, 101, 2)))
ggplot(train_knn, highlight = TRUE)

knn_predict <- small_movie %>% filter(movieId == 296) %>%
  predict(train_knn, .)

small_movie %>% filter(movieId == 296) %>%
  ggplot(aes(rating, knn_predict)) + geom_point(alpha=0.5)
small_movie %>% filter(movieId == 296) %>% .$rating %>% RMSE(knn_predict)

# Dimension reduction, turn the edx into matrix, and 

edx %>% mutate(date = as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(movieId) %>%
  group_by(date) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(date, avg_rating)) +
  geom_point() +
  geom_smooth()
  

# Rating vs movie age
small <- edx %>% sample_n(10^5)
small <- small %>% mutate(date = as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(movieId) %>%
  mutate(movie_age = date - min(date)) %>%
  ungroup()

small %>%
  ggplot(aes(as.numeric(movie_age, units="weeks"), rating)) +
  geom_point(alpha=0.01) +
  geom_smooth()

# Good movies
small %>% group_by(movieId) %>%
  filter(mean(rating) >= 2.5) %>%
  ungroup() %>%
  ggplot(aes(as.numeric(movie_age, units="weeks"), rating)) +
  geom_point(alpha=0.01) +
  geom_smooth()

small %>% group_by(movieId) %>%
  filter(mean(rating) < 2.5) %>%
  ungroup() %>%
  ggplot(aes(as.numeric(movie_age, units="weeks"), rating)) +
  geom_point(alpha=0.01) +
  geom_smooth()

small %>% group_by(movie_age) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(as.numeric(movie_age, units="weeks"), avg_rating)) +
  geom_point() +
  geom_smooth()

# Movies with top averages
small %>% group_by(movieId) %>%
  filter(n() > 100) %>%
  summarise(title = unique(title), avg = mean(rating)) %>%
  arrange(desc(avg))

top_movie_ids <- small %>%
  group_by(movieId) %>%
  filter(n() > 100) %>%
  group_by(movieId) %>%
  summarise(avg = mean(rating)) %>%
  top_n(5, avg) %>%
  pull(movieId)
  
small %>% filter(movieId %in% top_movie_ids) %>%
  ggplot(aes(as.numeric(movie_age, units="weeks"), rating, color=movieId)) +
  geom_point() +
  geom_line()
  
small %>% filter(movieId == 912) %>%
  ggplot(aes(as.numeric(movie_age, units="weeks"), rating)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="gam")

edx2 <- edx %>% mutate(date = as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(movieId) %>%
  mutate(movie_age = date - min(date)) %>%
  ungroup()

edx2 %>% filter(movieId == 1198) %>%
  ggplot(aes(as.numeric(movie_age, units="weeks"), rating)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="gam")

edx2 %>% filter(movieId %in% top_movie_ids) %>%
  group_by(movieId)# %>%
  #ggplot(aes(as.numeric(movie_age, "weeks"), rating, colors = movieId)) +
  #geom_point() +
  #geom_smooth()

edx2 %>% filter(movieId == 1198) %>%
  ggplot(aes(date, rating)) +
  geom_smooth() +
  geom_point()

edx2 %>% group_by(movieId) %>%
  filter(n() <= 25)

# Genres
edx %>% separate_rows(genres, sep = "\\|")
edx %>% filter(movieId == 2001)
n_movies <- unique(edx$movieId)
movie_and_genres <- edx %>% select(movieId, genres) %>% unique()
movie_and_genres %>% separate_rows(genres, sep = "\\|") %>% .$genres %>% unique()

# Build up table for user genre bias.
#calc_user_genre_bias <- function(data, user) {
#  user_data <- data %>% filter(userId == user) %>%
#    separate_rows(genres, sep = "\\|") %>%
#    group_by(movieId) %>%
#    mutate(n = n()) %>%
#    ungroup() %>%
#    #mutate(bias = rating / n)
#    mutate(bias = rating)
#  user_data
#}
calc_user_genre_bias <- function(data, user, base_model) {
  user_data <- data %>% filter(userId == user)
  resids <- user_data$rating - predict_reg_movie_user(base_model, user_data)
  user_data <- user_data %>%
    mutate(resids = resids) %>%
    separate_rows(genres, sep = "\\|") %>%
    group_by(genres) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    group_by(movieId) %>%
    mutate(bias = (resids * n) / sum(n)) %>%
    ungroup() %>%
    group_by(genres) %>%
    summarise(userId = user, bias = mean(bias))
  user_data
  
#  user_data <- data %>% filter(userId == user) %>%
#    separate_rows(genres, sep = "\\|") %>%
#    group_by(genres) %>%
#    mutate(n = n()) %>%
#    ungroup() %>%
#    mutate(bias = rating) %>%
#    group_by(movieId) %>%
#    mutate(bias = (bias * n) / sum(n)) %>%
#    ungroup() %>%
#    group_by(genres) %>%
#    summarise(userId = user, bias = mean(bias))
#  user_data
}
#calc_user_genre_bias(edx, 2) %>% spread(genres, bias)
batch_calc_user_genre_bias <- function(data, users, base_model) {
  progress_len = 100
  output_progress <- users[seq(1, length(users), length = progress_len)]
  pb <- txtProgressBar(min = 0, max = progress_len, style = 3)
  
  results <- lapply(users, function(user) {
    if (user %in% output_progress) {
      setTxtProgressBar(pb, which(user == output_progress))
    }
    calc_user_genre_bias(data, user, base_model)
  })
  bind_rows(results)
  
  #for (user in user_ids[2:10000]) {
  #  bias <- calc_user_genre_bias(edx, user)
  #  bind_rows(user_genre_bias, bias)
  #}
}

user_ids <- unique(edx$userId)
base_model <- fit_model_reg_movie_user(edx, best_lambda_reg_movie_user)
user_genre_bias = calc_user_genre_bias(edx, 1, base_model)

users_genre_bias <- batch_calc_user_genre_bias(edx, user_ids[1:1000], base_model)
