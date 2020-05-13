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

#movie_ids <- unique(small_movie$movieId)
#fits <- sapply(movie_ids, function(id) {
#  data <- small_movie %>% filter(movieId == id)
#  ifelse(nrow(data) > 50,
#         mgcv::gam(rating ~ s(period, bs = "cs"), data=data),
#         NA)
#})

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