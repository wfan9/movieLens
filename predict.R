# Generate movie prediction ratings and RMSE scores.
# Code taken from report.RMD to satisfy project submission requirements of separate script.

# Turn on/off saving/loading large data objects from local file system.
use_cache <- FALSE

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

cache_edx_filename <- "rda/edx.RData"
if (use_cache & file.exists(cache_edx_filename)) {
  load(cache_edx_filename, envir=.GlobalEnv, verbose=TRUE)
} else {
  ################################
  # Create edx set, validation set
  ################################

  # Note: this process could take a couple of minutes

  if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
  if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

  # MovieLens 10M dataset:
   # https://grouplens.org/datasets/movielens/10m/
   # http://files.grouplens.org/datasets/movielens/ml-10m.zip

  dl <- tempfile()
   download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))

  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
   colnames(movies) <- c("movieId", "title", "genres")
   movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                              title = as.character(title),
                                              genres = as.character(genres))

  movielens <- left_join(ratings, movies, by = "movieId")

  # Validation set will be 10% of MovieLens data
  set.seed(1, sample.kind="Rounding")
  # if using R 3.5 or earlier, use `set.seed(1)` instead
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
   edx <- movielens[-test_index,]
   temp <- movielens[test_index,]

  # Make sure userId and movieId in validation set are also in edx set
  validation <- temp %>%
        semi_join(edx, by = "movieId") %>%
        semi_join(edx, by = "userId")

  # Add rows removed from validation set back into edx set
  removed <- anti_join(temp, validation)
  edx <- rbind(edx, removed)
  rm(dl, ratings, movies, test_index, temp, movielens, removed)

  if (use_cache) {
    save(edx, validation, file = cache_edx_filename)
  }
}

#' Given movie data, split into test and training sets.
#'
#' @param data Movie data to split
#' @param test_portion Portion of movie data to treat as test data.
#'
#' @return List containing "test" and "train" data frames.
split_set <- function(data, test_portion = 0.1) {
  test_index <- createDataPartition(y = data$rating, times = 1, p = test_portion, list = FALSE)
  train_set <- data[-test_index,]
  temp <- data[test_index,]

  # Make sure userId and movieId in test set are also in train set
  test_set <- temp %>%
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")

  # Add rows removed from test set back into train set
  removed <- anti_join(temp, test_set)
  train_set <- rbind(train_set, removed)
  list("test" = test_set, "train" = train_set)
}

#' Calculate root mean square error.
#'
#' @param true_ratings Vector of true ratings.
#' @param predicted_ratings Vector of predicted ratings.
#'
#' @return RMSE.
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Cross validation to determine optimal lambda.
# K-fold with k = 5

lambdas <- seq(0, 10, 0.25)
K <- 5

#' Create sets for cross validation
#'
#' @param data Movie data to create training and test sets from
#' @param k Number of test and train set pairs to create.
#'
#' @return List of test_set and train_set.
create_cv_sets <- function(data, k) {
  sapply(1:k, function(i) {
    sets <- split_set(data)
    test_set <- sets$test
    train_set <- sets$train
    list(test_set = test_set, train_set = train_set)
  })
}

#' Apply K fold cross validation to calculate RMSE values. func is called K times, with each pair
#' of test and training sets, and final returned RMSEs are the average of each result.
#'
#' @param cv_sets List of test and training sets.
#' @param lambdas Vector of lambda values to apply.
#' @param func Function that computes the RMSEs when given a test and train set.
#'
#' @return Mean RMSE of all calculated RMSE values.
calc_kfold_rmses <- function(cv_sets, lambdas, func) {
  cat("calc_kfold_rmses:")
  k_rmses <- sapply(1:K, function(k) {
    cv_test_set <- cv_sets[, k]$test_set
    cv_train_set <- cv_sets[, k]$train_set

    cat(" ", k)
    func(cv_test_set, cv_train_set)
  })

  cat("\n")
  rowMeans(k_rmses)
}

cache_final_cv_sets_filename <- "rda/final_cv_sets.rds"
if (use_cache & file.exists(cache_final_cv_sets_filename)) {
  final_cv_sets <- readRDS(cache_final_cv_sets_filename)
} else {
  final_cv_sets <- create_cv_sets(edx, K)

  if (use_cache) {
    saveRDS(final_cv_sets, cache_final_cv_sets_filename)
  }
}

########################################
#
# Regularized Movie and User Effects Model
#
########################################

#' Fit regularized movie and user effects model.
#'
#' @param train_set Movie data to train on.
#' @param lambda Penalty value to apply for regularization.
#'
#' @return Model consisting of value "mu", frame "b_i" effects per movie, and frame "b_u" effects per user.
fit_model_reg_movie_user <- function(train_set, lambda) {
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu) / (n() + lambda))

  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu) / (n() + lambda))

  list(mu = mu, b_i = b_i, b_u = b_u)
}

#' Predict ratings on data using given model.
#'
#' @param data Movie data to predict against.
#' @param model Obtained from fit_model_reg_movie_user()
#'
#' @return Vector of predicted ratings.
predict_reg_movie_user <- function(test_set, model) {
  test_set %>%
    left_join(model$b_i, by = "movieId") %>%
    left_join(model$b_u, by = "userId") %>%
    mutate(pred = model$mu + b_i + b_u) %>%
    pull(pred)
}

#' For the regularized movie and user effect model, calculate RMSEs for each lamba using cross validation.
#'
#' @param cv_sets List of test and training sets.
#' @param lambdas Vector of lambda values to apply.
#'
#' @return Mean RMSE for each lambda value.
calc_kfold_rmses_reg_movie_user <- function(cv_sets, lambdas) {
  calc_kfold_rmses(cv_sets, lambdas, function(test_set, train_set) {
    sapply(lambdas, function(lambda) {
      model <- fit_model_reg_movie_user(train_set, lambda)
      pred <- predict_reg_movie_user(test_set, model)
      RMSE(test_set$rating, pred)
    })
 })
}

########################################
#
# Regularized Movie and User, and Movie Time Effects Model
#
########################################

#' Given movie data frame/tibble, add a column called "date" that is the timestamp
#' converted to type datetime, rounded to week.
#'
#' @param data Movie data.
#'
#' @return Input data with converted date.
add_week_date <- function(data) {
  data %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "week"))
}

#' Fit regularized movie and user effects model, with movie time effect component.
#'
#' @param train_set Movie data to train on.
#' @param lambda Penalty to apply for regularization
#'
#' @return Model consisting of value "base_model" (regularized movie and user effect model), and
#'  object "loess_models" for movie time effect.
fit_model_movie_time_effect <- function(train_set, lambda) {
  model <- fit_model_reg_movie_user(train_set, lambda)

  # Calculate residuals i.e. values after removing mu, movie and user effects.
  print("Calculating residuals.")
  resids <- train_set %>%
    add_week_date() %>%
    left_join(model$b_i, by = "movieId") %>%
    left_join(model$b_u, by = "userId") %>%
    mutate(resids = rating - model$mu - b_i - b_u)

  print("Fitting loess.")
  movie_ids <- unique(resids$movieId)

  # Get the movie IDs that indicate we are at 10%, 20% etc so we can output some progress.
  progress_len = 100
  output_progress_movie_ids <- movie_ids[seq(1, length(movie_ids), length = progress_len)]
  pb <- txtProgressBar(min = 0, max = progress_len, style = 3)
  loess_models <- sapply(movie_ids, function(movie_id) {
    if (movie_id %in% output_progress_movie_ids) {
      setTxtProgressBar(pb, which(movie_id == output_progress_movie_ids))
    }
    data <- resids %>% filter(movieId == movie_id) %>%
      group_by(date) %>%
      summarise(resids = mean(resids))
    if (nrow(data) > 100) {
       # Only fit if there is a minimum number of points.
       loess(resids ~ as.numeric(date), data=data, control = loess.control(surface = "direct"))
    } else {
       list()
    }
  })

  loess_models <- tibble(movieId = movie_ids, loess_model = loess_models)

  list(base_model = model, loess_models = loess_models)
}

#' Predict ratings on data using given model.
#'
#' @param data Movie data to predict against.
#' @param model Obtained from fit_model_movie_time_effect()
#'
#' @return Vector of predicted ratings.
predict_movie_time_effect <- function(data, model) {

  data_with_date <- data %>%
    add_week_date() %>%
    left_join(model$base_model$b_i, by = "movieId") %>%
    left_join(model$base_model$b_u, by = "userId") %>%
    left_join(model$loess_models, by = "movieId")

  # Calculate time effect.
  time_effect <- apply(data_with_date, 1, function(x) {
    if (length(x$loess_model) == 0) {
      # No loess model, no bias
      0
    } else {
      predict(x$loess_model, x$date)
    }
  })

  # Compute predictions using all components.
  data_with_date %>%
    mutate(time_effect = time_effect) %>%
    mutate(pred = model$base_model$mu + b_i + b_u + time_effect) %>%
    pull(pred)
}

# Cross validation to work out best lambda value.
cache_final_rmses_reg_movie_user_filename <- "rda/final_rmses_reg_movie_user.rds"
if (use_cache & file.exists(cache_final_rmses_reg_movie_user_filename)) {
  final_rmses <- readRDS(cache_final_rmses_reg_movie_user_filename)
} else {
  final_rmses <- calc_kfold_rmses_reg_movie_user(final_cv_sets, lambdas)

  if (use_cache) {
    saveRDS(final_rmses, cache_final_rmses_reg_movie_user_filename)
  }
}
final_best_lambda_reg_movie_user <- lambdas[which.min(final_rmses)]

########################################
#
# Genre Effects Model
#
########################################

#' Calculate genre bias for users.
#'
#' @param data Movie data.
#' @param user_ids Vector of userIds to operate on.
#' @param base_model Base model used to calculate predictions.
#' @param base_predict_func Function that takes the base_model and movie data as input, then calculates predictions.
#
#' @return Data frame with userId, genre, and bias columns.
#'
calc_user_genre_bias <- function(data, user_ids, base_model, base_predict_func) {

  # Filter for only necessary data and then calculate residuals.
  user_data <- data %>% filter(userId %in% user_ids)
  resids <- user_data$rating - base_predict_func(user_data, base_model)

  user_data <- user_data %>%
    # Reduce memory usage by selecting only the columns needed.
    select(userId, movieId, genres) %>%
    mutate(resids = resids) %>%
    # Split genres into individual genre values.
    separate_rows(genres, sep = "\\|") %>%
    # Calculate the number of times a genre has been rated per user.
    group_by(userId, genres) %>%
    mutate(n_genre_per_user = n()) %>%
    ungroup() %>%
    # Divide the residual of a user's movie rating between the genres of the movie,
    # weighted according to how frequently the user has watched movies of a genre.
    group_by(movieId, userId) %>%
    mutate(bias = (resids * n_genre_per_user) / sum(n_genre_per_user)) %>%
    ungroup() %>%
    # For each genre (per user), calculate the bias as the mean of all bias.
    group_by(userId, genres) %>%
    summarise(bias = mean(bias))
  user_data
}

#' Calculate users' biases for genres, breaking data into smaller pieces to reduce memory requirements at the
#' expense of being slower.
#'
#' @param data Movie data.
#' @param base_model Base model used to calculate predictions, from which residuals are calculated.
#' @param base_predict_func Function that takes the base_model and movie data as input, then calculates predictions.
#' @param num_chunks Number of chunks to break data into. 1 indicates to process whole data at once.
#'
#' @return Data frame with variables userId, genres, and bias.
calc_user_genre_bias_chunked <- function(data, base_model, base_predict_func, num_chunks = 1) {

  # Get all user IDs in the data and split if necessary.
  user_ids <- unique(data$userId)
  if (num_chunks == 1) {
    chunks <- list(user_ids)
  } else {
    chunks <- split(user_ids, cut(seq_along(user_ids), num_chunks, labels = FALSE))
  }

  i <- 0
  genre_biases <- lapply(chunks, function(chunk) {
    i <<- i + 1
    cat(" ", i)
    calc_user_genre_bias(data, chunk, base_model, base_predict_func)
  })
  genre_biases <- bind_rows(genre_biases)
  genre_biases
}

#' Predict ratings using base model and user genre bias.
#'
#' @param data Movie data.
#' @param base_model Base model used to calculate predictions, from which residuals are calculated.
#' @param base_predict_func Function that takes the base_model and movie data as input, then calculates predictions.
#' @param user_genre_bias Obtained from calc_user_genre_bias etc.
#' @param num_chunks Number of chunks to break data into. 1 indicates to process whole data at once.
#' 
#' @return Vector of predicted ratings.
predict_genre_base_model <- function(data, base_model, base_predict_func, user_genre_bias, num_chunks = 1) {

  print("Predicting base model values.")
  preds <- base_predict_func(data, base_model)

  print("Applying genre biases.")

  # Split into desired number of chunks.
  chunks <- data %>% group_by((row_number() - 1) %/% (n() / num_chunks)) %>% nest %>% pull(data)
  cat("chunk:")
  i <- 0
  genre_biases <- lapply(chunks, function(chunk) {
    i <<- i + 1
    cat(" ", i)

    # We will break apart genres into separate rows, which will increase memory usage. Before doing that
    # pull out only the columns we need and avoid duplicating unnecessary data.
    # Then join against user's genre biases, and total up per user + movie to come up with a single
    # genre bias value for the user's rating of the movie.
    chunk %>% select(userId, movieId, genres) %>%
      separate_rows(genres, sep = "\\|") %>%
      left_join(user_genre_bias, by = c("userId", "genres")) %>%
      group_by(userId, movieId) %>%
      summarise(bias = sum(bias, na.rm = TRUE))
  })
  genre_biases <- bind_rows(genre_biases)

  preds + genre_biases$bias
}

########################################
#
# Full Model
#
########################################

#' Create model that is combination of regularized movie and user effects, movie time effect, and genre effect.
#'
#' @param train_set Movie data.
#' @param num_chunks To reduce memory requirements, perform calculations on chunks of data. A value of 1 indictes no split.
#' @param lambda Penalty value to apply for regularization.
#'
#' @return Model consisting of value "base_model" (regularized movie and user effect model), and
#'  frame "user_genre_bias".
fit_model_full <- function(train_set, num_chunks, lambda) {

  print("Fitting base model.")
  base_model <- fit_model_movie_time_effect(train_set, lambda)

  print("Generating user-genre biases.")
  user_genre_bias <- calc_user_genre_bias_chunked(train_set, base_model, predict_movie_time_effect, num_chunks)

  list(base_model = base_model, user_genre_bias = user_genre_bias)
}

#' Predict ratings using model (object obtained from fit_model_full).
#'
#' @param data Movie data to predict against.
#' @param model Model obtained from fit_model_full.
#' @param num_chunks To reduce memory requirements, perform calculations on chunks of data. A value of 1 indictes no split.
#'
#' @return Vector of predicted ratings.
predict_full <- function(data, model, num_chunks = 1) {
  predict_genre_base_model(data, model$base_model, predict_movie_time_effect, model$user_genre_bias, num_chunks)
}

########################################
#
# Fit model and predict.
#
########################################

cache_final_model_full_filename <- "rda/final_model_full.rds"
if (use_cache & file.exists(cache_final_model_full_filename)) {
  model_full <- readRDS(cache_final_model_full_filename)
} else {
  model_full <- fit_model_full(edx, 5, final_best_lambda_reg_movie_user)

  if (use_cache) {
    saveRDS(model_full, cache_final_model_full_filename)
  }
}
predicted_ratings <- predict_full(validation, model_full, 5)
final_rmse_full <- RMSE(validation$rating, predicted_ratings)

final_rmse_full
rm(model_full, predicted_ratings)
