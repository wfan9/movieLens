# Functions for creating and saving edx data to local file system,
# and subsequent loading.

default_edx_filename <- "rda/edx.RData"

#' Create edx and validation data sets, and save them to local file system.
edx_create_save <- function(filename=default_edx_filename) {
  source("movie_lens_creation.R")
  save(edx, validation, file=filename)
}

#' Load in edx and validation objects from file.
edx_load_from_file <- function(filename=default_edx_filename) {
  load(filename, envir=.GlobalEnv, verbose=TRUE)
}

#' Ensure the edx and validation data are loaded. Either downloads and creates
#' the data, or loads it from a local cached file.
edx_load_data <- function(filename=default_edx_filename) {
  if (file.exists(filename)) {
    edx_load_from_file(filename)
  } else {
    edx_create_save(filename)
  }
}