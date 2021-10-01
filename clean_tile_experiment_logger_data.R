
# Project: Tile experiment

# Title: Clean the logger data from the tile experiment

# load relevant libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(here)
library(stringr)

# get a list of files in the tile_logger_data
log_files <- list.files(path = here("tile_logger_data"))

# set-up a list to retrieve these data tiles
log_dat_list <- vector("list", length = length(log_files))

# run a loop to read in all the files and write them into a list
for (i in 1:length(log_files)) {
  
  # load one of the datasets
  d1 <- read_csv(paste(here("tile_logger_data"), "/", log_files[i], sep = ""), skip = 1)
  
  # only select certain columns
  names(d1)
  d1 <- d1[, c(2, 3, 4)]
  names(d1) <- c("date_time", "temperature_C", "intensity_lux")
  
  # write a function to split the variables
  choose_split <- function(x, splitter, n_splits, n_choice) {
    y <- str_split_fixed(string = x, pattern = splitter, n = n_splits)
    y[n_choice]
  }
  
  # add a date column
  d1$date <- 
    sapply(d1$date_time, function(y) 
    { choose_split(x = y, splitter = " ", n_splits = 2, n_choice = 1)}, USE.NAMES = FALSE )
  
  d1$date <- as.Date(d1$date, "%d/%m/%y")
  
  # add a date column
  d1$time <- 
    sapply(d1$date_time, function(y) 
    { choose_split(x = y, splitter = " ", n_splits = 2, n_choice = 2)}, USE.NAMES = FALSE )
  
  # decompose the filenames
  fname <- sub("..[0-9]+", "", sub(".csv", "", log_files[i] ))
  
  # add a tilename
  d1$tile <- choose_split(x = fname, splitter = "", n_splits = 2, n_choice = 1)
  
  # add a water height
  d1$height <- choose_split(x = fname, splitter = "", n_splits = 2, n_choice = 2)
  
  # reorganise the columns
  d1 <- 
    d1 %>%
    select(tile, height, date, time, temperature_C, intensity_lux)
  
  # write the file into a list
  log_dat_list[[i]] <- d1
  
}

log_dat_list









