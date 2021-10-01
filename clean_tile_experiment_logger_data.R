
# Project: Tile experiment

# Title: Clean the logger data from the tile experiment

# Next steps...

# Fix the problems with the dates because there are only four dates

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
  
  d1$date <- as.Date(d1$date, "%d/%m/%Y")
  
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

# bind this into a data.frame
log_data <- bind_rows(log_dat_list, .id = "tile_id")
head(log_data)

# which variables would we like to generate?

# classic temperature data

# maximum temperature
# minimum temperature
# temperature range
# mean temperature
# coefficient of variation in temperature

# biologically meaningful temperature variables

# 1. time spent above 27 degrees (upper tolerance for F. vesiculosus: https://www.sciencedirect.com/science/article/pii/S0022098115001276?casa_token=MVT95rCJjKIAAAAA:lFAFLcQy7v3NTwhVUV9lpSnPgykRdccV34sWmyvEySd4QdQV1Urlnt61ebUSMHY4402-vMpGhv4)
# Ascophyllum might have similar thermal limits: https://www.sciencedirect.com/science/article/pii/S1385110105000365?casa_token=iUfVLnbMJ-4AAAAA:6x8Mso59wCSegKSVKVHFGLjFRNB65G4ykDsQ2hYCDBDGvi_C-gvfiRPo-1CBoIra38R0RHlHcXI
# Fucus serratus: https://www.sciencedirect.com/science/article/pii/S1874778713000871?casa_token=9bE-J_MvWoUAAAAA:pCq4lCynQ5V7sAjcERYNef2ksPQXj10j_G8R7roEAeejBO4oEo7qvQTqI_HUtAKPdUv3qYMo0sk
# Fucus spiralis?

# 5th July - 15th August

# subset the correct dates
log_data$date %>% unique()
log_data %>%
  filter(date > as.Date("2021-07-25")) %>%
  filter(date < as.Date("2021-08-15"))

# explore the data
log_data %>%
  filter(temperature_C > 40) %>%
  View()

hist(log_data$temperature_C)








