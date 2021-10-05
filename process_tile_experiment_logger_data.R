
# Project: Tile experiment

# Title: Generate ecologically relevant variables from the logger data

# Next steps:

# - calculate summary statistics for light intensity
# - calculate the length of time spent continuously over 27 degrees


# load libraries using groundhog
library(groundhog)
groundhog.day <- "2020-06-1"
pkgs <- c("here", "dplyr", "tidyr", "readr", "ggplot2")
groundhog.library(pkgs, groundhog.day)

# check the loaded packages for their correct versions
sessionInfo()

# download the 20 raw logger data from ResearchBox: https://researchbox.org/435&PEER_REVIEW_passcode=ECOTGX
# save this into a folder called tile_logger_data

# make a folder to export the cleaned data
if(! dir.exists(here("tile_logger_data"))){
  print("make a folder called tile_logger_data in the working directory and save the 20 raw data files into this folder see ReadMe for more details")
}

# load the files from the tile_logger_data folder
logdat_names <- list.files(here("tile_logger_data"), pattern = ".csv")

logdat_list <- vector("list", length = length(logdat_names))
for(i in 1:length(logdat_list) ) {
  
  logdat_list[[i]] <- read_csv(file = paste(here("tile_logger_data"), "/", logdat_names[i], sep = "" ),
                               col_types = list(site_code = col_character(), 
                                                water_level_treat = col_character(),
                                                date = col_date(),
                                                time = col_time(),
                                                temperature_C = col_double(),
                                                intensity_lux = col_double()))
  
}

# the logger did not record changes in the date
lapply(logdat_list, function(x) { x$date %>% unique()  } )
lapply(logdat_list, function(x) { x$date[1]  } )
lapply(logdat_list, function(x) { x$time[1]  } )
lapply(logdat_list, function(x) {tail(x) } )


# add a corrected date column

# get rid of the final rows until the previous 00:00

# select how many rows to search backward
test_back <- 24

# use lapply to implement this across all the different datasets
logdat_list_trim <- 
  
  lapply(logdat_list, function(data) {
  
  df <- data
  nr <- nrow(df)
  
  sq <- (nr-test_back):nr
  for (i in sq) {
    
    if (df[i, ]$time == 00:00) {
      
      row_stop <- i
      
    }
    
  }
  
  # correct for cases where this passes more than two 00:00
  row_stop <- ifelse(length(row_stop) > 1, row_stop[1], row_stop)
  
  # subset rows from 1 to the final row with a 00:00
  df[1:row_stop, ]
  
})

# check if this worked
lapply(logdat_list_trim, function(x) {tail(x) } )

# logically select 00:00 values, then count hours between those
# we then cumulatively add days whenever a new 00:00 is encountered

logdat_date_corrected <- 
  
  lapply(logdat_list_trim, function(data) {
    
    # get a vector of TRUE and FALSES for midnight
    y <- data$time[-1] == 00:00
    
    # use diff() and which() to get the number of values between two trues
    # https://stackoverflow.com/questions/31848404/efficient-way-of-counting-false-between-true
    v <- diff( which( c(TRUE, y, TRUE)) )
    
    # replicate each value a set number of times
    z <- rep(0:(length(v)-1), times = v)
    
    # test
    if (nrow(data) != length(z)){
      stop("row numbers and added days vectors are not the same length")
    }
    
    data$date_corrected <- (data$date[1] + z)
    
    data %>%
      select(site_code, water_level_treat, date, date_corrected, time, temperature_C, intensity_lux)
    
  })

# list of logger data with corrected dates
logdat_date_corrected

# these should be be cross referenced with the direct hobo-logger data at some point

# subset out time periods before and after the measurements were done
logvars <- 
  lapply(logdat_date_corrected, function(df) {
    
    df %>%
      filter(date_corrected < as.Date("2021-08-15"), date_corrected > as.Date("2021-07-05")) %>%
      select(-date)
    
  })

# check the output
logvars[[1]]$date_corrected %>% range()  


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

# calculate several temperature summary statistics
# also calculate the number of hours spent over 27 degrees
logvars[[1]] %>%
  mutate(exceed_27 = if_else(temperature_C > 27, 1, 0)) %>%
  group_by(site_code, water_level_treat) %>%
  summarise(hours_exceeding_27 = sum(exceed_27),
            mean_temp_C = mean(temperature_C),
            sd_temp_C = sd(temperature_C),
            max_temp_C = max(temperature_C),
            min_temp_C = min(temperature_C),
            range_temp_C = diff(range(temperature_C)),
            cv_temp_C = sd(temperature_C)/mean(temperature_C))


# next, calculate the longest period spent over 27 degrees


x <- 
  df %>%
  mutate(date_time = paste(date_corrected, time)) %>%
  group_by(site_code, water_level_treat, date_corrected) %>%
  filter(time == first(time) | time == last(time)) %>%
  summarise(min_time = first(date_time) ,
            max_time = last(date_time) ) %>%
  ungroup()

y <- mapply(difftime, y$max_date_time, y$min_date_time)

z <- as.numeric(y)

x$hours_27_over <- z






df %>%
  group_by(site_code, water_level_treat, date_corrected) %>%
  filter(time == first(time) | time == last(time)) %>%
  summarise(min_time = as.character(first(time)),
            max_time = as.character(last(time))) %>%
  ungroup() %>%
  mutate(min_date_time = print(paste(date_corrected, min_time)),
         max_date_time = print(paste(date_corrected, max_time)) ) %>%
  select(-min_time, -max_time) %>%
  mutate(time_difference = difftime(max_date_time, min_date_time, units = "secs"))

y <-
  df %>%
  group_by(site_code, water_level_treat, date_corrected) %>%
  filter(time == first(time) | time == last(time)) %>%
  summarise(min_time = as.character(first(time)),
            max_time = as.character(last(time))) %>%
  ungroup() %>%
  mutate(min_date_time = print(paste(date_corrected, min_time)),
         max_date_time = print(paste(date_corrected, max_time)) ) %>%
  select(-min_time, -max_time)



difftime(y$min_date_time[1], y$max_date_time[1])

  group_by(site_code, water_level_treat, date_corrected) %>%
  filter(time == first(time) | time == last(time)) %>%
  summarise(min_time = as.character(first(date_time)) ,
         max_time = as.character(last(date_time)) ) %>%
  mutate(time_difference = difftime(max_time, min_time) ) %>%
  ungroup()

x <- 
  df %>%
  mutate(date_time = paste(date_corrected, time)) %>%
  group_by(site_code, water_level_treat, date_corrected) %>%
  filter(time == first(time) | time == last(time)) %>%
  summarise(min_time = first(date_time) ,
            max_time = last(date_time) )

x$min_time[1]


df1 <- 
  df %>%
  mutate(date_time = paste(date_corrected, time))

df1

difftime(df1$date_time[1], df1$date_time[3])

strptime( paste(dat[,1], dat[,2]), "%Y-%m-%d %H:%M:%S")

difftime( c(df$time[3], df$time[1]) )

logvars[[1]]$time[3] -  logvars[[1]]$time[1]




