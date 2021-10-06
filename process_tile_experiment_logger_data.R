
# Project: Tile experiment

# Title: Generate ecologically relevant variables from the logger data

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

# classic summary statistics for both temperature and light

# 1. maximum 
# 2. minimum 
# 3. range
# 4. mean
# 5. coefficient of variation

# biologically meaningful temperature variables

# 27 degrees is the upper tolerance for F. vesiculosus: https://www.sciencedirect.com/science/article/pii/S0022098115001276?casa_token=MVT95rCJjKIAAAAA:lFAFLcQy7v3NTwhVUV9lpSnPgykRdccV34sWmyvEySd4QdQV1Urlnt61ebUSMHY4402-vMpGhv4)
# Ascophyllum nodosum: https://www.sciencedirect.com/science/article/pii/S1385110105000365?casa_token=iUfVLnbMJ-4AAAAA:6x8Mso59wCSegKSVKVHFGLjFRNB65G4ykDsQ2hYCDBDGvi_C-gvfiRPo-1CBoIra38R0RHlHcXI
# Fucus serratus: https://www.sciencedirect.com/science/article/pii/S1874778713000871?casa_token=9bE-J_MvWoUAAAAA:pCq4lCynQ5V7sAjcERYNef2ksPQXj10j_G8R7roEAeejBO4oEo7qvQTqI_HUtAKPdUv3qYMo0sk
# Fucus spiralis: ?

# 1. total hours over 27 degrees
# 2. mean length of consecutive periods with temperatures over 27 degrees
# 3. longest consecutive period with temperatures over 27 degrees

logvars_summary <- 
  lapply(logvars, function(data) {
    
    df <- 
      data %>%
      mutate(exceed_27 = if_else(temperature_C > 27, 1, 0))
    
    # calculate periods of consecutive hours that exceeded 27 degrees
    x <- rle(df$exceed_27)
    y <- x$lengths[x$values == 1]
    
    z <- 
      df %>%
      group_by(site_code, water_level_treat) %>%
      summarise(hours_exceeding_27 = sum(exceed_27),
                con_hours_exceeding_27_mean = if_else(length(y) == 0, 0, as.numeric(mean(y)) ),
                con_hours_exceeding_27_max = if_else(length(y) == 0, 0, as.numeric(max(y)) ),
                mean_temp_C = mean(temperature_C),
                sd_temp_C = sd(temperature_C),
                max_temp_C = max(temperature_C),
                min_temp_C = min(temperature_C),
                range_temp_C = diff(range(temperature_C)),
                cv_temp_C = sd(temperature_C)/mean(temperature_C),
                mean_light_C = mean(intensity_lux),
                sd_light_C = sd(intensity_lux),
                max_light_C = max(intensity_lux),
                min_light_C = min(intensity_lux),
                range_light_C = diff(range(intensity_lux)),
                cv_light_C = sd(intensity_lux)/mean(intensity_lux))
    
    z
    
  })

# bind this into a data.frame
# ignore the warning
logvars_summary <- 
  logvars_summary %>%
  bind_rows(.) %>% 
  arrange(site_code, water_level_treat)
  
View(logvars_summary)

### END
