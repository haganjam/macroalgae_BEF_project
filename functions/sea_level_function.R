
# Project: Tile experiment

# Title: Function to generate ecologically relevant variables related to sea level

# arguments:

# focal_depth - depth relative to the RH2000 standard (cm)

# sea_data - data containing sea level relative to the RH2000 standard
# - date_col - name of the column containing the date and time in "POSIXct" format
# - sea_level_col - name of the column specifying the water level in cm relative to the RH2000 standard

# start_date - date to start calculating summary statistics in "POSIXct" format
# end_date - date to stop calculating summary statistics in "POSIXct" format
# - note: if not start and end dates are supplied then the entire time series will be used

# output_variable: 
# - "time_submerged_mins"
# - "time_exposed_mins"
# - "mean_length_submerged_mins"
# - "mean_length_exposed_mins"
# - "sd_length_exposed_mins"
# - "frequency_dessication_2_hours" (per week)
# - "top_5%_dessication_length_mins"

sea_level_func <- function(focal_depth, sea_data, date_col, sea_level_col, start_date = NA, end_date = NA, output_variable) {
  
  # check that the correct version of dplyr is loaded
  l <- sessionInfo()
  
  if ( ("dplyr" %in% names(l$otherPkgs)) ) {
    
    m <- l$otherPkgs[names(l$otherPkgs) == "dplyr"][[1]]
    
  } else {
    
    stop("error, this function requires dplyr to run")
    
  }
  
  # run a warning if the dplyr version is different
  if( m$Version != "1.0.0" ) {
    
    warning("this function was written using dplyr 1.0.0")
    
  }
  
  # check that the date_col is in the correct format otherwise stop
  if(class(sea_data[[date_col]])[1] != "POSIXct") {
    
    stop("error, make sure the date_col is in the correct format: POSIXct")
    
  }
  
  # check that the sea_level_col is in the correct format otherwise stop
  if(class(sea_data[[sea_level_col]]) != "numeric") {
    
    stop("error, make sure the sea_level_col is in the correct format: numeric")
    
  }
  
  # make sure start dates are present and in the correct format
  # otherwise use the first row in the sea_data data
  if (class(start_date)[1] == "POSIXct") {
    
    d1 <- start_date
    
  } else {
    
    d1 <- sea_data[1, date_col][[date_col]]
    
  }
  
  # make sure end dates are present and in the correct format
  # otherwise use the final row in the sea_data data
  if (class(end_date)[1] == "POSIXct") {
    
    d2 <- end_date
    
  } else {
    
    d2 <- sea_data[nrow(sea_data), date_col][[date_col]]
    
  }
  
  # subset the sea_data based on the start and end dates
  df <- sea_data[, c(date_col, sea_level_col)]
  names(df) <- c("date_time", "water_level_cm")
  
  # filter the data to match the chosen timeframe
  df <- 
    df %>%
    filter(date_time > d1, date_time < d2)
  
  sapply(focal_depth, function(x) {
    
    # classify points as either below (submerged, 0) or above water (out of water, 1)
    # here, I could just add the standard error estimates to focal depth
    
    # i.e. using just the prediction or using the error range as well
    
    df$dessication_point <- if_else( (x - df$water_level_cm) > 0 , 1, 0)
    
    # get the time difference in minutes between each point
    df$time_mins <- c(as.double(diff(df$date_time), units = "mins"), NA)
    
    # calculate the number of dessicated points
    y <- rle(df$dessication_point)
    u <- rep(1:length(y$lengths), y$lengths)
    v <- rep(y$values, y$lengths)
    
    # make a variable for each period of either above or below water
    df$groups <- u
    
    # classify these periods as above or below water
    df$above_below <- v
    
    # output the response variable of interest
    if(output_variable == "time_submerged_mins"){
      
      df.x <- 
        df %>%
        filter(above_below == 0)
      
      output <- sum(df.x$time_mins, na.rm = TRUE)
      
    } else if(output_variable == "time_exposed_mins") {
      
      df.x <- 
        df %>%
        filter(above_below == 1)
      
      output <- sum(df.x$time_mins, na.rm = TRUE)
      
    } else if(output_variable == "mean_length_submerged_mins") {
      
      df.x <- 
        df %>%
        filter(above_below == 0) %>%
        group_by(groups) %>%
        summarise(length_below_water_mins = sum(time_mins, na.rm = TRUE))
      
      output <- mean(df.x$length_below_water_mins, na.rm = TRUE)
      
    } else if(output_variable == "mean_length_exposed_mins") {
      
      df.x <- 
        df %>%
        filter(above_below == 1) %>%
        group_by(groups) %>%
        summarise(length_below_water_mins = sum(time_mins, na.rm = TRUE))
      
      output <- mean(df.x$length_below_water_mins, na.rm = TRUE)
      
    } else if(output_variable == "sd_length_exposed_mins") {
      
      df.x <- 
        df %>%
        filter(above_below == 1) %>%
        group_by(groups) %>%
        summarise(length_below_water_mins = sum(time_mins, na.rm = TRUE))
      
      output <- sd(df.x$length_below_water_mins, na.rm = TRUE)
      
    } else if(output_variable == "frequency_dessication_2_hours") {
      
      y <- as.double(diff(c(d1, d2)), units = "mins")
      z <- ((y/60)/24)/7
      
      df.x <- 
        df %>%
        filter(above_below == 1) %>%
        group_by(groups) %>%
        summarise(length_below_water_mins = sum(time_mins, na.rm = TRUE), .groups = "drop") %>%
        filter(length_below_water_mins > 120)
      
      output <- nrow(df.x)/z
      
    } else if(output_variable == "top_5%_dessication_length_mins") {
      
      df.x <- 
        df %>%
        filter(above_below == 1) %>%
        group_by(groups) %>%
        summarise(length_below_water_mins = sum(time_mins, na.rm = TRUE))
      
      y <- quantile(df.x$length_below_water_mins, 0.95)
      
      output <- mean(df.x$length_below_water_mins[df.x$length_below_water_mins > y], na.rm = TRUE)
      
    } else {
      
      stop("error, select an output variable")
      
    }
    
    return(output)
    
  })
  
}

### END
