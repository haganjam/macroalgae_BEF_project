
# Project: Tile experiment

# Title: Function to fix the date variable

# fix the date and time variables

# this function is useful in combination with dplyr's group_by and mutate
# it works by assuming that we only filled in the first date for a certain tile
# it then fills in the rest of that tile with the date associated with that tile

# x is a date column

# write a function to do this
date_fixer <- function(x) {
  
  if( sum(is.na(x)) == length(x) ) {
    
    # if the tile has not date, then we keep it as an NA
    y <- NA
    
  } else {
    
    # get the unique dates in the group
    z <- unique(x)[!is.na(unique(x))]
    
    # if the values are NAs, fill in the date from the group
    # if there are more than one date, then sample them randomly
    # and fill them in
    y <- sapply(x, function(w) {
      
      if_else(is.na(w), z[sample(1:length(z), 1)], w)
      
    })
    
  } 
  
  return(y)
  
}

# example code:
# use the date_fixer function to fill in missing dates
# init_dat <- 
  # init_dat %>%
  # group_by(tile_id) %>%
  # mutate(date_corrected = date_fixer(date) )

### END
