#'
#' @title: get_groundhog_date()
#' 
#' @description: Simple function where we can choose the date once and all subsequent
#' package loads from the groundhog package will use the same date.
#' 
#' @authors: James G. Hagan (james_hagan(at)outlook.com)
#' 
#' @param date - date string in the format: "YYYY-MM-DD"
#' 

get_groundhog_date <- function(date = "2022-01-17") {
  return(date)
}

### END
