#'
#' @title: Unzip the ResearchBox data and save it in the current directory
#' 
#' @description: This script unzips the ResearchBox fie downloaded from 
#' the our ResearchBox that stores all the raw data from the project. The raw data 
#' are stored at the following URL: https://researchbox.org/435&PEER_REVIEW_passcode=ECOTGX.
#' You should go to the research box and click the button: "Download Entire Box". The
#' file that is downloaded is a .zip file. This should be stored in your current
#' working directory.
#' 
#' @authors: James G. Hagan (james_hagan(at)outlook.com)
#' 
#' 
#' 

#In the following scripts these packages will be used, here is a list of all of them.
#install.packages(c("dplyr", "readr", "ggplot2", "ggbeeswarm", "lubridate", "ggpubr", "ggforce", "gghalves", "MuMIn", "lme4", "lmerTest", "emmeans", "ggfortify", "car", "ggdist", "tidyr", "stringr", "here"))



# load relevant libraries
require(here)

# unzip the file
unzip(zipfile = here("ResearchBox_435.zip"),
      exdir = here())

# remove the zip file
unlink(here("ResearchBox_435.zip"))

### END
