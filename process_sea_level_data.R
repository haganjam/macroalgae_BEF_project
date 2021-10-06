
# Project: Tile experiment

# Title: Generate ecologically relevant variables from the sea-level data

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2020-06-1"
pkgs <- c("here", "dplyr", "tidyr", "readr", "ggplot2", "lubridate")
groundhog.library(pkgs, groundhog.day)

# check the loaded packages for their correct versions
sessionInfo()

# download the raw sea level data from ResearchBox: https://researchbox.org/435&PEER_REVIEW_passcode=ECOTGX
# save this into a folder called sea_level_data

# make a folder to export the cleaned data
if(! dir.exists(here("sea_level_data"))){
  print("make a folder called sea_level_data in the working directory and save the raw sea level data file into this directory")
}

# load the raw sea level data
sea_dat <- read_csv(here("sea_level_data/sea_level_data_raw_2015_2021.csv"))

# convert the UTC time into CEST to harmonise with the field measurements

# check the time zone names available
OlsonNames()

# we will essentially to test these in a time-zone specific way... could be tricky...

sea_dat$date_time_UTC[2]
with_tz(sea_dat$date_time_UTC[2], tzone =  "Europe/Berlin")

sea_dat$date_time_UTC[2]
with_tz(sea_dat$date_time_UTC[2], tzone =  "Etc/GMT-2")


sea_dat$date_time_UTC[1]
as.POSIXct(sea_dat$date_time_UTC[1], tzone = "Europe/Berlin")
















