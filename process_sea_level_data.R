
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

# with_tz() with tzone = "Europe/Berlin" harmonises to CET and CEST
head(sea_dat)

# convert time from UTC to CET
sea_dat <- 
  sea_dat %>%
  mutate(date_time_CET = with_tz(date_time_UTC, tzone = "Europe/Berlin")) %>%
  select(date_time_CET, water_level_cm, quality, measure_depth)

head(sea_dat)

sea_dat %>%
  filter(date_time_CET > as.POSIXct("2021-06-23 11:00:00", tz = "CET"),
         date_time_CET < as.POSIXct("2021-06-23 12:00:00", tz = "CET")) %>%
  View()














