
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

# test the time and sea-level with the reported sea-levels from the field
sea_dat %>%
  filter(date_time_CET > as.POSIXct("2021-06-23 11:00:00", tz = "CET"),
         date_time_CET < as.POSIXct("2021-06-23 12:00:00", tz = "CET")) %>%
  View()

# we will need a more robust testhing mechanism once the data are all inputted
# in addition, we will need to decide whether to use the time or the water level

# generate some ecologically meaningful variables from these time-series data
# given a particular water height

# we will have to define the depth of each point relative to the RH2000 standard

# e.g. water level -22, depth + 2

# we should do these calculations for the last 6 years but then also for
# the study period specifically

# we need to make a generalisable function with sea-level data with a date
# and a sea level

# point is -20 below RH2000
x <- 20

df <- sea_dat[1:100, ]
df$row_id <- c(1:nrow(df))
df$water_level_cm %>% summary()
df$dessication_point <- if_else(df$water_level_cm < (x), 1, 0)
df

y <- rle(df$dessication_point)
y

u <- rep(1:length(y$lengths), y$lengths)
u

v <- rep(y$values, y$lengths)
v

df$dessication_groups <- u

df

z <- 
  df[v == TRUE, ] %>%
  group_by(dessication_groups) %>%
  filter(row_id == first(row_id) |
         row_id == last(row_id)) %>%
  summarise(time_diff = as.numeric(diff(date_time_CET)) )

z

diff(z$date_time_CET)
z$date_time_CET[1]
z$date_time_CET[2]

y$values
y$lengths
y



sea_dat$water_level_cm %>% hist()


















