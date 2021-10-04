
# Project: Tile experiment

# Title: Generate ecologically relevant variables from the logger data

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2021-10-04"
pkgs <- c("here", "dplyr", "tidyr", "readr", "ggplot2")
groundhog.library(pkgs, groundhog.day)

# Next steps...

# Fix the problems with the dates because there are only four dates

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