
# Project: Tile experiment

# Title: Clean the sea-level data

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2020-06-1"
pkgs <- c("here", "dplyr", "tidyr", "readr", "ggplot2")
groundhog.library(pkgs, groundhog.day)

# load sea level data to export a cleaned version
sea_level_raw <- read_delim(here("sea_level_data/smhi-opendata_13_2130_20211006_152803.csv"),
                            delim = ";", 
                            skip = 7,
                            col_names = c("date_time_UTC", "water_level_cm", "quality", "measure_depth"),
                            col_types = list(col_datetime(), 
                                             col_double(),
                                             col_character(),
                                             col_double(),
                                             col_skip(),
                                             col_skip()))
head(sea_level_raw)

# details regarding the sea-level data

# Havsvattenstånd RH2000, minutvärde (https://www.smhi.se/data/oceanografi/ladda-ner-oceanografiska-observationer#param=sealevelMinutes,stations=all,stationid=2130)

# Accessed: 2021/10/06

# Stationsnummer: 2130 (Kungsvik)

# Latitude (dd): 58.9967
# Longitud (dd): 11.1272
# Driftsatt: 1973-08-01
# I drift till: Aktiv
# Aktiv: Ja
# Mobil: Nej

# quality
# Grön (G) = Kontrollerade och godkända värden (Controlled and approved values)
# Gul (Y) = Grovt kontrollerade värden, misstänkta eller aggregerade värden (Roughly controlled values, suspicious or aggregated values)
# Orange (O) = Okontrollerade värden (Uncontrolled values)




