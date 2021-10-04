
# Project: Macroalgae BEF

# Title: Loading Google sheets and other files on the Google Drive directly into R

# Loading Google Sheets using googlesheets4

# an overview of the package can be found:
# https://googlesheets4.tidyverse.org/

# install the googlesheets4 package
install.packages("googlesheets4")

# load the googlesheets4 package
library(googlesheets4)

# read a Google sheet
allo_dat <- read_sheet("https://docs.google.com/spreadsheets/d/167zCNjbmZ1PV5V1vZeGe9QcIrvhJ4diZ8q2rF9Ggry0/edit#gid=0", sheet = "Sheet1")

# select Yes to authenticate the package to access your Google sheets
# you only have to do this once per session (I think)

# read the metadata sheet
meta_dat <- read_sheet("https://docs.google.com/spreadsheets/d/167zCNjbmZ1PV5V1vZeGe9QcIrvhJ4diZ8q2rF9Ggry0/edit#gid=0", sheet = "metadata")


# Loading files directly from the Google Drive

# Example based on loading the logger data on the Google Drive

# load relevant libraries
library(googledrive)
library(here)

# make a folder to download the tile data
if(! dir.exists(here("tile_logger_data"))){
  dir.create(here("tile_logger_data"))
}

# get a list of the logger data files in the Google Drive
file_id <- drive_ls(path = "hobo_logger_tiles")

# subset the csv files
file_names <- file_id$name[grepl(pattern = ".csv", file_id$name) ]

# write a loop to download the files onto the local machine
for(i in 1:length(file_names)) {
  
  drive_download(file = file_names[i], 
                 path = paste(here("tile_logger_data"),"/", file_names[i], sep = "") )
  
}

### END
