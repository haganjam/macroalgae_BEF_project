
# Project: Tile experiment

# Title: Download the logger data from the google drive

# load relevant libraries
library(googledrive)
library(here)

# get the file names of the raw data files


# get a list of the logger data files in the Google Drive
file_id <- drive_ls(path = "hobo_logger_tiles")

# subset the csv files
file_names <- file_id$name[grepl(pattern = ".csv", file_id$name) ]

# write a loop to download the files onto the local machine
for(i in 1:length(file_names)) {
  
  drive_download(file = file_names[i], 
                 path = paste(here("tile_logger_data"),"/", file_names[i], sep = "") )
  
}

