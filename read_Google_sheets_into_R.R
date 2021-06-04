
# Project: Macroalgae BEF

# Title: Loading Google sheets directly into R

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

### END
