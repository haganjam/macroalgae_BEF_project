####Libraries####
library(googlesheets4)
library(ggpubr)
library(dplyr)
library(Hmisc)

######Data Import#####
rec_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1E6GlAiaGzwFhqGQT28uVcoEix2-A3FTLbMND8EHrG2I/edit#gid=0",
                      na="-9999")


cor_dat=rec_dat %>% select(dry_weight_g,receptacle_dry_weight_g,length_cm,circum_cm,receptacle_mean_size_cm,n_receptacles)

rcorr(as.matrix(cor_dat))


plot(cor_dat)
