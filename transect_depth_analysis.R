
# Project: Functional value of macroalgal biodiversity

# Title: Species x depth gradient

# load relevant libraries
library(googlesheets4)
library(dplyr)
library(here)
library(slider)


# import the transect data

# import the raw data from the Google Sheet
tra_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1H2_RtGusheNz6ibKzoCqLrEAYURznQuDS1bPCPnF_SA/edit#gid=0", sheet = "density_data",
                       col_types = c("cccnnncnncc"),
                       na = c("NA"))


# make a corrected depth column
tra_dat %>%
  View()

zoo::rollapply(c(1, 2, 3, 4, 5), width = 2, FUN = mean)

diff_func <- function(x) {
  
  x[length(x)] - x[1]
  
}


y <- 
  tra_dat %>% 
  mutate(depth_correct = (water_level_cm + depth) ) %>%
  select(date, transect_id, position, depth_correct) %>%
  distinct() %>%
  filter(!is.na(depth_correct)) %>%
  filter(transect_id == "1") %>%
  summarise(position_id = c(zoo::rollapply(data = position, width = 2, FUN = function(y)y[length(y)]  )),
            m = (zoo::rollapply(data = depth_correct, width = 2, FUN = diff_func))/(zoo::rollapply(data = position, width = 2, FUN = diff_func)*10 ) ,
            con = zoo::rollapply(data = depth_correct, width = 2, FUN = first))

z <- 
  tra_dat %>% 
  mutate(depth_correct = (water_level_cm + depth) ) %>%
  select(date, transect_id, position, depth_correct) %>%
  distinct() %>%
  filter(!is.na(depth_correct)) %>%
  filter(transect_id == "1") %>%
  mutate(seq_x = slide(position, function(x){seq(x[1], x[length(x)], 1)*10 }, .before = 1, .after = 0, .complete = TRUE  ) ) %>%
  tidyr::unnest(cols = c("seq_x")) %>%
  rename(position_id = position)

y

z

full_join(z, y, by = "position_id") %>%
  mutate(depth_interpolate = m*seq_x + con)

tra_dat %>% 
  mutate(depth_correct = (water_level_cm + depth) ) %>%
  select(date, transect_id, position, depth_correct) %>%
  distinct() %>%
  filter(transect_id == "1")



abs(zoo::rollapply(data = x, width = 2, FUN = diff))

abs(zoo::rollapply(data = position, width = 2, FUN = diff))
