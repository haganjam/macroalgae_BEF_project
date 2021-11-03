
# Project: Tile experiment

# Title: Calibrate the Viva water level measurements with the published water level measurements

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2020-06-1"
pkgs <- c("here", "dplyr", "readr", "glmnet", "tidyr", "mgcv", "ggplot2", "lubridate")
groundhog.library(pkgs, groundhog.day)

# load the calibration data
sea_cal <- read_csv(file = here("analysis_data/sea_level_viva_calibration_data.csv"))
head(sea_cal)
str(sea_cal)

# add a row.id variable
sea_cal$row_id <- 1:nrow(sea_cal)

# samples are horribly autocorrelated
paste(hour(sea_cal$date_time_CET), date(sea_cal$date_time_CET)) %>%
  unique() %>%
  length()

set.seed(135830153)
sea_sub <- 
  sea_cal %>%
  group_by(hour(date_time_CET), date(date_time_CET)) %>%
  sample_n(size = 1) %>%
  ungroup()

# plot the data
ggplot(data = sea_sub,
       mapping = aes(x = water_level_cm_viva, y = water_level_cm)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm", colour = "red") +
  theme_classic()

gam.1 <- 
  mgcv::gam(water_level_cm ~ s(water_level_cm_viva, k=-1,fx=FALSE,bs="tp",m=NA,by=NA,xt=NULL,id=NULL,sp=NULL,pc=NULL),
            data = sea_sub, family = gaussian(), method = "REML")
par(mfrow = c(2,2))
gam.check(gm.1)

lm.1 <- 
  lm(water_level_cm ~ water_level_cm_viva, data = sea_sub)
par(mfrow = c(2, 2)) 
plot(lm.1)

lm.2 <- 
  lm(water_level_cm ~ poly(water_level_cm_viva, 2), data = sea_sub)
par(mfrow = c(2, 2)) 
plot(lm.2)

lm.null <- 
  lm(water_level_cm ~ 1, data = sea_sub)

# test the predictions of these models on the rest of the data
sea_test <- sea_cal[!(sea_cal$row_id %in% sea_sub$row_id), ]

x <- sea_test[, "water_level_cm_viva"]

# collate the models into a list
mod.list <- c("gam.1", "lm.1", "lm.2", "lm.null")

# create an output list for the different models
pred.out <- vector("list", length = length(mod.list))

for(i in 1:length(mod.list)) {
  
  # get the predicted values 
  y <- predict(get(mod.list[i]), newdata = x)
  
  # calculate root mean square error
  rmse <- sqrt( sum( ((x[[1]] - y)^2)/length(y) ) )
  
  # calculate mean absolute error
  mae <- ( sum( abs( (x[[1]] - y) ) ) )/length(y)
  
  pred.out[[i]] <- data.frame(model = mod.list[i], 
                              metric = c("rmse", "mae"),
                              value = c(rmse, mae))
  
}

mod.summary <- bind_rows(pred.out)

# the gam has the lowest rmse
mod.summary %>%
  filter(metric == "rmse") %>%
  filter(value == min(value))

# the gam has the lowest mae
mod.summary %>%
  filter(metric == "mae") %>%
  filter(value == min(value)) 

# save the gam model
saveRDS(gam.1, file = here("analysis_data/sea_cal_gam.rds"))

### END
