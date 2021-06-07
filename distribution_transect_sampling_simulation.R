
# Project: Macroalgae BEF

# Title: Simulate landscapes

# load relevant packages
library(dplyr)
library(tidyr)
library(ggplot2)

# set x range
xmin <- 0
xmax <- 0.6

# set y range
ymin <- 0
ymax <- 2

# n random points
n <- 100000

# get random points
x <- runif(n = n, min = xmin, max = xmax)
y <- runif(n = n, min = ymin, max = ymax)

xy_ran <- cbind(x = x, y = y)
head(xy_ran)

# calculate some natural densities

# area of sampling strip
area <- (xmax - xmin) * (ymax - ymin)

# empirical range of holdfast densities
den_range <- seq(5, 150, 2)

# calculate the number of clusters of plants for one natural density
n_plants <- den_range*area
n_plants


sim_dat <- vector("list", length = length(n_plants))
for (j in 1:length(n_plants)) {
  
  # randomly sample
  xy_sp <- as.data.frame(xy_ran[sample(1:n_plants[j]), ])
  xy_sp
  
  # draw size of plant clusters from a poisson distribution
  xy_sp$pa <- rpois(n = n_plants[j], lambda = 1.5)
  
  # add clusters of plants
  xy_sp_list <- split(xy_sp, 1:nrow(xy_sp) )
  
  xy_clust <- 
    lapply(xy_sp_list, function(data){
      
      if(data$pa > 0) {
        
        z <- data.frame(x = data$x + rnorm(n = data$pa, mean = 0, sd = 0.02),
                        y = data$y + rnorm(n = data$pa, mean = 0, sd = 0.02))
        
      } else {
        
        z <- data[,1:2]
        
      }
      
      return(z)
      
    })
  
  xy_clust <- bind_rows(xy_clust, .id = "cluster") 
  
  # check true density
  true_den <- nrow(xy_clust)/area
  
  # benedikt method
  bb_samp <- xy_clust[(xy_clust$x > 0.25 & xy_clust$x < 0.35),  ]
  
  # how many plants to sample?
  nrow(bb_samp)
  
  # estimated density
  bb_den <- nrow(bb_samp)/(0.10*2)
  
  
  # james/lars suggestion
  
  p_10 <- rep(seq(0.10, 2, 0.10), each = 2) + c(-0.01, 0.01)
  loop_seq <- seq(2, length(p_10), 2)
  
  sample_coords <- vector("list", length = length(loop_seq))
  for(i in 1:length(loop_seq)) {
    
    d <- loop_seq[i]
    
    z <- xy_clust[(xy_clust$x > 0.10 & xy_clust$x < 0.40), ]
    
    test_z <- (z$y > (p_10[d-1]) & z$y < p_10[d])
    
    if (sum(test_z) > 0) {
      
      z1 <- z[(z$y > (p_10[d-1]) & z$y < p_10[d]), ]
      
    } else {
      
      z1 <- NULL
      
    }
    
    sample_coords[[i]] <- z1
    
  }
  
  jg_samp <- bind_rows(sample_coords)
  
  # how many plants to sample?
  nrow(jg_samp)
  
  # estimated density
  a <- length(loop_seq)*(0.4*(0.02))
  
  jg_den <- nrow(jg_samp)/a
  
  # output data
  
  df <- data.frame(method = c("true", "bb", "jg"),
             number_plants = c(nrow(xy_clust), nrow(bb_samp), nrow(jg_samp)),
             density = c(true_den, bb_den, jg_den))
  
  sim_dat[[j]] <- df
  
}
  
bind_rows(sim_dat, .id = "sim") %>%
  pivot_longer(cols = c("number_plants", "density"),
               names_to = "var",
               values_to = "val") %>%
  ggplot(data = .,
         mapping = aes(x = method, y = val)) +
  geom_jitter(width = 0.1) +
  geom_hline(yintercept = 304, colour = "red") +
  facet_wrap(~var, scales = "free")

bind_rows(sim_dat, .id = "sim") %>% 
  group_by(method) %>%
  summarise(mean = mean(number_plants))

bind_rows(sim_dat, .id = "sim") %>% 
  group_by(method) %>%
  summarise(mean = mean(density))


