
# Project: Tile experiment

# Title: Calibrate the Viva water level measurements with the published water level measurements

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2020-06-1"
pkgs <- c("here", "dplyr", "readr", "glmnet")
groundhog.library(pkgs, groundhog.day)

library(glmnet)

as.matrix(allo_dat.fu_ve$cyl_vol)

cv.out <- 
  cv.glmnet(x = matrix(c(log(allo_dat.fu_ve$cyl_vol), (allo_dat.fu_ve$len_circum_ratio)),
                       ncol = 2, nrow = nrow(allo_dat.fu_ve)), 
          y = as.matrix(log(allo_dat.fu_ve$dry_weight_g) ),
          nfolds = 5, alpha = 0,
          type.measure = "mse")

cv.out

cv.out$lambda.min

coef(cv.out, s = "lambda.min") %>%
  as.matrix()

df[,1]

glmnet(x = matrix(c(log(allo_dat.fu_ve$cyl_vol), (allo_dat.fu_ve$len_circum_ratio)),
                      ncol = 2, nrow = nrow(allo_dat.fu_ve)), 
       y = as.matrix(log(allo_dat.fu_ve$dry_weight_g) ), 
      alpha = 0,
      lambda = cv.out$lambda.min)

print(cv.out)
coef(cv.out)



set.seed(1010)
n = 1000
p = 100
nzc = trunc(p/10)
x = matrix(rnorm(n * p), n, p)
beta = rnorm(nzc)
fx = x[, seq(nzc)] %*% beta
eps = rnorm(n) * 5
y = drop(fx + eps)
px = exp(fx)
px = px/(1 + px)
ly = rbinom(n = length(px), prob = px, size = 1)
set.seed(1011)
cvob1 = cv.glmnet(x, y)


x

dim(x)
length(y)

plot(cvob1)
coef(cvob1)

coef(cvob1, s = "lambda.min") %>%
  as.matrix()

predict(cvob1, newx = x[1:5, ], s = "lambda.min")
title("Gaussian Family", line = 2.5)
set.seed(1011)
cvob1a = cv.glmnet(x, y, type.measure = "mae")
plot(cvob1a)
title("Gaussian Family", line = 2.5)
set.seed(1011)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 4, 1))
cvob2 = cv.glmnet(x, ly, family = "binomial")
plot(cvob2)
title("Binomial Family", line = 2.5)
frame()
set.seed(1011)
cvob3 = cv.glmnet(x, ly, family = "binomial", type.measure = "class")
plot(cvob3)
title("Binomial Family", line = 2.5)
## Not run: 
cvob1r = cv.glmnet(x, y, relax = TRUE)
plot(cvob1r)
predict(cvob1r, newx = x[, 1:5])
set.seed(1011)
cvob3a = cv.glmnet(x, ly, family = "binomial", type.measure = "auc")
plot(cvob3a)
title("Binomial Family", line = 2.5)
set.seed(1011)
mu = exp(fx/10)
y = rpois(n, mu)
cvob4 = cv.glmnet(x, y, family = "poisson")
plot(cvob4)
title("Poisson Family", line = 2.5)
