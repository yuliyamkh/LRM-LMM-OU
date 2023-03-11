library(broom)
library(dplyr)
library(ggplot2)

set.seed(123)

# a single simulation (Linear Regression)

n_languages <- 150

predictor <- seq(0.3, 4.2, len=n_languages)

b0 <- 0.18
b1 <- 2
sd <- 2
error <- rnorm(n_languages, mean = 0, sd = sd)

response <- b0 + b1*predictor + error

dat <- data.frame(predictor, response)

model_fit <- lm(response ~ predictor, data = dat)
summary(model_fit)

# Function for the data simulating and model fitting process
sim_LR <- function(n = 150, from_n = 0.3, 
                   to_n = 4.2, b0 = 0.18, b1 = 2, sigma = 2) {
  predictor <- seq(from_n, to_n, len = n)
  error <- rnorm(n, mean = 0, sd = sigma)
  response <- b0 + b1*predictor + error
  dat.sim <- data.frame(predictor, response)
  model.fit <- lm(response~predictor, data = dat.sim)
  model.fit
}

# Test
set.seed(123)
sim_LR()

# Run the simulation many times (here, 1000)
# Result: a list of fitted LR models based on data simulated
# from the parameters I have set
sims <- replicate(1000, sim_LR(), simplify = FALSE)

# First three models
sims[c(1, 2, 3)]

