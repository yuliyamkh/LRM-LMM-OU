library(purrr)
library(broom)
library(dplyr)
library(ggplot2)

set.seed(157)

# a single simulation (Linear Regression)

n_languages <- 1500

predictor <- seq(0.3, 4.2, len=n_languages)

b0 <- 0.8
b1 <- 5
sd <- 2
error <- rnorm(n_languages, mean = 0, sd = sd)

response <- b0 + b1*predictor + error

dat <- data.frame(predictor, response)

model_fit <- lm(response ~ predictor, data = dat)
summary(model_fit)

# Function for the data simulating and model fitting process
sim_LR <- function(n = 1500, from_n = 0.3, 
                   to_n = 4.2, b0 = 0.8, b1 = 5, sigma = 2) {
  predictor <- seq(from_n, to_n, len = n)
  error <- rnorm(n, mean = 0, sd = sigma)
  response <- b0 + b1*predictor + error
  dat.sim <- data.frame(predictor, response)
  model.fit <- lm(response~predictor, data = dat.sim)
  model.fit
}

# Test
set.seed(157)
sim_LR()

# Run the simulation many times (here, 3000)
# Result: a list of fitted LR models based on data simulated
# from the parameters I have set
sims <- replicate(3000, sim_LR(), simplify = FALSE)

# First three models
sims[c(1, 2, 3)]

# What does the distribution of differences in mean response
# between data points look like?

# filter(map_df(sims[c(1, 2, 3)], tidy), term == 'predictor')
sims %>% 
  map_df(tidy) %>%
  filter(term == 'predictor') %>%
  ggplot(aes(x = estimate)) +
  geom_density(fill = 'blue', alpha = .5) +
  geom_vline(xintercept = b1, colour = 'red', linetype = 'dashed', size = 1)

# Extract hypothesis test results

# H_null = there is no statistically significant relationship
# between response and predictor; H_alternative = there is a
# statistically significant relationship between response and predictor

# Type I error rate: Reject H_null when it is actually true
# Type II error rate: Do not reject H_null when it is actually false

# Here, I give the proportion of models that correctly rejected
# the null hypothesis, given that I know the null hypothesis is not true.
# That's an estimate of the statistical power.
sims %>%
  map_df(tidy) %>%
  filter(term == 'predictor') %>%
  pull(p.value) %>%
  {. < 0.01} %>%
  mean() # 1

# Fit the LRM on the data set simulated
data_sim <- read.csv("data_sim.csv")
head(data_sim)

LRM_sim <- lm(response ~ x1, data = data_sim)
summary(LRM_sim)
tidy(LRM_sim)

# Fit the LMM on the data set simulated
library(lme4)
LMM_sim <- lmer(response ~ x1 + (1|area), data = data_sim)
summary(LMM_sim)
summary(LMM_sim)$coefficients

# Here, for instance, area 8 would have
# the smalles value for the response variable
coef(LMM_sim)$area 


