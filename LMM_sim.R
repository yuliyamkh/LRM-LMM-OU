library(purrr)
library(broom)
library(dplyr)
library(ggplot2)
library(lme4)

# Mixed-Effects Model

set.seed(456)

# Function for the data simulating and model fitting process
sim_LMM <- function(group_n = 10, lang_n = 1500, n = group_n*lang_n, 
                   from_n = 0.3, to_n = 4.2, 
                   b0 = 0.460, b1 = 3, sigma = 10,
                   sigma_ints = 8) {
  
  # Group identifiers
  group <- as.factor(rep(1:group_n, each = lang_n))
  
  # Predictor variable
  predictor <- rep(round(seq(from_n, to_n, len = lang_n), 2), group_n)
  
  # Put group identifiers and predictor variables into a tibble
  dat.sim <- tibble(gr = group, x1 = predictor)
  
  # Define intercept
  dat.sim$int <- b0
  
  # Generate varying intercepts for group
  dat.sim$group_ints <- rep(rnorm(group_n, sd = sigma_ints), each = lang_n)
  dat.sim$error <- rnorm(n, sd = sigma)
  dat.sim$effect <- b1*dat.sim$x1
  
  # Generate response
  dat.sim <- mutate(dat.sim, response = int + 
                   group_ints + 
                   error + 
                   effect)
  
  # Fit the model
  model_fit <- lmer(response ~ x1 + (1|gr), data = dat.sim, REML = FALSE)
  model_fit
}

# Test
sim_LMM()

# Run the simulation many times (here, 3000)
# Result: a list of fitted LR models based on data simulated
# from the parameters I have set
sims_LMM <- replicate(1000, sim_LMM(), simplify = FALSE)

# First three models
sims_LMM[c(1, 2, 3)]

# Extract slopes from fitted models
x1_s <- c()
for (i in 1:length(sims_LMM)) {
  x1_s[i] <- fixef(sims_LMM[[i]]) [2]
}

# What does the distribution of differences in mean response
# between data points look like?
x1_s <- tibble(x1 = x1_s)
ggplot(x1_s, aes(x = x1)) +
geom_density(fill = 'blue', alpha = .5) +
geom_vline(xintercept = 3, colour = 'red', linetype = 'dashed', size = 1)

