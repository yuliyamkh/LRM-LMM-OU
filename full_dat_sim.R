# Simulation of a full data set

# Create a simulated data set that includes nine different areas
# (e.g., Europe, Oceania, Southeast Asia)

set.seed(345)

# Define ten areas 
areas <- c()
for (i in 1:10) {
  areas[i] <- paste('area', i, sep = '_' )
}

# Group members (e.g., languages) by area
n_lang <- rep(areas, each = 20)
n_lang <- as.factor(n_members)
levels(n_lang)

# Define predictor
x_predictor <- sample(round(seq(0.3, 4.2, len=20), 2))
x_predictor <- rep(x_predictor, 10)

# Put created variables into a tibble
xdat <- tibble(lang_n = 1:length(n_lang), 
               area = n_lang, 
               x1 = x_predictor)
xdat[20:25, 1:2] # 5 rows, 2 columns

# Define intercept
xdat$int <- 0.460

# Generate varying intercepts for area
area_ints <- rnorm(10, sd = 8)
xdat$area_ints <- rep(area_ints, each = 20)

# Generate varying intercepts for languages
lang_ints <- rnorm(20, sd = 2)
xdat$lang_ints <- rep(lang_ints, 10)

# Add error
xdat$error <- rnorm(200, sd = 10)

# Define the effect of the predictor
xdat$effect <- 3*xdat$x1

# Check
xdat %>% head(5)

# Result: language identifiers (lang_n), 
# area identifiers (area),
# predictor values (x1),
# area-varying intercepts (area_ints),
# language-varying intercepts (lang_ints),
# error,
# effect of the predictor variable

# Create response
xdat <- mutate(xdat,
               response = int + area_ints + 
                 lang_ints + error + effect)

head(xdat)

# Export the data created to the .csv file
write.csv(xdat, "data_sim.csv", row.names = FALSE)
