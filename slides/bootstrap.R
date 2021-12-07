## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})

knitr::opts_chunk$set(cache = FALSE, message = FALSE,
                      linewidth = 50)

set.seed(3150)


## -----------------------------------------------------------------------------
# "Population" is all integers between 1 and 100
population <- seq(1, 100)
median(population)

# Generate B samples from sampling distribution
B <- 5000
n <- 10
results <- replicate(B, {
    some_sample <- sample(population, 
                          size = n)
    median(some_sample)
})
sd(results) # Unbiased estimate


## -----------------------------------------------------------------------------
# Take a single sample from population
one_sample <- sample(population, size = n)
median(one_sample)


## -----------------------------------------------------------------------------
# Jackknife----
theta_hat <- median(one_sample)
theta_i <- numeric(n)
for (i in 1:n) {
    theta_i[i] <- median(one_sample[-i])
}
# Too small...
sqrt((n-1)*mean((theta_i - mean(theta_i))^2))


## -----------------------------------------------------------------------------
# Bootstrap----
# How do we sample with replacement?
sample(n, n, replace = TRUE)


## -----------------------------------------------------------------------------
# Bootstrap estimate of SE
boot_theta <- replicate(5000, {
  # Sample with replacement
  indices <- sample(n, n, replace = TRUE)
  median(one_sample[indices])
})
# Closer to true value
sd(boot_theta)


## -----------------------------------------------------------------------------
library(bootstrap)
# Estimate of rho
(rho_hat <- cor(law$LSAT, law$GPA))


## -----------------------------------------------------------------------------
# Bootstrap estimate of SE
n <- nrow(law)
boot_rho <- replicate(5000, {
  # Sample with replacement
  indices <- sample(n, n, replace = TRUE)
  # We're sampling pairs of observations
  # to keep correlation structure
  cor(law$LSAT[indices], law$GPA[indices])
})


## -----------------------------------------------------------------------------
sd(boot_rho)


## -----------------------------------------------------------------------------
# law dataset
rho_hat <- cor(law$LSAT, law$GPA)

# Bootstrap estimate of bias
B <- 5000
n <- nrow(law)


## -----------------------------------------------------------------------------
boot_rho <- replicate(5000, {
  # Sample with replacement
  indices <- sample(n, n, replace = TRUE)
  # We're sampling pairs of observations
  # to keep correlation structure
  cor(law$LSAT[indices], law$GPA[indices])
})

(bias <- mean(boot_rho) - rho_hat)

# Debiased estimate
rho_hat - bias

