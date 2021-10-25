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
B <- 1989
norm_vars1 <- rnorm(B)
norm_vars2 <- rnorm(B)
# Compute statistic
gvars <- abs(norm_vars1 - norm_vars2)
mean(gvars)
sd(gvars)/sqrt(B)


## -----------------------------------------------------------------------------
B <- 5000

# Using replicate, or a for loop
dist_vec <- replicate(B, {
    point1 <- runif(2)
    point2 <- runif(2)
    dist <- sqrt(sum((point1 - point2)^2))
    return(dist)
})

mean(dist_vec)


## -----------------------------------------------------------------------------
hist(dist_vec, breaks = 50)


## -----------------------------------------------------------------------------
theta <- mean(dist_vec)
se_dist <- sd(dist_vec)/sqrt(B)

c(theta - 1.96*se_dist,
  theta + 1.96*se_dist)


## -----------------------------------------------------------------------------
mean(c(1,5,2,8, 4))
mean(c(1,5,2,8, 100))


## -----------------------------------------------------------------------------
# Generate a standard normal 
# sample of size 4
(norm_vars <- rnorm(4))
# Sort it
(norm_vars <- sort(norm_vars))

# Compute 1st level trimmed mean
mean(norm_vars[c(-1, -4)])
# Compare to sample mean
mean(norm_vars)


## -----------------------------------------------------------------------------
n <- 20
results <- replicate(3150, {
  norm_vars <- sort(rnorm(n))

c("TM" = mean(norm_vars[c(-1, -n)]),
  "SM" = mean(norm_vars))
})


## -----------------------------------------------------------------------------
# Bias
rowMeans(results) - 0
# MSE
rowMeans((results - 0)^2)


## -----------------------------------------------------------------------------
p <- 0.9
n <- 20; B <- 2209

results <- replicate(B, {
  sigmas <- sample(c(1, 10), n, replace = TRUE,
                   prob = c(p, 1 - p))
  contnorm_vars <- rnorm(n, sd = sigmas)
  contnorm_vars <- sort(contnorm_vars)
  c("TM" = mean(contnorm_vars[c(-1, -n)]),
    "SM" = mean(contnorm_vars))
})


## -----------------------------------------------------------------------------
# Bias
rowMeans(results) - 0
# MSE
rowMeans((results - 0)^2)


## -----------------------------------------------------------------------------
# Number of simulations
B <- 1000
# Sample size for data
n <- 20
# Same mean
mu1 <- mu2 <- 0
# Same variance; could also be different
sigma1 <- sigma2 <- 1


## -----------------------------------------------------------------------------
results <- replicate(B, {
  # Generate two samples
  norm_vars1 <- rnorm(n, mu1, sigma1)
  norm_vars2 <- rnorm(n, mu2, sigma2)
  # Perform t-test
  output <- t.test(norm_vars1, norm_vars2)
  # alpha = 0.05
  return(output$p.value < 0.05)
})

table(results)/B

