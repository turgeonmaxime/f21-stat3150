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


## ----message = FALSE----------------------------------------------------------
boxplot(chickwts$weight ~ chickwts$feed)


## -----------------------------------------------------------------------------
soy_vec <- chickwts$weight[chickwts$feed == "soybean"]
lin_vec <- chickwts$weight[chickwts$feed == "linseed"]

c(length(soy_vec), length(lin_vec))


## -----------------------------------------------------------------------------
# By default, it assumes unequal variance
(fit <- t.test(soy_vec, lin_vec, var.equal = TRUE))


## -----------------------------------------------------------------------------
# Let's bootstrap
B <- 5000
data <- chickwts[chickwts$feed %in% c("soybean", 
                                      "linseed"), ]
n <- nrow(data)


## ----boot_chick, message = FALSE, cache = TRUE--------------------------------
results <- replicate(B, {
  indices <- sample(n, n, replace = TRUE)
  data_b <- data[indices,]
  
  soy_b <- data_b$weight[data_b$feed == "soybean"]
  lin_b <- data_b$weight[data_b$feed == "linseed"]
  mean(soy_b) - mean(lin_b)
})


## -----------------------------------------------------------------------------
# 95% confidence interval
mu_diff <- mean(soy_vec) - mean(lin_vec)
se_boot <- sd(results)
c(mu_diff - 1.96*se_boot, mu_diff + 1.96*se_boot)


## -----------------------------------------------------------------------------
K <- 1000 # Number of permutations
combined_data <- c(soy_vec, lin_vec) # Combine data
N <- length(combined_data)
results <- replicate(K, {
  perm_data <- combined_data[sample(N)] # Permute
  soy_perm <- perm_data[1:length(soy_vec)] # Allocate
  lin_perm <- perm_data[(length(soy_vec) + 1):N]
  mean(soy_perm) - mean(lin_perm)
})


## -----------------------------------------------------------------------------
theta_hat <- mean(soy_vec) - mean(lin_vec)
hist(results, 50)
abline(v = theta_hat, lty = 2, lwd = 2)


## -----------------------------------------------------------------------------
# Is this the right p-value?
mean(c(theta_hat, results) >= theta_hat)
# What about this?
mean(abs(c(theta_hat, results)) >= abs(theta_hat))


## -----------------------------------------------------------------------------
results2 <- replicate(K, {
  perm_data <- combined_data[sample(N)] # Permute
  soy_perm <- perm_data[1:length(soy_vec)] # Allocate
  lin_perm <- perm_data[(length(soy_vec) + 1):N]
  t.test(soy_perm, lin_perm)$statistic
})


## -----------------------------------------------------------------------------
t_hat <- t.test(soy_vec, lin_vec)$statistic
hist(results2, 50)
abline(v = t_hat, lty = 2, lwd = 2)


## -----------------------------------------------------------------------------
# One-sided p-value
mean(c(t_hat, results2) >= t_hat)
# Two-sided p-value
mean(abs(c(t_hat, results2)) >= abs(t_hat))


## ----ks_perm, warning = FALSE-------------------------------------------------
results3 <- replicate(K, {
  perm_data <- combined_data[sample(N)] # Permute
  soy_perm <- perm_data[1:length(soy_vec)] # Allocate
  lin_perm <- perm_data[(length(soy_vec) + 1):N]
  ks.test(soy_perm, lin_perm)$statistic
})


## ----warning = FALSE----------------------------------------------------------
D_hat <- ks.test(soy_vec, lin_vec)$statistic
hist(results3, 50)
abline(v = D_hat, lty = 2, lwd = 2)


## -----------------------------------------------------------------------------
# Only one-sided p-value
mean(c(D_hat, results3) >= D_hat)


## -----------------------------------------------------------------------------
sun_vec <- chickwts$weight[chickwts$feed=="sunflower"]
c(length(sun_vec), length(lin_vec))


## ----warning = FALSE----------------------------------------------------------
K <- 1000 # Number of permutations
combined_data <- c(sun_vec, lin_vec) # Combine data
N <- length(combined_data)
results4 <- replicate(K, {
  perm_data <- combined_data[sample(N)] # Permute
  sun_perm <- perm_data[1:length(sun_vec)] # Allocate
  lin_perm <- perm_data[(length(sun_vec) + 1):N]
  ks.test(sun_perm, lin_perm)$statistic
})


## ----warning = FALSE----------------------------------------------------------
D_hat <- ks.test(sun_vec, lin_vec)$statistic
hist(results4, 50)
abline(v = D_hat, lty = 2, lwd = 2)


## -----------------------------------------------------------------------------
# Only one-sided p-value
mean(c(D_hat, results4) >= D_hat)

