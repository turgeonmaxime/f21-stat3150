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


## -----------------------------------------------------------------------------
library(bootstrap)
B <- 5000
n <- nrow(law)
boot_rho <- replicate(B, {
  # Sample with replacement
  indices <- sample(n, n, replace = TRUE)
  cor(law$LSAT[indices], law$GPA[indices])
})


## -----------------------------------------------------------------------------
rho_hat <- cor(law$LSAT, law$GPA)
bias <- mean(boot_rho) - rho_hat
se <- sd(boot_rho)


## -----------------------------------------------------------------------------
# 1. Standard normal
c(rho_hat - bias - 1.96*se,
  rho_hat - bias + 1.96*se)


## -----------------------------------------------------------------------------
# 2. Bootstrap percentile
quantile(boot_rho,
         probs = c(0.025, 0.975))


## -----------------------------------------------------------------------------
# 3. Basic bootstrap
crit_vals <- quantile(boot_rho,
                      probs = c(0.025, 0.975))
c(2*rho_hat - crit_vals[2],
  2*rho_hat - crit_vals[1],
  use.names = FALSE)


## ---- echo = FALSE------------------------------------------------------------
library(glue)
library(magrittr)
ci_str <- "({round(lower, 2)}, {round(upper, 2)})"
tibble::tribble(
  ~lower, ~upper,
  rho_hat - bias - 1.96*se, rho_hat - bias + 1.96*se,
  crit_vals[1], crit_vals[2],
  2*rho_hat - crit_vals[2], 2*rho_hat - crit_vals[1]
) %>% 
  glue_data(ci_str) -> cis

tibble::tibble(Method = c("Standard Normal", "Percentile", "Basic Bootstrap"),
               `95% CI` = cis) %>% 
  knitr::kable(caption = "Only the percentile method gives a sensible confidence interval, i.e. a CI that is contained within the interval $(-1, 1)$.")


## ---- echo = FALSE------------------------------------------------------------
hist(boot_rho, 50)


## ----bootT, cache=TRUE--------------------------------------------------------
# 4. Student bootstrap
boot_rho_t <- replicate(B, {
  indices <- sample(n, n, replace = TRUE)
  rho_b <- cor(law$LSAT[indices], law$GPA[indices])
  double_boot <- replicate(100, {
    double_ind <- sample(indices, n, replace = TRUE)
    cor(law$LSAT[double_ind], law$GPA[double_ind])
  })
  tb <- (rho_b - rho_hat)/sd(double_boot)
  return(c(rho_b, tb))
})


## -----------------------------------------------------------------------------
# The output has two rows:
# First row: rho_b values
# Second row: tb values
str(boot_rho_t)


## -----------------------------------------------------------------------------
# SE estimated using rho_b values
SE <- sd(boot_rho_t[1,])


## -----------------------------------------------------------------------------
# t critical values
tcrit_vals <- quantile(boot_rho_t[2,], 
                       probs = c(0.025, 0.975))


## -----------------------------------------------------------------------------
c(rho_hat - tcrit_vals[2]*SE,
  rho_hat - tcrit_vals[1]*SE,
  use.names = FALSE)


## ----eval = TRUE, echo = TRUE-------------------------------------------------
# First estimate z0 hat
z0_hat <- qnorm(mean(boot_rho < rho_hat))
z0_hat


## ----eval = TRUE, echo = TRUE, cache = TRUE-----------------------------------
# Next: Jackknife
rho_i <- numeric(n)

for (i in 1:n) {
  rho_i[i] <- cor(law$LSAT[-i], law$GPA[-i])
}


## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Then estimate a hat
rho_bar <- mean(rho_i)
ahat_num <- sum((rho_bar - rho_i)^3)
ahat_denom <- 6*sum((rho_bar - rho_i)^2)^(3/2)
(a_hat <- ahat_num/ahat_denom)


## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Putting everything together
beta1 <- pnorm(z0_hat + (z0_hat - 1.96) /
                 (1 - a_hat*(z0_hat - 1.96)))
beta2 <- pnorm(z0_hat + (z0_hat + 1.96) /
                 (1 - a_hat*(z0_hat + 1.96)))
c(beta1, beta2)


## ----eval = TRUE, echo = TRUE-------------------------------------------------
# BCa interval
quantile(boot_rho, probs = c(beta1, beta2))

# Compare with percentile
quantile(boot_rho, probs = c(0.025, 0.975))

