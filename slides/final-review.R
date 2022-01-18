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
dfn <- function(x){
    out <- numeric(length(x))
    out[x <= 0.5] <- dnorm(x[x <= 0.5])
    out[x > 0.5] <-  dnorm(x[x > 0.5]) + 
        dchisq(x[x > 0.5], df = 2)
    out
}


## -----------------------------------------------------------------------------
# Let's visualize
x_vec <- seq(-5, 5, length.out = 1000)
plot(x_vec, dfn(x_vec), type = 'l')


## -----------------------------------------------------------------------------
# Visualize the ratio
plot(x_vec, dfn(x_vec)/dnorm(x_vec), type = 'l')


## -----------------------------------------------------------------------------
# Visualize the ratio
plot(x_vec, dfn(x_vec)/dcauchy(x_vec), type = 'l')


## -----------------------------------------------------------------------------
# Set parameters----
C <- 4 # Constant
n <- 1000 # Number of variates
k <- 0 # counter for accepted
j <- 0 # iterations
y <- numeric(n) # Allocate memory


## -----------------------------------------------------------------------------
while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- rcauchy(1) # random variate from g
  if (u < dfn(x)/(C*dcauchy(x))) {
    k <- k + 1
    y[k] <- x
    }
}


## ----eval = FALSE-------------------------------------------------------------
## # Visualize the histogram
## hist(y, 50, freq = FALSE)


## ----eval = TRUE, echo = FALSE------------------------------------------------
# Visualize the histogram
hist(y, 50, freq = FALSE, ylim = c(0, 0.5))
norm_const <- integrate(dfn, -Inf, Inf)$value
x_vec <- seq(-5, 10, length.out = 1000)
lines(x_vec, dfn(x_vec)/norm_const, lwd = 2, col = "blue")


## -----------------------------------------------------------------------------
g_fun <- function(x) pi/(x^2 + cos(x)^2)
f_vars <- runif(1000, min = 0, max = pi)
mean(g_fun(f_vars))


## -----------------------------------------------------------------------------
phi_vars <- rexp(1000)
# Think of re-weighting
weights <- dunif(phi_vars, 
                 max = pi)/dexp(phi_vars)
mean(g_fun(phi_vars)*weights)

