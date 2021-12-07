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


## ---- message = FALSE---------------------------------------------------------
library(DAAG)

# Fit model
fit <- lm(magnetic ~ chemical, data = ironslag)
confint(fit)


## -----------------------------------------------------------------------------
# Plot fitted linear trend
plot(ironslag$chemical, 
     ironslag$magnetic)
abline(a = coef(fit)[1],
       b = coef(fit)[2])


## -----------------------------------------------------------------------------
# Fitted against residuals
plot(fitted(fit), residuals(fit))
abline(h = 0, lty = 2)


## ----message = FALSE----------------------------------------------------------
library(MASS)

dataset <- transform(mammals,
                     log_body = log(body))

# Fit model
fit <- lm(brain ~ log_body, data = dataset)


## ----message = FALSE----------------------------------------------------------
confint(fit)


## -----------------------------------------------------------------------------
# Plot fitted linear trend
plot(dataset$log_body, 
     dataset$brain)
abline(a = coef(fit)[1],
       b = coef(fit)[2])


## -----------------------------------------------------------------------------
# Fitted against residuals
plot(fitted(fit), residuals(fit))
abline(h = 0, lty = 2)


## ----message = FALSE----------------------------------------------------------
dataset <- transform(mammals,
                     log_body = log(body),
                     log_brain = log(brain))

# Fit model
fit2 <- lm(log_brain ~ log_body, data = dataset)


## ----message = FALSE----------------------------------------------------------
confint(fit2)


## -----------------------------------------------------------------------------
# Plot fitted linear trend
plot(dataset$log_body, 
     dataset$log_brain)
abline(a = coef(fit2)[1],
       b = coef(fit2)[2])


## -----------------------------------------------------------------------------
# Fitted against residuals
plot(fitted(fit2), residuals(fit2))
abline(h = 0, lty = 2)


## ---- message = FALSE---------------------------------------------------------
library(DAAG)
library(tidyverse)

# Fit model
fit <- lm(magnetic ~ chemical, data = ironslag)
confint(fit)


## ----message = FALSE----------------------------------------------------------
library(lmtest)
library(sandwich)
coefci(fit, vcov. = vcovHC(fit))


## ----message = FALSE----------------------------------------------------------
dataset <- mutate(mammals,
                  log_body = log(body),
                  log_brain = log(brain))

# Fit model
fit2 <- lm(log_brain ~ log_body, data = dataset)


## ----message = FALSE----------------------------------------------------------
confint(fit2)
coefci(fit2, vcov. = vcovHC(fit2))

