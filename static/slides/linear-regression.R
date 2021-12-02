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
library(readr)
dataset <- read_csv("heart.csv")

# Use function lm
fit <- lm(age ~ 1, data = dataset)
fit


## -----------------------------------------------------------------------------
# To compute confidence interval
# use confint
confint(fit)


## -----------------------------------------------------------------------------
fit <- lm(age ~ sex, data = dataset)
fit
confint(fit)


## -----------------------------------------------------------------------------
# What if we change the coding 0/1 to female/male?
dataset <- transform(dataset, sex = ifelse(sex == 1, 
                                           "male", 
                                           "female"))


## -----------------------------------------------------------------------------
fit <- lm(age ~ sex, data = dataset)
fit


## -----------------------------------------------------------------------------
fit <- lm(chol ~ sex, data = dataset)
confint(fit)


## -----------------------------------------------------------------------------
fit <- lm(age ~ chol, data = dataset)
fit
confint(fit)


## -----------------------------------------------------------------------------
fit <- lm(chol ~ age, data = dataset)
fit
confint(fit)


## -----------------------------------------------------------------------------
fit <- lm(chol ~ age, data = dataset)
# Extract coefficient estimates with coef
coef(fit)

plot(dataset$age, dataset$chol)
abline(a = coef(fit)[1],
       b = coef(fit)[2])


## -----------------------------------------------------------------------------
library(dslabs)

table(olive$region)


## ----linewidth = 60-----------------------------------------------------------
fit <- lm(oleic ~ region, data = olive)
fit


## -----------------------------------------------------------------------------
confint(fit)

