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


## ----echo = TRUE, eval = TRUE-------------------------------------------------
dhyper(3, m = 4, n = 4, k = 4)


## ----echo = TRUE, eval = TRUE-------------------------------------------------
dhyper(3, m = 4, n = 4, k = 4) +
  dhyper(4, m = 4, n = 4, k = 4)


## ----echo = TRUE, eval = TRUE-------------------------------------------------
dhyper(1, m = 4, n = 4, k = 4) +
  dhyper(0, m = 4, n = 4, k = 4)


## -----------------------------------------------------------------------------
data <- rep(c("Milk", "Tea"), each = 4)
data


## -----------------------------------------------------------------------------
results <- replicate(1000, {
  data_perm <- sample(data)
  # Treat first 4 values as if the lady 
  # had guessed milk
  sum(data_perm[1:4] == "Milk")
})

mean(c(3, results) >= 3)

