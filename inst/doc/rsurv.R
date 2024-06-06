## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example, message = FALSE-------------------------------------------------
library(rsurv)
library(survstan)
library(flexsurv)

set.seed(1234567890)

n <-  1000
tau <- 10  # maximum follow up time
simdata <- data.frame(
  age = rnorm(n),
  sex = sample(c("f", "m"), size = n, replace = TRUE)
) %>%
  mutate(
    t = raftreg(runif(n), ~ age*sex, beta = c(1, 2, -0.5), 
                dist = "llogis", shape = 1.5, scale = 1),
  ) %>%
  rowwise() %>%
  mutate(
    time = min(t, tau),
    status = as.numeric(time == t)
  ) 

glimpse(simdata)

fit <- aftreg(
  Surv(time, status) ~ age*sex,
  data = simdata, dist = "loglogistic"
)
estimates(fit)


