#
# n <- 1000
# xmat <- cbind(1, rnorm(n), rnorm(n))
# #ymat <- rnorm(n, xmat %*% c(0.5, -0.1, -0.2), 2)
# #ymat <- rbinom(n, 1, expit(xmat %*% c(0.5, -0.1, -0.2)))
# ymat <- rpois(n, exp(xmat %*% c(0.5, -0.1, -0.2)))
#
# data <- data.frame(yvar = ymat, x1 = xmat[, 2], x2 = xmat[, 3])
#
# library(StatModBayes)
#
# fit <- wrapper(formula = yvar ~ x1 + x2, data = data,
#                model = "poisson", N = 10000, controls = list(sigma_p = 0.01, df = 3))
#
# fit$accept_ratio
#
# plot(fit, nburnin = 500)
# summarise_chains(fit, nburnin = 500)
