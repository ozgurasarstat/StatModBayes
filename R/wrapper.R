
 wrapper <- function(formula, data = NULL, model,
                     N = 2000,
                     controls = list(mu_beta = 0, sigmasq_beta = 100,
                                     phi_sigmasq_inv = 0.01, delta_sigmasq_inv = 10,
                                     sigma_p = 0.2, df = 4)){

  x <- model.matrix(object = formula, data = data)
  y <- model.frame(formula = formula, data = data)[, 1] %>% matrix

  out <- mh(y = y, x = x, N = N, model = model, controls = controls)

  return(out)

 }
