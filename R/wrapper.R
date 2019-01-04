
 wrapper <- function(formula, data = NULL, model,
                     N = 2000,
                     controls = list(mu_beta = 0, sigmasq_beta = 100,
                                     phi_sigmasq_inv = 0.01, delta_sigmasq_inv = 10,
                                     sigma_p = 0.2, df = 4)){

  if(length(controls) < 6){
    controls_full <- list(mu_beta = 0, sigmasq_beta = 100,
                          phi_sigmasq_inv = 0.01, delta_sigmasq_inv = 10,
                          sigma_p = 0.2, df = 4)

    for(i in 1:6){
      if(!names(controls_full)[i] %in% names(controls)){
        controls[names(controls_full)[i]] <- controls_full[names(controls_full)[i]]
      }
    }

  }



  x <- model.matrix(object = formula, data = data)
  y <- model.frame(formula = formula, data = data)[, 1] %>% matrix

  out <- mh(y = y, x = x, N = N, model = model, controls = controls)

  return(out)

 }
