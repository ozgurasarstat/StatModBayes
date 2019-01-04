
 mh <- function(y, x, N, model, controls){

   p <- ncol(x)
   beta0 <- rep(0, p)

   param_beta <- list(mu = rep(controls$mu_beta, p), Sigma = controls$sigmasq_beta * diag(p))

   if(model == "normal"){
     param_sigmasq_inv <- list(phi = controls$phi_sigmasq_inv, delta = controls$delta_sigmasq_inv)
   }

   save_beta <- matrix(0, ncol = p, nrow = N)
   save_beta[1, ] <- beta0

   if(model == "normal"){
     save_log_sigmasq_inv <- matrix(0, nrow = N, ncol = 1)
   }

   accept <- 0
   i <- 1

   while(i < N){

     beta_star <- save_beta[i, ] + controls$sigma_p * rt(p, controls$df)

     if(model == "normal"){

       log_sigmasq_inv_star <- save_log_sigmasq_inv[i, ] + controls$sigma_p * rt(1, controls$df)

       mh_ratio_upp <- log_posterior(y = y, x = x, sigmasq_inv = exp(log_sigmasq_inv_star),
                                     param_sigmasq_inv = param_sigmasq_inv,
                                     beta = beta_star, param_beta = param_beta,
                                     model = model)

       mh_ratio_low <- log_posterior(y = y, x = x, sigmasq_inv = exp(save_log_sigmasq_inv[i, ]),
                                     param_sigmasq_inv = param_sigmasq_inv,
                                     beta = save_beta[i, ], param_beta = param_beta,
                                     model = model)

     }else if(model %in% c("logistic", "poisson")){

       mh_ratio_upp <- log_posterior(y = y, x = x, beta = beta_star, param_beta = controls$param_beta)
       mh_ratio_low <- log_posterior(y = y, x = x, beta = save_beta[i, ], param_beta = controls$param_beta)

     }

     mh_ratio <- exp(mh_ratio_upp - mh_ratio_low)

     if(runif(1) < min(1, mh_ratio)){
       save_beta[i + 1, ] <- beta_star
       if(model == "normal"){
         save_log_sigmasq_inv[i, ] <- log_sigmasq_inv_star
       }
       accept <- accept + 1
     }else{
       save_beta[i + 1, ] <- save_beta[i, ]
     }

     i <- i + 1

   }

   if(model == "normal"){
     save <- cbind(save_beta, exp(save_log_sigmasq_inv))
   }else if(model %in% c("logistic", "poisson")){
     save <- save_beta
   }

   return(save)

 }
