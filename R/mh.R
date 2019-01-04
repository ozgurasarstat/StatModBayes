
 mh <- function(y, x, N, model, controls){

   ## number of columns in the x matrix
   p <- ncol(x)

   ## initial values
   beta0 <- rep(0, p)
   if(model == "normal"){
     log_sigmasq_inv0 <- 0
   }

   ## hyperparameters for beta
   param_beta <- list(mu = rep(controls$mu_beta, p),
                      Sigma = controls$sigmasq_beta * diag(p))

   ## hyperparameters for sigmasq_inv
   if(model == "normal"){
     param_sigmasq_inv <- list(phi = controls$phi_sigmasq_inv,
                               delta = controls$delta_sigmasq_inv)
   }

   ## a matrix to save the chains for beta
   save_beta <- matrix(0, ncol = p, nrow = N)
   save_beta[1, ] <- beta0

   ## a matrix to save the chains for logged sigmasq_inv
   if(model == "normal"){
     save_log_sigmasq_inv <- matrix(0, nrow = N, ncol = 1)
     save_log_sigmasq_inv[1, ] <- log_sigmasq_inv0
   }

   accept <- 0
   i <- 1

   while(i < N){

     beta_star <- save_beta[i, ] + controls$sigma_p * rt(p, controls$df)

     if(model == "normal"){

       log_sigmasq_inv_star <- save_log_sigmasq_inv[i, ] + controls$sigma_p * rt(1, controls$df)

       mh_ratio_upp <- log_posterior(y = y, x = x, beta = beta_star,
                                     sigmasq_inv = exp(log_sigmasq_inv_star),
                                     param_beta = param_beta,
                                     param_sigmasq_inv = param_sigmasq_inv,
                                     model = model)

       mh_ratio_low <- log_posterior(y = y, x = x, beta = save_beta[i, ],
                                     sigmasq_inv = exp(save_log_sigmasq_inv[i, ]),
                                     param_beta = param_beta,
                                     param_sigmasq_inv = param_sigmasq_inv,
                                     model = model)

     }else if(model %in% c("logistic", "poisson")){

       mh_ratio_upp <- log_posterior(y = y, x = x, beta = beta_star,
                                     param_beta = param_beta, model = model)
       mh_ratio_low <- log_posterior(y = y, x = x, beta = save_beta[i, ],
                                     param_beta = param_beta, model = model)

     }

     mh_ratio <- exp(mh_ratio_upp - mh_ratio_low)

     if(runif(1) < min(1, mh_ratio)){
       save_beta[i + 1, ] <- beta_star
       if(model == "normal"){
         save_log_sigmasq_inv[i + 1, ] <- log_sigmasq_inv_star
       }
       accept <- accept + 1
     }else{
       save_beta[i + 1, ] <- save_beta[i, ]
       if(model == "normal"){
         save_log_sigmasq_inv[i + 1, ] <- save_log_sigmasq_inv[i, ]
       }
     }

     i <- i + 1

   }

   if(model == "normal"){
     save <- cbind(save_beta, 1/exp(save_log_sigmasq_inv))
     colnames(save) <- c(colnames(x), "sigmasq")
   }else if(model %in% c("logistic", "poisson")){
     save <- save_beta
     colnames(save) <- colnames(x)
   }

   out <- list(chains = save, accept_ratio = accept/N)
   class(out) <- "mh"
   return(out)

 }
