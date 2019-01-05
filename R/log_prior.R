
 log_prior <- function(beta, param_beta,
                       sigmasq_inv = NULL, param_sigmasq_inv = NULL,
                       model){

   if(model == "normal"){
     out <- dmvnorm(x = as.numeric(beta), mean = param_beta$mu, sigma = param_beta$Sigma, log = TRUE) +
       d_inv_gamma(x = sigmasq_inv, phi = param_sigmasq_inv$phi, delta = param_sigmasq_inv$delta, log = TRUE)
   }else if(model %in% c("logistic", "poisson")){
     out <- dmvnorm(x = beta, mean = param_beta$mu, sigma = param_beta$Sigma, log = TRUE)
   }

   return(out)

 }
