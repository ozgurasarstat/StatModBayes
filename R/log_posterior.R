
 log_posterior <- function(y, x, beta, sigmasq_inv = NULL,
                           param_beta, param_sigmasq_inv = NULL, model){

     out <- log_likelihood(y = y, x = x, beta = beta, sigmasq_inv = sigmasq_inv, model = model) +
       log_prior(beta = beta, param_beta = param_beta, sigmasq_inv = sigmasq_inv,
                param_sigmasq_inv = param_sigmasq_inv, model = model)

     return(out)

 }
