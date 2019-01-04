
 log_likelihood <- function(y, x, beta, sigmasq_inv = NULL, model){

   if(model == "normal"){
     if(is.null(sigmasq_inv)) stop("Provide sigmasq_inv")
     out <- dnorm(x = y, mean = x %*% beta, sd = sqrt(1/sigmasq_inv), log = TRUE) %>% sum
   }else if(model == "logistic"){
     out <- dbinom(x = y, size = 1, prob = expit(x %*% beta), log = TRUE) %>% sum
   }else if(model == "poisson"){
     out <- dpois(x = y, lambda = exp(x %*% beta), log = TRUE) %>% sum
   }

   return(out)

 }
