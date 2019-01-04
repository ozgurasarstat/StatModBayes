
 log_likelihood <- function(y, x, beta, sigmasq_inv = NULL, mod = "normal"){

   if(model == "normal"){

     if(is.null(sigmasq_inv)) stop("Provide sigmasq_inv")

     out <- dnorm(y, x %*% beta, sqrt(1/sigmasq_inv), log = TRUE) %>% sum

   }else if(model == "logistic"){

     out <- dbinom(y, 1, expit(x %*% beta), log = TRUE) %>% sum

   }else if(model == "poisson"){

     out <- dpois(y, exp(x %*% beta), log = TRUE)

   }

   return(out)

 }
