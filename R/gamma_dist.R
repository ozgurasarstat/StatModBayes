
 d_gamma <- function(x, phi, delta, log = FALSE){
   if(log == FALSE){
     out <- 1/(gamma(phi) * delta^phi) * x^(phi-1) * exp(-x/delta)
   }else{
     out <- -log(gamma(phi)) - phi*log(delta) + (phi-1) * log(x) - x/delta
   }
   return(out)
 }

 d_inv_gamma <- function(x, phi, delta, log = FALSE){
   if(log == FALSE){
     out <- 1/(gamma(phi) * delta^phi) * x^(-phi-1) * exp(-1/(x*delta))
   }else{
     out <- -log(gamma(phi)) - phi*log(delta) - (phi+1)*log(x) - 1/(x*delta)
   }
 }




