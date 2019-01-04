
 expit <- function(x){
   1/(1+exp(-x))
 }

 summarise <- function(x){
   acfs <- acf(x = x, lag.max = 30, plot = FALSE)$acf
   neff <- length(x)/(1 + 2 * sum(acfs))
   out <- c(mean(x), sd(x), quantile(x, c(0.025, 0.5, 0.975)), neff)
   return(out)
 }
