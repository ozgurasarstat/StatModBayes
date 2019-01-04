
 summarise_chains <- function(object, nburnin = 0, ...){

   chains <- object$chains
   chains_chopped <- chains[(nburnin + 1):nrow(chains), ]

   out <- apply(chains_chopped, 2, StatModBayes::summarise) %>% t
   rownames(out) <- colnames(chains_chopped)
   colnames(out) <- c("mean", "sd", "2.5%", "50%", "97.5%", "neff")
   return(out)

 }
