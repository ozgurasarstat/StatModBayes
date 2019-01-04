
 plot.mh <- function(object, nburning = 0){

   chains <- object$chains
   chains_chopped <- chains[(nburnin + 1):nrow(chains), ]

   plot.ts(chains_chopped)

 }
