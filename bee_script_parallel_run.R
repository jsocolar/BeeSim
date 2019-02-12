# Run bee_script_updated() in parallel.
# THE ENTIRETY OF bee_script_updated.R MUST BE RUN PRIOR TO EXECUTING THIS CODE

library(doParallel)
# doParallel is the workhorse pacakge that we'll use for parallel computing

# doParallel requires that we express everything we want to do on each parallel core *as a function*.
# Below, we define the function run_beesim(). Running run_beesim() once will give us one set of simulations for
# each combination of parameter values that we pass as inputs to run_beesim(). Running run_beesim() multiple times
# will give us replicate simulations. Using doParallel to run run_beesim() on n cores will return a list of length
# n with one full replicate simulation (including all parameter values) as each element of the list.
run_beesim <- function(bee_logmean1, bee_logmean2, bee_logsd1, bee_logsd2, rare_advantage, EP, e21, e12, beefluct, bee_plant_ratio){
  n1 <- length(bee_logmean1)
  n2 <- length(bee_logmean2)
  n3 <- length(bee_logsd1)
  n4 <- length(bee_logsd2)
  n5 <- length(rare_advantage)
  n6 <- length(EP)
  n7 <- length(e21)
  n8 <- length(e12)
  n9 <- length(beefluct)
  n10 <- length(bee_plant_ratio)
  
  output <- list()
  
  for(i1 in 1:n1){
    output[[i1]] <- list()
    for(i2 in 1:n2){
      output[[i1]][[i2]] <- list()
      blms <- c(bee_logmean1[i1], bee_logmean2[i2])
      for(i3 in 1:n3){
        output[[i1]][[i2]][[i3]] <- list()
        for(i4 in 1:n4){
          output[[i1]][[i2]][[i3]][[i4]] <- list()
          blss <- c(bee_logsd1[i3], bee_logsd2[i4])
          for(i5 in 1:n5){
            output[[i1]][[i2]][[i3]][[i4]][[i5]] <- list()
            for(i6 in 1:n6){
              output[[i1]][[i2]][[i3]][[i4]][[i5]][[i6]] <- list()
              for(i7 in 1:n7){
                output[[i1]][[i2]][[i3]][[i4]][[i5]][[i6]][[i7]] <- list()
                for(i8 in 1:n8){
                  output[[i1]][[i2]][[i3]][[i4]][[i5]][[i6]][[i7]][[i8]] <- list()
                  for(i9 in 1:n9){
                    output[[i1]][[i2]][[i3]][[i4]][[i5]][[i6]][[i7]][[i8]][[i9]] <- list()
                    for(i10 in 1:n10){
                      output[[i1]][[i2]][[i3]][[i4]][[i5]][[i6]][[i7]][[i8]][[i9]][[i10]] <-
                        beesim(bee_logmeans=blms, bee_logsds=blss, bee_mean_abunds=c(100,100), 
                               plant_inits=rep(EP/2, 2), n_years=100, visits_per_bee=c(25000,25000), 
                               pollen_transfer_probs=c(1,1,1,1,0.7,0.5,0.2), pollen2plant_prob=c(.2,.2), fixed=FALSE,
                               visitprobtype=1, rare_advantage=rare_advantage[i5], plotting=FALSE, EP=EP[i6], e21=e21[i7], 
                               e12=e12[i8], maxprob=.2, beefluct=beefluct[i9], r_times_k=.3)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  return(output)
}

# We define the inputs that we want to pass to run_beesim(). In this code block, we can specify multiple combinations
# of inputs to explore different regions of parameter space.
run_beesim_inputs.1 <- list(bee_logmean1 = c(0, 4, 10), bee_logmean2 = c(-10, -4, 0, 4, 10), bee_logsd1 = c(0, 1, 3), bee_logsd2 = c(0, 1, 3), rare_advantage = c(.1, .7, 1, 1.3), EP = c(500000), e21 = c(0, .75, 1), e12 = c(0, .75, 1, 1.5), beefluct = c('none', 'independent', 'plant-based'), bee_plant_ratio = c(.001))
run_beesim_inputs.2 <- list(bee_logmean1 = c(0, 4, 10), bee_logmean2 = c(-10, -4, 0, 4, 10), bee_logsd1 = c(0, 1, 3), bee_logsd2 = c(0, 1, 3), rare_advantage = c(.1, .7, 1, 1.3), EP = c(500000), e21 = c(0, .75, 1), e12 = c(0, .75, 1, 1.5), beefluct = c('none', 'independent', 'plant-based'), bee_plant_ratio = c(.001))

# Below, we select which version of run_beesim_inputs.X we want to use in our simulation. 
run_beesim_inputs <- run_beesim_inputs.1

# Below, we create the cluster and run the job.
s <- proc.time()                          # for keeping track of how long the job takes
cl <- parallel::makeCluster(4)            # make a cluster (called cl) with 4 cores. Note that this function comes from the package 'parallel' on which 'doParallel' depends
doParallel::registerDoParallel(cl)        # some doParallel voodoo to get the cores ready for work
# Below, we execute run_beesim on our cluster
bee_output <- foreach(i=1:4) %dopar% run_beesim(bee_logmean1 = run_beesim_inputs$bee_logmean1, 
                                                bee_logmean2 = run_beesim_inputs$bee_logmean2,
                                                bee_logsd1 = run_beesim_inputs$bee_logsd1, 
                                                bee_logsd2 = run_beesim_inputs$bee_logsd2,
                                                rare_advantage = run_beesim_inputs$rare_advantage, 
                                                EP = run_beesim_inputs$EP, 
                                                e21 = run_beesim_inputs$e21, 
                                                e12 = run_beesim_inputs$e12,
                                                beefluct = run_beesim_inputs$beefluct, 
                                                bee_plant_ratio = run_beesim_inputs$bee_plant_ratio)
parallel::stopCluster(cl)                # Closing down our cluster
elapsed <- proc.time() - s               # how long the job took

# save output.  IMPORTANT: DO NOT FORGEET TO CHANGE FILENAME WHEN USING DIFFERENT PARAMETER INPUTS.  OTHERWISE
#                          YOU'LL OVERWRITE THE OLD SIMULATIONS!
save(bee_output, file = "/Users/JacobSocolar/Dropbox/Work/Beesim_fullrun/bee_output.Rdata")
