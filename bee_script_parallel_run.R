# Function to call and run bee_script_updated in parallel on 4 cores.
library(doParallel)


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
                               plant_inits=c(500000,500000), n_years=100, visits_per_bee=c(25000,25000), 
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

bee_logmean1 <- c(0, 4, 10)
bee_logmean2 <- c(-10, -4, 0, 4, 10)
bee_logsd1 <- c(0, 1, 3)
bee_logsd2 <- c(0, 1, 3)
rare_advantage <- c(.1, .7, 1, 1.3)
EP <- c(1000000)
e21 <- c(0, .75, 1)
e12 <- c(0, .75, 1, 1.5)
beefluct <- c('none', 'independent', 'plant-based')
bee_plant_ratio <- c(.0004)

export_vars <- c('bee_logmean1', 'bee_logmean2', 'bee_logsd1', 'bee_logsd2', 'rare_advantage', 'EP', 'e21', 'e12',
                 'beefluct', 'bee_plant_ratio')

s <- proc.time()
cl <- makeCluster(4)
registerDoParallel(cl)
bee_output <- foreach(i=1:4) %dopar% run_beesim(bee_logmean1, bee_logmean2, bee_logsd1, bee_logsd2,
                                                rare_advantage, EP, e21, e12, beefluct, bee_plant_ratio)
stopCluster(cl)
elapsed <- proc.time() - s
