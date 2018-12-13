library(ggplot2)
# Supporting functions:

# Probability of visiting plant 1 on a given visit, as a function of plant abundances,
# plant preference, and rare-species advantage.
# Two options: rare species advantage is applied on absolute abundances, or rare 
# species advantage is applied to effective abundances accounting for prefernce.

visitprob_type1 <- function(plant1, plant2, pref, rare){
  p_ <- pref/(1+pref)
  p1eff_ <- plant1^rare
  p2eff_ <- plant2^rare 
  vp1 <- p1eff_*p_/(p1eff_*p_+p2eff_*(1-p_))
  return(vp1)
}
### prob 2 hasn't been updated; don't use it
visitprob_type2 <- function(plant1, plant2, pref, rare){
  p1eff_ <- plant1*pref
  vp1 <- (p1eff_^rare)/(p1eff_^rare + plant2^rare)
  return(vp1)
}
visitprob <- list(visitprob_type1, visitprob_type2)


beesim <- function(bee_logmeans=c(0,0), bee_logsds=c(0,0), bee_mean_abunds=c(100,100), 
                   plant_inits=c(500000,500000), n_years=100, visits_per_bee=c(25000,25000), 
                   pollen_transfer_probs=c(1,1,1,1,0.7,0.5,0.2), pollen2plant_prob=c(.2,.2), fixed=FALSE,
                   visitprobtype=1, rare_advantage=1, plotting=FALSE, EP=1000000, e21=1, e12=1,
                   maxprob=.2, beefluct='none', r_times_k=.3, bee_plant_ratio = .0002){
  # Parameter description:
  #   bee preferences are lognormally distributed within species
  #       bee_logmeans gives the mean logarithm
  #       bee_logsds gives the sd of the logarithm
  #   bee_mean_abunds gives the abundance of each bee species (if bee populations are fixed), or the expected
  #     bee abundances if bees fluctuate independently.  If bee fluctiuations are plant-based, then bees are
  #     initialized at these abundances, but there is no guarantee that these will be the long-term expected
  #     abundances
  #   plant_inits gives the initial values for the plant populations.
  #   n_years gives the number of years to simulate
  #   visits_per_bee gives the number of floral visits performed per individual bee (per annum)
  #   pollen_transfer_probs: the nth element gives the probability of pollen transfer n visits after picking up the
  #     pollen. Transfer probabilities beyond the length of the vector are zero.
  #   pollen2plant_prob: the probability that a pollen transfer event results in a germinated seed
  #   fixed: are bee preferences fixed through time (TRUE), or re-sampled from bee_logmeans and bee_logsds each 
  #     year (FALSE)? May be TRUE **only** if beefluct == 'none'.
  #   visitprobtype: DEPRECATED, **must** be set to 1
  #   rare_advantage: to what extent do bees preferentially visit common (> 1) or rare (< 1) plants? See helper 
  #     function visitprob_type1
  #   plotting: produce a graph of output?
  #   EP: Carrying capacity for plants.  If plants compete strongly, then the expected combined population (in absence
  #     of pollen limitation) will be EP.  If plants do not compete, then the expected combined population will be
  #     2 * EP.
  #   e12 & e21: Competition coefficient giving the per-capita effect of plant 2 on the growth rate of plant 1 and
  #     vice versa. Values of 1 represent no stabilization. Values of 0 represent no interspecific competition. 
  #     Values greater than 1 represent competitive dominance.  At most one of the two values should be great than one.
  #   maxprob: regardless of what the plant growth equation says, a germinated seed will never become an adult with 
  #     probability greater than maxprob.
  #   beefluct: what sort of population dynamics do the bees get?  Can be 'none' (i.e. deterministic nest-site 
  #     limitation), 'independent' (i.e. nest site limitation with some stochasticity), or 'plant-based' (i.e. limited
  #     by floral resources).  If fixed == TRUE, then beefluct **must** be 'none'.
  #   r_times_k: In plant-based bee fluctuations, the bees grow according to a logistic difference equation whose 
  #     carrying capacity is controlled by the plant abundances. r_times_k gives the logistic growth rate when bee
  #     populations are scaled by their carrying capacity.
  
  
  # Some initial parameter checking
  if(!is.vector(bee_logmeans)){stop("bee_logmeans not a numeric vector")}
  if(!is.vector(bee_logsds)){stop("bee_logsds not a numeric vector")}
  if(!is.vector(bee_mean_abunds)){stop("bee_mean_abunds not a numeric vector")}
  if(!is.vector(plant_inits)){stop("plant_inits not a numeric vector")}
  if(!is.vector(visits_per_bee)){stop("visits_per_bee not a numeric vector")}
  if(!is.vector(pollen2plant_prob)){stop("pollen2plant_prob not a numeric vector")}
  if(!is.numeric(bee_logmeans)){stop("bee_logmeans not a numeric vector")}
  if(!is.numeric(bee_logsds)){stop("bee_logsds not a numeric vector")}
  if(!is.numeric(bee_mean_abunds)){stop("bee_mean_abunds not a numeric vector")}
  if(!is.numeric(plant_inits)){stop("plant_inits not a numeric vector")}
  if(!is.numeric(visits_per_bee)){stop("visits_per_bee not a numeric vector")}
  if(!is.numeric(pollen2plant_prob)){stop("pollen2plant_prob not a numeric vector")}
  if(length(bee_logmeans) != length(bee_logsds)){stop("bee_logmeans and bee_logsds unequal lengths")}
  if(length(bee_logmeans) != length(bee_mean_abunds)){stop("bee_logmeans and bee_mean_abunds unequal lengths")}
  if(length(bee_logmeans) != length(visits_per_bee)){stop("bee_logmeans and visits_per_bee unequal lengths")}
  if(length(plant_inits) != 2){stop("plant_inits not of length 2")}
  if(length(pollen2plant_prob) != 2){stop("pollen2plant_prob not of length 2")}
  if(min(pollen2plant_prob)<=0){stop("pollen2plant_prob must contain valid nonzero probabilities")}
  if(max(pollen2plant_prob)>1){stop("pollen2plant_prob must contain valid nonzero probabilities")}
  n_years <- as.integer(n_years)
  if(is.na(n_years)){stop("n_years must be a single positive number (non-integers will be rounded down to integers)")}
  if(length(n_years) != 1){stop("n_years must be a single positive number (non-integers will be rounded down to integers)")}
  if(n_years < 1){stop("n_years must be at least one")}
  if(fixed == T & beefluct != "none"){stop("if beefluct is not \"none\", fixed must be FALSE")} 
  
  bee_abunds <- bee_mean_abunds
  
  bvals <- list()
  
  if(beefluct == "none"){
    # The preference value for each bee.  The jth member of the ith bee species will
    # have its preference value in bvals[[i]][j]
    for(i in 1:length(bee_logmeans)){
      bvals[[i]] <- rlnorm(bee_abunds[i], bee_logmeans[i], bee_logsds[i])
    }
  }
  
  # Initialize a dataframe to hold the plant population timeseries
  plant_timeseries <- as.data.frame(matrix(data=NA, nrow=n_years+1, ncol=3))
  names(plant_timeseries) <- c("year", "plant1", "plant2")
  plant_timeseries$year <- (0:n_years)
  plant_timeseries[1,2:3] <- plant_inits
  
  # Initialize a dataframe to hold the bee population timeseries
  bee_timeseries <- as.data.frame(matrix(data=NA, nrow=n_years+1, ncol=3))
  names(bee_timeseries) <- c("year", "bee1", "bee2")
  bee_timeseries$year <- (0:n_years)
  bee_timeseries[1,2:3] <- bee_mean_abunds
  
  # Run the population dynamics, one year at a time
  for(y in 1:n_years){
    # if fixed is TRUE, then be preferences are stably fixed across bee generations
    # otherwise, we draw new bee preferences from the species-specific distributions
    # each year.
    bee_abunds <- as.numeric(bee_timeseries[y, 2:3])
    
      if(beefluct == "independent"){
        for(i in 1:2){
          bee_timeseries[y+1, 2:3] <- ceiling(bee_abunds[i] * rlnorm(1, log(bee_mean_abunds[i]/bee_abunds[i]), 0.1))
        }
      }
      
      if(beefluct == "plant-based"){
        K <- rep(NA,2)
        r <- rep(NA,2)
        for(i in 1:2){
          K[i] <- bee_plant_ratio * weighted.mean(x = c(plant_timeseries$plant1[y], plant_timeseries$plant2[y]),
                                       w = c(exp(bee_logmeans[i]), exp(-bee_logmeans[i])))
          r[i] <- r_times_k / K[i]
          bee_timeseries[y+1, 2:3] <- ceiling(bee_abunds[i] * exp(r[i] * (K[i] - sum(bee_abunds))))
        }
      }
    
    if(beefluct=="none"){
      bee_timeseries[y+1, 2:3] <- bee_timeseries[y, 2:3]
    }
    
    
    
    if(!fixed){
      for(i in 1:length(bee_logmeans)){
        bvals[[i]] <- rlnorm(bee_abunds[i], bee_logmeans[i], bee_logsds[i])
      }
    }
    
    # Initialize variables to keep track of how many successful pollen transfer events
    # plant species 1 and 2 receive this year
    np1 <- 0
    np2 <- 0
    
    # Calculate the number of pollen transfers by getting them across all bees.
    
    # For each bee species...
    for(bsp in 1:length(bee_logmeans)){
      # For each individual of that species...
      for(bindiv in 1:bee_abunds[bsp]){
        # Calculate and then add up the number of pollen transfer events attributable
        # to sequential visits separated by a given number of intervening visits, up to
        # length(pollen_transfer_probs).  To get these numbers, we call one of the
        # visitprob functions depending on the the control parameter visitprobtype
        for(nseq in 1:length(pollen_transfer_probs)){
          # binomial sample from # of total visits with probability:
          # (probability of sequential visits) * (probability of pollen transfer)
          # For plant 1
          np1 <- np1 + rbinom(1, visits_per_bee[bsp]-nseq, pollen_transfer_probs[nseq]*visitprob[[visitprobtype]](plant_timeseries$plant1[y],plant_timeseries$plant2[y],bvals[[bsp]][bindiv],rare_advantage)^2)
          # For plant 2
          np2 <- np2 + rbinom(1, visits_per_bee[bsp]-nseq, pollen_transfer_probs[nseq]*(1 - visitprob[[visitprobtype]](plant_timeseries$plant1[y],plant_timeseries$plant2[y],bvals[[bsp]][bindiv],rare_advantage))^2)
        }
      }
    }
    
    # Get number of plants in the next generation by binomially sampling with 
    # number of trials given by the number of pollen transfer events, and prob. of
    # success given by pollen2plant_prob
    P1_seeds <- rbinom(1, np1, pollen2plant_prob[1])
    P2_seeds <- rbinom(1, np2, pollen2plant_prob[2])
    
    plant_timeseries$plant1[1+y] <- rbinom(1, P1_seeds, min(maxprob, EP/(P1_seeds + e21*P2_seeds)))
    plant_timeseries$plant2[1+y] <- rbinom(1, P2_seeds, min(maxprob, EP/(P2_seeds + e12*P1_seeds)))
  }
  
  # Plot timeseries if plotting=TRUE
  if(plotting){
    pt <- as.data.frame(rbind(as.matrix(plant_timeseries[,c(1,2)]), as.matrix(plant_timeseries[,c(1,3)])))
    names(pt) <- c("year", "abun")
    pt$plant <- as.factor(c(rep(1,dim(plant_timeseries)[1]), rep(2, dim(plant_timeseries)[1])))
    timeseries_plot <- ggplot(data=pt, aes(x=year, y=abun, group=plant)) + geom_line(aes(x=year, y=abun, color=plant))
    print(timeseries_plot)
  }
  
  output <- list(plant_timeseries = plant_timeseries, bee_timeseries = bee_timeseries)
  return(output)
}
