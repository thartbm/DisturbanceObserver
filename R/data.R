
# OSF download -----

# function to download all experimental data from OSF:

getData <- function() {
  
  Reach::downloadOSFdata( repository = '5dtqn',
                          filelist = list('data'=c('steady_state_data.csv',
                                                   'short_demographics.csv',
                                                   'summary_performance.zip')),
                          folder = 'data',
                          overwrite = TRUE,
                          unzip = TRUE,
                          removezips = TRUE)
  
}


# pre-processing ------

preProcessParticipant <- function(ppid, baseline='overall') {
  
  # collect pre-processed data frames here:
  ppdata <- list()
  
  for (runno in c(1,2)) {
    
    # data frames for this run here:
    ppdata[[runno]] <- list()
    
    filename <- sprintf('data/summary_performance/%s_run%d_performance.csv', ppid, runno)
    
    rdf <- read.csv(filename, stringsAsFactors = F)
    
    instructions <- c("ignore", "ignore", "ignore", "ignore", "learn",  "learn",  "learn",  "learn")
    rotations    <- c(-15,      15,       -15,      15,       -15,      15,       -15,      15)
    alphas       <- c(1.0,      0.8,      0.6,      0.4,      1.0,      0.8,      0.6,      0.4)
    
    labels <- c("ignore_d-15_a1.0",
                "ignore_d15_a0.8",
                "ignore_d-15-a0.6",
                "ignore_d15-a0.4",
                "learn_d-15_a1.0",
                "learn_d15_a0.8",
                "learn_d-15-a0.6",
                "learn_d15-a0.4")
    
    for (ti in c(1:8)) {
      
      # get data fur this sub-task:
      label       <- labels[ti]
      stdf <- rdf[which(rdf$label == label),]
      stdf$trialno <- c(1:(dim(stdf)[1]))
      # print(label)
      # remove "outliers"?
      
      # normalize for rotation direction:
      if (rotations[ti] > 0) {
        stdf$reachdeviation_deg <- -1 * stdf$reachdeviation_deg
      }
      
      # remove "outliers":
      stdf$reachdeviation_deg[which(stdf$reachdeviation_deg < -30)] <- NA
      stdf$reachdeviation_deg[which(stdf$reachdeviation_deg >  90)] <- NA
      
      if (baseline == 'pertarget') {
        # calculate per-target baseline:
        baselines <- aggregate(reachdeviation_deg ~ targetangle_deg, 
                               data = stdf[c(21:40),], 
                               FUN=median)
        
        # correct reaches for baseline biases:
        for (target in baselines$targetangle_deg) {
          bias <- baselines$reachdeviation_deg[which(baselines$targetangle_deg == target)]
          idx <- which(stdf$targetangle_deg == target)
          stdf$reachdeviation_deg[idx] <- stdf$reachdeviation_deg[idx] - bias
        }
      }
      
      # print( baseline == 'overall' )
      if (baseline == 'overall') {
        BL <- mean(stdf$reachdeviation_deg[c(21:40)], na.rm=TRUE)
        stdf$reachdeviation_deg <- stdf$reachdeviation_deg - BL
      }
      
      stdf$participant <- sprintf('%s_run%d', ppid, runno)
      stdf$instruction <- instructions[ti]
      stdf$alpha       <- alphas[ti]
      stdf$perturbation <- rotations[ti]
      
      # store pre-processed sub data frames in list:
      ppdata[[runno]][[label]] <- stdf   

    }
    
  }
  
  return(ppdata)
  
}