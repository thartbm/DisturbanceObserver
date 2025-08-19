
# notes on 3 alpha paradigms:

# alpha:

# baseline = 111 - 171 (again 60 trials)
# labels: baseline-training / baseline-nocursor (30 each)
# 200 rotation trials: 171 - 370 
# labels: rotated-training / rotated-nocursor (100 each)

# some 3-trial retention blocks and washout... all different labels

# alpha_2:

# 129 - 188 = 60 baseline:
# labels: baseline-training / baseline-nocursor (30 each)
# 160 rotated trials: 189-348:
# labels: rotated-training / rotated-nocursor (80 each)

# then we have a few repetitions with 5 trials with each instruction, washout, and relearning, all with different labels

# HAS AIMING TRIALS

# alpha 3:

# there is missing data from 8 participants in the reference condition: 30 deg rotation with alpha = 1

# baseline: 129 - 188 (60 trials)
# labels: baseline-training / baseline-nocursor (30 each)
# 189 - 348 = 160 rotated trials
# labels: rotated-training / rotated-nocursor (80 each)

# HAS AIMING TRIALS


getAlphaPilotsData <- function() {
  
  # go through subfolders in alphas folder:
  
  # groups <- list.dirs('data/alphas/', 
  #                     full.names = FALSE, 
  #                     recursive = FALSE)
  
  # don't want the acquire data? not for now...
  
  # groups <- c("r15_a050", "r25_a083", "r30_a100", "r30_a100_acq", "r45_a150", "r60_a200", "r7.5_a025")
  
  groupnames   <- c("r15_a050", "r25_a083", "r30_a100", "r45_a150", "r60_a200", "r7.5_a025")
  grouprots    <- c(        15,         25,         30,         45,         60,         7.5)
  groupsalphas <- c(      0.50,        5/6,          1,        1.5,        2.0,        0.25)
  
  group <- c()
  pilot <- c()
  rotation <- c()
  alpha <- c()
  steadystate.ignore <- c()
  steadystate.learn <- c()
  aiming <- c()
  participant <- c()
  
  for (groupname in groupnames) {
    
    groupparticipants <- list.dirs(sprintf('data/alphas/%s/', groupname), 
                                   full.names = FALSE, 
                                   recursive = FALSE)
    # print(groupname)
    
    groupidx <- which(groupnames == groupname)
    rot <- grouprots[groupidx]
    alph <- groupsalphas[groupidx]
    # print(rot)
    # print(alpha)
    # print(strsplit(x = groupparticipants,
             # split = '_', fixed=TRUE))
    for (ppid in groupparticipants) {
      descriptors <- strsplit(x=ppid, split='_', fixed=TRUE)
      # print(descriptors)
      ppid <- descriptors[[1]][2]
      pilotn <- descriptors[[1]][1] # not really necessary
      # print(c(ppid, pilot))
      
      filename <- sprintf('data/alphas/%s/%s_%s/%s_%s_performance.csv', groupname,pilotn,ppid,pilotn,ppid)
      if (!file.exists(filename)) {
        next
      }
      df <- read.csv( filename,
                      stringsAsFactors = FALSE)
      
      # get 4 medians, last 20 trials in baseline and rotated for training and nocursor trials
      baselinetraining <- getSteadyState(df, label='baseline-training', trials=20)
      baselinenocursor <- getSteadyState(df, label='baseline-nocursor', trials=20)
      rotatedtraining <- getSteadyState(df, label='rotated-training', trials=20)
      rotatednocursor <- getSteadyState(df, label='rotated-nocursor', trials=20)
      
      
      group              <- c(group, groupname)
      pilot              <- c(pilot, pilotn)
      rotation           <- c(rotation, rot)
      alpha              <- c(alpha, alph)
      steadystate.ignore <- c(steadystate.ignore, rotatednocursor-baselinenocursor)
      steadystate.learn  <- c(steadystate.learn, rotatedtraining-baselinetraining)
      # aiming             <- c()
      participant        <- c(participant, ppid)
      
      
      if (pilotn %in% c('alpha2','alpha3')) {
        # handle aiming?
      } else {
        # no aiming
      }
    }
  }
  
  new_df <- data.frame(   group,
                          pilot,
                          rotation,
                          alpha,
                          steadystate.ignore,
                          steadystate.learn,
                          
                          participant)
  
  # return(new_df)
  write.csv(new_df,
            file = 'data/alphas/steadystates.csv',
            row.names = FALSE,
            quote = TRUE)
  
}


getSteadyState <- function(df, label, trials) {
  
  idx <- rev(which(df$label == label))[1:trials]
  # print(idx)
  return(median(df$reachdeviation_deg[idx]))
  
}

plotAlphaPilotSteadyStates <- function() {
  
  df <- read.csv('data/alphas/steadystates.csv',stringsAsFactors = FALSE)
  
  avg.ign <- aggregate(steadystate.ignore ~ alpha, data=df, FUN=mean)
  CI.ign  <- aggregate(steadystate.ignore ~ alpha, data=df, FUN=Reach::getConfidenceInterval)
  
  avg.lrn <- aggregate(steadystate.learn ~  alpha, data=df, FUN=mean)
  CI.lrn  <- aggregate(steadystate.learn ~  alpha, data=df, FUN=Reach::getConfidenceInterval)
  
  colors <- getColors() # from 'R/steady_state.R'
  
  plot(-1000,-1000,
       main='',xlab='alpha',ylab='steady state [°]',
       xlim = c(0,2), ylim=c(0,30),
       ax=F,bty='n')
  
  # ignore = red
  # learn = blue
  
  # confidence intervals = polygons
  
  X <- c(CI.ign$alpha, rev(CI.ign$alpha))
  Y <- c(CI.ign$steadystate.ignore[,1], rev(CI.ign$steadystate.ignore[,2]))
  polygon(X,Y,
          col=colors$tr[2],
          border=NA)
  
  X <- c(CI.lrn$alpha, rev(CI.lrn$alpha))
  Y <- c(CI.lrn$steadystate.learn[,1], rev(CI.lrn$steadystate.learn[,2]))
  polygon(X,Y,
          col=colors$tr[5],
          border=NA)
  
  lines( x = avg.ign$alpha,
         y = avg.ign$steadystate.ignore,
         col = colors$op[2])
  
  lines( x = avg.lrn$alpha,
         y = avg.lrn$steadystate.learn,
         col = colors$op[5])
  
  axis(side = 1, at = c(0,CI.ign$alpha), labels = sprintf('%0.2f',c(0,CI.ign$alpha)))
  axis(side = 2, at = c(0,10,20,30))
  
  legend(x = 1, y = 10,
         legend = c('hand / ignore', 'cursor / learn'),
         lty=c(1,1),
         col=colors$op[c(2,5)],
         bty='n')
  
}