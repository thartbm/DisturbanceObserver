
getColors <- function() {
  
  cols.op <- c(rgb(255, 147, 41,  255, max = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  255, max = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 255, max = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 255, max = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 255, max = 255)) # blue:   164, 255, 68
  
  cols.tr <- c(rgb(255, 147, 41,  32,  max = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  32,  max = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 32,  max = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 32,  max = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 32,  max = 255)) # blue:   164, 255, 68
  
  cols <- list()
  cols$op <- cols.op
  cols$tr <- cols.tr
  
  return(cols)
  
}

# data processing -----

getSteadyStateData <- function() {
  
  demographics <- read.csv('data/short_demographics.csv', stringsAsFactors = F)
  
  # we'll use all of the participants' data (if available):
  participants <- unique(demographics$participant)
  
  # # collect it in here:
  # participant <- c()
  # run         <- c()
  # instruction <- c()
  # alpha       <- c()
  # steadystate <- c()
  
  steady_states <- NA
  
  # loop through participants:
  for (ppid in participants) {
    
    # runfiles <- sprintf('data/summary_performance/%s_run%d_performance.csv', ppid, c(1,2))
    # print(runfiles)
    
    
    # files_exist <- file.exists(sprintf('data/summary_performance/%s_run%d_performance.csv', ppid, c(1,2)))
    # print(files_exist)
    
    # if (all(file.exists(sprintf('data/summary_performance/%s_run%d_performance.csv', ppid, c(1,2))))) {
    #   sdf <- getParticipantSteadyStates(ppid)
    #   # print(sdf)
    # } else {
    #   # cat(sprintf('participant does not have 2 runs: %s\n', ppid))
    #   next
    # }
    sdf <- getParticipantSteadyStates(ppid)
    # add the participants' data to the overall data frame:
    
    if (is.data.frame(steady_states)) {
      steady_states <- rbind( steady_states, sdf)
    } else {
      steady_states <- sdf
    }
    # print(dim(steady_states))
    
  }
  
  write.csv(steady_states, 'data/steady_state_data.csv', quote=TRUE, row.names = FALSE)
  
  
}


getParticipantSteadyStates <- function(ppid) {
  
  # collect it in here:
  participant <- c()
  run         <- c()
  instruction <- c()
  alpha       <- c()
  steadystate <- c()
  rotation    <- c()
  
  ppdata <- preProcessParticipant(ppid, baseline='overall')
  
  for (runno in c(1,2)) {
    
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
      
      stdf <- ppdata[[runno]][[labels[ti]]]
        
      # stdf <- rdf[which(rdf$label == label),]
      idx <- rev( which(stdf$rotation == rotations[ti]) )[c(1:20)]
      # print(labels[ti])
      # print(idx)
      asymptote <- mean(stdf$reachdeviation_deg[idx], na.rm=TRUE)
      # print(asymptote)
      
      # collect it in here:
      participant <- c(participant, ppid)
      run         <- c(run, runno)
      instruction <- c(instruction, instructions[ti])
      alpha       <- c(alpha, alphas[ti])
      steadystate <- c(steadystate, asymptote)
      rotation    <- c(rotation, rotations[ti])
      
      
    }
    
  }
  
  df <- data.frame( participant,
                       run,
                       instruction,
                       alpha,
                       steadystate,
                       rotation)
  
  return(df)
  
}

# statistical analyses -----

runSteadyStateANOVA <- function() {
  
  df <- read.csv('data/steady_state_data.csv')
  
  my_aov <- afex::aov_ez(id          = 'participant', 
                         dv          = 'steadystate', 
                         data        = df, 
                         within      = c('run','instruction','alpha'),
                         include_aov = TRUE)
  
  print(my_aov)

  
  # now we want to do some targeted post-hoc contrasts
  # we want to ignore run
  # and then compare the effect of learn vs. ignore instructions, at each level of alpha
  # and each successive level of alpha within each instruction
  
}

steadyStateANOVA_posthocs <- function() {
  
  df <- read.csv('data/steady_state_data.csv')
  
  my_aov <- afex::aov_ez(id          = 'participant', 
                         dv          = 'steadystate', 
                         data        = aggregate(steadystate ~ participant + instruction + alpha, data=df, FUN=mean), 
                         within      = c('instruction','alpha'),
                         include_aov = TRUE)
  
  print(my_aov)
  
  cellmeans <- emmeans::emmeans(my_aov$aov, specs=c('alpha','instruction'))
  
  a04_instr <- c(1,0,0,0,-1,0,0,0)
  a06_instr <- c(0,1,0,0,0,-1,0,0)
  a08_instr <- c(0,0,1,0,0,0,-1,0)
  a10_instr <- c(0,0,0,1,0,0,0,-1)
  
  ign_a04_06 <- c(1,-1,0,0,0,0,0,0)
  ign_a06_08 <- c(0,1,-1,0,0,0,0,0)
  ign_a08_10 <- c(0,0,1,-1,0,0,0,0)
  
  lrn_a04_06 <- c(0,0,0,0,1,-1,0,0)
  lrn_a06_08 <- c(0,0,0,0,0,1,-1,0)
  lrn_a08_10 <- c(0,0,0,0,0,0,1,-1)
  
  alpha_contrasts <- list("alpha 0.4 - instructions"=a04_instr, 
                          "alpha 0.6 - instructions"=a06_instr, 
                          "alpha 0.8 - instructions"=a08_instr,
                          "alpha 1.0 - instructions"=a10_instr,
                          
                          "ignore - alpha 0.4 <> 0.6" = ign_a04_06,
                          "ignore - alpha 0.6 <> 0.8" = ign_a06_08,
                          "ignore - alpha 0.8 <> 1.0" = ign_a08_10,
                          
                          "learn - alpha 0.4 <> 0.6" = lrn_a04_06,
                          "learn - alpha 0.6 <> 0.8" = lrn_a06_08,
                          "learn - alpha 0.8 <> 1.0" = lrn_a08_10
  )
  
  print(emmeans::contrast(cellmeans, alpha_contrasts, adjust='sidak'))
  
}

# plotting functions -----

# plotRunInstructionInteraction <- function() {
#   
#   df <- getSteadyStateData()
#   
#   run <- c()
#   instruction <- c()
#   
#   avg <- c()
#   CIlo <- c()
#   CIhi <- c()
#   
#   for (runno in c(1,2)) {
#     for (instr in c('ignore','learn')) {
#       
#       sdf <- df[which(df$run == runno & df$instruction == instr),]
#       
#       run <- c(run, runno)
#       instruction <- c(instruction, instr)
#       
#       avg <- c(avg, mean(sdf$steadystate))
#       CI <- Reach::getConfidenceInterval(data=sdf$steadystate)
#       
#       CIlo <- c(CIlo, min(CI))
#       CIhi <- c(CIhi, max(CI))
#       
#     }
#   }
#   
#   colors <- getColors()
#   
#   ndf <- data.frame(run, instruction, avg, CIlo, CIhi)
#   
#   print(ndf)
#   
#   
#   plot(-1000,-1000,
#        main='',xlab='instruction',ylab='steady state [°]',
#        xlim=c(0.75,2.25),ylim=c(0,25),
#        ax=F,bty='n')
#   
#   for (runno in c(1,2)) {
#     
#     idx <- which(ndf$run == runno)
#     X <- c(1,2,2,1)
#     Y <- c(ndf$CIlo[idx],rev(ndf$CIhi[idx]))
#     
#     polygon( x = X,
#              y = Y,
#              border = NA,
#              col=colors$tr[runno])
#     
#   }
#   
#   for (runno in c(1,2)) {
#     
#     idx <- which(ndf$run == runno)
#     lines( x = c(1,2),
#            y = ndf$avg[idx],
#            col = colors$op[runno] )
#     
#   }
#   
#   axis( side = 1, at = c(1,2), labels = c('ignore', 'learn'))
#   axis( side = 2, at = seq(0,25,5))
#   
#   legend( x = 1,
#           y = 25,
#           legend = c('run 1', 'run 2'),
#           lwd = 1,
#           col = colors$op[c(1,2)],
#           bty='n')
#   
# }


plotAlphaInstructionInteraction <- function() {
  
  df <- read.csv('data/steady_state_data.csv')
  
  alpha <- c()
  instruction <- c()
  
  avg <- c()
  CIlo <- c()
  CIhi <- c()
  
  for (alphano in c(1:4)) {
    alpha_level = c(1.0,0.8,0.6,0.4)[alphano]
    for (instr in c('ignore','learn')) {
      
      sdf <- df[which(df$alpha == alpha_level & df$instruction == instr),]
      # print(sdf)
      
      alpha <- c(alpha, alpha_level)
      instruction <- c(instruction, instr)
      
      avg <- c(avg, mean(sdf$steadystate))
      CI <- Reach::getConfidenceInterval(data=sdf$steadystate)
      
      CIlo <- c(CIlo, min(CI))
      CIhi <- c(CIhi, max(CI))
      
    }
  }
  
  # print(data.frame(instruction,
  #                  alpha,
  #                  avg,
  #                  CIlo,
  #                  CIhi))
  
  colors <- getColors()
  
  ndf <- data.frame(alpha, instruction, avg, CIlo, CIhi)
  
  # print(ndf)
  
  
  plot(-1000,-1000,
       main='',xlab='alpha',ylab='steady state [°]',
       xlim=c(0.2,1.2),ylim=c(0,45),
       ax=F,bty='n')
  
  lines(x=c(0.35,1.05),y=c(15,15),col='#AAAAAA',lty=1,lw=2)
  
  lines(x=seq(0.35,1.05,0.05),y=15/seq(0.35,1.05,0.05),col='#000000',lty=3,lw=2)
  
  alphalevels <- c(1.0,0.8,0.6,0.4)
  
  for (instr_no in c(1,2)) {
    
    instr <- c('ignore','learn')[instr_no]
    
    idx <- which(ndf$instruction == instr)
    X <- c(alphalevels, rev(alphalevels))
    Y <- c(ndf$CIlo[idx],rev(ndf$CIhi[idx]))
    
    polygon( x = X,
             y = Y,
             border = NA,
             col=colors$tr[c(2,5)[instr_no]])
    
  }
  
  for (instr_no in c(1,2)) {
    
    instr <- c('ignore','learn')[instr_no]
    
    idx <- which(ndf$instruction == instr)
    lines( x = alphalevels,
           y = ndf$avg[idx],
           col = colors$op[c(2,5)[instr_no]],
           lw = 2)
    
  }
  
  axis( side = 1, at = alphalevels)
  axis( side = 2, at = seq(0,45,15))
  
  legend( x = 0.6,
          y = 40,
          legend = c('ignore', 'learn', 'rotation', 'full compensation'),
          lwd = 2,
          lty = c(1,1,1,3),
          col = c(colors$op[c(2,5)],'#AAAAAA','#000000'),
          bty='n')
  
}

