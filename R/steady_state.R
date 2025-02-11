
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


getSteadyStateData <- function() {
  
  df <- read.csv('data/steady_state_data.csv')
  
  return(df)

}

# statistical analyses -----

runSteadyStateANOVA <- function() {
  
  df <- getSteadyStateData()
  
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
  
  df <- getSteadyStateData()
  
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
  
  print(contrast(cellmeans, alpha_contrasts, adjust='sidak'))
  
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
  
  df <- getSteadyStateData()
  
  alpha <- c()
  instruction <- c()
  
  avg <- c()
  CIlo <- c()
  CIhi <- c()
  
  for (alphano in c(1:4)) {
    alpha_level = c(1.0,0.8,0.6,0.4)[alphano]
    for (instr in c('ignore','learn')) {
      
      sdf <- df[which(df$alpha == alpha_level & df$instruction == instr),]
      
      alpha <- c(alpha, alpha_level)
      instruction <- c(instruction, instr)
      
      avg <- c(avg, mean(sdf$steadystate))
      CI <- Reach::getConfidenceInterval(data=sdf$steadystate)
      
      CIlo <- c(CIlo, min(CI))
      CIhi <- c(CIhi, max(CI))
      
    }
  }
  
  colors <- getColors()
  
  ndf <- data.frame(alpha, instruction, avg, CIlo, CIhi)
  
  # print(ndf)
  
  
  plot(-1000,-1000,
       main='',xlab='alpha',ylab='steady state [°]',
       xlim=c(0.2,1.2),ylim=c(0,40),
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
  axis( side = 2, at = seq(0,40,10))
  
  legend( x = 0.6,
          y = 40,
          legend = c('ignore', 'learn', 'rotation', 'full compensation'),
          lwd = 2,
          lty = c(1,1,1,3),
          col = c(colors$op[c(2,5)],'#AAAAAA','#000000'),
          bty='n')
  
}

