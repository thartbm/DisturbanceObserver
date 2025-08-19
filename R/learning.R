

# plot learning curves?


plotLearningCurves <- function() {
  
  colors <- getColors()
  
  demographics <- read.csv('data/short_demographics.csv', stringsAsFactors = F)
  
  # we'll use all of the participants' data (if available):
  participants <- unique(demographics$participant)
  
  alldata <- list()
  
  alldata[[1]] <- list()
  alldata[[2]] <- list()
  
  for (pid in participants) {
    
    ppdata <- preProcessParticipant(pid)
    
    for (runno in c(1,2)) {
      for (condition in (names(ppdata[[runno]]))) {
        
        if (condition %in% names(alldata[[runno]])) {
          alldata[[runno]][[condition]] <- rbind(alldata[[runno]][[condition]], ppdata[[runno]][[condition]])
        } else {
          alldata[[runno]][[condition]] <- ppdata[[runno]][[condition]]
        }
        
      }
    }
  }
  
  labels = names(alldata[[runno]])
  print(labels)
  # layout(mat=matrix(c(1,2),nrow=1,byrow=T))
  
  plot(-1000,-1000,
       main='',xlab='trial',ylab='reach deviation [°]',
       xlim=c(0,261),ylim=c(-40,40),
       bty='n', ax=F)
  
  lines( x   = c(0,40,40,140,140,180),
         y   = c(0, 0,15, 15,  0,  0),
         col = '#AAAAAA')
  lines( x   = c(140,200,200,240),
         y   = c( 15, 15,  0,  0),
         col = '#AAAAAA',
         lty = 2)
  
  
  for (labelno in c(1:4)) {
    label <- labels[labelno]
    ldat <- NA
    for (runno in c(1,2)) {
      tempdf <- alldata[[runno]][[label]]
      if (is.data.frame(ldat)) {ldat <- rbind(ldat,tempdf)} else {ldat <- tempdf}
    }
    aggdat <- aggregate(reachdeviation_deg ~ trialno, 
                        data = ldat,
                        FUN=mean)
    
    lines(aggdat$reachdeviation_deg, col=colors$op[labelno])
    print(mean(aggdat$reachdeviation_deg[c(121:140)], na.rm=T))
  }
  
  axis(side=1, at=c(1,40,140,180,200,240))
  axis(side=2, at=c(-40,-20,0,20,40))
  
  
  plot(-1000,-1000,
       main='',xlab='trial',ylab='reach deviation [°]',
       xlim=c(0,261),ylim=c(-40,40),
       bty='n', ax=F)
  
  lines( x   = c(0,40,40,140,140,180),
         y   = c(0, 0,15, 15,  0,  0),
         col = '#AAAAAA')
  
  for (labelno in c(1:4)) {
    label <- labels[labelno+4]
    ldat <- NA
    for (runno in c(1,2)) {
      tempdf <- alldata[[runno]][[label]]
      if (is.data.frame(ldat)) {ldat <- rbind(ldat,tempdf)} else {ldat <- tempdf}
    }
    aggdat <- aggregate(reachdeviation_deg ~ trialno, 
                        data = ldat,
                        FUN=mean)
    
    lines(aggdat$reachdeviation_deg, col=colors$op[labelno])
    
    if (labelno == 4) {
      print(mean(aggdat$reachdeviation_deg[c(181:200)], na.rm=T))
    } else {
      print(mean(aggdat$reachdeviation_deg[c(121:140)], na.rm=T))
    }
    
    
  }
  
  axis(side=1, at=c(1,40,140,180))
  axis(side=2, at=c(-40,-20,0,20,40))
  
  
}