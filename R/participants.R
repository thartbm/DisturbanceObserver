
`%notin%` <- Negate(`%in%`)


# this function was used to clean up the demographics file:

checkParticipants <- function() {
  
  # participants named in the steady state data
  steady_states <- getSteadyStateData()
  data_participants <- unique(steady_states$participant)
  
  # participants in the demographics file:
  demographics <- read.csv('data/demographics.csv', stringsAsFactors = F)
  # only use participants who provided informed consent
  demographics <- demographics[which(demographics$consent == "I agree to participate in the study"),] 
  demo_participants <- unique(demographics$participant)
  
  # select demographics lines to keep and to remove:
  remove_demographics <- demographics[which(demographics$participant %notin% data_participants),]
  keep_demographics <- demographics[which(demographics$participant %in% data_participants),]
  
  # move duplicated lines over to the remove_demographics df:
  duplicants <- which(duplicated(keep_demographics$participant))
  remove_demographics <- rbind(remove_demographics, keep_demographics[duplicants,])
  keep_demographics <- keep_demographics[-duplicants,]
  
  # store the result:
  write.csv(keep_demographics, 'data/short_demographics.csv', quote=T, row.names=F)
  write.csv(remove_demographics, 'data/unused_demographics.csv', quote=T, row.names=F)
  
  # only the 'short_demographics.csv' is made public on OSF
  
}


# this function was used to clean up the summary data sets:

cleanSummaryPerformance <- function() {
  
  demographics <- read.csv('data/short_demographics.csv', stringsAsFactors = F)
  participants <- demographics$participant
  
  # check participants for which we have 2 summary performance files:
  keep_files <- c()
  for (participant in participants) {
    run1file <- sprintf('data/summary_performance/%s_run1_performance.csv', participant)
    run2file <- sprintf('data/summary_performance/%s_run2_performance.csv', participant)
    
    if (all(file.exists(c(run1file, run2file)))) {
      keep_files <- c(keep_files, sprintf('%s_run%d_performance.csv', participant, c(1,2)))
    }
  }
  
  # get all files in the summary_performance folder:
  all_SP_files <- list.files(path='data/summary_performance/')
  
  unused_files <- setdiff(all_SP_files, keep_files)
  
  for (file in unused_files) {
    file.rename(from = sprintf('data/summary_performance/%s', file),
                to   = sprintf('data/unused_summaries/%s',    file))
  }
  
  
  
}