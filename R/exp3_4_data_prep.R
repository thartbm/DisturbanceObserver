
# checks of data (presence of files / folder) against demographics and vice versa for exp 3 & 4
prep34data <- function() {
  
  demographics <- read.csv("data/DOrepo/exp3_4/demographics.csv", stringsAsFactors = FALSE)
  
  demographics$first  <- ''
  demographics$second <- ''
  
  conditions <- c('IGN-CCW-a0.50', 'IGN-CCW-a0.75', 'IGN-CCW-a1.00', 'IGN-CW-a0.50', 'IGN-CW-a0.75', 'IGN-CW-a1.00', 'LRN-CCW-a0.50', 'LRN-CCW-a0.75', 'LRN-CCW-a1.00', 'LRN-CW-a0.50', 'LRN-CW-a0.75', 'LRN-CW-a1.00')
  
  
  # correct participant ID (remove leading string "PID"):
  demographics$participant <- gsub('PID', '', demographics$participant, fixed=TRUE)
  
  
  # which conditions did the participant do?
  # for (ppid in c('0b15d6')) {
  for (ppid in demographics$participant) {
    # print(ppid)
    ppconds <- c()
    for (cond in conditions) {
      filename <- paste0('data/DOrepo/exp3_4/raw/', cond, '/data/', ppid, '/', ppid, '_performance.csv')
      # print(filename)
      if (file.exists(filename)) {
        ppconds <- c(ppconds, cond)
      }
    }
    # print(ppconds)
    if (length(ppconds) < 2) {
      cat(paste0("Not enough data for participant: ", ppid, "\n"))
      next
    }
    one_df <- read.csv(paste0('data/DOrepo/exp3_4/raw/', ppconds[1], '/data/', ppid, '/', ppid, '_performance.csv'), stringsAsFactors = FALSE)
    two_df <- read.csv(paste0('data/DOrepo/exp3_4/raw/', ppconds[2], '/data/', ppid, '/', ppid, '_performance.csv'), stringsAsFactors = FALSE)
    one_start <- one_df$trialstarttime_s[1]
    two_start <- two_df$trialstarttime_s[1]
    if (one_start < two_start) {
      demographics$first[demographics$participant == ppid]  <- ppconds[1]
      demographics$second[demographics$participant == ppid] <- ppconds[2]
    } else {
      demographics$first[demographics$participant == ppid]  <- ppconds[2]
      demographics$second[demographics$participant == ppid] <- ppconds[1]
    }
  }
  
  # DELETE DELETE participants with missing data
  
  
  write.csv(demographics, "data/DOrepo/exp3_4/demographics.csv", row.names = FALSE)
  
  
  # now do the reverse check: which files in the data set do not belong to a participant in the demographics file?
  
}