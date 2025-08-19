
prep12data <- function() {
  
  demographics <- read.csv("data/DOrepo/exp1_2/demographics.csv", stringsAsFactors = FALSE)
  
  # check if files/folders are there for all participants in demographics:
  for (ppid in demographics$participant) {
    checks <- c()
    checks <- c(checks, file.exists(paste0('data/DOrepo/exp1_2/summary/', ppid, '_run1_performance.csv')))
    checks <- c(checks, file.exists(paste0('data/DOrepo/exp1_2/summary/', ppid, '_run2_performance.csv')))
    checks <- c(checks, dir.exists( paste0('data/DOrepo/exp1_2/raw/', ppid, '_run1/')))
    checks <- c(checks, dir.exists( paste0('data/DOrepo/exp1_2/raw/', ppid, '_run2/')))
    if (!all(checks)) {
      cat(paste0("Missing files for participant: ", ppid, "\n"))
      print(checks)
    }
  }
  
  # check if all summary files and all raw data folders align with a participant in the demographics file:
  summary_files <- list.files('data/DOrepo/exp1_2/summary/', pattern = '_performance.csv')
  for (sf in summary_files) {
    ppid <- strsplit(sf, '_')[[1]][1]
    if (!(ppid %in% demographics$participant)) {
      cat(paste0("No demographics for summary file: ", sf, "\n"))
      # remove file?
    }
  }
  
  raw_folders <- list.files('data/DOrepo/exp1_2/raw/')
  for (rf in raw_folders) {
    ppid <- strsplit(rf, '_')[[1]][1]
    if (!(ppid %in% demographics$participant)) {
      cat(paste0("No demographics for raw data folder: ", rf, "\n"))
      # remove folder?
    }
  }
}