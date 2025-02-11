

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