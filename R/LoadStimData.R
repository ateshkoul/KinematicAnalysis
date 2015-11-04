#' Function to load stim data containing Tracking ratio from BeGaze
#'
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
LoadStimData <- function(){
  # function to load stim data containing Tracking ratio from BeGaze
  # 13-05-2015

  # Author:
  # Atesh Koul, RBCS (atesh.koul@iit.it)
  # Istituto Italiano di technologia

 file <- file.choose()
 # stringsAsFactors allows character to be loaded as characters instead of factors
 Data <- read.csv(file,stringsAsFactors=F,,na.strings =  c('-','NaN'))


  return(Data)
}
