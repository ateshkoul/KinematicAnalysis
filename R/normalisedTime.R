#' Function to get normalised parameters from Eye tracking time series raw data
#'
#'
#' Needs the following:
#' :
##' \enumerate{
##' \item SubDat
##'   \enumerate{
#'      \item Subject.(Name or No)
#'      \item Trial
#'      \item Time (has MSG as one value)
#'    \item X and Y gaze positions
#' }
#'  \item stimData
#'    \enumerate{
#'    \item Duration
#' }
#' }
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
normalisedTime <- function(SubData,stimData,order){
  # Author:
  # Atesh Koul, RBCS (atesh.koul@iit.it)
  # Istituto Italiano di technologia
  #
  # last changes
  # 04-11-2015
  if(!require(plyr)){install.packages('plyr')}


  # Getting the time interval fixed in human readable form:
  library(plyr)
  if(missing(order)){
    order <- "No"
  }

  # This issue arises because there is a message in X and it takes it as a factor

  if(order=="Name"){
    SubDat$Subject.Name <- as.character(SubDat$Subject.Name)
    SubDat <- SubDat[order(as.character(SubDat$Subject.Name)),]
    stimData <- stimData[order(as.character(stimData$Subject.Name)),]

  } else if(order=="No"){
    SubDat$Subject.No <- as.numeric(SubDat$Subject.No)
    SubDat <- SubDat[order(SubDat$Subject.No),]
    stimData$Subject.No <- as.numeric(stimData$Subject.No)
    stimData <- stimData[order(stimData$Subject.No),]
  }


  # The idea is to divide each trial duration by the video length. This is important
  # so that we can compare things.
  # The way this would be done is by creating a separate column in SubDat data that repeats
  # the value of Duration of video. This value would be used to normalise SubDat
  # at the same time


  # get the number of data points for each trial (Here instead of Time could be any other variable)
  # Time is used as this would be there for all the values (compared to say X which may have NA value)
  DataPoints <- ddply(SubDat,c(paste("Subject.",order,sep=""),"Trial"),summarise,len = length(Time))

  # Add a column to the data structure
  SubDat[,"Duration"] <- rep(stimData$Duration,DataPoints$len)

  # Check if the values are correct

  pepa <- ddply(SubDat,c(paste("Subject.",order,sep=""),"Trial"),summarise,dura = unique(Duration))
  sum(pepa$dura - stimData$Duration)
  length(pepa)==nrow(SubDat)
  #


  SubDat <- ddply(SubDat,c(paste("Subject.",order,sep=""),"Trial"),
                  mutate,T0=Time[Type=="MSG"],CorrectTime = (Time-T0)/1000,NormalisedTime = CorrectTime/Duration)

  return(SubDat)


}
