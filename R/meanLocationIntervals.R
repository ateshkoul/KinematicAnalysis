#' Function to get mean values of X and Y co-ordinates after time = time for each subject
#'
#' Assumes the rawData has columns = X,Y and NormalisedTime
#' it also has a column Subject
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
meanLocationIntervals <- function(RawData,timeInter,order){
  # Function to get mean values of X and Y co-ordinates after time = time
  # for each subject

  # Assumes the rawData has columns = X,Y and NormalisedTime
  # it also has a column Subject

  # Author:
  # Atesh Koul, RBCS (atesh.koul@iit.it)
  # Istituto Italiano di technologia
  # last changes 04-11-2015
  if(!require(plyr)){install.packages('plyr')}


  if(missing(order)){
    order <- "No"
  }



  # libraries

  require(plyr)
  # preprocessing and checking


  # Use only the time after start of the Message
  # 12-05-2015
  # some issues with the way Raw Data was processed.
  # Using CorrectTime was fine but using Normalised Time instead created NA values
  # Don't know why
  RawData <- RawData[RawData$CorrectTime>=0,]


  RawData$X <- as.numeric(RawData$X)
  RawData$Y <- as.numeric(RawData$Y)
  RawData$NormalisedTime <- as.numeric(RawData$NormalisedTime)

  MeanSubjects <- list()
  MeanSubjectsTrial <- list()
  for(i in 1:(length(timeInter)-1)){
    # necessary to include an interval as most values would not have exactly the time as .5
    # The value 0.003 is arbitrary (in some sense) as it is derived as a general value from the
    # difference of two consecutive acquisitions in the data

    # Inserting Trial is necessary so as to remove the outliers
    # Currently (12-05-2015) there doesn't seem to be a good way to encode bad Trials in the
    # Raw time series data. It's easier to perform it at the interpretation level (or at the data level)
    MeanSubjectsTrial[[i]] <- ddply(RawData[which(RawData$NormalisedTime>=timeInter[i] & RawData$NormalisedTime<=timeInter[i+1]),],c(paste("Subject.",order,sep=""),"Trial"),summarise,
                                    MeanX = mean(X,na.rm=T),MeanY = mean(Y,na.rm=T),Time=NormalisedTime[1])
}

  return(MeanSubjectsTrial)


}
