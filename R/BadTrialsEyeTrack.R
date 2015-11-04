#' Remove BadTrials from EyeTracking Data
#'
#' \code{BadTrialsEyeTrack} uses information from Raw Data and finds trials that satisfy certain criteria
#' In current implementation, it finds raw values that are at 0,0 or negative x, negative y values
#' It uses a rough threshold of 50 such values (can be changed)
#' in addition, it needs stim file to find tracking ratio and uses a rough criteria of 97% tracking ratio.
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}

BadTrialsEyeTrack <-  function(SubDat,stimData,nSub,nTrials,order){
  # Function to remove the outlier values from EyeTracking data
  # written on 24-04-2015
  # The function uses information from Raw Data and finds trials that satisfy certain criteria
  # In current implementation, it finds raw values that are at 0,0 or negative x, negative y values
  # It uses a rough threshold of 50 such values (can be changed)
  # in addition, it needs stim file to find tracking ratio and uses a rough criteria of 97% tracking ratio.

  # Original script was developed for two sequence data
#   SubDatAB <- SubDat[SubDat$Seq=="AB"]
#   SubDatBA <- SubDat[SubDat$Seq=="BA"]


#   # Get some information about subjects and trials
#   nSubAB <- length(unique(SubDatAB$Subject))
#   nTrialsAB <- length(unique(SubDatAB$Trial))
#
#   # In BA
#   nSubBA <- length(unique(SubDatAB$Subject))
#   nTrialsBA <- length(unique(SubDatAB$Trial))

  # Very critical to have both SubData and StimData in the same order.
  # The script will try to order them in the same way in any case.
  #
  if(!require(plyr)){install.packages('plyr')}


  library(plyr)
  if(missing(nSub)){
    nSub <- length(unique(SubDat$Subject.No))
  }
  #ifelse(missing("nSub"),nSub,nSub <- length(unique(SubDat$Subject.No)))
  if(missing(nTrials)){
    nTrials <- length(unique(SubDat$Trial))
  }
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



  SubDat$X <- as.numeric(SubDat$X)
  SubDat$Y <- as.numeric(SubDat$Y)

#remove trials that have negative X and Y
# negativeXYAB <- which(SubDatAB$X<0|SubDatAB$Y<0)
# negativeXYBA <- which(SubDatBA$X<0|SubDatBA$Y<0)
negativeXY <- which(SubDat$X<0|SubDat$Y<0)


# Remove that have 0 X and Y values
# zeroXYAB <- which(SubDatAB$X==0 & SubDatAB$Y==0)
# zeroXYBA <- which(SubDatBA$X==0 & SubDatBA$Y==0)
zeroXY <- which(SubDat$X==0 & SubDat$Y==0)



# Assign them to be NA
# SubDatAB[c(negativeXYAB,zeroXYAB),c("X","Y")] <- NA
# SubDatBA[c(negativeXYBA,zeroXYBA),c("X","Y")] <- NA
SubDat[c(negativeXY,zeroXY),c("X","Y")] <- NA



# find Trials that have  more than 50 NA time points (arbitrary).

# missingDataAB <- ddply(SubDatAB,c("Subject","Trial"),summarise,N = sum(is.na(X)*1))
# missingDataBA <- ddply(SubDatBA,c("Subject","Trial"),summarise,N = sum(is.na(X)*1))

missingData <- ddply(SubDat,c(paste("Subject.",order,sep=""),"Trial"),summarise,N = sum(is.na(X)*1))
missingData <- missingData[order(missingData[,paste("Subject.",order,sep="")]),]

#missingData <- ddply(SubDat,c("Subject.No","Trial"),summarise,N = sum(is.na(X)*1))
#missingData <- missingData[order(as.character(missingData$Subject.Name)),]

# missingDatAB <- which(missingDataAB$N>50)
# missingDatBA <- which(missingDataBA$N>50)

missingDat <- which(missingData$N>50)


# load stim information
# stimDataAB<-stimData[stimData$Sequence=="AB",]
# stimDataBA<-stimData[stimData$Sequence=="BA",]


# Check that both missinData and stim data correspond to same subjects
if(any(!(stimData[,paste("Subject.",order,sep="")]==missingData[,paste("Subject.",order,sep="")])) | (!(sum((stimData$Order-missingData$Trial)>0)==0))) {
  print("Error")
  warning('Order of subjects is not the same for Raw Data and stimData (tracking ratio), order them accordingly')
  stop("Order Different")
}

# Check that both missinData and stim data correspond to same subjects
# if(!(sum((stimData[,paste("Subject.",order,sep="")]==missingData[,paste("Subject.",order,sep="")])>0)==0 & sum((stimData$Order-missingData$Trial)>0)==0)){
#   print("Error")
#   warning('Order of subjects is not the same for Raw Data and stimData (tracking ratio), order them accordingly')
#   stop("Order Different")
# }


# 97 is arbitrary
# LowTrackingAB <- which(as.numeric(stimDataAB$Tracking.Ratio....)<97)
# LowTrackingBA <- which(as.numeric(stimDataBA$Tracking.Ratio....)<97)

LowTracking <- which(as.numeric(stimData$Tracking.Ratio....)<97)


# badTrialsAB <- sort(unique(c(missingDatAB,LowTrackingAB)))
# badTrialsBA <- sort(unique(c(missingDatBA,LowTrackingBA)))

badTrials <- sort(unique(c(missingDat,LowTracking)))


# have to automatically select no. of trials and subjects
#nTrials <- 400
#nSub <- 9
# BadTrialsNoAB<-matrix(c(rep(0,nTrialsAB*nSubAB,1)),nTrialsAB*nSubAB,1)
# BadTrialsNoAB[badTrialsAB] <- 1
#
#
# BadTrialsNoBA<-matrix(c(rep(0,nTrials*nSub,1)),nTrials*nSub,1)
# BadTrialsNoBA[badTrialsBA] <- 1

BadTrialsNo<-matrix(c(rep(0,nTrials*nSub,1)),nTrials*nSub,1)
BadTrialsNo[badTrials] <- 1





#save(badTrialsAB,badTrialsBA,file="C:/Users/koul/Google Drive/Research_Project/Understanding intentions/Reading Intention/Scripts/ActionIntentionObservation/Eye tracking/PlaceVsPour/Results/badTrialsAutomatic.RData")
# TotalBadTrialsAB <- sort(unique(c(which(BadTrialsNoAB==1),which(ManualBadTrialsNoAB==1))))
# TotalBadTrialsBA <- sort(unique(c(which(BadTrialsNoBA==1),which(ManualBadTrialsNoBA==1))))

# If we have information about manual bad trials; otherwise the same
#TotalBadTrials <- sort(unique(c(which(BadTrialsNo==1),which(ManualBadTrialsNo==1))))



TotalBadTrials <- matrix(c(BadTrialsNo),length(BadTrialsNo),1)

# EyeDataBottle <- EyeDataBottle[-TotalBadTrials,]
# EyeDataHand <- EyeDataHand[-TotalBadTrials,]
result = list(missingData=missingData, TotalBadTrials = TotalBadTrials,nSub=nSub,nTrials=nTrials,BadTrialsNo = BadTrialsNo,LowTracking=LowTracking,missingDat=missingDat,stim=as.numeric(stimData$Tracking.Ratio....))
return(result)

}
