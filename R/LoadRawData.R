#' Function to load the Raw Data exported from BeGaze software.
#'
#' This is necessary for removal of the outlier trials.
#' This feeds data to the wrongTrials.R function
#' This also assumes that you export data as comma separated file in BeGaze.
#' Only input is the delimiter in the file after which the subject name or subject No.
#' (or both) are present. This in brief is the last portion of the name of the BeGaze experiment.
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
LoadRawData <- function(delim){

  # Function to load the Raw Data exported from BeGaze software.
  # This is necessary for removal of the outlier trials.
  # This feeds data to the wrongTrials.R function

  # This also assumes that you export data as comma separated file in BeGaze.
  # Only input is the delimiter in the file after which the subject name or subject No.
  # (or both) are present. This in brief is the last portion of the name of the BeGaze experiment.



  # 28-04-2015


  files <- choose.files(caption="Select Raw Data Files")
  SubDat <- list()
  nSub <- length(files)
  for (i in 1:nSub) {
    # assumes that the file name is of the kind - "C:\\....\\SeqAB\\PourVsDrinkAllSubjects_1_MC_001 Samples.txt"
    SubNo <- strsplit(strsplit(files[i],delim)[[1]][2],"_")[[1]][1]
    SubName <- strsplit(strsplit(files[i],delim)[[1]][2],"_")[[1]][2]
    sub <- read.csv(files[i],stringsAsFactors = F,na.strings =  c('-','NaN'))
    sub <- cbind(SubNo,SubName,sub)
    SubDat <- rbind(SubDat,sub)
  #SubDat[[i]] <- cbind(SubNames[i],sub)
}
rm(sub)


# based on the assumption that details of AOI hit, and stimulus properties are checked in the BeGaze output
colnames(SubDat) <- c("Subject.No","Subject.Name","Time","Type","Trial","X","Y","AOI.Hit","Event","VideoName")

SubDat$Subject.No <- as.numeric(levels(SubDat$Subject.No))[SubDat$Subject.No]

return(SubDat)
#save(SubDatAB,SubDatBA,file="SubRawDataUncorrected.RData")
}
