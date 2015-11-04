#' Function to load AOI Data from BeGaze (exported as csv file)
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
LoadAOIData <- function(){
  # Function to load AOI Data from BeGaze (exported as csv file)

  # 13-05-2015

  file <- file.choose()
  # stringsAsFactors allows character to be loaded as characters instead of factors
  Data <- read.csv(file,stringsAsFactors=F,na.strings =  c('-','NaN'))

  Data <- ddply(Data,.(Subject),mutate,Subject.No = strsplit(Subject,"_")[[1]][1],Subject.Name = strsplit(Subject,"_")[[1]][2])



  return(Data)
}
