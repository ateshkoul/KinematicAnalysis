#' Function to select a  specific trial from raw or data containing subject and Trial information
#'
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
selectTrial <- function(Dataframe,Subject,trial){

  # Atesh koul (atesh.koul@iit.it)
  # RBCS,Istituto Italiano di technologia
  # 15-05-2015

  # I wanted to give you the freedom to choose whether you want to ask from subject.Name or Subject.No
  # But it's just too much to ask. May be later.
  # SubjectCol <- names(Dataframe)[grep("^Sub",names(Dataframe))[1]]


  Data <- Dataframe[which(Dataframe[,"Subject.No"]==Subject & Dataframe[,"Trial"]==trial),]
  return(Data)

}
