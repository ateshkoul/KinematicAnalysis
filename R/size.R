#' Matlab like size function
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
size <- function(Dataframe){

  # Author:
  # Atesh Koul, RBCS (atesh.koul@iit.it)
  # Istituto Italiano di technologia
  rows <- nrow(Dataframe)
  cols <- ncol(Dataframe)
  print(paste('nrows = ',rows,'ncols = ',cols))
  return(list(nrows=rows,ncols =cols))


}
