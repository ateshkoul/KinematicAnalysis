#'simple function to generate F-scores (Fisher scores) for ranking features
#'
#' Needs the following:
#' :
##' \enumerate{
##' \item Inputs
##'   \enumerate{
##'     \item Data = Data dataframe
#'      \item featSel = column with different classes
#'
#'  \enumerate{
##' \item Outputs
##'   \enumerate{
##'     \item f-scores
#'
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
fscore <- function(Data,featSep,featureCol){
  tr <- function(m) return(sum(diag(m)))
  # separate positive and negative feature sets
  posIns  <- Data[Data[,featSep]==unique(Data[,featSep])[1],]
  negIns  <- Data[Data[,featSep]==unique(Data[,featSep])[2],]
  f_score <- rep(NA,times=length(featureCol))
  
  for(feature in featureCol){
    f_score[match(feature,featureCol)] = (norm(as.matrix(mean(posIns[,feature])-mean(negIns[,feature])))^2)/(tr(cov(as.matrix(posIns[,feature]))) + tr(cov(as.matrix(negIns[,feature]))))
  }
  return(f_score)
}