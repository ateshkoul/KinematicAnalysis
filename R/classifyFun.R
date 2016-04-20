#'simple function to create permutation testing of a classifier
#'
#' Needs the following:
#' :
##' \enumerate{
##' \item Inputs
##'   \enumerate{
##'     \item X = Data matric
#'      \item Y = labels
#'
#'  \enumerate{
##' \item Outputs
##'   \enumerate{
##'     \item p-value from permutation
#'
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
classifyFun <- function(classifier,Data,predictorCol,selectedCols){
  # a simplistic k-fold crossvalidation
  # For cross validation
  library(e1071)
  library(caret)
  # dont use a constant set.seed with permutation testing
  # u will get a constant accuracy!!
  set.seed(111)

  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))

  # convert string to function
  classifierFun <- get(classifier)


  # get the features
  selectedColNames <- names(Data)[selectedCols]
  # get feature columns without response
  featureColNames <- selectedColNames[-grep(names(Data)[predictorCol],selectedColNames)]

  Data = Data[,selectedCols]
  Data[,predictorCol] = factor(Data[,predictorCol])

  # if predictor has missing, remove those columns
  if(sum(is.na(Data[,predictorCol]))>0) Data <- Data[!is.na(Data[,predictorCol]),]



  # defaults
  k = 3
  # use stratified cross validation instead
  # use 60% data for training
  # divide the data into 3 parts:
  # 1. for tuning parameters
  # 2. for training model
  # 3. for testing prediction (on a data that it has never seen ever)
  #
  # Keeping more for tuning
  trainIndexOverall <- createDataPartition(Data[,predictorCol], p = .6,list = FALSE,times = k)


  # leave first part for tuning the classifier
  tuneTrainData <- Data[trainIndexOverall[,1],]
  ModelTrainData <- Data[trainIndexOverall[,2],]
  ModelTestData <- Data[trainIndexOverall[,3],]


  print('Begining Tuning Classifier')
  obj <- tune(classifierFun, train.y = tuneTrainData[,predictorCol],train.x = tuneTrainData[,featureColNames],
              ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
              tunecontrol = tune.control(sampling = "fix"))

  print(summary(obj))
  print(plot(obj))

  # obtain best parameter values
  if(classifier=="svm"){
    cost = as.numeric(obj$best.parameters[grep("cost",names(obj$best.parameters))])
    gamma = as.numeric(obj$best.parameters[grep("gamma",names(obj$best.parameters))])
  }




  kFold <- 10
  #initialising vectors
  acc <- rep(NA,kFold)
  accTest <- rep(NA,kFold)

  trainIndexModel <- createDataPartition(ModelTrainData[,predictorCol],
                                         p = .8,list = FALSE,times = kFold)

  print('Begining k-fold Classification')
  for (i in 1:kFold){
    trainDataFold <- ModelTrainData[trainIndexModel[,i],]
    testDataFold <- ModelTrainData[-trainIndexModel[,i],]
    # the classifier has generic
    model <- classifierFun(y=trainDataFold[,predictorCol],x=trainDataFold[,featureColNames],gamma=gamma,cost=cost)
    # test with train data
    pred <- predict(model, testDataFold[,featureColNames])
    acc[i] <- sum(1 * (pred==testDataFold[,predictorCol]))/length(pred)
    predTest <- predict(model, ModelTestData[,featureColNames])
    accTest[i] <- sum(1 * (predTest==ModelTestData[,predictorCol]))/length(predTest)
  }
  print(paste("Mean CV Accuracy",mean(acc)))
  print(paste("Mean Test Accuracy",mean(accTest)))


Results <- list(acc=acc,tuneObj = obj,accTest=accTest)
return(Results)

}
