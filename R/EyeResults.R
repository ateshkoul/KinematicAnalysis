#' Function to save results for AOI fixation
#'
#' Specific for AOIs of Hand and Bottle for Reading intention experiment
#' Assumes EyeData has columns of , intention,Subject.No or Subject.Name,
#' Data columns are Net.Dwell.Time..ms., Entry.Time..ms.
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}

EyeResults <- function(EyeDataHand,EyeDataBottle,order){
  # Function to save results for AOI fixation
  # Specific for AOIs of Hand and Bottle for Reading intention experiment

  # Assumes EyeData has columns of , intention,Subject.No or Subject.Name,
  # Data columns are Net.Dwell.Time..ms., Entry.Time..ms.

  # 13-05-2015
  # last changes
  # 04-11-2015
  if(!require(plyr)){install.packages('plyr')}


  if(missing(order)){
    order <- "No"
  }

  library(plyr)





  # This is a bit tricky if you don't have both Subject.No and Subject.Name;
  # if you get an error that no. of rows don't match. It's just that one or more
  # of the parameters in this case are not defined.
predictiveEye <-  data.frame(predictEye = (EyeDataHand$Entry.Time..ms. < EyeDataBottle$Entry.Time..ms.)*1,
                             neitherTargets = ((is.na(EyeDataHand$Entry.Time..ms.) & is.na(EyeDataBottle$Entry.Time..ms.))*1),
                             BothTargets = ((!is.na(EyeDataHand$Entry.Time..ms.) & !is.na(EyeDataBottle$Entry.Time..ms.))*1),
                             BottleOnly = (!is.na(EyeDataBottle$Entry.Time..ms.) & is.na(EyeDataHand$Entry.Time..ms.))*1,
                             HandOnly = (is.na(EyeDataBottle$Entry.Time..ms.) & !is.na(EyeDataHand$Entry.Time..ms.))*1,
                             Subject.No = EyeDataBottle$Subject.No,Subject.Name=EyeDataBottle$Subject.Name,
                             intention=EyeDataBottle$intention)
                             #blockNo.=EyeDataBottle$blockNo.,




SubjectDetails <- ddply(EyeDataBottle,c("intention",paste("Subject.",order,sep="")),summarize,N = length(as.numeric(Net.Dwell.Time..ms.)))

EyeDataResultsBottleAll <- ddply(EyeDataBottle,c("intention",paste("Subject.",order,sep="")),summarise,
                                 N    = length(Net.Dwell.Time..ms.),
                                 NetDwellBottleMean = mean(Net.Dwell.Time..ms.,na.rm=TRUE),
                                 NetDwellBottleMeanPercent = mean(Net.Dwell.Time....,na.rm=TRUE),
                                 #NetDwellBottleSd   = sd(Net.Dwell.Time..ms.,na.rm=TRUE),
                                 #NetDwellBottleSe   = NetDwellBottleSd / sqrt(N),
                                 EntryTimeBottleMean = mean(as.numeric(Entry.Time..ms.),na.rm=TRUE)
                                 #,EntryTimeBottleSd   = sd(as.numeric(Entry.Time..ms.),na.rm=TRUE),
                                 #EntryTimeBottleSe   = EntryTimeBottleSd / sqrt(N)
)[,-(1:3)]



EyeDataResultsHandAll <- ddply(EyeDataHand,c("intention",paste("Subject.",order,sep="")),summarise,
                               N    = length(Net.Dwell.Time..ms.),
                               NetDwellHandMean = mean(Net.Dwell.Time..ms.,na.rm=TRUE),
                               NetDwellHandMeanPercent = mean(Net.Dwell.Time....,na.rm=TRUE),
                               #NetDwellHandSd   = sd(Net.Dwell.Time..ms.,na.rm=TRUE),
                               #NetDwellHandSe   = NetDwellHandSd / sqrt(N),
                               EntryTimeHandMean = mean(as.numeric(Entry.Time..ms.),na.rm=TRUE)
                               #,EntryTimeHandSd   = sd(as.numeric(Entry.Time..ms.),na.rm=TRUE),
                               #EntryTimeHandSe   = EntryTimeHandSd / sqrt(N)
)[,-(1:3)]

predictEyeResultsAll <- ddply(predictiveEye,c("intention",paste("Subject.",order,sep="")),summarise,
                              N    = length(predictEye),
                              predictEye = sum(predictEye,na.rm =T),
                              percentPredict = (predictEye/N)*100,
                              neitherTargetpercent = (sum(neitherTargets)/N)*100,
                              BothTargetpercent = (sum(BothTargets)/N)*100,
                              BottleOnlypercent = (sum(BottleOnly)/N)*100,
                              HandOnlypercent = (sum(HandOnly)/N)*100
                              #,Total = neitherTarget+BothTarget+BottleOnly+HandOnly
)[,-(1:2)]



EyeDataResultsAll <- cbind(SubjectDetails,EyeDataResultsBottleAll,EyeDataResultsHandAll,predictEyeResultsAll)
return(EyeDataResultsAll)

}

