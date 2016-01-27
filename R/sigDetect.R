#' Part of Data Analysis behavioral Data from Eprime/text/Excel files specifying single or multiple accuracy columns
#'
#' \code{sigDetect} calculates signal detection measures from the input dataframe
#' its necessary to have columns of participant response (MovieStart.RESP and ITI.Resp), condition column (Condizione)
#' and Confidence
#'
#' The current setup is based on assumption that the participants can respond in two slides - respSlides[1] and respSlides[2]
#' if this is not the case, it goes to NA
#'
#' Inputs:
#'        Data** - Data frame that is input with columns of response, condition and confidence
#'        signal** - condition that will be used as the signal (Compulsary)
#'        condition - name of the column that has the condition (guesses if not present)
#'        respSlides - response slides where participants can respond (in Eprime e,g,)
#'        yesResp - The response (1 or 2) that is associated with a correct response
#'                  (calculates automatically if not provided)
#'                  that is, which response is yes to the signal. this arises because usually
#'                  the keys are interchanged in psychology experiments (to control for bias)
#'                  This roughly corresponds to the HIT (and is calculated in that fashion)#'
#'
#' Output:
#'       Data - dataframe with columns of Dprime, Dprime_all, signal_all, signal and roc confidence
#'              Dprime_all and signal_all - consider even the missed responses
#'              Better to use Dprime and signal (as HIT,MISS,FA and CR are not really applicable in missed response)
#'
#'
#' Currently,

#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}


sigDetect <- function(Data,signal,yesResp,condition,respSlides){

  # default to c("MovieStart.RESP","ITI.RESP")
  if(missing(respSlides)){
    respSlides <- c("MovieStart.RESP","ITI.RESP")
  }

  if(missing(condition)){
    # guess the name (valid if you do experiment in italian and spanish to)
    condition <- names(Data)[grep("Condi",names(Data))]
  }

  # if only one response slide is present, there is no other alternative and it will be NA
  if(length(respSlides)==1) {
    respSlides[2]= NA
  }

  if(missing(yesResp)){
    yesResp = unique(Data[which(Data[,condition] == signal & Data$ACC==1),"Resp"])
  }

    if(!"b" %in% colnames(Data)){
    # if not already implemented in analyse_behav.R
    Data[,"Resp"]  <- ifelse(!is.na(Data[,respSlides[1]]),Data[,respSlides[1]],Data[,respSlides[2]])
  }


  Data[,"signal_all"] <- ifelse(Data[,condition]==signal,1,0)
  # using only the trial where the participants responded (used in according to Andrea)
  Data[,"signal"] <- ifelse(!is.na(Data$Resp),Data[,"signal_all"],NA)

  #Data[,"Dprime"] <- ifelse(Data$signal==1 & Data$Resp ==yesResp,"HIT",ifelse(Data$signal==1 & Data$Resp ==yesResp,"MISS",ifelse(Data$signal==0 & Data$Resp ==yesResp,"CR",ifelse(Data$signal==0 & Data$Resp ==yesResp,"FA",NA))))
  Data[,"Dprime_all"] <- ifelse(Data$signal==1 & Data$Resp ==yesResp,"HIT",ifelse(Data$signal==0 & Data$Resp ==yesResp,"FA",ifelse(Data$signal==1 & !(Data$Resp ==yesResp),"MISS",ifelse(Data$signal==0 & !(Data$Resp ==yesResp),"CR",NA))))

  # using only the trial where the participants responded (used in according to Andrea)
  Data[,"Dprime"] <- ifelse(!is.na(Data$Resp),Data[,"Dprime_all"],NA)

  Data[,"ROC_confid"] <- ifelse(Data$Dprime=="HIT" | Data$Dprime=="FA",Data$Confidence.RESP+4,abs(Data$Confidence.RESP-5))
  return(Data)

}
