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
#'        Data - Data frame that is input with columns of response, condition and confidence
#'        signal - condition that will be used as the signal
#'        condition - name of the column that has the condition
#'        respSlides - response slides where participants can respond (in Eprime e,g,)
#'
#' Output:
#'       Data - dataframe with columns of Dprime and roc confidence
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
    condition <- "Condizione"
  }

  # if only one response slide is present, there is no other alternative and it will be NA
  if(length(respSlides)==1) {
    respSlides[2]= NA
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

  # this was suggested but this would be esentially the same as just signal (there is a value for Dprime if there is a response)
  # we dont know how to best characterize missed responses (don't fit in either category of HIT,MISS,FA or CR)
  # Data[,"signal_Dprime"] <- ifelse(is.na(Data$Dprime),NA,Data$signal)

  # Add cases where we don't have the confidence slide
  if(!is.null(Data$Confidence.RESP)){
    Data[,"ROC_confid"] <- ifelse(Data$Dprime=="HIT" | Data$Dprime=="FA",Data$Confidence.RESP+4,abs(Data$Confidence.RESP-5))
  }

  return(Data)

}
