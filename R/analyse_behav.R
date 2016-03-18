#' Analyse behavioral Data from Eprime/text/Excel files specifying single or multiple accuracy columns
#'
#' \code{analyse_behav} uses takes information from a named text file and extracts multiple behavioral measures
#' In current implementation, it finds raw values that are at 0,0 or negative x, negative y values
#' uses packages: openxlsx, plyr
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}


analyse_behav <- function(resultfiles,scriptfile,signal,yesResp,save,integration,ResultCat=NULL){
  #
  # Atesh koul
  # atesh.koul@gmail.com
  # 06-10-2015
  # adapted from Motor simulation.R script
  # 06-11-2015
  # updated to join both functions:
  # now the script goes directly from the raw txt data to signal detection parameters
  # I deliberately kept signal and yesResp as compulsary as otherwise its dangerous and do bad things
  # 07-01-2016

  # load the library
  library(openxlsx)
  library(plyr)

#   if(missing(signal) | missing(yesResp)){
#     stop("Signal or yesResp has not been provided. Please check again")
#   }

    if(missing(signal)){
      # good to provide the signal
      stop("Signal or yesResp has not been provided. Please check again")
    }


  # default to TRUE
  if(missing(save)){
    save <- F
  }
    if(missing(resultfiles)){
    # choose the tex
    filenames <- choose.files(filters = Filters[c("txt"),])
    directory <- dirname(filenames)
  }

  # forcebly say integration is 0 for now.
  # integration = 0

  # colnames from previous experiment Exp4 predizioni
  # For main experiment
  colNames <-  c("ExperimentName","Subject","Age","Sex","Condizione","Video","StaticMs","DurataVideo","MovieStart.FramesDropped","MovieStart.RESP","ITI.RESP","MovieStart.ACC","ITI.ACC","MovieStart.RT","ITI.RT","Confidence.RESP","Confidence.RT")

  # for part 2
  # colNames <-  c("ExperimentName","Subject","Age","Sex","Condizione","Video","StaticMs","DurataVideo","MovieStart.FramesDropped","MovieStart.RESP","ITI.RESP","MovieStart.ACC","ITI.ACC","MovieStart.RT","ITI.RT","VasSlide.VAS","VasSlide.RT")


  DataFull <- NULL
  k <- list()
  DataSig <- list()
  Results <- list()
  ResultsSelf <- list()

  wbResults <- createWorkbook("Wilma")
  SubResults <- createWorkbook("Freddy")
  #wb <- createWorkbook("Fred")

  #signal <- "Drinking"
  RawMerged <- NULL
  for(i in filenames){
    Data <- read.table(file = i,header = T,sep='\t')
    Data <- Data[Data$Procedure=="ShowStim"|Data$Procedure=="ShowStim2",colNames]
    RawMerged <- rbind(RawMerged,Data)
  }

  # get the condition column (anything that contains Condi); not obvious as it could have
  # been in any language, condizione, condition.. whatever helps u sleep at night :)
  # you can use any of the files as they all should contain same colnames
  ConditionCol <- names(Data)[grep("Condi",names(Data))]

  # categories based on which u want ur results
  ResultCat <- c(ResultCat,ConditionCol)
  rm(Data)


  for(subs in unique(RawMerged$Subject)){
    # old implementation with each filename for loading data. now i load data before and loop over the subjects
    # Data <- read.table(filenames,T,'\t')

    # Take one subject data at a time
    Data <- RawMerged[RawMerged$Subject==subs,]

    #Data <- Data[Data$Procedure=="ShowStim"|Data$Procedure=="ShowStim2",colNames]


    # get the condition column (anything that contains Condi); not obvious as it could have
    # been in any language, condizione, condition.. whatever helps u sleep at night :)
    ConditionCol <- names(Data)[grep("Condi",names(Data))]

    # categories based on which u want ur results
    ResultCat <- c(ResultCat,ConditionCol)


    # Get the subject no. for saving data
    sub = unique(as.character(Data$Subject))

    # mentioning what are the response slides (also implemented in sigDetect.R)
    respSlides <- c("MovieStart.RESP","ITI.RESP")

    # generating a generic resp column
    Data[,"Resp"]  <- ifelse(!is.na(Data[,respSlides[1]]),Data[,respSlides[1]],Data[,respSlides[2]])


    # better way to formalise rather than copy the one in excel
    Data[,"ACC"] <- ifelse(Data$MovieStart.ACC+Data$ITI.ACC==2,1,Data$MovieStart.ACC+Data$ITI.ACC)

    # accuracy only for cases where the participants responded
    Data[,"Resp_Acc"] <- ifelse(!is.na(Data$Resp),Data[,"ACC"],NA)

    # getting correct normalised RT in 1 step
    #Data[,"NormRT"] <- ifelse((Data$MovieStart.ACC & Data$ITI.ACC)==1,(Data$MovieStart.RT/Data$DurataVideo),
    #                          (Data$MovieStart.RT/Data$DurataVideo)*Data$MovieStart.ACC + ((Data$ITI.RT+Data$DurataVideo)/Data$DurataVideo)*Data$ITI.ACC)
    #
    # Wrong way - there are cases where participants press button in both slides and can get wrong in the next one
    # Data[,"NormRT"] <- ifelse((Data$MovieStart.ACC & Data$ITI.ACC)==1,(Data$MovieStart.RT/Data$DurataVideo),
    #                          (Data$MovieStart.RT/Data$DurataVideo) + (Data$ITI.RT+Data$DurataVideo)/Data$DurataVideo)
    #
    # This case takes care of the scenarios, if the participant responded first, irrespective of whether it
    Data[,"NormRT"] <- ifelse(Data$MovieStart.RT>0,(Data$MovieStart.RT/Data$DurataVideo),ifelse(Data$ITI.RT>0,(Data$ITI.RT+Data$DurataVideo)/Data$DurataVideo,NA))

    # get corrected normalized RTs for only correct trials
    Data[!(Data[,"ACC"]==0),"CorrectRT"] <- Data[!(Data[,"ACC"]==0),"NormRT"]
    Data[which(!(scale(Data$NormRT) >2.5) & !(scale(Data$NormRT) < -2.5)),"RT_outRemoved"] <- Data[which(!(scale(Data$NormRT) >2.5) & !(scale(Data$NormRT) < -2.5)),"NormRT"]

    Data[!is.na(Data$NormRT),"Confidence"] <- Data$Confidence.RESP[!is.na(Data$NormRT)]
    Data[!is.na(Data$NormRT),"ConfRT"] <- Data$Confidence.RT[!is.na(Data$NormRT)]

    # automatically determine which is the yesResponse; based on the logic that yesResponse is
    # basically the hit - response key that provides correct accuracy on signal trials
    yesResp = unique(Data[which(Data$Condizione == signal & Data$ACC==1),"Resp"])

      DataSig[[subs]] <- sigDetect(Data,signal,yesResp)



    DataSig[[subs]] <- sigDetect(Data,signal,yesResp)




    ## integration with stumuli information
    if(integration ==1){
      if(missing(scriptfile)){
      scriptfile <- choose.files(caption = "Select Script File")
      wbSub <- loadWorkbook(scriptfile)
      }


      ## gets information about the subject number - this is assumed to be in the filename e.g. "I:\\Motor_simulation_strategies\\Results\\Actionvideo_SO_BA_14-1-1.txt"
      # sub = strsplit(strsplit(basename(i),"_")[[1]][4],"-")[[1]][1]
      # use the subject number in the Data
      sub = as.character(unique(DataSig[[subs]]$Subject))

      # specific to motor simulation
      # I may check if I can implement equivalent checks for other experiments
      # for now (28-01-2016), I solve the MS issue

      DataTrials <- readWorkbook(wbSub,sheet = sub)
      DataTrials <- DataTrials[,1:10]

#       # this sum should be =30
#       MSGVideos <- ifelse((sum(1*(unique(Data$Video[order(Data$Video)]) ==unique(DataTrials$Video[order(DataTrials$Video)])))==30),"Good : videos are the same from script and experiment",
#                           "Bad : videos are the NOT same from script and experiment")
#
#
#
#       # check no. of trials should be 120
#       MSGNTrials <- ifelse((dim(Data)[1]==120),paste0("Good : No. of trials are correct - ",as.character(dim(Data)[1])),paste0("Bad : No. of trials are NOT correct. The no. of trials are - ",as.character(dim(Data)[1])))
#
#       # no. of movie frames dropped - should be 1 or 0
#       MSGDroppedFrames <- ifelse(any(Data$MovieStart.FramesDropped>1),paste0("Bad : one of the dropped frames is more than 1. those are : ",as.character(which(Data$MovieStart.FramesDropped==2))),
#                                  paste0("Good : dropped frames are less than 1"))
#
#
#
#
#       # checking if no. of self, other and diff videos are equal
#       k[[i]] <- merge(DataTrials,Data,by="Video")
#
#       # change depending on what is the signal
#       #signal <- "Pouring"
#
#       #k[[i]] <- sigDetect(k[[i]],signal,yesResp)
#       #     k[[i]][,"Resp"]  <- ifelse(!is.na(k[[1]]$MovieStart.RESP),k[[1]]$MovieStart.RESP,k[[1]]$ITI.RESP)
#       #     k[[i]]$signal <- ifelse(k[[i]]$Condizione.x==Signal,1,0)
#       #
#       #     k[[i]]$Dprime <- ifelse(k[[i]]$signal==1 & k[[i]]$Resp ==2,"HIT",ifelse(k[[i]]$signal==1 & k[[i]]$Resp ==1,"MISS",ifelse(k[[i]]$signal==0 & k[[i]]$Resp ==1,"CR",ifelse(k[[i]]$signal==0 & k[[i]]$Resp ==2,"FA",NA))))
#       #     k[[i]]$ROC_confid <- ifelse(k[[i]]$Dprime=="HIT" | k[[i]]$Dprime=="FA",k[[i]]$Confidence+4,abs(k[[i]]$Confidence-5))
#       p <- ddply(k[[i]],c("Mtype","Condizione.x"),summarise,n=length(unique(Video)),sub=unique(Sub),resp=unique(CorrResp))
#
#       MSGVideo2 <- ifelse((length(unique(p$n))==1 & unique(p$n)==5),paste0("Good : 5 trials have been selected"),paste0("Bad : more than 5 trials or different no. of trials present"))
#       MSGMtype <- ifelse(!length(unique(p$Mtype))==3,"Bad : Mtype not correct","Good : Mtype Correct")
#       MSGSubNo <- ifelse(!length(unique(p$sub))==3,"Bad : more than 3 Subjects","Good : Correct no. of subjects")
#
#       # prints all messages at the same time
#       print(rbind(MSGNTrials,MSGDroppedFrames,MSGVideos,MSGVideo2,MSGMtype,MSGSubNo))
      #DataSig[[subs]]$Video <- ifelse(!is.numeric(DataSig[[subs]]$Video),as.numeric(levels(DataSig[[subs]]$Video)[DataSig[[subs]]$Video]),DataSig[[subs]]$Video)
      DataSig[[subs]] <- merge(DataTrials,DataSig[[subs]],by="Video")
      ResultsSelf[[subs]] <- ddply(DataSig[[subs]],c("Mtype","Condizione.x","cluster"),summarise,acc = mean(ACC),rt = mean(CorrectRT,na.rm=T))


      # MS part

      # generate column for merging with cluster data
      # this has to be so as the cluster data (for no particular reason) are as sub, trial of the execution data
      # these could have also been simply Video


    }    else {
      print("No integration selected")
    }




    # addWorksheet(wbResults,"Results_custom")
    # writeData(wbResults,sheet = "Results_custom",Results[[subs]])


   

    # addWorksheet(wbResults,"Results_custom")
    # writeData(wbResults,sheet = "Results_custom",Results[[subs]])

    addWorksheet(SubResults,sub)
    writeData(SubResults,sheet = sub,DataSig[[subs]])

    DataFull <- rbind(DataFull,DataSig[[subs]])


#     # for the time being as with the trial I just had the same subject no. which will definately not be the case in actual life
#     addWorksheet(wb,strsplit(strsplit(basename(i),"_")[[1]][4],"-")[[1]][1])
#     writeData(wb,sheet = strsplit(strsplit(basename(i),"_")[[1]][4],"-")[[1]][1],Data)
#     #addWorksheet(wb,as.character(unique(Data$Subject)))
#     #writeData(wb,sheet = as.character(unique(Data$Subject)),Data)
#     DataFull <- rbind(DataFull,Data)

  }

  if(!"Sub" %in% colnames(DataFull) & !"Trial" %in% colnames(DataFull)){
    SublevelStim <- choose.files(caption = "Select Sub level stimulus File")
    wbSub <- loadWorkbook(SublevelStim)
    DataTrials <- readWorkbook(wbSub,1)
    # all this is to create only a single column called cluster
    # that provides the cluster number irrespective of condition
    # for now, I remove the clusters for placing (4) and passing (2) (we dont use them)
    # and drop the columns of Condizione and Placing which are reduncdant

#     DataTrials <- reshape(DataTrials[which(!DataTrials$Condizione==2 & !DataTrials$Condizione==4),],varying = c("Pouring","Drinking"),
#                           idvar = c("Sub","Trial"),
#                           direction = "long",
#                           v.names="cluster",
#                           times = c("Pouring","Drinking"),
#                           timevar = "Condition",drop=c("Condizione","Placing"))

    #colnames(DataTrials)[3] <- "Condition"

#
#     temp <- data.frame(matrix(unlist(strsplit(as.character(DataFull$Video),"_")), ncol = 2, byrow = TRUE))
#     colnames(temp) <- c("Sub","Trial")
#
#     DataFull <- cbind(DataFull,temp)
#     DataFull$Sub <- ifelse(!is.numeric(DataFull$Subject),as.numeric(levels(DataFull$Sub)[DataFull$Sub]),DataFull$Subject)
#     DataFull$Trial <- ifelse(!is.numeric(DataFull$Trial),as.numeric(levels(DataFull$Trial)[DataFull$Trial]),DataFull$Trial)
#    DataFull <- merge(DataFull,DataTrials,by=c("Sub","Trial"))
    if("Condizione" %in% colnames(DataTrials)){
      DataTrials <- DataTrials[,!names(DataTrials) %in% "Condizione"]
    }
    DataFull <- merge(DataFull,DataTrials,by=c("Video"))
}
  # get the results according to the parameters that u like
  Results_cus <- ddply(DataFull,ResultCat,summarise,acc = mean(ACC),rt = mean(CorrectRT,na.rm=T))

  ResultCat <- c("Subject","AccLevel")
  ResultsSigDetect <- ddply(DataFull,.(Subject,AccLevel),summarise,

    DataTrials <- reshape(DataTrials[which(!DataTrials$Condizione==2 & !DataTrials$Condizione==4),],varying = c("Pouring","Drinking"),
                          idvar = c("Sub","Trial"),
                          direction = "long",
                          v.names="cluster",
                          times = c("Pouring","Drinking"),
                          timevar = "Condition",drop=c("Condizione","Placing"))
    
    #colnames(DataTrials)[3] <- "Condition"
    
    
    temp <- data.frame(matrix(unlist(strsplit(as.character(DataFull$Video),"_")), ncol = 2, byrow = TRUE))
    colnames(temp) <- c("Sub","Trial")
    
    DataFull <- cbind(DataFull,temp)
    DataFull$Sub <- ifelse(!is.numeric(DataFull$Subject),as.numeric(levels(DataFull$Sub)[DataFull$Sub]),DataFull$Subject)
    DataFull$Trial <- ifelse(!is.numeric(DataFull$Trial),as.numeric(levels(DataFull$Trial)[DataFull$Trial]),DataFull$Trial)
    DataFull <- merge(DataFull,DataTrials,by=c("Sub","Trial"))
}
  # get the results according to the parameters that u like
  Results_cus <- ddply(DataFull,ResultCat,summarise,acc = mean(ACC),rt = mean(CorrectRT,na.rm=T))
  
  
  ResultsSigDetect <- ddply(DataFull,.(Subject),summarise,

        Age = unique(Age),
        pHit = ifelse(!(sum(Dprime=="HIT",na.rm=T)/sum(signal==1,na.rm=T) >=1),sum(Dprime=="HIT",na.rm=T)/sum(signal==1,na.rm=T),sum(Dprime=="HIT",na.rm=T)/sum(signal==1,na.rm=T)-0.05),
        pMISS = sum(Dprime=="MISS",na.rm=T)/sum(signal==1,na.rm=T),
        pCR = sum(Dprime=="CR",na.rm=T)/sum(signal==0,na.rm=T),
        pFA = ifelse(!(sum(Dprime=="FA",na.rm=T)/sum(signal==0,na.rm=T) <=0),sum(Dprime=="FA",na.rm=T)/sum(signal==0,na.rm=T),sum(Dprime=="FA",na.rm=T)/sum(signal==0,na.rm=T)+0.05),
        Dprime = qnorm(pHit)-qnorm(pFA),
        C = -(qnorm(pHit)+qnorm(pFA))/2,
        Controllo_C = ifelse(C<0,"Bias a rispondere 1","Bias a rispondere 2"),
        ACC = mean(ACC,na.rm=T),
        Resp1 = mean(Resp==1,na.rm=T),
        Resp2 = mean(Resp==2,na.rm=T))


  ClusterResults <- ddply(DataFull,.(Subject,Condizione,cluster),summarise,clustAcc = mean(ACC,na.rm=T),Condizione=unique(Condizione),Age = unique(Age))





  addWorksheet(SubResults,"Combined_Data")
  writeData(SubResults,"Combined_Data",DataFull)


  addWorksheet(SubResults,"Combined_Results")
  writeData(SubResults,"Combined_Results",Results_cus)

  addWorksheet(SubResults,"SigDetect_Results")
  writeData(SubResults,"SigDetect_Results",ResultsSigDetect)

  #addWorksheet(SubResults,"ClusterResults")

  
  addWorksheet(SubResults,"Combined_Results")
  writeData(SubResults,"Combined_Results",Results_cus)
  
  addWorksheet(SubResults,"SigDetect_Results")
  writeData(SubResults,"SigDetect_Results",ResultsSigDetect)

  addWorksheet(SubResults,"ClusterResults")


  if(save){
    saveWorkbook(SubResults,file=paste0(directory[1],"/","Subject_results_prior.xlsx"),overwrite = TRUE)
  }

  Results = list(DataFull=DataFull,Data=Data,wbResults = wbResults, SubResults = SubResults,ResultsSigDetect=ResultsSigDetect)

}
