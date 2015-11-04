#' Read Data from multiple sheets of an Excel file
#'
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
readBehavData <- function(SheetName,SheetNo){
  if(!require(openxlsx)){install.packages('openxlsx')}

  require("openxlsx")
  nSheets <- length(SheetNo)

  Data <- list()
  for(i in 1:nSheets){
    DataSheet <- read.xlsx(xlsxFile = SheetName,sheet = SheetNo[i])
    Data <- rbind(Data,DataSheet)

  }
  return(Data)



}
