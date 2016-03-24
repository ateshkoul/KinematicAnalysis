#' Read Data from multiple files/sheets of Excel files and create a single dataframe
#'
#' \code{mergeExcelData} takes in multiple filenames/single excel file with multiple sheets and merges as
#' a single dataframe.
#' uses packages: openxlsx
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
#'The function assumes that the files to be merged have the same number of columns
#'(necessity for rbind)

mergeExcelData <- function (filenames, SheetNo,selectedData = NULL,...)
{
  if (!require(openxlsx)) {
    install.packages("openxlsx")
  }
  require("openxlsx")
  Data <- list()
  for (filename in filenames){
    # good to know which files u are merging
    print(filename)
    excel = loadWorkbook(filename)

    # take care if u input sheet names instead of sheet numbers
    # even though when it says SheetNo :)
    if(!is.double(SheetNo)){
      print('SheetNo is not a number')
      Sheets = SheetNo
    }else{
      Sheets = names(excel)
      Sheets = Sheets[SheetNo]
    }

    nSheets <- length(Sheets)
    for (sheetName in Sheets) {
      DataSheet <- readWorkbook(excel, sheet = sheetName,...)
      # although u can use the cols option in readWorkbook, it works only
      # with column numbers.
      if(!is.null(selectedData)) DataSheet <- DataSheet[,selectedData]
      Data <- rbind(Data, DataSheet)
    }
  }
  return(Data)
}

