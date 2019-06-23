#data manipulation
if (!require("dplyr")) {
  install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
}

if(!(exists('AllDataRaw') && is.data.frame(get('AllDataRaw')))) {
  AllDataRaw <- read.csv2(file = "../Data/TableauExportv2.csv", 
                          header = T,
                          strip.white = TRUE,
                          dec = ".",
                          sep = ',', na.strings = c('', 'NA')
  )
  oldColumns <- names(AllDataRaw)
  newColumns <- gsub("\\.",    "", oldColumns, perl=TRUE)
  #Strange name for AnimalId
  newColumns[1] <- "AnimalId"
  #Duplicate name for DaysInMilk
  newColumns[19] <- "DaysInMilkBin"
  names(AllDataRaw) <- newColumns
  AllDataRaw <- AllDataRaw %>% 
    dplyr::arrange(
      HerdId,
      AnimalId,
      Date
    )
}