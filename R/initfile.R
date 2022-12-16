##Function: initfile##
##Function used to create subsets of larger dataset and write files for each species based on
##required file types specified.
##Used to create versions of initial dataset that are raw, log, or rank transformed.
#' init file: function used to create subsets of larger dataset and write files for each species for raw, log, and rank transformations
#'
#' @param data a dataframe to be split. This dataframe should have the species code as "spcode"
#' @param grpCol A string. name of column with species codes in it.
#' @param dataType A string. Indicate what data type label for output csv files
#' @param pathFolder A string passed to here function indicate folder for file placement
#' @param returnCSV TRUE or FALSE. Do you wish to have subsets output as csv files? Default is TRUE

#' @return Returns a list of group specific subsets of the larger data frame and .csv files(optional)
#' @export
#'
#' @importFrom utils write.csv
#' @importFrom here here
#' @importFrom dplyr filter
#'

initfile<-function(data, #dataframe
                   grpCol,
                   dataType, #file type
                   returnCSV=T,
                   pathFolder){#folder of output

  groups<-unique(data[,{{grpCol}}])

  #grps<-ifelse(is.factor(groups), as.list(as.character(groups)), groups)

  spc<-lapply(groups, function(grp) dplyr::filter(data, {{grpCol}}==grp))

  if(returnCSV==T){

    lapply(1:length(groups),
           function(grp) write.csv(spc[grp], here(pathFolder, paste(spc[grp],dataType, ".csv", sep = "_")), row.names = F))
  }

  return(spc)

}


