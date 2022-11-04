
##Function: initfile##
##Function used to create subsets of larger dataset and write files for each species based on
##required file types specified.
##Used to create versions of initial dataset that are raw, log, or rank transformed.
#' init file: function used to create subsets of larger dataset and write files for each species for raw, log, and rank transformations
#'
#' @param spnames species names
#' @param dat a dataframe to be split. This dataframe should have the species code as "spcode"
#' @param raw true/false binary indicating type of transformation
#' @param log true/false binary indicating type of transformation
#' @param rank true/false binary indicating type of transformation
#' @param path_folder folder for output to be saved
#' @param spcode name of column with species codes in it.
#'
#' @return returns a species specific subset of the larger dataframe
#' @export
#'
#' @importFrom utils write.csv
#'

initfile<-function(spnames=NA, #species names
                   dat=NA, #dataframe
                   spcode="string",
                   raw=F, log=F, rank=F, #file type
                   path_folder){#folder of output

  for(sp in spnames){#create for loop to create .csv file for each species

    spc<-subset(dat, spcode == sp)#subset large dataset into one for each species

    #The following if statement should write species-specific files, name them
    #accordingly and produce files without species and individual info
    if (raw == T){

      write.csv(spc[,-c(1:3)],here(path_folder, paste(sp, "RAWData.csv", sep="_")), row.names = F, na="")#write csv for each species

    }else if(log == T){

      write.csv(spc[,-c(1:3)],here(path_folder, paste(sp, "LOGData.csv", sep="_")), row.names = F, na = "")#write csv for each species

    }else {

      write.csv(spc[,-c(1:3)],here(path_folder, paste(sp,"RANKData.csv", sep = "_")), row.names = F, na = "")#write csv for each species
    }
  }
}

