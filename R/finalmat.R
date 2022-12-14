#####finalmat#######
### Create corr matrix with the p values and correlation coefficients together
### This function combines the values from the raw, log, and rank data in that
### order
#' finalmat:Create corr matrix with the p values and correlation coefficients together
#'
#' @param dat1 data frame containing combined correlation coeficients and p values as stars
#' @param dat2 data frame containing combined correlation coeficients and p values as stars
#' @param dat3 data frame containing combined correlation coeficients and p values as stars
#' @param filename string containing the name for the output final file, including ".csv" at the end
#' @param outputCSV Should R output csv files. Default is False
#' @return csv file with combine raw, log and rank correlation coef. and p value as stars
#' @export
#'
#' @importFrom utils write.csv
#'
#'

finalmat<-function(dat1,#raw data
                   dat2,#log data
                   dat3=NA,#rank data
                   outputCSV=F,
                   filename="insertfilename"){

  FinalCorrMatrix = dat1
  nn = dim(dat1)[2]

  for (ii in 1:nn){
    for(jj in 1:nn){

      if (exists("dat3")){
        FinalCorrMatrix[ii,jj] = paste(dat1[ii,jj], dat2[ii,jj], dat3[ii,jj],
                                       sep ="; ")
      }else{
        FinalCorrMatrix[ii,jj] = paste(dat1[ii,jj], dat2[ii,jj],
                                       sep ="; ")
      }
    }
  }
if(outputCSV==T){
  write.csv(FinalCorrMatrix, file=filename)
}

  return(FinalCorrMatrix)

}
