##Function:corr_adj##
##Function used to update "p value files" created with corr matrices to only have
##stars or ns based on the significance of the correlation.

#' corr_adj: formats p value files created with corr matrices
#'
#' @param corData dataframe of correlation coefficients in the form of a matrix
#' @param pData dataframe of p values associated with corData r values
#' @param filename string containing name of output file
#'
#' @return csv file with correlation and p values concatenated
#'
#' @export
#'
#' @importFrom stats cor.test
#' @importFrom utils write.csv
#' @import here
#'
#'
corr_adj<-function(corData, pData, filename='what is wrong'){

  nn = dim(corData)[2]
  Star = corData
  DataStar = pData

  for (ii in 1:nn){
    for(jj in 1:nn){
      Star[ii,jj] = as.character(cut(pData[ii,jj],
                                     breaks = c(0, 0.001, 0.01, 0.05, 1),
                                     include.lowest = T,
                                     labels = c('***', '**', '*', 'ns')))
      DataStar[ii,jj] = paste(lapply(corData[ii,jj], round, 2), Star[ii,jj], sep = "")
    }}

  write.csv(DataStar, file=here(filename), na = "")

  return(DataStar)
}


