##Function: corr_mat##
##Version: 1.0
##Function used to compute correlation matrix
##input: dataframe containing data; check that the data has enough non-NA values as well as no negative values.
##output: the function outputs a csv correlation matrix and a csv containing the p values associated with the correlation coefficients

#' Function used to compute correlation matrix
#' @details For more information on the use of the set of functions within the package see the help vignette: \code{vignette("Overview_on_usage", package = "corrfx")}
#'
#' @param dat a data frame used to test correlations
#' @param raw true/false binary indicating type of transformation
#' @param rank true/false binary indicating type of transformation
#' @param log true/false binary indicating type of transformation
#' @param file1 output file name for r values
#' @param file2 output file name for p values
#' @param outputCSV Should R output csv files. Default is False
#'
#' @return returns two csv files one with the r values and one with p values
#' @export
#'
#' @importFrom stats cor.test
#' @importFrom utils write.csv

corr_mat<-function(dat=dat, raw=F, rank=F, log=F,outputCSV=F, file1='correlation matrix file name',file2='p value file name'){

  n<-dim(dat)[2]                                  # number of variables/columns

  cor.matrix <- matrix(nrow=n, ncol=n)            # create empty matrices
  t.cor <- matrix(nrow=n, ncol=n)
  p.cor <- matrix(nrow=n, ncol=n)

  for (i in 1:n) {                                # for each column of data = x

    for (j in 1:n)  {                            # for each column of data = y

      if(is.na(j|i)){

      }else{

        lm.fit <- cor.test(dat[,i],dat[,j], method = "pearson", use = "pairwise.complete.obs", exact = FALSE) # Calculate correlation among variables in dat

      }

      cor.matrix[i,j] <- round(lm.fit$estimate,digits=2)# and place in matrix

      p.cor[i,j] <- lm.fit$p.value     # Pick out p value from two-tailed t test on r

    }
  }

  cor.df<-as.data.frame(cor.matrix)               # change the output matrix into a dataframe (to allow naming of columns and rows)
  p.df<-as.data.frame(p.cor)


  names(cor.df)<-names(dat); row.names(cor.df)<-names(dat)  # name columns and rows
  names(p.df)<-names(dat); row.names(p.df)<-names(dat)      # name columns and rows

if(outputCSV==T){
  #create outputs
  if(raw == T){

    write.csv(cor.df, file=file1)# write output to text files
    write.csv(p.df, file=file2)

  } else if (rank == T) {

    write.csv(cor.df, file= file1)# write output to text files
    write.csv(p.df, file=file2)

  } else {

    write.csv(cor.df, file=file1)# write output to text files
    write.csv(p.df, file=file2)
  }
}
  return(list(cor.df, p.df))
}
