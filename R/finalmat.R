#####finalmat#######
### Create corr matrix with the p values and correlation coefficients together
### This function combines the values from the raw, log, and rank data in that
### order
finalmat<-function(dat1,#raw data
                   dat2,#log data
                   dat3=NA,#rank data
                   filename="Why pree dis?"){

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

  write.csv(FinalCorrMatrix, file=filename)

}
