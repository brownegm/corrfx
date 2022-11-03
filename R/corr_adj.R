
##Function:corr_adj##
##Function used to update "p value files" created by corr_mat function to only have
##stars or ns based on the significance of the correlation.

corr_adj<-function(corData, pData, filename='what is wrong'){ #raw=T, log=F, rank=F,){

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
}


