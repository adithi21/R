

#------------------------------Count of Outliers----------------------------------------------------------------



OutliersCount <- function(df)
{
  if(!is.data.frame(df))
    stop('The given object in not a data frame.')
  
  if(sum(is.na(df))>0)
    stop('The data frame contains missing values and cannot proceed further.')
  
  # return the number of data points above the upper benchmark
  outUb <- function(x)
  {
    if(is.numeric(x))
    {
      IQR = quantile(x)['75%'][[1]] - quantile(x)['25%'][[1]]
      ub = quantile(x)['75%'][[1]] + 1.5*IQR
      {if(length(x[x>ub])>0 & unique(x)>10)
        return(length(x[x>ub]))
      else
        return('0')}
    }
    if(is.factor(x))
    {
      return('0')
    }
  }
  
  # return the number of data points below the lower benchmark
  outLb <- function(x)
  {
    if(is.numeric(x))
    {
      IQR = quantile(x)['75%'][[1]] - quantile(x)['25%'][[1]]
      lb = quantile(x)['25%'][[1]] - 1.5*IQR
      {if(length(x[x>lb])>0 & unique(x)>10)
        return(length(x[x<lb]))
      else
        return('0')}
    }
    if(is.factor(x))
    {
      return('0')
    }
  }
  
  olU <- apply(df, 2, outUb)
  olL <- apply(df, 2, outLb)
  newDf <- cbind.data.frame(df)
  for (i in 1:ncol(newDf)) {
    if(is.numeric(newDf[,i]))
    {
      newDf[,i] = log(newDf[,i] + 1)   # apply log after adding 1 to take care of any data = 0
    }
  }
  olU_Log <- apply(newDf, 2, outUb)
  olL_Log <- apply(newDf, 2, outLb)
  View(df)
  # create a matrix using the 4 vectors
  mat <- cbind(olU, olL, olU_Log, olL_Log)
  
  colnames(mat)<-c('Upper Count', 'Lower Count', 'Upper Count After Log', 'Lower Count After Log')
  return(mat)
}

#----------------------------------------------------------------------------------------------------
