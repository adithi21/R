

#------------------------------Count of Outliers----------------------------------------------------------------



OutliersCount <- function(df)
{
  # return the number of data points above the upper benchmark
  outUb <- function(x)
  {
    IQR = quantile(x)['75%'] - quantile(x)['25%']
    ub = quantile(x)['75%'] + 1.5*IQR
    if (length(x[x>ub])>0 & unique(x)>10)
    {
      return(length(x[x>ub]))
    }
    else
    {
      return('0')
    }
  }
  
  # return the number of data points below the lower benchmark
  outLb <- function(x)
  {
    IQR = quantile(x)['75%'] - quantile(x)['25%']
    lb = quantile(x)['25%'] - 1.5*IQR
    if (length(x[x>lb])>0 & unique(x)>10)
    {
      return(length(x[x<lb]))
    }
    else
    {
      return('0')
    }
  }
  
  updatedDf = df[complete.cases(df),]
  olU <- apply(updatedDf, 2, outUb)
  olL <- apply(updatedDf, 2, outLb)
  A <- log(updatedDf + 1)                 # apply log after adding 1 to take care of any data = 0
  olU_Log <- apply(A, 2, outUb)
  olL_Log <- apply(A, 2, outLb)
  
  # create a matrix using the 4 vectors
  mat <- cbind(olU, olL, olU_Log, olL_Log)
  
  colnames(mat)<-c('Upper Count', 'Lower Count', 'Upper Count After Log', 'Lower Count After Log')
  return(mat)
}

#----------------------------------------------------------------------------------------------------
