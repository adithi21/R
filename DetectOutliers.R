

DetectOutliers <- function(df)
{
  # function to check if there are outliers in the data
  out <- function(x)
  {
    # check if data is discrete
    {if(unique(x)<10)
    {
      return('No Outliers')
    }
      else
      {
        IQR = quantile(x)['75%'] - quantile(x)['25%']         # calculate IQR
        ub = quantile(x)['75%'] + 1.5*IQR                     # calculate upper benchmark
        {if (length(x[x>ub])>0)
        {
          # if there are data points above the upper benchmark, then return: 
          return('Outlier Detected')
        }
          else
          {
            lb = quantile(x)['25%'] - 1.5*IQR                   # calculate lower benchmark
            # check if there are datapoints below the lower benchmark
            {if (length(x[x<lb])>0)
            {
              return('Outlier Detected')
            }
              else
              {
                return('No Outliers')
              }}
          }}
      }}
  }
  
  updatedDf = df[complete.cases(df),]                       # remove the missing values
  ol <- apply(updatedDf, 2, out)                            # apply out function on each column of the dataframe
  ol <- as.matrix(ol)                                       # convert the dataframe to matrix
  colnames(ol)<-'Outlier'                                   # rename the column as 'Outliers'
  
  return(ol)
}