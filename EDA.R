EDA <- function(data,v=1:ncol(data),d=getwd())
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  dir.create("Graphs")
  dropCol <- character()
  setwd(paste(d,"Graphs",sep="\\"))
  for(i in v)
  {
    if(is.numeric(data[,i]))
    {
      
      png(paste(names(data)[i], ".png", sep="")) 
      
      par(mfrow=c(1,2))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "Count", col = "lightgreen", border=F)
      
      dev.off()  
    }
    if(is.factor(data[,i]))
    {
      
      png(paste(names(data)[i], ".png", sep="")) 
      
      par(mfrow=c(1,1))
      
      barplot(table(data[,i]), main = paste("Barplot of", names(data)[i]), 
              xlab = names(data)[i],ylab = "Count", border = "grey5",
              horizontal = T)
      
      tb <- table(data[,i])
      lb <- paste("(",names(tb),")", "\n", sep="")
      pie(tb, labels = lb, main = paste("Pie chart of", names(data)[i]))
      
      dev.off()  
      
      dropCol <- append(dropCol, colnames(data)[i])
    }
    if(length(dropCol)>0 & unique(data[,i])>10 & is.numeric(data[,i]))
    {
      for (j in 1:length(dropCol)) {
        png(paste(names(data)[i], '_',dropCol[j], ".png", sep="")) 
        
        boxplot(data[,i]~data[,dropCol[j]], main = paste("Boxplot of", names(data)[i], 
                                                         'and', dropCol[j]), ylab = names(data)[i], 
                xlab = dropCol[j], col = "maroon", border = "grey5")
        dev.off() 
      }
    }
  }
  
  newDf <- data[, v]
  if(length(dropCol)>0)
  {
    newDf <- newDf[, !(names(data) %in% dropCol)] 
  }
  corMat <- round(cor(newDf),2)
  
  # write the correlation matrix to the directory
  write.csv(corMat, file = paste(d, '\\', 'Correlation Matrix', ".csv", sep=""))
  
  # 'corrplot' package required for plotting correlation matrix.
  # check if the package is installed. Install if not installed.
  if("corrplot" %in% rownames(installed.packages())==FALSE)
  {
    install.packages("corrplot")
  }
  
  library(corrplot)
  
  # plot the correlation
  #Positive correlations are displayed in blue and negative correlations in red color. Color intensity 
  #and the size of the circle are proportional to the correlation coefficients. In the right side of the 
  #correlogram, the legend color shows the correlation coefficients and the corresponding colors.
  
  #The correlation matrix is reordered according to the correlation coefficient using "hclust" method.
  # tl.col (for text label color) and tl.srt (for text label string rotation) are used to change text colors 
  #and rotations. Possible values for the argument type are : "upper", "lower", "full"
  
  png(paste(d, '\\', 'Correlation Plot', ".png", sep="")) 
  corrplot(round(cor(cars),2), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
  dev.off()  
  
}
