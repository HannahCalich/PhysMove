#' Root-Mean-Square of Displacements
#'
#' This function allows you to calculate root-mean-square displacements and plot them scaled with time
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{plSample}}, \code{\link{expSample}}, or \code{\link{lnormSample}}
#' @param wBins Bin width refers to the size of the bins used to calculate how frequently displacements occurred. Default is 1.
#' @param timeUnit Unit used to calculate time between locations (e.g., "secs", "mins", "hours", "days", "weeks"). Default is "days".
#' @param plot Plot the root-mean-square of displacements versus the mean displacements against their corresponding time periods. Default is TRUE.
#' @param colours Colours to plot the root-mean-square and mean displacement values, respectively. Default is colours=c("black","grey").
#' @param pchType Pch symbols to plot the root-mean-square and mean displacement values, respectively. Pch values between 1 and 20 are valid.
#' Default is pchType=c(16,16).
#' @param legend Adds legend to plot and specifies legend location. Default is c(TRUE, "topleft").
#' @param lm Calculate a linear regression to examine the relationship between the root-mean-square displacement values (target variable)
#' and time (predictor variable) and add fit line to the plot. The slope of this relationship is the RMS statistic and it can be determined by typing
#' 'RMSlinearModel$coefficients[2]'. Default is TRUE.
#' @return A data frame of the 'RMSresults' that includes: the 'TimeWindows' in log scale, the 'Count' (the cumulative
#' count of displacements between each location within each log time window), the Mean Displacements 'MeanDisp', the mean-square displacement values
#' 'dRMS', and the the mean displacement and root-mean-square displacement values normalized by count ('MeanDisp_per_count' and 'Sqrt_dRMS_per_count'), which
#' are shown in the plot. A plot of the mean displacement values and the root-mean-square displacement values (per count) against their corresponding
#' time periods (if plot=TRUE), and the results from the linear regression 'RMSlinearModel' (if lm=TRUE).
#' @examples
#' RMS(expSample)
#' RMS(expSample, timeUnit="days", wBins=1.1, plot=TRUE, pchType=c(16,16), colours=c("black","grey"), legend=c(TRUE, "topleft"), lm=FALSE)
#' @export


RMS <- function (species_df, timeUnit="days", wBins=1.1, plot=TRUE, pchType=c(16,16), colours=c("black","grey"), legend=c(TRUE, "topleft"), lm=TRUE){

  MydistHaversine <- function(lon1, lat1, lon2, lat2) {
    radlat1 = rad * lat1
    radlat2 = rad * lat2
    dlat = radlat2 - radlat1
    dlon = rad * (lon2 - lon1)
    a = (sin(dlat/2)^2) + cos(radlat1)*cos(radlat2)*(sin(dlon/2)^2)
    a = 2*asin(sqrt(a))
    return(a*Radius)
  }

  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x}) #Convert to Ana terms
  Radius <- 6371 #Earth Radius in km (disp are in km)
  rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
  bins <- seq(1,400,1) #400 time windows
  tmin <- 1.0/(60*60*24) # 1 second is min time
  sumDist2<-sumDist<-Timefreq <- rep(0, length(bins))
  j<-k<-n<-b<-1

  for(j in 1:dim(species_df)[1]){ # for each row
    n <- species_index[[paste(species_df[j,1])]][length(species_index[[paste(species_df[j,1])]])] #row where each individual ends
    for(k in (j+1):n){ #for each shark, calculate the distance between all locations (k)
      if (j+1 <= n){
        myTime <- as.numeric(difftime(species_df[k,4],species_df[j,4],units=timeUnit))
        b <- floor(log(myTime/tmin)/log(wBins) + 0.5) #log scale time bin
        Timefreq[b] <- Timefreq[b] + 1 #Cumulative count of displacements between each location within each log time bin
        Dist <- MydistHaversine(species_df[k,2], species_df[k,3], species_df[j,2], species_df[j,3]) #Calculates distance from point 1 to all sucessive points
        sumDist[b] <- sumDist[b] + Dist
        sumDist2[b] <- sumDist2[b] + Dist^2
      }
    }
  }

  mybins <- rep(0, length(bins)) # for the x axis of the plot
  for(b in 1: length(bins)){
    mybins[b] <- tmin*wBins^(b)
  }
  MyRMS <- as.data.frame(cbind("TimeWindows_log"=mybins, "Count"=Timefreq, "MeanDisp"=sumDist, "dRMS"=sumDist2))## SPEAK TO ANA -- WHY WERE WE CALLING THIS A MEAN
  MyRMS <- MyRMS[(MyRMS[,1]!= 0) & (MyRMS[,3]!= 0) & (MyRMS[,4]!= 0),]
  MyRMS$MeanDisp_per_count <- MyRMS[,3]/MyRMS[,2]
  MyRMS$Sqrt_dRMS_per_count <- sqrt(MyRMS[,4]/MyRMS[,2])
  MyRMS_Export<-cbind(MyRMS[,c(1,5,6)])
  names(MyRMS_Export)<-c("TmeWindows", "Displacements", "RMSdisplacements")
  assign("RMSresults", MyRMS_Export, envir = .GlobalEnv)

  if (plot==TRUE){    # Plot RMS of displacements, and mean displacements on log-log scale

    yminval = log(min(c(min(MyRMS$MeanDisp_per_count),min(MyRMS$Sqrt_dRMS_per_count))))
    ymaxval = log(max(c(max(MyRMS$MeanDisp_per_count),max(MyRMS$Sqrt_dRMS_per_count))))
    plot(log(MyRMS[,1]),log(MyRMS$MeanDisp_per_count),  xaxt="n", yaxt="n", ylim=c(yminval,ymaxval),
         xlim=c(log(min(MyRMS$TimeWindows_log)),log(max(MyRMS$TimeWindows_log))),col=colours[1],pch=pchType[1],
         xlab=paste("T(", timeUnit, ")", sep = ""),ylab="")#,log="xy")
    myTicks = axTicks(1)
    axis(1, at = myTicks, labels = formatC(myTicks, digits = 0, format = 'e'))
    myTicks2 = axTicks(2)
    axis(2, at = myTicks2, labels = formatC(myTicks2, digits = 0, format = 'e'))
    title(ylab=expression('<'*d^q*'>'^(1/q)*(km)), line=2)
    points(log(MyRMS[,1]), log(MyRMS$Sqrt_dRMS_per_count), col=colours[2],pch=pchType[2])
    if (legend[1]==TRUE){
      legend(legend[2], bty="n", c("Mean Disp.", "RMS Disp."), col=colours, pch=pchType)
    }
  }

  if (lm==TRUE){
    model<-lm(log(MyRMS$Sqrt_dRMS_per_count) ~ log(MyRMS$TimeWindows_log), data = MyRMS)
    abline(model)
    assign("RMSlinearModel",model, envir = .GlobalEnv)
  }

}
