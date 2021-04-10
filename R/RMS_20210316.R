#' Root-Mean-Square of Displacement
#'
#' This function allows you to calculate the root-mean-square of displacements and plot them scaled with time
#' @param species_df A data frame containing location data (rows) and columns with the following headers: "ref", "lon", "lat", "day". "ref" is the unique id number for each animal
#'      (e.g., their satellite tag number; integer format). "lon" and "lat" are the longitude and latitide of each position estimate in decimal degrees (numeric format). "day"
#'      is the datetime stamp for each location estimate (POSIXct format following yyyy-mm-dd hh:mm:ss). See XXX data frame for an example.
#' @param bw Bin width refers to the size of the bins used to calculate how frequently displacements occurred. Altering this value will change the number of points on your plot. Default is 1.5.
#' @param timeunit Unit used to calculate time between locations (e.g., "days", "hours", "seconds"). Default is "hours".
#' @param plot Plot the root-mean-square of displacements and the mean displacements against their corresponding time periods. Default is TRUE.
#' @param pchtype Point type for plot. Default is 19
#' @param colours Colours for plot points. Default is c("black", "red").
#' @param legend Adds legend to plot and specifies legend location. Default is c(TRUE, "bottomleft").
#' @param lm Run a linear model
#' @return
#' @examples
#' RMS(species_df)
#' RMS(species_df,bw=1.5,plot=TRUE,colours=c("black","red"),legend=c(TRUE, "topleft"))
#' @export


RMS <- function (species_df, timeunit="hours", bw=1.1, plot=TRUE, pchtype=c(15,19), colours=c("black","red"), legend=c(TRUE, "topleft"), lm=FALSE){

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
  bins <- seq(1,400,1)
  tmin <- 1.0/(60*60*24) # 1 second in min time
  sumDist2<-sumDist<-Timefreq <- rep(0, length(bins))
  j<-k<-n<-b<-1
  for(j in 1:dim(species_df)[1]){ # for each row
    n <- species_index[[paste(species_df[j,1])]][length(species_index[[paste(species_df[j,1])]])] #row where each individual ends
    for(k in (j+1):n){ #for each shark, calculate the distance between all locations (k)
      if (j+1 <= n){
        myTime <- as.numeric(difftime(species_df[k,4],species_df[j,4],units=timeunit))
        b <- floor(log(myTime/tmin)/log(bw) + 0.5) #log scale time bins
        Timefreq[b] <- Timefreq[b] + 1 #Cumulative count of displacements between each location within each log time bin
        Dist <- MydistHaversine(species_df[k,2], species_df[k,3], species_df[j,2], species_df[j,3]) #Calculates distance from point 1 to all sucessive points
        sumDist[b] <- sumDist[b] + Dist
        sumDist2[b] <- sumDist2[b] + Dist^2
      }
    }
  }

  mybins <- rep(0, length(bins)) # for the x axis of the plot
  for(b in 1: length(bins)){
    mybins[b] <- tmin*bw^(b)
  }
  MyRMS <- as.data.frame(cbind("TimeWindows_log"=mybins, "Count"=Timefreq, "MeanDisp"=sumDist, "dRMS"=sumDist2))
  MyRMS <- MyRMS[(MyRMS[,1]!= 0) & (MyRMS[,3]!= 0) & (MyRMS[,4]!= 0),]
  MyRMS$MeanDisp_per_count <- MyRMS[,3]/MyRMS[,2]
  MyRMS$Sqrt_dRMS_per_count <- sqrt(MyRMS[,4]/MyRMS[,2])
  assign("RMS_Values", MyRMS, envir = .GlobalEnv)

  if (plot==TRUE){    # Plot RMS of displacements, and mean displacements on log-log scale

    yminval = min(c(min(MyRMS$MeanDisp_per_count),min(MyRMS$Sqrt_dRMS_per_count)))
    ymaxval = max(c(max(MyRMS$MeanDisp_per_count),max(MyRMS$Sqrt_dRMS_per_count)))

    plot(MyRMS[,1], MyRMS$MeanDisp_per_count, log="xy", xaxt="n", yaxt="n", ylim=c(yminval,ymaxval),
         xlim=c(min(MyRMS$TimeWindows_log),max(MyRMS$TimeWindows_log)),col=colours[1],pch=pchtype[1],
         xlab=paste("T(", timeunit, ")", sep = ""),ylab="")
    myTicks = axTicks(1)
    axis(1, at = myTicks, labels = formatC(myTicks, digits = 0, format = 'e'))
    myTicks2 = axTicks(2)
    axis(2, at = myTicks2, labels = formatC(myTicks2, digits = 0, format = 'e'))
    title(ylab=expression('<'*d^q*'>'^(1/q)*(km)), line=2)
    points(MyRMS[,1], MyRMS$Sqrt_dRMS_per_count, col=colours[2],pch=pchtype[2])
    if (legend[1]==TRUE){
      legend(legend[2], bty="n", c("Mean Disp.", "RMS Disp."), col=colours, pch=pchtype)
    }
  }

  if (lm==TRUE){
    model<-lm(formula = TimeWindows_log ~ dRMS, data = MyRMS)
    model
    assign("RMS_LinearModel",model, envir = .GlobalEnv)
  }

}
