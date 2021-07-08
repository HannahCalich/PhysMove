#' Randomized Trajectories
#'
#' Plot locations from original and reshuffled trajectories using randomizedLat and randomizedLong outputs from the \code{\link{Randomize}} function
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{plSample}}, \code{\link{expSample}}, or \code{\link{lnormSample}}.
#' @param ref Reference number of trajectory from species_df to plot.
#' @param numPlot Number of randomized trajectories to plot. Input value can be any number that does not exceed the number of trajectories created in
#' the \code{\link{Randomize}} function. Default is 1:5.
#' @param colours Colours to plot points from original and randomized trajectories, respectively. Default is colours=c("black","grey").
#' @param pchType Pch symbols to plot points from original and randomized trajectories, respectively. Pch values between 1 and 20 are valid. Default is pchType=c(16,16).
#' @param startPoint Colour and pch symbol for origin location. startPoint=NULL will force the startPoint symbology to match the symbology of the original
#' trajectory. Pch values between 1 and 20 are valid. Default is startPoint=c("red",10)
#' @param endPoint Colour and pch symbol for destination location. endPoint=NULL will force the endPoint symbology to match the symbology of the original
#' trajectory. Pch values between 1 and 20 are valid. Default is endPoint=c("blue",10)
#' @param tracks Add track lines to original and randomized trajectories respectively. Default is tracks=c("TRUE","TRUE).
#' @param legend Add legend to plot and specify location. Legend=c(TRUE, "topleft").
#' @return Plot showing real and reshuffled trajectory locations and a data frame with location data for the reshuffled trajectories on the map('randomizedTrajectories').
#' @examples PlotReshuffTracks<-function(expSample, ref=1)
#' @examples PlotReshuffTracks(expSample, ref=NULL, numPlot=1:5, colours=c("black","grey"), pchType=c(16,16), startPoint=c("red",10), endPoint=c("blue",10), tracks=c("TRUE","TRUE"), legend=c(TRUE, "topleft"))
#' @export

PlotRandomTracks<-function(species_df, ref=NULL, numPlot=1:5, colours=c("black","grey"), pchType=c(16,16), startPoint=c("red",10),
                           endPoint=c("blue",10), tracks=c("TRUE","TRUE"), legend=c(TRUE, "topleft")){

  if ((exists("randomizedLat")& exists("randomizedLong")& exists("randTraj"))==FALSE){
    stop("Please create randomized trajectoiries using the Randomize function prior to executing PlotRandomTracks")
  }

  if(is.null(ref)==TRUE || !(ref %in% species_df$ref)){
    stop("What trajectory would you like to plot? Please update the 'ref' parameter to a valid reference number from your species_df")
  }

  if (length(numPlot)>randTraj){
    stop("You cannot plot more random trajectories than you created with the Randomize function. Please adjust the numPlot value or
         re-run Randomize to create more trajectories")
  }

  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  Individual <- species_df[which(species_df$ref ==ref),]
  a <-  as.character(unique(Individual$ref))
  plotpoints<-data.frame("lat"=integer(0),"lon"=integer(0))

  for(T in numPlot){ #For the reshuffled data
    lat_long<-data.frame("RandomTraj"=T, "lon"=randomizedLong[species_index[[a]],T],"lat"=randomizedLat[species_index[[a]],T])
    plotpoints<-rbind(plotpoints,lat_long)
    }

  assign("randomizedTrajectories", plotpoints, envir = .GlobalEnv)
  xmin = min(c(plotpoints[,2],Individual[,2]))
  xmax = max(c(plotpoints[,2],Individual[,2]))
  ymin = min(c(plotpoints[,3],Individual[,3]))
  ymax = max(c(plotpoints[,3],Individual[,3]))

  plot(plotpoints$lon, plotpoints$lat, col=colours[2], pch=pchType[2], ylab = "Latitude", xlab="Longitude", xlim=c(xmin-0.5,xmax+0.5), ylim=c(ymin-0.5,ymax+0.5))

  if (tracks[2]==TRUE){ #if we want lines for randomized trajectories
    for(T in numPlot){ #for each trajectory
      lines(plotpoints[which(plotpoints$RandomTraj==T),2], plotpoints[which(plotpoints$RandomTraj==T),3], col=colours[2])
    }
  }
  points(Individual[,2],Individual[,3], col=colours[1], pch=pchType[1]) #Plot real track on top
  if (tracks[1]==TRUE){
    lines(Individual[,2],Individual[,3], col=colours[1])
  }
  if (!is.null(startPoint)){
    points(Individual[1,2],Individual[1,3], col=startPoint[1], pch=as.numeric(startPoint[2]))
  }
  if (!is.null(endPoint)){
    points(Individual[nrow(Individual),2],Individual[nrow(Individual),3], col=endPoint[1], pch=as.numeric(endPoint[2]))
  }

  if (legend[1]==TRUE){
    legend(legend[2], legend=c("Original", "Randomized"), pch=pchType, col=colours, bty="n")
  }
}

