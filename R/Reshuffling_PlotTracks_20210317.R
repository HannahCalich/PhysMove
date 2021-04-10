#' Randomized Trajectories
#'
#' Plot locations from real and reshuffled trajectories using RandomizedLat and RandomizedLong outputs from \code{\link{Randomize}} function
#' @param species_df A data frame containing location data (rows) and columns with the following headers: "ref", "lon", "lat", "day". "ref" is the unique id number for each animal
#'      (e.g., their satellite tag number; integer format). "lon" and "lat" are the longitude and latitide of each position estimate in decimal degrees (numeric format). "day"
#'      is the datetime stamp for each location estimate (POSIXct format following yyyy-mm-dd hh:mm:ss). See XXX data frame for an example.
#' @param Ref Trajectory ID number from species_df to plot
#' @param RandTraj Random trajectory(ies) to plot. Values can include either 1 number, or a range of numbers following #:# format. Default is 1:10.
#' @param Colours Colours for real trajectory locations (Colour option 1) and reshuffled trajectory locations (colour option 2). Default is Colours=c("black","red").
#' @param Legend Add legend to plot and specify location. Legend=c(TRUE, "topleft").
#' @return Plot showing real and reshuffled trajectory locations.
#' @examples PlotReshuffTracks<-function(species_df, Ref=33933, RandomizedLat, RandomizedLong, RandTraj=1)
#' @examples PlotReshuffTracks(species_df, Ref=33933, RandomizedLat, RandomizedLong, RandTraj=1:10, Colours=c("black","red"))
#' @export

PlotRandomTracks<-function(species_df, Ref, RandTraj=10, colours=c("black","red"), pchtype=c(19,19),  title="", legend=c(TRUE, "topleft")){

  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  Individual <- species_df[which(species_df$ref ==Ref),]
  a <-  as.character(unique(Individual$ref))

  plotpoints<-data.frame("lat"=integer(0),"lon"=integer(0))

  for(T in 1:RandTraj){ #For the reshuffled data
    lat_long<-data.frame("lon"=RandomizedLong[species_index[[a]],T],"lat"=RandomizedLat[species_index[[a]],T])
    plotpoints<-rbind(lat_long,plotpoints)
    }

  xmin = min(c(plotpoints[,1],Individual[,2]))
  xmax = max(c(plotpoints[,1],Individual[,2]))
  ymin = min(c(plotpoints[,2],Individual[,3]))
  ymax = max(c(plotpoints[,2],Individual[,3]))

  plot(plotpoints$lon, plotpoints$lat, col=colours[2], pch=pchtype[2], ylab = "Latitude", xlab="Longitude", xlim=c(xmin-0.5,xmax+0.5), ylim=c(ymin-0.5,ymax+0.5))
  points(Individual[,2],Individual[,3], col=colours[1], pch=pchtype[1]) #Plot real track on top
  lines(Individual[,2],Individual[,3], col=colours[1])
  if (legend[1]==TRUE){
    legend(legend[2], legend=c("Real", "Randomized"), pch=pchtype, col=colours, bty="n")
  }
}
