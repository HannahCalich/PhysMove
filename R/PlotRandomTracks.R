#' Plot Randomised Tracks
#'
#' Plot locations from original and reshuffled tracks using RandomisedLat and RandomisedLong outputs from the \code{\link{Randomise}} function
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{tracks}}.
#' @param ref Reference number of track from species_df to plot.
#' @param numPlot Number of Randomised tracks to plot. The Randomised tracks were consecutively numbered from 1 to however many you set in the
#' \code{\link{Randomise}} function. The input value can either be any of these individual numbers (e.g., 23), or a range of numbers (e.g., 1:10),
#' which will plot all of the random tracks created within the range. Default is 1:5.
#' @param colours Colours to plot points from original and Randomised tracks, respectively. Default is colours=c("black","grey70").
#' @param tracks Add track lines to the plot. Default is TRUE.
#' @param startCol Colour for origin location. startCol=NULL will cause the symbology of the origin location to match the symbology of the rest
#' of the original track. Default is startCol="red".
#' @param endCol Colour for destination location. endCol=NULL will cause the symbology of the destination location to match the symbology of the rest
#' of the original track. Default is endCol="blue".
#' @param legend legend Add legend with legend=TRUE (default).
#' @return Plot showing the original and Randomised track locations and the Randomised tracks data used to create the map (original tracks are
#' from species_df).
#' @examples PlotRandomTracks<-function(tracks, ref=1)
#' @examples PlotRandomTracks<-(tracks, ref=1, numPlot=1:5, colours=c("black","grey70"), tracks=TRUE, startCol="red", endCol="blue", legend=TRUE)
#' @export

PlotRandomTracks<-function(species_df, ref=NULL, numPlot=1:5, colours=c("black","grey70"),
                           tracks=TRUE, startCol="red", endCol="blue", legend=TRUE){

  if ((exists("RandomisedLat")& exists("RandomisedLong"))==FALSE){
    stop("Please create Randomised tracks using the Randomise function prior to executing PlotRandomTracks")
  }

  if(is.null(ref)==TRUE || !(ref %in% species_df$ref)){
    stop("What track would you like to plot? Please update the 'ref' parameter to a valid reference number from your species_df")
  }

  if (length(numPlot)>ncol(RandomisedLat)){
    stop("You cannot plot more random tracks than you created with the Randomise function. Please adjust the numPlot value or
         re-run Randomise to create more tracks")
  }

  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  Individual <- species_df[which(species_df$ref == ref),]
  a <-  as.character(unique(Individual$ref))
  plotpoints <- data.frame("lat"=integer(0),"lon"=integer(0)) # Format the df, the 0s are removed

  for(T in numPlot){ #For the Randomised data
    lat_long <- data.frame("RandomTraj"=T, "lon"=RandomisedLong[species_index[[a]],T],"lat"=RandomisedLat[species_index[[a]],T])
    plotpoints <- rbind(plotpoints,lat_long)
  }

  Individual <- cbind(Track=rep("Original",nrow(Individual)),Individual)
  Individual <- Individual[,1:4]
  plotpoints <- cbind(Track=rep("Randomised",nrow(plotpoints)),plotpoints)
  names(plotpoints) <- names(Individual) # make sure col names match
  plot.df <- rbind(plotpoints,Individual)
  plot.df <- cbind(plot.df, "trackRef"=paste(plot.df$Track,plot.df$ref,sep="_"))

  if (is.null(startCol)){
    startCol <- colours[1]
  }
  startPt <- Individual[1,]
  startPt$Track <- "Start"
  startPt$trackRef <- "Original_1"
  plot.df <- rbind(plot.df, startPt)

  if (is.null(endCol)){
    endCol <- colours[1]
  }
  endPt <- Individual[nrow(Individual),]
  endPt$Track <- "End"
  endPt$trackRef <- "Original_1"
  plot.df <- rbind(plot.df, endPt)

  plot.df$Track <- factor(plot.df$Track, levels = c("Original", "Randomised", "Start","End"))
  colours <- c(colours, startCol, endCol)

  a <- ggplot2::ggplot(plot.df, ggplot2::aes(x=lon, y=lat, color=Track)) +
    ggplot2::geom_point(ggplot2::aes(x=lon, y=lat, color=Track), size=1.5)+
    ggplot2::geom_point(data=startPt, ggplot2::aes(x=lon, y=lat, group=Track, color="Start"), size=1.5) +
    ggplot2::geom_point(data=endPt, ggplot2::aes(x=lon, y=lat, group=Track, color="End"), size=1.5) +
    ggplot2::theme_bw(base_size=18)+
    ggplot2::theme(axis.line=ggplot2::element_line(colour="black"),
                                        panel.grid.major=ggplot2::element_line(),
                                        panel.grid.minor=ggplot2::element_blank(),
                                        axis.text.x=ggplot2::element_text(margin=ggplot2::margin(t=10), colour="black"),
                                        axis.text.y=ggplot2::element_text(margin=ggplot2::margin(r=10), colour="black"),
                                        legend.title = ggplot2::element_blank())+
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude")

  if (tracks==TRUE){
    a <- a +
      ggplot2::geom_path(data=subset(plot.df, Track=='Randomised'))+
      ggplot2::geom_path(data=subset(plot.df, Track=='Original'))+
      ggplot2::geom_point(data=subset(plot.df, Track=='Original'))+
      ggplot2::geom_point(data=startPt, ggplot2::aes(x=lon, y=lat, group=Track, color="Start")) +
      ggplot2::geom_point(data=endPt, ggplot2::aes(x=lon, y=lat, group=Track, color="End"))
  }

  if (startCol==colours[1] & endCol==colours[1]){ #If start and end colours were not provided or match the original track colour
    a <- a + ggplot2::scale_colour_manual(values = colours, labels = c("Original", "Randomised", "", ""),
                                          guide = ggplot2::guide_legend(override.aes = list(
                                          linetype = c("solid","solid", "blank", "blank"),
                                          shape = c(16,16, NA, NA),
                                          labels = c("Original", "Randomised", "", ""))))
  } else {
   a <- a + ggplot2::scale_colour_manual(values = colours, labels = c("Original", "Randomised", "Start point", "End point"),
                                         guide = ggplot2::guide_legend(override.aes = list(
                                         linetype = c("solid","solid", "blank", "blank"),
                                         shape = c(16, 16, 16, 16))))
  }

  if (legend==FALSE){
    a <- a + ggplot2::theme(legend.position = "none")
  }
  plot(a)
  plotpoints <- plotpoints[,2:4]
  colnames(plotpoints)[1] <- c("randTrack")
  return(plotpoints)
}
