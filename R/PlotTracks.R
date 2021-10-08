#' Plot Tracks
#'
#' Plot species' location estimates and tracks .
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{speciesA}}.
#' @param ref Reference number of track from species_df to plot, options include an individual number (ref=1) or a range of numbers (ref=1:10).
#' By default all unique reference numbers are plotted. Defalt is NULL.
#' @param colours Colour(s) for plot points. Valid input options include: base R (grDevices) color pallets (e.g., colours=rainbow), RColorBrewer
#' palettes (e.g., colours="Dark2"), and colour names or hex numbers (e.g.,colours=c("darkred", "#4682B4", "#00008B", "darkgreen")). Note that grDevies color
#' pallets do not use quotations. If the palette does not have enough distinct colours to match the communities being plotted the function will automatically
#' create a continuous pallet with the colours provided. Default is "Dark2".
#' @param tracks Add track lines to the plot. Default is TRUE.
#' @return Plot showing the original and randomized track locations and the randomized tracks data used to create the map (original tracks are
#' from species_df).
#' @examples PlotRandomTracks<-function(speciesA, ref=1)
#' @examples PlotRandomTracks<-(speciesA, ref=1, numPlot=1:5, colours=c("black","grey70"), tracks=TRUE, startCol="red", endCol="blue")
#' @export

PlotTracks<-function(species_df, ref=NULL, tracks=TRUE, colours=rainbow){

  if(!is.null(ref)){
    if (!all(ref %in% species_df$ref)){
      stop("What track would you like to plot? Please update the 'ref' parameter to a valid reference number from your species_df")
    }
  }

  if(!is.null(ref)){
    plot.df <- species_df[(species_df$ref %in% ref),]
  } else {
    plot.df <- species_df
  }

  if (class(colours)=="function"){ # If a grDevices colour pallet is used
    myColoursPal <- colours(length(unique(plot.df$ref)))
  } else if (colours[1] %in% rownames(RColorBrewer::brewer.pal.info)){ # If a RColourBrewer pallet is used
    myColoursPal <- colorRampPalette(RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[colours,1], colours))(length(unique(plot.df$ref))) # Use the submitted colour palette and extend if to the number of colours needed
  } else {
    myPal <- colorRampPalette(colours) # If hex codes or colour names are used
    myColoursPal <- myPal(length(unique(plot.df$ref)))
  }

  a <- ggplot2::ggplot(plot.df, ggplot2::aes(x=lon, y=lat))+#, color=as.factor(ref))) +
    ggplot2::geom_point(ggplot2::aes(fill=as.factor(ref)),pch=21,size=1.8,colour="grey20",stroke=0.5)+
    # ggplot2::geom_point(size=1)+
    # ggplot2::geom_point(size=2, colour="black")+
    ggplot2::theme_bw(base_size=18)+
    ggplot2::theme(axis.line=ggplot2::element_line(colour = "black"),
                                        panel.grid.major = ggplot2::element_line(),
                                        panel.grid.minor = ggplot2::element_blank(),
                                        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), colour="black"),
                                        axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), colour="black"),
                                        legend.position = "none")+
    ggplot2::scale_colour_manual(values=c(unique(myColoursPal)))+
    ggplot2::xlab("Longitude")+
    ggplot2::ylab("Latitude")
  tryCatch({
    a <- a +
      ggplot2::borders("world", colour ="gray50", fill ="gray50", xlim = c(min(plot.df$lon), max(plot.df$lon)),
                       ylim = c(min(plot.df$lat), max(plot.df$lat)))
  }, error = function(e){message('Note: World polygon does not overlap with location data')})

  if (tracks==TRUE){
    a <- a +
      ggplot2::geom_path(ggplot2::aes(colour=as.factor(ref)))+
      ggplot2::geom_point(ggplot2::aes(fill=as.factor(ref)),pch=21,size=1.8,colour="grey20",stroke=0.5)
  }
  plot(a)
}
