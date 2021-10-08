#' Plot turning angles with a spider/radar chart
#'
#' This function allows you to create a spider/radar chart illustrating the frequency of turning angles from
#' the \code{\link{TurningAngles}} function.
#' @param angleList List of angles calculates with the \code{\link{TurningAngles}} function.
#' @param timePlot Plot angles from all time windows or only plot angles from one specific time window. For example,
#' timePlot=1 will only plot angles from the first time window while timePlot="all" will plot all time windows.
#' Default is timePlot="all".
#' @param colours Colour(s) for lines in spiderPlot. Valid input options include: base R (grDevices) color pallets (e.g., colours=rainbow),
#' RColorBrewer palettes (e.g., colours="Dark2"), and colour names or hex numbers (e.g.,colours=c("darkred", "#4682B4", "#00008B", "darkgreen")). Note that
#' grDevices color pallets are functions and do not use quotations. If the palette does not have enough distinct colours to match the lines being plotted the function will
#' automatically create a continuous pallet with the colours provided. Default is rainbow.
#' @param legend Add a legend to the spider plot. Default is TRUE.
#' @return Spider/radar chart of the angles calculated with the \code{\link{TurningAngles}} function and the data used to create the spider/radar chart.
#' @examples PlotAngles(angleList)
#' @examples PlotAngles(angleList, timePlot="all", colours=rainbow, legend=TRUE)
#' @export

PlotAngles<-function(angleList, timePlot="all", colours=rainbow, legend=c(TRUE)){

  bins <- 360 / 45
  timeWindows <- as.numeric(names(angleList))
  for (d in 1:length(angleList)){
    h <- hist(unlist(angleList[[d]]), plot = FALSE, breaks = seq(-180, 180, bins)) # angleList is all angels for a time period from all individuals
    probability <- h$counts/length(unlist(angleList[[d]]))
    probability <- c(probability[1:23],probability[23],probability[24:45]) # Duplicated angle at 0 since 360=0 and 360 is needed for plot
    Cols <- h$mids
    angles <- c(Cols[c(1:22)]+360,360,Cols[c(23:45)]) # Added 360 to list for plot. Angles at 360 are the same as at 0

    if (d==1){
      spider <- as.data.frame(cbind(angles, probability, timeWindows=c(rep(timeWindows[d], length(probability)))))
    }
    if (d > 1){
      spider_temp <- as.data.frame(cbind(angles, probability, timeWindows=c(rep(timeWindows[d], length(probability)))))
      spider <- rbind(spider, spider_temp)
    }
  }

  if (timePlot!="all"){
    spider <- spider[which(spider$timeWindows==timePlot),]
  }

  spider <- spider[complete.cases(spider), ] #remove rows with no data
  spider$timeWindows <- round(spider$timeWindows,3)
  spider <- spider[,c(3,2,1)]

  if (class(colours)=="function"){ # If a grDevices colour pallet is used
    myColoursPal <- colours(length(unique(spider$timeWindows)))
  } else if (colours[1] %in% rownames(RColorBrewer::brewer.pal.info)){ # If a RColourBrewer pallet is used
    myColoursPal <- colorRampPalette(RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[colours,1], colours))(length(unique(spider$timeWindows))) # Use the submitted colour palette and extend if to the number of colours needed
  } else {
    myPal <- colorRampPalette(colours) # If hex codes or colour names are used
    myColoursPal <- myPal(length(unique(spider$timeWindows)))
  }

  if (legend==TRUE){
    title <- "Time Windows"
    legendPos <- "right"
  } else {
    title <- ""
    legendPos <- "ggplot2::element_blank()"
  }

  spider_plot <- ggplot2::ggplot(spider, ggplot2::aes(x = angles, y = probability, group=as.factor(timeWindows),colour=as.factor(timeWindows)))+
    ggplot2::coord_polar()+
    ggplot2::geom_hline(yintercept = c(0, max(spider$probability)+0.01), colour = "black", size = 0.25) +
    ggplot2::geom_vline(xintercept = seq(0, 360, by = 90), colour = "black", size = 0.25) +
    ggplot2::geom_point(size = 0.4) +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_colour_manual(title, values = myColoursPal)+
    ggplot2::scale_x_continuous(limits = c(0, 360), breaks = c(0,90,180,270), labels = c("0","90","180","270"))+ #0°
    ggplot2::xlab("") +
    ggplot2::ylab("")+
    ggplot2::labs(title="")+
    ggplot2::theme_bw(base_size=18)+
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 15, face = "bold"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = legendPos,
      panel.border = ggplot2::element_blank(),
      panel.grid  = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0,0,0,0), "cm"))
  plot(spider_plot)
  return(spider)
}
