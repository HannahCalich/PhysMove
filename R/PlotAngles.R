#' Plot turning angles with a circle plot
#'
#' This function allows you to create a circle plot illustrating the frequency of turning angles from
#' the \code{\link{TurningAngles}} function.
#' @param angleList List of angles calculates with the \code{\link{TurningAngles}} function.
#' @param timePlot Plot angles from all time windows or only plot angles from one specific time window. For example,
#' timePlot=1 will only plot angles from the first time window while timePlot="all" will plot all time windows.
#' Default is timePlot="all".
#' @param colours Colour(s) for lines in circle plot Valid input options include: base R (grDevices) color pallets (e.g., colours=rainbow),
#' RColorBrewer palettes (e.g., colours="Dark2"), and colour names or hex numbers (e.g.,colours=c("darkred", "#4682B4", "#00008B", "darkgreen")). Note that
#' grDevices color pallets are functions and do not use quotations. If the palette does not have enough distinct colours to match the lines being plotted the function will
#' automatically create a continuous pallet with the colours provided. Default is rainbow.
#' @param legend Add a legend to the circle plot. Default is TRUE.
#' @return Circle plot of the angles calculated with the \code{\link{TurningAngles}} function and the data used to create the circle plot.
#' @importFrom rlang .data
#' @examples PlotAngles(angleList)
#' @examples PlotAngles(angleList, timePlot="all", colours=rainbow, legend=TRUE)
#' @export

PlotAngles<-function(angleList, timePlot="all", colours=rainbow, legend=TRUE){

  bins <- 360 / 45
  timeWindows <- as.numeric(names(angleList))
  for (d in 1:length(angleList)){
    h <- graphics::hist(unlist(angleList[[d]]), plot = FALSE, breaks = seq(-180, 180, bins)) # angleList is all angels for a time period from all individuals
    probability <- h$counts/length(unlist(angleList[[d]]))
    probability <- c(probability[1:23],probability[23],probability[24:45]) # Duplicated angle at 0 since 360=0 and 360 is needed for plot
    Cols <- h$mids
    angles <- c(Cols[c(1:22)]+360,360,Cols[c(23:45)]) # Added 360 to list for plot. Angles at 360 are the same as at 0

    if (d==1){
      circle.plot <- as.data.frame(cbind(angles, probability, timeWindows=c(rep(timeWindows[d], length(probability)))))
    }
    if (d > 1){
      circle.plot_temp <- as.data.frame(cbind(angles, probability, timeWindows=c(rep(timeWindows[d], length(probability)))))
      circle.plot <- rbind(circle.plot, circle.plot_temp)
    }
  }

  if (timePlot!="all"){
    circle.plot <- circle.plot[which(circle.plot$timeWindows==timePlot),]
  }

  circle.plot <- circle.plot[stats::complete.cases(circle.plot), ] #remove rows with no data
  circle.plot$timeWindows <- round(circle.plot$timeWindows,3)
  circle.plot <- circle.plot[,c(3,2,1)]

  if ("function" %in% is(colours)){ # If a grDevices colour pallet is used
    myColoursPal <- colours(length(unique(circle.plot$timeWindows)))
  } else if (colours[1] %in% rownames(RColorBrewer::brewer.pal.info)){ # If a RColourBrewer pallet is used
    myColoursPal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[colours,1], colours))(length(unique(circle.plot$timeWindows))) # Use the submitted colour palette and extend if to the number of colours needed
  } else {
    myPal <- grDevices::colorRampPalette(colours) # If hex codes or colour names are used
    myColoursPal <- myPal(length(unique(circle.plot$timeWindows)))
  }

  if (legend==TRUE){
    title <- "Time Windows"
    legendPos <- "right"
  } else {
    title <- ""
    legendPos <- "ggplot2::element_blank()"
  }

  circle.plot_plot <- ggplot2::ggplot(circle.plot, ggplot2::aes(x = .data$angles, y = .data$probability, group=as.factor(timeWindows),colour=as.factor(timeWindows)))+
    ggplot2::coord_polar(clip="off")+
    ggplot2::geom_hline(yintercept = c(0, max(circle.plot$probability)+0.01), colour = "black", size = 0.25) +
    ggplot2::geom_vline(xintercept = seq(0, 360, by = 90), colour = "black", size = 0.25) +
    ggplot2::geom_point(size = 0.4) +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_colour_manual(title, values = myColoursPal)+
    ggplot2::scale_x_continuous(limits=c(0, 360), breaks=c(0,90,180,270), labels=c("0"," 90","180","270  "))+
    ggplot2::xlab("") +
    ggplot2::ylab("")+
    ggplot2::labs(title="")+
    ggplot2::theme_bw(base_size=18)+
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = legendPos,
      panel.border = ggplot2::element_blank(),
      panel.grid  = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0,0,0,0), "cm"),
      legend.box.margin=grid::unit(c(0,0,0,0), "cm"))
  plot(circle.plot_plot)
  colnames(circle.plot)[2] <- c("frequency")
  return(circle.plot)
}
