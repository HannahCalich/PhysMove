#' Create probability density function (PDF) plots of displacements
#'
#' This function allows you to plot probability density functions (PDFs) of raw or normalised displacements. Displacements must be in
#' list format where each list element corresponds to displacements calculated over a specific time window, which is the default output
#' format from the \code{\link{CalcDisp}} function.
#' @param displacements Displacements in list format (e.g., the output from the \code{\link{CalcDisp}} function).
#' @param normalised Normalise the displacements by the mean displacement for each time window.
#' @param colours Colour(s) for plot points. Valid input options include: base R (grDevices) color pallets (e.g., colours=rainbow), RColorBrewer
#' palettes (e.g., colours="Dark2"), and colour names or hex numbers (e.g.,colours=c("darkred", "#4682B4", "#00008B", "darkgreen")). Note that grDevies color
#' pallets do not use quotations. If the palette does not have enough distinct colours to match the communities being plotted the function will automatically
#' create a continuous pallet with the colours provided. Default is "Dark2".
#' @param legend Add legend with legend=TRUE. Default is TRUE.
#' @return Probability density function (PDF) plots of binned raw or normalised (if normalised=TRUE) displacements.
#' @examples DispPDFplot(displacements)
#' @examples DispPDFplot(displacements, normalised=TRUE, colours=rainbow)
#' @export

PlotDispPDF<-function (displacements, normalised=TRUE, colours=rainbow, legend=TRUE){

  if (class(colours)=="function"){ # If a grDevices colour pallet is used
    myColoursPal <- colours(length(displacements))
  } else if (colours[1] %in% rownames(RColorBrewer::brewer.pal.info)){ # If a RColourBrewer pallet is used
    myColoursPal <- colorRampPalette(RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[colours,1], colours))(length(displacements)) # Use the submitted colour palette and extend if to the number of colours needed
  } else {
    myPal <- colorRampPalette(colours) # If hex codes or colour names are used
    myColoursPal <- myPal(length(displacements))
  }

  disp <- c()
  logbase <- 2 # create variable to change base of log -> this allows the bin width in the loops below for easiness of visualisation
  disp0 <- 0.0000001 # zeroing my displacements (i.e., add this value to avoid displ < 1 that result in negative logs)
  bins <- seq(1,100000,1) # define a much larger number of bins than expected to be needed
  pdfPlot <- matrix(0, length(bins), 3)
  pdfPlotAll <- data.frame(y=c(0),x=c(0),timeWindow=c(0))

  #################################################################
  ## Log-log plot of displacements, not normalised by mean disp. ##
  #################################################################
  if (normalised!=TRUE){

    for(d in 1:length(displacements)){  #for each time period
      freq <- rep(0, length(bins)) # to count the number of displacements in each bin
      disp <- displacements[[d]]

      for(i in 1:length(disp)){ #for each displacement in the time period. To determine what bin that displacement belongs in,
        #calculate frequency of the displacements and tally the frequency in "freq", adding 0.5 shifts the bins so the values are on the midpoints of the bars
        if(disp[i] != 0){
          b <- floor(log(disp[i]/disp0)/log(logbase) + 0.5 ) # to convert the displacements in km to log & zero them
          freq[b] <- freq[b] + 1
        }
      }

      size <- sizelinear <- ratio <- mybins <- rep(0, length(bins))
      pdfPlot <- matrix(0, length(bins), 2)
      r <- 1

      for(b in 1:length(bins)){
        if(freq[b] != 0){
          size[b] <- (logbase^(b +0.5)- logbase^(b-0.5)) * disp0  # when using log scale we need log-sizes bin widths #Log scale & normalised log
          # Because we want the probability, we need to divide by the total number of displacements
          ratio[b] <- freq[b]/size[b]/ length(disp) # need to divide by the area of each bins if we are in log bins #log bins
          mybins[b] <- disp0*logbase^(b) #log bins
          pdfPlot[r,] <- c(ratio[b], mybins[b]) # stores the values for each bin #log and all normalised bins
          r <- r + 1
        }
      }
      pdfPlot <- as.data.frame(pdfPlot)
      names(pdfPlot) <- c("y","x")
      pdfPlot[pdfPlot==0] <- NA
      pdfPlot <- pdfPlot[complete.cases(pdfPlot),]
      pdfPlot$timeWindow <- rep(d,nrow(pdfPlot))
      pdfPlotAll <- rbind(pdfPlotAll, pdfPlot) # Add data to df
    }
    pdfPlotAll <- pdfPlotAll[-1,] # Remove initializing row

    a <- ggplot2::ggplot(data=pdfPlotAll, ggplot2::aes(x=x, y=y, colour=as.factor(timeWindow))) +
      ggplot2::geom_point() +
      ggplot2::scale_colour_manual(name="Time Window",values=unique(myColoursPal)) +
      ggplot2::scale_x_log10(
        breaks = function(x) {
          brks <- scales::extended_breaks(Q = c(1, 5))(log10(x))
          10^(brks[brks %% 1 == 0])
        },
        labels = scales::math_format(format = log10)
      ) +
      ggplot2::scale_y_log10(
        breaks = function(x) {
          brks <- scales::extended_breaks(Q = c(1, 5))(log10(x))
          10^(brks[brks %% 1 == 0])
        },
        labels = scales::math_format(format = log10),
      ) +
      ggplot2::theme_bw(base_size=18)+
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                          panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                                          axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), colour="black"),
                                          axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), colour="black"),
                                          legend.title = ggplot2::element_text())+
      ggplot2::annotation_logticks(short=grid::unit(-0.1, "cm"), mid=grid::unit(-0.1, "cm"), long=grid::unit(-0.3,"cm")) +
      ggplot2::coord_cartesian(clip="off")+
      ggplot2::xlab("Displacements (km)")+
      ggplot2::ylab("pdf")
    if(legend==FALSE){
      a <- a + ggplot2::theme(legend.position = "none")
    }
    plot(a)
    colnames(pdfPlotAll) <- c("pdf", "disp", "timeWindow")
    return(pdfPlotAll)
  }
  #####################################################################################
  ## log-log plot of displacements normalised by mean displacement per time interval ##
  #####################################################################################
  if (normalised ==TRUE){
    MeanDisp <- c()
    for(d in 1:length(displacements)){  #for each time period
      freq <- rep(0, length(bins)) # to count the number of displacements in each bin
      disp <- displacements[[d]]
      MeanDisp <- mean(displacements[[d]])

      for(i in 1:length(disp)){ #for each displacement in the time period. To determine what bin that displacement belongs in, calculate frequency of the displacements and tally the frequency in "freq", adding 0.5 shifts the bins so the values are on the midpoints of the bars
        if(disp[i] != 0){
          b <- floor(log(disp[i]/disp0)/log(logbase) + 0.5 ) # to convert the displacements in km to log & zero them
          freq[b] <- freq[b] + 1
        }}

      size <- sizelinear <- ratio <- mybins <- rep(0, length(bins))
      pdfPlot <- matrix(0, length(bins), 2)
      r<-1

      for(b in 1:length(bins)){
        if(freq[b] != 0){
          size[b] <- (logbase^(b +0.5)- logbase^(b-0.5)) * disp0  # when using log scale we need log-sizes bin widths
          ratio[b] <- (MeanDisp*(freq[b]/size[b])) / length(disp) #normalised log
          mybins[b] <- (disp0*logbase^(b))/MeanDisp #normalised log bins
          pdfPlot[r,] <- c(ratio[b], mybins[b]) # stores the values for each bin #log and all normalised bins
          r <- r + 1
        }
      }
      pdfPlot <- as.data.frame(pdfPlot)
      names(pdfPlot) <- c("y","x")
      pdfPlot[pdfPlot==0] <- NA
      pdfPlot <- pdfPlot[complete.cases(pdfPlot),]
      pdfPlot$timeWindow <- rep(d,nrow(pdfPlot))
      pdfPlotAll <- rbind(pdfPlotAll, pdfPlot) # Add data to df
    }
    pdfPlotAll <- pdfPlotAll[-1,] # Remove initializing row

    a <- ggplot2::ggplot(data=pdfPlotAll, ggplot2::aes(x=x, y=y, colour=as.factor(timeWindow))) +
        ggplot2::geom_point() +
        ggplot2::scale_colour_manual(name="Time Window",values=unique(myColoursPal)) +
        ggplot2::scale_x_log10(
          breaks = function(x) {
            brks <- scales::extended_breaks(Q = c(1, 5))(log10(x))
            10^(brks[brks %% 1 == 0])
          },
          labels = scales::math_format(format = log10)
        ) +
        ggplot2::scale_y_log10(
          breaks = function(x) {
            brks <- scales::extended_breaks(Q = c(1, 5))(log10(x))
            10^(brks[brks %% 1 == 0])
          },
          labels = scales::math_format(format = log10),
        ) +
        ggplot2::theme_bw(base_size=18)+
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                            panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                                            axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), colour="black"),
                                            axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), colour="black"),
                                            legend.title = ggplot2::element_text())+
        ggplot2::annotation_logticks(short=grid::unit(-0.1, "cm"), mid=grid::unit(-0.1, "cm"), long=grid::unit(-0.3,"cm")) +
        ggplot2::coord_cartesian(clip="off")+
        ggplot2::xlab("Normalised displacements")+
        ggplot2::ylab("pdf")

    if(legend==FALSE){
      a <- a + ggplot2::theme(legend.position = "none")
    }
    plot(a)
    colnames(pdfPlotAll) <- c("pdf", "disp", "timeWindow")
    return(pdfPlotAll)
  }
}
