#' Create probability density function plots (PDFs) of displacements
#'
#' This function allows you to plot probability density functions (PDFs) of raw displacements and
#' displacements normalized by the mean displacement for each time window. Displacements must have been
#' previously calculated with the \code{\link{CalcDist}} function.
#' @param displacements The output from the \code{\link{CalcDisp}} function.
#' @param colours Colour(s) for points in pdf plot. Valid options include: base R (grDevices) color pallets (e.g., colours=rainbow),
#' specific colours (e.g., colours=c("Navy"), or colours=c("red","blue")), or hex numbers (e.g., colours=c("#FF0000")). Default is rainbow.
#' @param legend Add a legend to the plot when TRUE and change the position of the legend. Default is legend=c(TRUE, "bottomleft")
#' @return Probability density function (PDF) plots of raw and normalized displacements.
#' @examples DispPDFPlot(displacements)
#' @examples DispPDFPlot(displacements, colours=rainbow, legend=c(TRUE, "bottomleft"))
#' @export

DispPDFPlot <- function (displacements, colours=rainbow, legend=c(TRUE, "bottomleft")){

  if (exists("displacements")==FALSE){
    stop("Please calculate displacements using CalcDisp and fit distriubtions using FitDisp prior to executing PlotDist")
  }

  if (class(colours)=="function"){ # If a colour pallet is added assign colours based on the pallet
    plotColours <- colours(length(displacements))
  } else { # Else if colours are added as a vector of text or hex numbers, make sure there are enough colours to plot
    if (length(colours)!=length(displacements)){
      plotColours <- rep(colours, length(displacements))
    }
  }

  layout(matrix(c(1,2), 1, 2, byrow = TRUE), c(1,1), c(1,1), TRUE)


  for (d in 1:length(timeWindows)){
    h <- hist(displacements[[d]], breaks = c(0, seq(0.5, round(range(displacements[[d]])[2])+1.5)), plot = FALSE)
    Counts <- h$counts
    mylength <- length(Counts)
    P_Dist <- Counts/length(displacements[[d]])

    if (d == 1){
      plot(h$mids, P_Dist, log = "xy", xlim = c(min(h$mids),max(h$mids)),
                     ylim =  c(min(P_Dist),max(P_Dist)), ylab = "Prob(Disp)", xlab = "Displacements (km)",
                     xaxt = "n", yaxt = "n", col = plotColours[d], type="p")
      myTicks = axTicks(1)
      axis(1, at = myTicks, labels = formatC(myTicks, digits = 0, format = 'e'))
      myTicks2 = axTicks(2)
      axis(2, at = myTicks2, labels = formatC(myTicks2, digits = 0, format = 'e'))

      # plot(h$mids, P_Dist, log="xy", xlab = "Distance (km)", ylab = "Prob(Distance)", type = "l", xlim = c(1, 10000), ylim = c(0.000001, 0.15), col=colour[1])
    } else {
      par(new=T)
      plot(h$mids, P_Dist, log="xy",  xlab = "",  ylab = "", type = "p", xaxt="n", yaxt="n", xlim = c(min(h$mids),max(h$mids)),
           ylim =  c(min(P_Dist),max(P_Dist)), col=colour[d], lty=1)
    }

  }




  # #################################################################
  # ## Log-log plot of displacements, not normalized by mean disp. ##
  # #################################################################
  #
  # disp <- c()
  # logbase <- 2 # create variable to change base of log -> this allows the bin width in the loops below for easiness of visualisation
  # disp0 <- 0.0000001 # zeroing my displacements (i.e., add this value to avoid displ < 1 that result in negative logs)
  # bins <- seq(1,100000,1) # define a much larger number of bins than expected to be needed
  #
  # for(d in 1:length(timeWindows)){  #for each time period
  #   freq <- rep(0, length(bins)) # to count the number of displacements in each bin
  #   disp <- displacements[[d]]
  #
  #   for(i in 1:length(disp)){ #for each displacement in the time period. To determine what bin that displacement belongs in,
  #     #calculate frequency of the displacements and tally the frequency in "freq", adding 0.5 shifts the bins so the values are on the midpoints of the bars
  #     if(disp[i] != 0){
  #       b <- floor(log(disp[i]/disp0)/log(logbase) + 0.5 ) # to convert the displacements in km to log & zero them
  #       freq[b] <- freq[b] + 1
  #     }
  #   }
  #
  #   size <- sizelinear <- ratio <- mybins <- rep(0, length(bins))
  #   pdfPlot <- matrix(0, length(bins), 2)
  #   r<-1
  #
  #   for(b in 1:length(bins)){
  #     if(freq[b] != 0){
  #       size[b] <- (logbase^(b +0.5)- logbase^(b-0.5)) * disp0  # when using log scale we need log-sizes bin widths #Log scale & normalized log
  #       # Because we want the probability, we need to divide by the total number of displacements
  #       ratio[b] <- freq[b]/size[b]/ length(disp) # need to divide by the area of each bins if we are in log bins #log bins
  #       mybins[b] <- disp0*logbase^(b) #log bins
  #       pdfPlot[r,] <- c(ratio[b], mybins[b]) # stores the values for each bin #log and all normalized bins
  #       r <- r + 1
  #     }
  #   }
  #
  #   if (d == 1){
  #     plot(pdfPlot[1:r-1,2], pdfPlot[1:r-1,1], log = "xy", xlim = c(min(pdfPlot[1:r-1,2]),max(pdfPlot[1:r-1,2])),
  #          ylim =  c(min(pdfPlot[1:r-1,1]),max(pdfPlot[1:r-1,1])), ylab = "Prob(Disp)", xlab = "displacements (km)",
  #          xaxt = "n", yaxt = "n", col = plotColours[d])
  #     myTicks = axTicks(1)
  #     axis(1, at = myTicks, labels = formatC(myTicks, digits = 0, format = 'e'))
  #     myTicks2 = axTicks(2)
  #     axis(2, at = myTicks2, labels = formatC(myTicks2, digits = 0, format = 'e'))
  #
  #   } else {
  #     par(new=T)
  #     plot(pdfPlot[1:r-1,2], pdfPlot[1:r-1,1], log="xy", xlim = c(min(pdfPlot[1:r-1,2]),max(pdfPlot[1:r-1,2])),
  #          ylim =  c(min(pdfPlot[1:r-1,1]),max(pdfPlot[1:r-1,1])), xaxt="n", yaxt="n", xlab = "",  ylab = "",
  #          col=plotColours[d], lwd=1, pch=1)
  #   }
  # }
  # if (legend[1]==TRUE){
  #   Days <- timeWindows/(24*60*60)
  #   legend(legend[2], legend = Days, lwd = 1, col = plotColours, bty = "n", pch = 16, y.intersp = 0.7, x.intersp = 0.4)
  # }
  #
  # #####################################################################################
  # ## log-log plot of displacements normalized by mean displacement per time interval ##
  # #####################################################################################
  #
  # disp <- c()
  # bins <- seq(1,10000,1) # define a much larger number of bins than expected to be needed
  # MeanDisp <- c()
  #
  # for(d in 1:length(timeWindows)){  #for each time period
  #   freq <- rep(0, length(bins)) # to count the number of displacements in each bin
  #   disp <- displacements[[d]]
  #   MeanDisp <- mean(displacements[[d]])
  #
  #   for(i in 1:length(disp)){ #for each displacement in the time period. To determine what bin that displacement belongs in, calculate frequency of the displacements and tally the frequency in "freq", adding 0.5 shifts the bins so the values are on the midpoints of the bars
  #     if(disp[i] != 0){
  #       b <- floor(log(disp[i]/disp0)/log(logbase) + 0.5 ) # to convert the displacements in km to log & zero them
  #       freq[b] <- freq[b] + 1
  #     }}
  #
  #   size <- sizelinear <- ratio <- mybins <- rep(0, length(bins))
  #   pdfPlot <- matrix(0, length(bins), 2)
  #   r<-1
  #
  #   for(b in 1:length(bins)){
  #     if(freq[b] != 0){
  #       size[b] <- (logbase^(b +0.5)- logbase^(b-0.5)) * disp0  # when using log scale we need log-sizes bin widths
  #       ratio[b] <- (MeanDisp*(freq[b]/size[b])) / length(disp) #Normalized log
  #       mybins[b] <- (disp0*logbase^(b))/MeanDisp #Normalized log bins
  #       pdfPlot[r,] <- c(ratio[b], mybins[b]) # stores the values for each bin #log and all normalized bins
  #       r <- r + 1
  #     }
  #   }
  #
  #   if (d == 1){
  #     plot(pdfPlot[1:r-1,2], pdfPlot[1:r-1,1], log = "xy", xlim = c(min(pdfPlot[1:r-1,2]),max(pdfPlot[1:r-1,2])),
  #          ylim =  c(min(pdfPlot[1:r-1,1]),max(pdfPlot[1:r-1,1])), ylab = "Prob(Disp)", xlab = "Normalized displacements",
  #          xaxt = "n", yaxt = "n", col = plotColours[d])
  #     myTicks = axTicks(1)
  #     axis(1, at = myTicks, labels = formatC(myTicks, digits = 0, format = 'e'))
  #     myTicks2 = axTicks(2)
  #     axis(2, at = myTicks2, labels = formatC(myTicks2, digits = 0, format = 'e'))
  #
  #   } else {
  #     par(new=T)
  #     plot(pdfPlot[1:r-1,2], pdfPlot[1:r-1,1], log="xy", xlim = c(min(pdfPlot[1:r-1,2]),max(pdfPlot[1:r-1,2])),
  #          ylim =  c(min(pdfPlot[1:r-1,1]),max(pdfPlot[1:r-1,1])), xaxt="n", yaxt="n", xlab = "",  ylab = "",
  #          col=plotColours[d], lwd=1, pch=1)
  #   }
  # }
  # if (legend[1]==TRUE){
  #   Days <- timeWindows/(24*60*60)
  #   legend(legend[2], legend = Days, lwd = 1, col = plotColours, bty = "n", pch = 16, y.intersp = 0.7, x.intersp = 0.4)
  # }
}
