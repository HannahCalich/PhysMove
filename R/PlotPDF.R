#' Plot a probability density function
#'
#' This function allows you to plot a probability density function (pdf).
#'
#' @param result Data used to create plot.
#' @param desc Description of input data. This parameter is used to determine how the data are plotted and to assign appropriate x and y plot labels.
#' Valid input options include: "occupancy" (e.g., from the \code{\link{Occupancy}} function), "gyrationRad"  (e.g., from the \code{\link{GyrationRad}} function),
#' "entropy" (e.g., from the \code{\link{Entropy}} function), and "predictability" (from the \code{\link{Predictability}} function). Occupancy pdfs are based on
#' the total number of cells globally based on the provided grid cell size and are created on created on a log-log scale due to the nature of the data ranges while the other desc types are create on a standard xy plot (to plot occupancy on a standard
#' xy scale leave desc as default). Default is NULL.
#' @param nBins Number of bins used to calculate the pdf plot (e.g., nBins=25). By default, if desc="Occupancy" the code will use 20 log-sized bins (due to the
#' nature of the data ranges) else the number of bins is determined by the data ranges. If the input result values range from 0 to 1 (e.g., entropy or
#' predictability results) the code will automatically use 40 bins, and if the input results fall outside the 0 to 1 range (e.g., gyration radius results) the
#' code will automatically use 15 bins.
#' @return A pdf plot of the results and the data used to create the plot.
#' @importFrom rlang .data
#' @examples plotPDF(occupancyResults$Occupancy, desc="occupancy")
#' @export

plotPDF <- function(result, desc=NULL, nBins){

  if ("data.frame" %in% is(result)){
    stop ("A data frame has been entered. Please re-run this function and identify the column of data you want to plot following a dataframe$column structure
  or input the data as a vector")
  }

  if (is.null(desc)){
    desc <- "generic"
  }

  if (length(result)>1){ # A pdf of 1 value is not meaningful
    if (desc=="occupancy"){ # occupancy is separate because it is done on a log scale with log-sized bins and there are warnings about cell size being too small
      if (missing(nBins)){
        nBins <- 20
      }
      Occmin <- min(result)
      bw <- log(max(result)/Occmin)/log(nBins) # This calculation to determine bin width is usually fine but in some circumstances the value is too small when log transformed so the following loop helps determine a new bw
      if (bw<1.2){
        break_start <- 20
        repeat {
          h <- graphics::hist(log(result), breaks=break_start, plot=FALSE)
          bw <- exp(h$breaks[2]-h$breaks[1])
          if (bw>=1.2){
            break
          } else {
            break_start <- break_start - 1
          }
        }
      }
      freq <- rep(0, nBins+1)
      for(i in 1:length(result)){
        if (result[i] > 0){
          b <- floor(log(result[i]/Occmin)/log(bw) + 0.5)
          freq[b+1] <- freq[b+1] + 1 # the b+1 is necessary due to scenarios where b=0 above
        }
      }
      if (anyNA(freq)){
        stop ("Cell size too small to create pdf plot")
      }
      norm <- sum(freq)
      freq <- freq[freq>0]
      xs <- ys <- rep(0, length(freq))

      for (i in 1:length(freq)){
          ys[i] <- freq[i]/(norm*Occmin*((bw^((i-1)+0.5))-(bw^((i-1)-0.5))))
          xs[i] <- Occmin*(bw^(i-1))
      }
      plot.df <- data.frame(xs, ys)
      names(plot.df) <- labels <- c('Occupancy(km^-2)',"pdf")
      xlabel <- expression('Occupancy (km'^'-2'*')')
      plotLog <-"xy" # Restricted to occupancy because the plot is based on log-10 scales which is too large for the other data types

    } else { # If description is not "Occupancy"
      if (all(result>=0 & result<=1)){ # If values range from 0-1 (e.g., entropy or predictability scores)
        if (missing(nBins)){
          nBins <- 40
        }
        bw <- 1/nBins
        freq <- xs <- rep(0, nBins+1)
        len <- length(result)
        for(i in 1:len){
          b <- floor(result[i]/bw+0.5)
          freq[b+1] <- freq[b+1] +1 # the b+1 is necessary due to scenarios where b=0 above
        }
        for (i in 1:nBins){
          xs[i+1] <- i*bw #the +1 ensures the 1st value is 0
        }

        for (i in 1:length(freq)){
          freq[i] <- freq[i]/(bw*len) # normalizing
        }
      } else { # If values fall outside the 0-1 range they need to be normalized (e.g., gyration radius scores)
        if (missing(nBins)){
          nBins <- 15
        }
        minval <- min(result)
        bw <- (max(result)-minval)/nBins
        freq <- xs <- rep(0, nBins+1)
        len <- length(result)
        for(i in 1:len){
          b <- floor((result[i]-minval)/bw + 0.5)+1 # +1 ensures the minimum b value is 1
          freq[b] <- freq[b] + 1
        }
        for(i in 1:(nBins+1)){
          xs[i] <- minval+(i-1)*bw
        }
        for (i in 1:length(freq)){
          freq[i] <- freq[i]/(bw*len)
        }
      }
    plot.df <- data.frame(xs, freq)
    plotLog <- ""
    }

    if (desc=="predictability"){
      names(plot.df) <- c("Limit of Predictability","pdf")
      xlabel <- expression('Limit of Predictability')
    }
    if (desc=="entropy"){
      names(plot.df) <- labels <- c("Normalised Entropy","pdf")
      xlabel <- expression('Normalised Entropy')
    }
    if (desc=="gyrationRad"){
      names(plot.df) <- labels <- c("Gyration Radius (km)","pdf")
      xlabel <- expression('Gyration Radius (km)')
    }
    if (desc=="generic"){
      names(plot.df) <- labels <- c("x","pdf")
      xlabel <- expression('x')
    }
    if (plotLog=="xy"){
      a <- ggplot2::ggplot(data=plot.df, ggplot2::aes(plot.df[,1], plot.df[,2])) +
        ggplot2::geom_line()+
        ggplot2::geom_point()+
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
          labels = scales::math_format(format = log10)
        ) +
        ggplot2::theme_bw(base_size=12)+
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                            panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                                            axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), colour="black"),
                                            axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), colour="black"))+
        ggplot2::annotation_logticks(short=grid::unit(-0.1, "cm"), mid=grid::unit(-0.1, "cm"), long=grid::unit(-0.3,"cm")) +
        ggplot2::coord_cartesian(clip="off")+
        ggplot2::xlab(xlabel)+
        ggplot2::ylab("pdf")
      plot(a)
    } else {
      b <- ggplot2::ggplot(data=plot.df, ggplot2::aes(plot.df[,1], plot.df[,2])) +
        ggplot2::geom_line()+
        ggplot2::geom_point()+
        ggplot2::theme_bw(base_size=12)+
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                            panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                                            axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), colour="black"),
                                            axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), colour="black"))+
        ggplot2::xlab(xlabel)+
        ggplot2::ylab("pdf")
      plot(b)
    }
  } else {
    warning("Cannot create pdf plot with only 1 data point")
  }
  return(plot.df)
}
