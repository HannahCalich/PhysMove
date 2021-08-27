#' Plot a probability density function
#'
#' This function allows you to plot a probability density function (pdf).
#'
#' @param result Data used to create plot.
#' @param desc Description of input data. This parameter is used to determine how the data are plotted and to assign appropriate x and y plot labels.
#' Valid input options include: "Occupancy" (e.g., from the \code{\link{Occupancy}} function), "GyrationRad"  (e.g., from the \code{\link{GyrationRad}} function),
#' "Entropy" (e.g., from the \code{\link{Entropy}} function), and "Predictability" (from the \code{\link{Predictability}} function). Occupancy pdfs are based on
#' the total number of cells globally based on the provided grid cell size and are created on created on a log-log scale due to the nature of the data ranges while the other desc types are create on a standard xy plot (to plot occupancy on a standard
#' xy scale leave desc as default). Default is NULL.
#' @param nBins Number of bins used to calculate the pdf plot (e.g., nBins=25). By default, if desc="Occupancy" the code will use 20 log-sized bins (due to the
#' nature of the data ranges) else the number of bins is determined by the data ranges. If the input result values range from 0 to 1 (e.g., entropy or
#' predictability results) the code will automatically use 40 bins, and if the input results fall outside the 0 to 1 range (e.g., gyration radius results) the
#' code will automatically use 15 bins.
#' @return A pdf plot of the results and the data used to create the plot.
#' @examples pdfPlot(result)
#' @examples pdfPlot(result, desc="Occupancy", nbins=NULL)
#' @export

pdfPlot<-function(result, desc=NULL, nBins){

  if (class(result)=="data.frame"){
    stop ("A data frame has been entered. Please re-run this function and identify the column of data you want to plot following a dataframe$column structure
  or input the data as a vector")
  }

  if (is.null(desc)){
    desc <- "generic"
  }

  if (length(result)>1){ # A pdf of 1 value is not meaningful
    if (desc=="Occupancy"){ # Occupancy is separate because it is done on a log scale with log-sized bins and there are warnings about cell size being too small
      if (missing(nBins)){
        nBins <- 20
      }
      Occmin <- min(result)
      bw <- log(max(result)/Occmin)/log(nBins) # This calculation to determine bin width is usually fine but in some circumstances the value is too small when log transformed so the following loop helps determine a new bw
      if (bw<1.2){
        break_start <- 20
        repeat {
          h <- hist(log(result), breaks=break_start, plot=FALSE)
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
        b <- floor(log(result/Occmin)/log(bw) + 0.5)
        freq[b] <- freq[b] + 1
      }
      if (anyNA(freq)){
        stop ("Cell size too small to create pdf plot")
      }
      sumFreq <- sum(freq)
      freq <- c(totalCells-sumFreq, freq)
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
      plotLog<-"xy" # Restricted to occupancy because the plot is based on log-10 scales which is too large for the other data types
    } else { # If description is not "Occupancy"
      if (all(result>=0 & result<=1)){ # If values range from 0-1 (e.g., entropy or predictability scores)
        if (missing(nBins)){
          nBins <- 40
        }
        bw <- 1/nBins
        freq <- xs <- rep(0, nBins)
        len <- length(result)
        for(i in 1:len){
          b <- floor(result[i]/bw+0.5)
          freq[b] <- freq[b] +1
        }
        for (i in 1:nBins){
          xs[i] <- i*bw
        }
        for (i in 1:length(freq)){
          freq[i] <- freq[i]/(bw*len)
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
    plot.df <- plot.df[plot.df$freq !=0,] #If the bin didn't have data the freq will be 0
    plotLog <- ""
    }

    if (desc=="Predictability"){
      names(plot.df) <- c("pi^'MAX'","pdf")
      xlabel <- expression(pi^'MAX')
    }
    if (desc=="Entropy"){
      names(plot.df) <- labels <- c("'S/S'[unif]","pdf")
      xlabel <- expression('S/S'[unif])
    }
    if (desc=="GyrationRad"){
      names(plot.df) <- labels <- c("r'[G]*'(km)","pdf")
      xlabel <- expression('r'[G]*'(km)')
    }
    if (desc=="generic"){
      names(plot.df) <- labels <- c("x","pdf")
      xlabel <- expression('x')
    }
    if (plotLog=="xy"){
      if (max(log10(plot.df[,1]))-min(log10(plot.df[,1]))<1){ # if the x axis range is too narrow axis labels don't appear
        maxround <- max(plot.df[,1])*1.5
        minround <- min(plot.df[,1])*1.5
      } else {
        maxround <- max(plot.df[,1])*1.05 # 5% is ggplot standard
        minround <- min(plot.df[,1])*1.05
      }
      a <- ggplot2::ggplot(plot.df, ggplot2::aes(plot.df[,1], plot.df[,2])) +
        ggplot2::geom_line()+
        ggplot2::geom_point()+
        ggplot2::scale_x_log10(
          breaks = function(x) {
            brks <- scales::extended_breaks(Q = c(1, 5))(log10(x))
            10^(brks[brks %% 1 == 0])
          },
          labels = scales::math_format(format = log10),
          expand = c(minround, maxround)
        ) +
        ggplot2::scale_y_log10(
          breaks = function(x) {
            brks <- scales::extended_breaks(Q = c(1, 5))(log10(x))
            10^(brks[brks %% 1 == 0])
          },
          labels = scales::math_format(format = log10)
        ) +
        ggplot2::theme_bw()+ ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                            panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                                            axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), colour="black"),
                                            axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), colour="black"))+
        ggplot2::annotation_logticks(short=grid::unit(-0.1, "cm"), mid=grid::unit(-0.1, "cm"), long=grid::unit(-0.3,"cm")) +
        ggplot2::coord_cartesian(clip="off")+
        ggplot2::xlab(xlabel)+
        ggplot2::ylab("pdf")
      plot(a)
    } else {
      b <- ggplot2::ggplot(plot.df, ggplot2::aes(plot.df[,1], plot.df[,2])) +
        ggplot2::geom_line()+
        ggplot2::geom_point()+
        ggplot2::theme_bw()+ ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
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

#LOG PLOT

# major <- 10^(seq(min(as.numeric(round(log10(myTicks),1))),(max(as.numeric(round(log10(myTicks),1)))+1),1))
# labels <- sapply(seq(min(as.numeric(round(log10(myTicks),1))),(max(as.numeric(round(log10(myTicks),1)))+1),1), function(i)  as.expression(bquote(10^ .(i))))
# axis(1, at=major, labels=labels, tcl=-0.5)
#
#
# major2 <- 10^(seq(min(as.numeric(round(log10(myTicks2),1))),(max(as.numeric(round(log10(myTicks2),1)))+1),1))
# labels2 <- sapply(seq(min(as.numeric(round(log10(myTicks2),1))),(max(as.numeric(round(log10(myTicks2),1)))+1),1), function(i)  as.expression(bquote(10^ .(i))))
# axis(2, at=major2, labels=labels2, tcl=-0.5)
# minor <- outer(0:9, 10^(min(major):max(major)))
# axis(2, at=minor, tcl= -0.2, labels=NA)

# ##NOT LOG PLOT
# axis(1, at=myTicks, tcl=-0.5)
# axis(2, at=myTicks2, tcl=-0.5)
# axis(1, at=seq(min(myTicks), max(myTicks), (myTicks[2]-myTicks[1])/2), labels=NA, tcl=-0.2)
# axis(2, at=seq(min(myTicks2), max(myTicks2), (myTicks2[2]-myTicks2[1])/2), labels=NA, tcl=-0.2)
