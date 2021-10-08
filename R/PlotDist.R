#' Plot best-fit distributions to complementary cumulative distribution function (CCDF) of displacements
#'
#' This function allows you to plot a complementary cumulative distribution function (CCDF) of displacements with fit lines
#' based on the displacements output from the \code{\link{CalcDisp}} function and the distribution fits calculated with the
#' \code{\link{FitDist}} function.
#' @param displacements List of displacements output from the \code{\link{CalcDisp}} function.
#' @param distResults Data frame of results output from the \code{\link{FitDist}} function.
#' @param fitLines Add fit lines based on the parameters calculated with the \code{\link{FitDist}} function. Default is TRUE.
#' @param setDist Plot a subset of lines for each distribution fit calculated with the \code{\link{FitDist}} function (e.g., setDist=c("pl","exp"))
#' Options include "pl", "exp", and "lnorm". The lines will be drawn in order from "pl", then "exp", then "lnrom" (when applicable).
#' By default all lines are plotted. Default is NULL.
#' @param colours Colours for each fit line. The colours correspond to the drawing order: "pl", "exp", "lnorm" (when applicable).
#' Valid input options include colour names or hex numbers. Default is colours=c("red","gold2","blue").
#' @param legend Add legend with legend=TRUE. Default is TRUE.
#' @return Complementary cumulative distribution function (CCDF) plot of displacements with fit lines (if fitLines=TRUE).
#' @examples PlotDist(displacements, distResults)
#' @examples PlotDist(displacements, distResults, fitLines=TRUE, setDist=NULL, colours=c("red","gold2","blue"))
#' @export

PlotDist <- function(displacements, distResults, fitLines=TRUE, setDist=NULL, colours=c("red","gold2","blue"), legend=TRUE){

  if (exists("displacements")==FALSE){
    stop("Please calculate displacements using CalcDisp and fit distriubtions using FitDisp prior to executing PlotDist")
  }

  if (exists("distResults")==FALSE){
    stop("Please fit distributions using the FitDist function prior to executing PlotDist")
  }

  if (is.null(setDist)){
    setDist <- distResults$distribution # Use all distributions used in FitDist
  } else {
    setDist <- as.vector(sort(factor(setDist, ordered=TRUE, levels=c("lnorm","exp","pl")))) # Order and sort the distribution names to make sure the legend is accurate
  }

  # to make plot colours match dist
  z <- 1
  if("lnorm" %in% setDist){
    plotCol <- colours[z]
    z <- 2
  } else {
    plotCol <- NA
  }

  if("exp" %in% setDist){
    plotCol <- c(plotCol,colours[z])
    z <- 3
  } else {
    plotCol <- c(plotCol,NA)
  }

  if("pl" %in% setDist){
    plotCol <- c(plotCol,colours[z])
  } else {
    plotCol <- c(plotCol,NA)
  }

  if (normalise){
    x <- list()
    for (d in 1:length(displacements)){
      disp <- unlist(displacements[d])
      x[[d]] <- disp/mean(disp)
    }
    x <- unlist(x)
    xlabel <- "Normalised displacements"
  } else {
    x <- unlist(displacements)
    xlabel <- "Displacements (km)"
  }

  x <- sort(x)
  n <- length(x)
  ccdf <- 1-((0:(n - 1))/n)
  df <- data.frame(x=sort(x), y=ccdf)

  a <- ggplot2::ggplot(df, ggplot2::aes(df[,1], df[,2])) +
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
      labels = scales::math_format(format = log10),
    ) +
    ggplot2::theme_bw(base_size = 18)+
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                        panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                                        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), colour="black"),
                                        axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), colour="black"),
                                        legend.title = ggplot2::element_blank())+
    ggplot2::annotation_logticks(short=grid::unit(-0.1, "cm"), mid=grid::unit(-0.1, "cm"), long=grid::unit(-0.3,"cm")) +
    ggplot2::coord_cartesian(clip="off")+
    ggplot2::xlab(xlabel)+
    ggplot2::ylab("CCDF")

  if (fitLines==TRUE){
    if ("lnorm" %in% setDist){
      MyLogNormalPDF <- function(parameters, displacements){ # 1=mu, 2= sigma
        LN_PDF = exp((plnorm(displacements, parameters[1], parameters[2], lower.tail=FALSE, log = TRUE)) -
                       (plnorm(parameters[3],parameters[1], parameters[2], lower.tail = FALSE, log.p = TRUE)))
        return(LN_PDF)
      }
      LN_xmin <- distResults[which(distResults$distribution=="lnorm"),"xmin"]
      LN_mu <- distResults[which(distResults$distribution=="lnorm"),"parameter1"]
      LN_sigma <- distResults[which(distResults$distribution=="lnorm"),"parameter2"]
      xval <- exp(seq(log(LN_xmin), log(max(x)), length.out = 100)) # log spaced sequence of displacements for log-log plot
      yval <- MyLogNormalPDF(c(LN_mu, LN_sigma, LN_xmin), xval)
      yval[xval < LN_xmin] = 0
      dif <- x - LN_xmin
      upper <- which(dif >= 0)[1]
      lower <- max(upper - 1, 1)
      x_dif <- x[lower] - x[upper]
      y_dif <- ccdf[lower] - ccdf[upper]
      scale <- ccdf[lower] + y_dif * (LN_xmin - x[lower])/x_dif
      if (is.nan(scale)){
        scale <- 1
      }
      yval <- yval * scale
      lnormLine <- as.data.frame(cbind(xval,yval))
      if (lnormLine[1,2] == 0){ #If the first yval == 0, omit it from plot
        lnormLine <- lnormLine[-1,]
      }
      a <- a +
        ggplot2::geom_line(data=lnormLine, ggplot2::aes(x=xval, y=yval, colour=plotCol[3]),lwd=1)
    }
    if ("exp" %in% setDist){
      MyExponentialPDF<- function(parameters, displacements){
        Exp_CDF = exp(pexp(displacements, parameters[1], lower.tail = FALSE, log.p = TRUE) - pexp(parameters[2], parameters[1], lower.tail = FALSE, log.p = TRUE))
        # Exp_CDF = parameters*exp(-parameters*displacements)
        return(Exp_CDF)
      }
      Exp_xmin <- distResults[which(distResults$distribution=="exp"),"xmin"]
      Exp_lambda <- distResults[which(distResults$distribution=="exp"),"parameter1"]
      xval <- exp(seq(log(Exp_xmin), log(max(x)), length.out = 100)) # log spaced sequence of displacements for log-log plot
      yval <- MyExponentialPDF(c(Exp_lambda, Exp_xmin), xval)
      yval[xval < Exp_xmin] = 0
      dif <- x - Exp_xmin
      upper <- which(dif >= 0)[1]
      lower <- max(upper - 1, 1)
      x_dif <- x[lower] - x[upper]
      y_dif <- ccdf[lower] - ccdf[upper]
      scale <- ccdf[lower] + y_dif * (Exp_xmin - x[lower])/x_dif
      if (is.nan(scale)){
        scale <- 1
      }
      yval <- yval * scale
      expLine <- as.data.frame(cbind(xval,yval))
      if (expLine[1,2] == 0){ #If the first yval == 0, omit it from plot.
        expLine <- expLine[-1,]
      }
      a <- a +
        ggplot2::geom_line(data=expLine, ggplot2::aes(x=xval, y=yval, colour=plotCol[2]),lwd=1)
    }
    if ("pl" %in% setDist){
      MyPowerLawCDF <- function(parameters, displacements){
        PL_CDF  = 1 - (displacements/parameters[2])^(-parameters[1]+1)
        return(PL_CDF)
      }
      PL_xmin <- distResults[which(distResults$distribution=="pl"),"xmin"]
      PL_alpha <- distResults[which(distResults$distribution=="pl"),"parameter1"]
      xval <- exp(seq(log(PL_xmin), log(max(x)), length.out = 100)) # log spaced sequence of displacements for log-log plot
      yval <- 1- MyPowerLawCDF(c(PL_alpha, PL_xmin), xval)
      yval[xval < round(PL_xmin)] = 0
      dif <- x - PL_xmin
      upper <- which(dif >= 0)[1]
      lower <- max(upper - 1, 1)
      x_dif <- x[lower] - x[upper]
      y_dif <- ccdf[lower] - ccdf[upper]
      scale <- ccdf[lower] + y_dif * (PL_xmin - x[lower])/x_dif
      if (is.nan(scale)){
        scale <- 1
      }
      yval <- yval * scale
      plLine <- as.data.frame(cbind(xval,yval))
      if (plLine[1,2] == 0){ #If the first yval == 0, omit it from plot.
        plLine <- plLine[-1,]
      }
      a <- a +
        ggplot2::geom_line(data=plLine, ggplot2::aes(x=xval,y=yval, colour=plotCol[1]),lwd=1)
    }
    a <- a + ggplot2:: scale_color_identity(breaks=na.omit(plotCol), labels=setDist, guide="legend")
    if(legend==FALSE){
      a <- a + ggplot2::theme(legend.position = "none")
    }
  }
  plot(a)
  return(df)
}

