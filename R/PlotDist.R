#' Plot best-fit distributions to complementary cumulative distribution function (CCDF) of displacements
#'
#' This function allows you to plot a complementary cumulative distribution function (CCDF) of displacements with fit lines
#' based on the displacements output from the \code{\link{CalcDisp}} function and the distribution fits calculated with the
#' \code{\link{FitDist}} function.
#' @param displacements List of displacements output from the \code{\link{CalcDisp}} function.
#' @param distResults Data frame of results output from the \code{\link{FitDist}} function.
#' @param fitLines Add fit lines based on the parameters calculated with the \code{\link{FitDist}} function. Default is TRUE.
#' @param colours Colours for each fit line. Valid input options include colour names or hex numbers. Default is colours=c("red","gold2","blue").
#' @param setDist Plot a subset of lines for each distribution fit calculated with the \code{\link{FitDist}} function (e.g., setDist=c("pl","exp"))
#' Options include "pl", "exp", and "lnorm". By default all fit lines are plotted. Default is NULL
#' @param legend Add a legend to the plot when TRUE and change the position of the legend. Default is legend=c(TRUE, "bottomleft")
#' @return Complementary cumulative distribution function (CCDF) plot of displacements with fit lines (if fitLines=TRUE).
#' @examples PlotDist(displacements,distResults)
#' @examples PlotDist(displacements, distResults, fitLines=TRUE, colours=c("red","gold2","blue"), setDist=FALSE, legend=c(TRUE, "bottomleft"))
#' @export

PlotDist <- function(displacements, distResults, fitLines=TRUE, colours=c("red","gold2","blue"), setDist=NULL, legend=c(TRUE, "bottomleft")){

  if (exists("displacements")==FALSE){
    stop("Please calculate displacements using CalcDisp and fit distriubtions using FitDisp prior to executing PlotDist")
  }

  if (exists("distResults")==FALSE){
    stop("Please fit distributions using the FitDist function prior to executing PlotDist")
  }

  if (is.null(setDist)){
    setDist <- distResults$distribution # Use all distributions used in FitDist
  }

  if (normalize){
    x <- list()
    for (d in 1:length(displacements)){
      disp <- unlist(displacements[d])
      x[[d]] <- disp/mean(disp)
    }
    x <- unlist(x)
    xlabel <- "Normalized displacements"
  } else {
    x <- unlist(displacements)
    xlabel <- "displacements (km)"
  }

  x <- sort(x)
  n <- length(x)
  ccdf <- 1-((0:(n - 1))/n)
  df <- data.frame(x=sort(x), y=ccdf)
  plot(df, log="xy", ylab="CCDF", xlab=xlabel, xaxt="n", yaxt="n", col="black")
  myTicks <- axTicks(1)
  axis(1, at=myTicks, labels=formatC(myTicks, digits=0, format='e'))
  myTicks2 <- axTicks(2)
  axis(2, at=myTicks2, labels=formatC(myTicks2, digits=0, format='e'))
  names(df) <- c(xlabel,"CCDF")

  if (fitLines==TRUE){
    legendcols <- c()
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
      lines(xval, yval, col=colours[1],lwd=2)
      legendcols <- c(legendcols, colours[1])
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
      lines(xval, yval, col=colours[2],lwd=2)
      legendcols <- c(legendcols, colours[2])
    }
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
      lines(xval, yval, col=colours[3],lwd=2)
      legendcols <- c(legendcols, colours[3])
      }
    if (legend[1]==TRUE){
        legend(legend[2], legend=setDist, lwd=1, col=legendcols, bty="n", y.intersp=0.75)
    }
  }
  return(df)
}

