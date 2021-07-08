#' Plot best-fit distributions to complementary cumulative distribution function (CCDF) of displacements
#'
#' This function allows you to plot a complementary cumulative distribution function (CCDF) of displacements with fit lines
#' based on the distribution fits calculated with the \code{\link{FitDist}} function.
#' @param displacements The output from the \code{\link{CalcDisp}} function.
#' @param fitLines Add fit lines based on the parameters calculated with the \code{\link{FitDist}} function. Default is TRUE.
#' @param colours Colours for each fit line. Default is colours=c("red","gold2","blue").
#' @param setDist Plot  a subset of the distribution fit lines calculated with the \code{\link{FitDist}} function. To plot a specific
#' distribution fit line use setDist="pl", or to plot multiple distributions use setDist=c("pl","exp"). Options include "pl", "exp", and "lnorm".
#' Default is NULL
#' @param legend Add a legend to the plot when TRUE and change the position of the legend. Default is legend=c(TRUE, "bottomleft")
#' @return Complementary cumulative distribution function (CCDF) plot of displacements with fit lines (if fitLines=TRUE).
#' @examples PlotDist(displacements, fitLines=TRUE, colours=c("red","gold2","blue"), setDist=FALSE, legend=c(TRUE, "bottomleft"))
#' @examples PlotDist(displacements)
#' @export

PlotDist <- function (displacements, fitLines=TRUE, colours=c("red","gold2","blue"), setDist=NULL, legend=c(TRUE, "bottomleft")){

  if (exists("displacements")==FALSE){
    stop("Please calculate displacements using CalcDisp and fit distriubtions using FitDisp prior to executing PlotDist")
  }

  if (exists("distResults")==FALSE){
    stop("Please fit distributions using the FitDist function prior to executing PlotDist")
  }

  if (is.null(setDist)){
    setDist=dist
  }

  if (Normalize){
    x <- list()
    for (d in 1:length(TimeWindows)){
      disp <- unlist(displacements[d])
      x[[d]] <- disp/mean(disp)
    }
    x <- unlist(x)
    xlabel <- "Normalized displacements"
  } else {
    x <- unlist(displacements)
    xlabel<-"displacements (km)"
  }

  x <- sort(x)
  n <- length(x)
  ccdf <- 1-((0:(n - 1))/n)
  df <- data.frame(x=sort(x), y=ccdf)
  plot(df, log="xy", ylab="CCDF", xlab=xlabel, xaxt="n", yaxt="n", col="black") #works same as powerlaw
  myTicks = axTicks(1)
  axis(1, at = myTicks, labels = formatC(myTicks, digits = 0, format = 'e'))
  myTicks2 = axTicks(2)
  axis(2, at = myTicks2, labels = formatC(myTicks2, digits = 0, format = 'e'))

  names(df)=c(xlabel,"CCDF")
  assign("CCDFplotData", df, envir = .GlobalEnv)

  if (fitLines==TRUE){
    legendcols<-c()
    if ("pl" %in% setDist){
      MyPowerLawCDF <- function(parameters, displacements){
        PL_CDF  = 1 - (displacements/parameters[2])^(-parameters[1]+1)
        return(PL_CDF)
      }
      PL_xmin <- DistResults[which(DistResults$Distribution=="pl"),"xmin"]
      PL_alpha <- DistResults[which(DistResults$Distribution=="pl"),"Parameter1"]
      xval<-exp(seq(log(PL_xmin), log(max(x)), length.out = 100)) #log spaced sequence of displacements for log-log plot, matches poweRlaw x val
      yval <- 1- MyPowerLawCDF(c(PL_alpha, PL_xmin), xval)
      yval[xval < round(PL_xmin)] = 0
      dif = x - PL_xmin #PLAGARIZED FROM HERE TO SCALE STEP
      upper = which(dif >= 0)[1]
      lower = max(upper - 1, 1)
      x_dif = x[lower] - x[upper]
      y_dif = ccdf[lower] - ccdf[upper]
      scale = ccdf[lower] + y_dif * (PL_xmin - x[lower])/x_dif
      if (is.nan(scale)){
        scale = 1
        }
      yval = yval * scale
      lines(xval, yval, col=colours[1],lwd=2)
      legendcols<-c(legendcols, colours[1])
    }
    if ("exp" %in% setDist){
      MyExponentialPDF<- function(parameters, displacements){
        Exp_CDF = exp(pexp(displacements, parameters[1], lower.tail = FALSE, log.p = TRUE) - pexp(parameters[2], parameters[1], lower.tail = FALSE, log.p = TRUE))
        # Exp_CDF = parameters*exp(-parameters*displacements)
        return(Exp_CDF)
      }
      Exp_xmin <- DistResults[which(DistResults$Distribution=="exp"),"xmin"]
      Exp_lambda <- DistResults[which(DistResults$Distribution=="exp"),"Parameter1"]
      xval<-exp(seq(log(Exp_xmin), log(max(x)), length.out = 100)) #log spaced sequence of displacements for log-log plot, matches poweRlaw x val
      yval <- MyExponentialPDF(c(Exp_lambda, Exp_xmin), xval) # This works rounded, might not need to change
      yval[xval < Exp_xmin] = 0
      dif = x - Exp_xmin #PLAGARIZED FROM HERE TO SCALE STEP
      upper = which(dif >= 0)[1]
      lower = max(upper - 1, 1)
      x_dif = x[lower] - x[upper]
      y_dif = ccdf[lower] - ccdf[upper]
      scale = ccdf[lower] + y_dif * (Exp_xmin - x[lower])/x_dif
      if (is.nan(scale)){
        scale = 1
      }
      yval = yval * scale
      lines(xval, yval, col=colours[2],lwd=2)
      legendcols<-c(legendcols, colours[2])
    }
    if ("lnorm" %in% setDist){
      MyLogNormalPDF <- function(parameters, displacements){ # 1=mu, 2= sigma
        LN_PDF = exp((plnorm(displacements, parameters[1], parameters[2], lower.tail=FALSE, log = TRUE)) -
        (plnorm(parameters[3],parameters[1], parameters[2], lower.tail = FALSE, log.p = TRUE)))
         return(LN_PDF)
      }
      LN_xmin <- DistResults[which(DistResults$Distribution=="lnorm"),"xmin"]
      LN_mu <- DistResults[which(DistResults$Distribution=="lnorm"),"Parameter1"]
      LN_sigma <- DistResults[which(DistResults$Distribution=="lnorm"),"Parameter2"]
      xval<-exp(seq(log(LN_xmin), log(max(x)), length.out = 100)) #log spaced sequence of displacements for log-log plot, matches poweRlaw x val
      yval <- MyLogNormalPDF(c(LN_mu, LN_sigma, LN_xmin), xval)
      yval[xval < LN_xmin] = 0
      dif = x - LN_xmin #PLAGARIZED FROM HERE TO SCALE STEP
      upper = which(dif >= 0)[1]
      lower = max(upper - 1, 1)
      x_dif = x[lower] - x[upper]
      y_dif = ccdf[lower] - ccdf[upper]
      scale = ccdf[lower] + y_dif * (LN_xmin - x[lower])/x_dif
      if (is.nan(scale)){
        scale = 1
      }
      yval = yval * scale
      lines(xval, yval, col=colours[3],lwd=2)
      legendcols<-c(legendcols, colours[3])
      }
    if (legend[1]==TRUE){
        legend(legend[2], legend=setDist, lwd=1, col=legendcols, bty="n", y.intersp=0.75)
    }
  }
}
