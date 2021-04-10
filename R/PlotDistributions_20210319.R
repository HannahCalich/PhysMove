#' Plot best-fit distributions to CCDF of displacements
#'
#' This function allows you to create a complementary cumulative distribution function (CCDF) plot with fit lines (if  desired) for the distribution fits calculated with the  \code{\link{FitDist}} function.
#' normalizes all of the displacements by dividing each displacement by the mean displacement from it's corresponding time period and combines the results into a vector of Normalized values.
#' @param Displacements Input the output from \code{\link{CalculateDisplacements}} function.
#' @param lines Add fit lines based on parameters calculated with \code{\link{FitDist}} function. Default is TRUE.
#' @param colours Colours for each fit line. Default includes: red (#D55E00), yellow (#DDCC77), and blue (#0072B2).
#' @param legend Adds legend to plot and specifies legend location. Default is c(TRUE, "bottomleft").
#' @return Complementary cumulative distribution function (CCDF) plot with fit lines (if  desired).
#' @examples PlotDist(Displacements, Lines=TRUE, colours=c("#D55E00", "#DDCC77", "#0072B2"))
#' @examples PlotDist(Displacements)
#' @export

PlotDist <- function (Displacements, lines=TRUE, colours=c("#D55E00", "#DDCC77", "#0072B2"), legend=c(TRUE, "bottomleft")){

  if (exists("Displacements")==FALSE){
    stop("Please Calculate Displacements using (CalcDisp) and fit distriubtions (FitDisp) prior to executing PlotDist")
  }

  if (exists("DistResults")==FALSE){
    stop("Please Fit Distributions using the FitDist function prior to executing PlotDist")
  }

  if (Normalize){
    x <- list()
    for (d in 1:length(TimeWindows)){
      disp <- unlist(Displacements[d])
      x[[d]] <- disp/mean(disp)
    }
    xlabel<-"Normalized Displacements"
  } else {
    x <- unlist(Displacements)
    xlabel<-"Displacements (km)"
  }

  x <- round(unlist(x), digits=8) # To limit discrepancies with floating numbers
  x <- sort(x)
  n <- length(x)
  ccdf <- 1-((0:(n - 1))/n)
  df <- data.frame(x=sort(x), y=ccdf)
  plot(df, log="xy", ylab="CCDF", xlab=xlabel, xaxt="n", yaxt="n", col="black") #works same as powerlaw
  myTicks = axTicks(1)
  axis(1, at = myTicks, labels = formatC(myTicks, digits = 0, format = 'e'))
  myTicks2 = axTicks(2)
  axis(2, at = myTicks2, labels = formatC(myTicks2, digits = 0, format = 'e'))

  if (lines==TRUE){
    legendcols<-c()
    if ("pl" %in% dist){
      MyPowerLawCDF <- function(parameters, Displacements){
        PL_CDF  = 1 - (Displacements/parameters[2])^(-parameters[1]+1)
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
    if ("exp" %in% dist){
      MyExponentialPDF<- function(parameters, Displacements){
        Exp_CDF = exp(pexp(Displacements, parameters[1], lower.tail = FALSE, log.p = TRUE) - pexp(parameters[2], parameters[1], lower.tail = FALSE, log.p = TRUE))
        # Exp_CDF = parameters*exp(-parameters*Displacements)
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
    if ("lnorm" %in% dist){
      MyLogNormalPDF <- function(parameters, Displacements){ # 1=mu, 2= sigma
        LN_PDF = exp((plnorm(Displacements, parameters[1], parameters[2], lower.tail=FALSE, log = TRUE)) -
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
        legend(legend[2], legend=dist, lwd=1 ,col=legendcols,bty="n")
    }
  }
}
