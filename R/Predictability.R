#' Predictability of trajectories
#'
#' This function allows you to calculate the limit of predictability for each trajectory based on each individual's entropy. This function requires
#' 'indivEntropy', 'occurrences', and 'normEnt' from the \code{\link{Entropy}} function.
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{plSample}}, \code{\link{expSample}}, or \code{\link{lnormSample}}.
#' @param normEnt Normalized entropy values for each trajectory from the \code{\link{Entropy}} function.
#' @param startVal Starting value used to find a root for the limit of predictability equation. Function will loop through values
#' starting at startVal and decrease by 0.01 at each iteration until an acceptable root value is identified. Default is 0.99
#' @param histPlot Plot a histogram of the limit of predictability scores. Default is TRUE.
#' @param legend Add a legend to the histPlot when TRUE and change the position of the legend. Default is legend=c(TRUE, "topleft")
#' @param pdfPlot Create a probability density function line plot of the limit of predictability scores. Default is TRUE.
#' @param nBins Number of bins used to calculate the pdf plot. Default is 40.
#' @return Limit of predictability values for each trajectory. If histPlot=TRUE a histogram of the limit of predictability scores is created. If pdfPlot=TRUE, a
#' probability density function line plot of results is created and the data used to create the pdf plot are automatically assigned to the global environment
#' ('predictPDFData').
#' @examples
#' Predictability(expSample, normEnt)
#' Predictability(expSample, startVal=0.99, histPlot=TRUE, legend=c(TRUE, "topleft"), pdfPlot=FALSE, nBins=40)
#' @export

Predictability<-function(species_df, entResults, startVal=0.99, histPlot=TRUE, legend=c(TRUE, "topleft"), pdfPlot=FALSE, nBins=40){

  if (nrow(entResults)!=length(unique(species_df$ref))){
    stop("The number of individuals in the species_df does not match the number of normalized entropy values, check to ensure the right data has \n  been entered.")
  }

  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  Predictability <- c()

  for (i in 1:length(species_index) ){
    model <- function(x) c(F1 = x*log(x) + (1-x)*log(1-x) - (1-x)*log(entResults$cellsVisited[i]-1) + entResults$indivEntropy[i])

    if(startVal==0.99){
      ss <- suppressWarnings(rootSolve::multiroot(f = model, start = 1-entResults$normalizedEntropy[i]))
      if (ss$root > 0 & ss$root < 1){
        Predictability[i] <- ss$root
    } else {
        start_value_default <- 0.99
        repeat{
          ss <- suppressWarnings(rootSolve::multiroot(f = model, start = start_value_default))
          if (ss$root > 0 & ss$root < 1){
            break
          }
          else {
            startVal<-startVal-0.01
          }
        }
        Predictability[i] <- ss$root
      }
    }
    else if (startVal!=0.99) {
      ss <- rootSolve::multiroot(f = model, start = startVal)
      Predictability[i] <- ss$root
    }
  }

  if (histPlot==TRUE){
    hist <- hist(Predictability, breaks=seq(0, 1, length.out = 21), plot=FALSE) #Determine hist values so you can automate plot better
    hist <- hist(Predictability, main="", xlab="Predictability", xlim=c(0,1), ylim=c(0,(max(hist$counts)+2)), axes=FALSE,
               breaks=seq(0, 1, length.out=21), border="black", col= "grey", xaxs="i", yaxs="i")
    segments(x0=0.5, y0=0, x1=0.5, y1=0.9*(max(hist$counts)+2) ,col="red", lty=2, lwd =2)
    myTicks <- axTicks(1)
    myTicks2 <- axTicks(2)
    axis(1, at=myTicks)
    axis(1, at=seq(min(myTicks), max(myTicks), (myTicks[2]-myTicks[1])/2), labels=NA, tcl=-0.2)
    axis(2, at = myTicks2)
    axis(2, at=seq(min(myTicks2), max(myTicks2), (myTicks2[2]-myTicks2[1])/2), labels=NA, tcl=-0.2)
    if (legend[1]==TRUE){
      legend(legend[2], bty="n", c("Predictability = 0.5"), lty=2, lwd=2, col="red")
    }
  }

  if (pdfPlot==TRUE){
    if (length(unique(species_df$ref))>1){
      bw <- 1/nBins
      freq <- xs <- rep(0, nBins)
      nind <- length(Predictability)
      for(i in 1:nind){
        b <- floor(Predictability[i]/bw+0.5)
        freq[b] <- freq[b] +1
      }
      for (i in 1:nBins){
        xs[i] <- i*bw
      }
      for (i in 1:length(freq)){
        freq[i] <- freq[i]/(bw*nind)
      }
      predictplot <- data.frame(xs, freq)
      plot(predictplot$xs, predictplot$freq, type="l", col="black", ylab="pdf",xlab=expression(pi^'MAX'))
      points(predictplot$xs, predictplot$freq, col="black", pch=19)
      myTicks <- axTicks(1)
      myTicks2 <- axTicks(2)
      axis(1, at=myTicks, tcl=-0.5)
      axis(1, at=seq(min(myTicks), max(myTicks), (myTicks[2]-myTicks[1])/2), labels=NA, tcl=-0.2)
      axis(2, at = myTicks2)
      axis(2, at=seq(min(myTicks2), max(myTicks2), (myTicks2[2]-myTicks2[1])/2), labels=NA, tcl=-0.2)

      names(predictplot)=c("piMAX","pdf")
      assign("predictPDFplot", predictplot, envir = .GlobalEnv)
    } else {
      warning("Cannot create pdf plot with data from only 1 individual")
    }
  }
  return(Predictability)
}
