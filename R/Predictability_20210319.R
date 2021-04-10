#' Predictability
#'
#' This function allows you to calculate the predictability of each trajectory based on each individual's entropy. This function requires "Ind_Entropy","Occurrences", and "NormalizedEntropy" from \code{\link{Entropy}} function
#' @param species_df A data frame containing location data (rows) and columns with the following headers: "ref", "lon", "lat", "day". "ref" is the unique id number for each animal
#'      (e.g., their satellite tag number; integer format). "lon" and "lat" are the longitude and latitide of each position estimate in decimal degrees (numeric format). "day"
#'      is the datetime stamp for each location estimate (POSIXct format following yyyy-mm-dd hh:mm:ss). See XXX data frame for an example.
#' @param start_val Starting value used to find a root for the Limit of Predictability equation. If no value is provided function will loop through values starting at 0.99 and decreasing by 0.01 at each iteration until acceptable root values are identified. Default is 0.99
#' @param plot Line plot illustrating the predictability of each trajectory.  Default is TRUE.
#' @param nb Number of bins, used to determine the size of the bins used to calculate predictability frequencies for plots. Default is 40.
#' @return Predictability vector containing predictability values for each trajectory, line plot (if desired).
#' @examples
#' Predictability(species_df)
#' Predictability(species_df, start_val=0.99, plot=TRUE, nb=40)
#' @export


Predictability<-function(species_df, start_val=0.99, hist = TRUE, pdfplot=FALSE, nb=40, legend=c(TRUE, "topleft")){

  exists.entropy <- function(x) {
    all(sapply(x, exists))
  }

  if (exists.entropy(c("Ind_Entropy","Occurrences", "NormalizedEntropy"))){
    species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
    Predictability <- c()

    for (i in 1:length(species_index) ){
      CellsVisited <- length(which(Occurrences[[i]]!=0))
      model <- function(x) c(F1 = x*log(x) + (1-x)*log(1-x) - (1-x)*log(CellsVisited-1) + Ind_Entropy[i])

      if(start_val==0.99){
        ss <- suppressWarnings(rootSolve::multiroot(f = model, start = 1-NormalizedEntropy[i]))
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
              start_val<-start_val-0.01
            }
          }
          Predictability[i] <- ss$root
        }
      }
      else if (start_val!=0.99) {
        ss <- rootSolve::multiroot(f = model, start = start_val)
        Predictability[i] <- ss$root
      }
    }

  assign("Predictability",Predictability, envir = .GlobalEnv)

  if (hist==TRUE){
    hist<-hist(Predictability, breaks = seq(0, 1, length.out = 21), plot=FALSE) #Determine hist values so you can automate plot better
    hist<-hist(Predictability, main="", xlab = "Predictability", xlim=c(0,1), ylim=c(0,(max(hist$counts)+2)),axes=FALSE,
               breaks = seq(0, 1, length.out = 21), border="black", col= "grey",xaxs="i",yaxs="i")
    abline(v=0.5, col="red", lty=2, lwd =2)
    myTicks = axTicks(1)
    myTicks2 = axTicks(2)
    axis(1, at=myTicks)
    axis(1, at=seq(min(myTicks), max(myTicks), (myTicks[2]-myTicks[1])/2), labels=NA, tcl=-0.2)
    axis(2, at = myTicks2)
    axis(2, at=seq(min(myTicks2), max(myTicks2), (myTicks2[2]-myTicks2[1])/2), labels=NA, tcl=-0.2)
    if (legend[1]==TRUE){
      legend(legend[2], bty="n", c("Predictability = 0.5"), lty=2, lwd=2, col="red")
    }
  }

  if (pdfplot==TRUE){
    bw <- 1/nb
    freq <- xs <- rep(0, nb)
    nind <- length(Predictability)
    for(i in 1:nind){
      b <- floor(Predictability[i]/bw+0.5)
      freq[b] <- freq[b] +1
    }
    for (i in 1:nb){
      xs[i] <- i*bw
    }
    for (i in 1:length(freq)){
      freq[i] <- freq[i]/(bw*nind)
    }
    predictplot<-data.frame(xs, freq)
    #predictplot<-predictplot[which(predictplot$freq != 0),]

    plot(predictplot$xs,predictplot$freq, type="l", col="black", ylab="pdf",xlab=expression(pi^'MAX'))
    points(predictplot$xs,predictplot$freq,col="black", pch=19)
    #abline(v=0.5, col="red", lty=2, lwd =2)
    myTicks = axTicks(1)
    myTicks2 = axTicks(2)
    axis(1, at=myTicks, tcl=-0.5)
    axis(1, at=seq(min(myTicks), max(myTicks), (myTicks[2]-myTicks[1])/2), labels=NA, tcl=-0.2)
    axis(2, at = myTicks2)
    axis(2, at=seq(min(myTicks2), max(myTicks2), (myTicks2[2]-myTicks2[1])/2), labels=NA, tcl=-0.2)

    names(predictplot)=c("piMAX","pdf")
    assign("PredictPDFData", predictplot, envir = .GlobalEnv)

    }
  }
  else warning("Results from Entropy function required to run Predictability function, please run Entropy function first")
}
