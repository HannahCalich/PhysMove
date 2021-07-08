#' Entropy of trajectories
#'
#' This function allows you to calculate the entropy of individual trajectories, which provides insight to how ordered (or disordered) the trajectories were.
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{plSample}}, \code{\link{expSample}}, or \code{\link{lnormSample}}.
#' @param gridCell Grid cell size in degrees. Default is 0.25.
#' @param histPlot Plot a histogram of the normalized  entropy values. Default is TRUE.
#' @param legend Add a legend to the histPlot when TRUE and change the position of the legend. Default is legend=c(TRUE, "topleft").
#' @param pdfPlot Create a  probability density function line plot of the entropy values. Default is TRUE.
#' @param nBins Number of bins used to calculate the pdf plot. Default is 40.
#' @return Data vector ('normalizedEntropy') containing normalized entropy values for each individual, a histogram plot of the normalized entropy values
#' (if histPlot=TRUE), a  probability density function line plot and a data frame with the data used to create the pdf plot ('entropyPDFplot', if pdfPlot=TRUE),
#' and data needed for the Predictability function (a list of occurrences and a vector of raw entropy values, 'occurrences' and 'indivEntropy', respectively).
#' @examples
#' Entropy(expSample)
#' Entropy(expSample, gridCell=0.25, histPlot=TRUE, legend=c(TRUE, "topleft"), pdfPlot=FALSE, nBins=40)
#' @export

Entropy<-function(species_df, gridCell=0.25, histPlot=TRUE, legend=c(TRUE, "topleft"), pdfPlot=FALSE, nBins=40){

  grid <- 1/gridCell
  longmin <- -180
  latmin <- -90
  longmax <- 180
  latmax <- 90
  longcells <- grid * (longmax - longmin)
  latcells <- grid * (latmax - latmin)
  totalcells <- longcells * latcells
  occurrences <- list() #list to store all counts in each cell (per individual)
  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})

  j<-i<-1
  for (i in 1:length(species_index)){ #Loop through each position and store counts when they occur in each cell
    Presence <- rep(0, totalcells) #vector to store counts of occurrences in each grid cell and store in list per individual
    for (j in 1:length((species_index[[i]]))){
      coordlong <- floor(grid * (species_df[species_index[[i]][j],2] - longmin))
      coordlat <- floor(grid * (species_df[species_index[[i]][j],3] - latmin))
      cellnum <- coordlong + grid * (longmax - longmin) * coordlat
      Presence[cellnum] <- Presence[cellnum] + 1 #recording how many occurrences occurred in each cell
    }
    occurrences[[i]] <- Presence #converts the occurrence count to a list for each individual shark
  }
  assign("occurrences", occurrences, envir = .GlobalEnv)

  Entropy <- probOccur <- occurrences #list to store all probabilities in each cell (per individual)
  indivEntropy <- normalizedEntropy <- c()
  j<-i<-1
  starting <- Sys.time()
  for (i in 1:length(species_index)){ # Loop through each position and store probability of the ind visiting each grid cell
    CellsVisited <- length(which(occurrences[[i]]!=0))# number of grid cells visited by individual i
    for (j in 1:totalcells){
      probOccur[[i]][j] <- occurrences[[i]][j] / length(species_index[[i]]) #number of occurrences from animal i in each grid cell of the world / number of points per animal i
      Entropy[[i]][j] <- probOccur[[i]][j] * log(probOccur[[i]][j]) #entropy calculation per cell (probability of occurrence in a cell * log of the probability of occurrence in that same cell)
    }
    indivEntropy[i] <- -1 * sum(na.omit(Entropy[[i]])) #calculate entropy by individual by summing the calculated entropies per cell following the equation: S = -Sum(probij * log(probij))
    normalizedEntropy[i] <- indivEntropy[i] / log(CellsVisited)  ### Normalization to allow for direct comparison of the entropies of trajectories with different numbers of visited areas
    # and informs about the complexity of the visitation pattern ranging between 0 (one visited cell) and 1 (uniform, every cell is visited with the same probability).
  }
  assign("indivEntropy", indivEntropy, envir = .GlobalEnv)
  assign("normalizedEntropy", normalizedEntropy, envir = .GlobalEnv)

  if (histPlot==TRUE){
    hist<-hist(normalizedEntropy, breaks=seq(0, 1, length.out = 21), plot=FALSE) #Determine hist values so you can automate plot better
    hist<-hist(normalizedEntropy, main="", xlab="Normalized Entropy", xlim=c(0,1), ylim=c(0,(max(hist$counts)+2)),axes=FALSE,
               breaks=seq(0, 1, length.out=21), border="black", col= "grey", xaxs="i", yaxs="i")
    segments(x0=0.5,y0=0,x1=0.5,y1=0.9*(max(hist$counts)+2),col="red", lty=2, lwd =2)
    myTicks = axTicks(1)
    myTicks2 = axTicks(2)
    axis(1, at=myTicks)
    axis(1, at=seq(min(myTicks), max(myTicks), (myTicks[2]-myTicks[1])/2), labels=NA, tcl=-0.2)
    axis(2, at = myTicks2)
    axis(2, at=seq(min(myTicks2), max(myTicks2), (myTicks2[2]-myTicks2[1])/2), labels=NA, tcl=-0.2)
    if (legend[1]==TRUE){
      legend(legend[2], bty="n", c("Normalized Entropy = 0.5"), lty=2, lwd=2, col="red")
    }
  }

  if (pdfPlot==TRUE){
    bw <- 1/nBins
    freq <- xs <- rep(0, nBins)
    nind <- length(normalizedEntropy)
    for(i in 1:nind){
      b <- floor(normalizedEntropy[i]/bw+0.5)
      freq[b] <- freq[b] +1
    }
    for (i in 1:nBins){
      xs[i] <- i*bw
    }
    for (i in 1:length(freq)){
      freq[i] <- freq[i]/(bw*nind)
    }
    entplot<-data.frame(xs, freq)
    plot(entplot$xs,entplot$freq, type="l", lwd = 1, col="black", ylab="pdf", xlim=c(0,1), xaxt="n",
         xlab=expression('S/S'[unif]), ylim=c(0,max(entplot$freq)+1),xaxs="i",yaxs="i")
    points(xs,freq,col="black", pch=19)
    myTicks = axTicks(1)
    myTicks2 = axTicks(2)
    axis(1, at=myTicks, tcl=-0.5)
    axis(1, at=seq(min(myTicks), max(myTicks), (myTicks[2]-myTicks[1])/2), labels=NA, tcl=-0.2)
    axis(2, at = myTicks2)
    axis(2, at=seq(min(myTicks2), max(myTicks2), (myTicks2[2]-myTicks2[1])/2), labels=NA, tcl=-0.2)
    names(entplot)=c("S/Sunif","pdf")
    assign("entropyPDFplot", entplot, envir = .GlobalEnv)
    }
}
