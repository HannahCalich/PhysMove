#' Entropy
#'
#' This function allows you to calculate the entropy of individual trajectories, which provides insights to how ordered (or disordered) a trajectory was.
#' @param species_df A data frame containing location data (rows) and columns with the following headers: "ref", "lon", "lat", "day". "ref" is the unique id number for each animal
#'      (e.g., their satellite tag number; integer format). "lon" and "lat" are the longitude and latitide of each position estimate in decimal degrees (numeric format). "day"
#'      is the datetime stamp for each location estimate (POSIXct format following yyyy-mm-dd hh:mm:ss). See XXX data frame for an example.
#' @param grid Grid used to split 1-degree by 1-degree cells. A grid of 4 results in 0.25-degree by 0.25 degree grid cells, while a grid of 0.25 results in 4-degrees by 4-degree grid cells. Default is 4.
#' @param plot Create line plot of each individual's Entropy. Default is TRUE.
#' @param nb Number of bins, used to determine the size of the bins used to calculate entropy frequency for plots. Default is 40.
#' @return Ind_Entropy vector containing Individual entropy values, NormalizedEntropy vector containing normalized values per individual, line plot (if desired).
#' @examples
#' Entropy(species_df)
#' Entropy(species_df,grid=4,plot=TRUE,nb=40)
#' @export

Entropy<-function(species_df, gridcell=0.25, hist = TRUE, pdfplot=FALSE, nb=40, legend=c(TRUE, "topleft")){

  grid <- 1/gridcell
  longmin <- -180
  latmin <- -90
  longmax <- 180
  latmax <- 90
  longcells <- grid * (longmax - longmin)
  latcells <- grid * (latmax - latmin)
  totalcells <- longcells * latcells

  occurrences <- list() #list to store all counts in each cell (per individual)
  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})

  #Loop through each position and store counts when they occur in each cell
  j<-i<-1
  for (i in 1:length(species_index)){
    Presence <- rep(0, totalcells) #vector to store counts of occurrences in each grid cell and store in list per individual
    for (j in 1:length((species_index[[i]]))){
      coordlong <- floor(grid * (species_df[species_index[[i]][j],2] - longmin))
      coordlat <- floor(grid * (species_df[species_index[[i]][j],3] - latmin))
      cellnum <- coordlong + grid * (longmax - longmin) * coordlat
      Presence[cellnum] <- Presence[cellnum] + 1 #recording how many occurrences occurred in each cell
    }
    occurrences[[i]] <- Presence #converts the occurrence count to a list for each individual shark
  }
  assign("Occurrences", occurrences, envir = .GlobalEnv)

  Entropy <- probOccur <- occurrences #list to store all probabilities in each cell (per individual)
  Ind_Entropy <- NormalizedEntropy <- c()

  ### Loop through each position and store probability of the ind visiting each grid cell
  j<-i<-1
  starting <- Sys.time()
  for (i in 1:length(species_index)){
    CellsVisited <- length(which(occurrences[[i]]!=0))# number of grid cells visited by individual i
    for (j in 1:totalcells){
      probOccur[[i]][j] <- occurrences[[i]][j] / length(species_index[[i]]) #number of occurrences from animal i in each grid cell of the world / number of points per animal i
      Entropy[[i]][j] <- probOccur[[i]][j] * log(probOccur[[i]][j]) #entropy calculation per cell (probability of occurrence in a cell * log of the probability of occurrence in that same cell)
    }
    Ind_Entropy[i] <- -1 * sum(na.omit(Entropy[[i]])) #calculate entropy by individual by summing the calculated entropies per cell following the equation: S = -Sum(probij * log(probij))
    NormalizedEntropy[i] <- Ind_Entropy[i] / log(CellsVisited)  ### Normalization to allow for direct comparison of the entropies of trajectories with different numbers of visited areas
    # and informs about the complexity of the visitation pattern ranging between 0 (one visited cell) and 1 (uniform, every cell is visited with the same probability).
  }

  assign("Ind_Entropy", Ind_Entropy, envir = .GlobalEnv)
  assign("NormalizedEntropy", NormalizedEntropy, envir = .GlobalEnv)

  if (hist==TRUE){
    hist<-hist(NormalizedEntropy, breaks = seq(0, 1, length.out = 21), plot=FALSE) #Determine hist values so you can automate plot better
    hist<-hist(NormalizedEntropy, main="", xlab = "Normalized Entropy", xlim=c(0,1), ylim=c(0,(max(hist$counts)+2)),axes=FALSE,
               breaks = seq(0, 1, length.out = 21), border="black", col= "grey",xaxs="i",yaxs="i")
    abline(v=0.5, col="red", lty=2, lwd =2)
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

  if (pdfplot==TRUE){
    bw <- 1/nb
    freq <- xs <- rep(0, nb)
    nind <- length(NormalizedEntropy)
    for(i in 1:nind){
      b <- floor(NormalizedEntropy[i]/bw+0.5)
      freq[b] <- freq[b] +1
    }
    for (i in 1:nb){
      xs[i] <- i*bw
    }
    for (i in 1:length(freq)){
      freq[i] <- freq[i]/(bw*nind)
    }
    entplot<-data.frame(xs, freq)
    #entplot<-entplot[which(entplot$freq != 0),]

    plot(entplot$xs,entplot$freq, type="l", lwd = 1, col="black", ylab="pdf", xlim=c(0,1), xaxt="n",
         xlab=expression('S/S'[unif]), ylim=c(0,max(entplot$freq)+1),xaxs="i",yaxs="i")

    points(xs,freq,col="black", pch=19)
    #abline(v=0.5, col="red", lty=2, lwd =2)
    myTicks = axTicks(1)
    myTicks2 = axTicks(2)
    axis(1, at=myTicks, tcl=-0.5)
    axis(1, at=seq(min(myTicks), max(myTicks), (myTicks[2]-myTicks[1])/2), labels=NA, tcl=-0.2)
    axis(2, at = myTicks2)
    axis(2, at=seq(min(myTicks2), max(myTicks2), (myTicks2[2]-myTicks2[1])/2), labels=NA, tcl=-0.2)

    #if (legend[1]==TRUE){
    #  legend(legend[2], bty="n", c("Entropy = 0.5"), lty=2, lwd=2, col="red")
    #}
    names(entplot)=c("S/Sunif","pdf")
    assign("EntropyPDFData", entplot, envir = .GlobalEnv)  }
}
