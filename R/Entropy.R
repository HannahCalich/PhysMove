#' Entropy of trajectories
#'
#' This function allows you to calculate the normalized entropy of individual trajectories (individual entropy scores are
#' normalized by the log number of cells each trajectory visited), which provides insight to how ordered or disordered the trajectories were.
#' Values close to 1 indicate high entropy (disordered trajectories) while values closer to 0 indicate low entropy (ordered trajectories).
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{speciesA}}.
#' @param gridCell Grid cell size in degrees. Default is 0.25.
#' @param histPlot Plot a histogram of the normalized  entropy values. Default is TRUE.
#' @return Data frame of the normalized entropy values for each trajectory (main result) as well as the individual entropy
#' values (not normalized) and the number of cells each trajectory visited. If histPlot=TRUE a histogram of the normalized entropy scores is created. If pdfPlot=TRUE, a
#' probability density function line plot of results is created and the data used to create the pdf plot are automatically assigned to the global environment
#' ('entropyPDFplot').
#' @examples
#' Entropy(speciesA)
#' Entropy(speciesA, gridCell=0.25, histPlot=TRUE)
#' @export

Entropy<-function(species_df, gridCell=0.25, histPlot=TRUE){

  grid <- 1/gridCell
  longmin <- -180
  latmin <- -90
  longmax <- 180
  latmax <- 90
  longcells <- grid * (longmax - longmin)
  latcells <- grid * (latmax - latmin)
  totalcells <- longcells * latcells
  occurrences <- list() # List to store all counts in each cell (per individual)
  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})

  for (i in 1:length(species_index)){ # Loop through each position and store counts when they occur in each cell
    Presence <- rep(0, totalcells) # Vector to store counts of occurrences in each grid cell and store in list per individual
    for (j in 1:length((species_index[[i]]))){
      coordlong <- floor(grid * (species_df[species_index[[i]][j],2] - longmin))
      coordlat <- floor(grid * (species_df[species_index[[i]][j],3] - latmin))
      cellnum <- coordlong + grid * (longmax - longmin) * coordlat
      Presence[cellnum] <- Presence[cellnum] + 1 # Recording how many occurrences occurred in each cell
    }
    occurrences[[i]] <- Presence # Converts the occurrence count to a list for each individual
  }

  Entropy <- probOccur <- occurrences # List to store all probabilities in each cell (per individual)
  CellsVisited <- indivEntropy <- normalizedEntropy <- c()

  for (i in 1:length(species_index)){ # Loop through each position and store probability of the ind visiting each grid cell
    CellsVisited[i] <- length(which(occurrences[[i]]!=0))# Number of grid cells visited by individual i
    for (j in 1:totalcells){
      probOccur[[i]][j] <- occurrences[[i]][j] / length(species_index[[i]]) # Number of occurrences from individual i in each grid cell of the world / number of points per individual i
      Entropy[[i]][j] <- probOccur[[i]][j] * log(probOccur[[i]][j]) # Entropy calculation per cell (probability of occurrence in a cell * log of the probability of occurrence in that same cell)
    }
    indivEntropy[i] <- -1 * sum(na.omit(Entropy[[i]])) # Calculate entropy by individual by summing the calculated entropies per cell following the equation: S = -Sum(probij * log(probij))
    normalizedEntropy[i] <- indivEntropy[i] / log(CellsVisited[i])  # Normalized to allow for direct comparison of the entropies of trajectories with different numbers of visited areas
    # and informs about the complexity of the visitation pattern ranging between 0 (one visited cell) and 1 (uniform, every cell is visited with the same probability).
    if (CellsVisited[i]==1){
      warning(paste("Ref",unique(species_df$ref)[i],"only visited 1 cell so normalized entropy scores cannot be calculated and NaN is produced"), immediate. = TRUE)
    }
  }
  entropyResults <- as.data.frame(cbind("ref"=unique(species_df$ref),"normalizedEntropy"=normalizedEntropy,"indivEntropy"=indivEntropy,"cellsVisited"=CellsVisited))

  if (histPlot==TRUE){
    h <- hist(normalizedEntropy, breaks=seq(0, 1, length.out = 21), plot=FALSE) # Determine hist values so you can automate plot better
    hist_plot <- ggplot2::ggplot(entropyResults, ggplot2::aes(normalizedEntropy))+
      ggplot2::geom_histogram(breaks=h$breaks, color="black", fill="darkgrey")+
      ggplot2::scale_y_continuous(breaks=function(x) seq(ceiling(x[1]), floor(x[2]), by = 2))+
      ggplot2::scale_x_continuous("Normalized Entropy", breaks=seq(0,1,0.1), labels=xlab)+
      ggplot2::labs(y = "Frequency")+
      ggplot2::theme_classic(base_size = 18)#+
      # ggplot2::geom_vline(ggplot2::aes(xintercept=0.5, color="0.5"), linetype="dashed", size=1) +
      # ggplot2::scale_color_manual(name = "", values = c("0.5" = "red"))
    plot(hist_plot)
  }
  return(entropyResults)
}
