#' Randomise tracks
#'
#' This function allows you to investigate the influence spatial and/or temporal correlations may have on an individuals' space use patterns.
#' This is done by maintaining the origin and end location of each track, randomizing the order displacements occurred in between these points,
#' and calculating how many grid cells the original and randomised tracks visited, which summarizes their space use.
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format,
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{tracks}}.
#' @param randTrack Number of randomised tracks per individual. Default is 100.
#' @param gridCell Grid cell size in degrees. Default is 0.25.
#' @param plot Plot the number of cells visited in the original track versus the average number of cells visited in the
#' reshuffled tracks. Default is TRUE.
#' @param lm Calculate a linear regression to examine the relationship between the number of cells visited in the original tracks
#' (target variable) and the average number of cells visited by the Randomised tracks (predictor variable). If plot = TRUE this parameter
#' adds a solid black fit line to the data points and a black dashed line, which represents a 1:1 relationship. The slope of the fit line can
#' be determined by typing 'RandomiselinearModel$coefficients[2]'. Default is TRUE.
#' @return List containing a dataframe of results (list element 1), the randomised longitude and latitude values (list elements 2 and 3, respectively),
#' which are needed for the \code{\link{PlotRandomTracks}} function, and if lm = TRUE, the results of the linear model are output (list element 4).
#' The dataframe of results includes columns for the number of cells visited by each original track and the average number of cells visited by the
#' Randomised tracks for each ref. Lastly, if plot = TRUE, a plot illustrating the number of cells visited by the original and randomised tracks is created,
#' and if lm = TRUE, a fit line and reference line are added to the plot.
#' @examples
#' \dontrun{
#'
#' Randomise(tracks, randTrack=100, gridCell=0.25, plot=TRUE, lm=TRUE)
#'
#' }
#' @export

Randomise <- function(species_df, randTrack=100, gridCell=0.25, plot=TRUE, lm=TRUE) {

  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  MyDiffLat <- MyDiffLong <- MyDiffTime <- rep(0, dim(species_df)[1]) # Create vector to store the differences in lat and long
  for (i in 2:dim(species_df)[1]){
    # For all rows starting at 2, subtract the difference between row i and the row before it.
    # Includes differences between individuals but those are called on in next section of code
    MyDiffLong[i] <- species_df[i,2] - species_df[i-1,2]
    MyDiffLat[i] <- species_df[i,3] - species_df[i-1,3]
  }
  # Shuffle the differences per individual using sample without replacement
  ShuffledLong <- ShuffledLat <- ShuffledTime <- matrix(0, nrow = dim(species_df)[1], ncol = randTrack) # Create vector to store the shuffled differences per individual

  message("Randomizing tracks, step 1/3")

  for (i in 1:length(species_index)){
    for(r in 1:randTrack){ # Number of Randomised tracks per individual
    # Randomise the order of the positions for each individual (e.g., instead of 1,2,3, the order might be 13,43,5)
      NewPositions <- sample(seq(1:(utils::tail(species_index[[i]],1) - species_index[[i]][1])), utils::tail(species_index[[i]],1) - species_index[[i]][1], replace = FALSE)
      # For each reshuffling (r), reserve the original first location per individual (i).
      ShuffledLong[species_index[[i]][1], r] <- species_df[species_index[[i]][1], 2] # first position for each individual is the origin
      ShuffledLat[species_index[[i]][1], r] <- species_df[species_index[[i]][1], 3]
      for(j in 1:length(NewPositions)){
        # Looping through new positions, length is 1 fewer than the number of locations per animal because first position is fixed
        # Create random locations based on the sum of previous location and the distance travelled between a random location and the next point in the track (MyDiff)
        # If the first new position is 26, we want to add the difference between the previous location (the origin), and the MyDiff[27],
        # which is the distance from the 26th to 27th points, moving forwards towards the end of the track.
        # Since NewPositions is n-1, working backwards with the MyDiff and adding 1 here makes sure the last MyDiff per species is included
        # (eg if an individual has 65 points, there are 64 new ones, but the code below will make sure the distance between the 65th and 64th point is included)
        # Since this loop is limited by length(NewPositions), we won't include the diffs calculated between individuals
        # Following this approach the math results in the same final destination point for each track
        ShuffledLong[species_index[[i]][j+1], r] <- ShuffledLong[species_index[[i]][j], r] + MyDiffLong[species_index[[i]][NewPositions[j]+1]]
        ShuffledLat[species_index[[i]][j+1], r] <- ShuffledLat[species_index[[i]][j], r] + MyDiffLat[species_index[[i]][NewPositions[j]+1]]
      }
    }
  }

  message("Calculating average number of cells visited by randomised tracks, step 2/3")

  # Compare the number of visited cells used in the original and reshuffled tracks
  grid <- 1/gridCell
  longmin <- -180
  latmin <- -90
  longmax <- 180
  latmax <- 90
  longcells <- grid * (longmax - longmin)
  latcells <- grid * (latmax - latmin)
  totalcells <- longcells * latcells

  SumShuffledOccurrences <- matrix(0, nrow = length(species_index), ncol = randTrack)
  AvgShuffledOccurrences <- SumOriginalOccurrences <- c() # vector to store cell counts for each individual

  # Loop through each shuffled position and store counts when individuals occur in each cell
  i <- r <- 1
  for (i in 1:length(species_index)){ # For each individual
    for(r in 1:randTrack){ # For each random track, determine what cell each point falls in
      j <- 1 # Do not comment this line because it marks the position for each index per individual
      ShuffledPresence <- rep(0, totalcells)
      for (j in 1:length(species_index[[i]])){ # For each point in a track
        coordlong <- floor(grid * (ShuffledLong[species_index[[i]][j], r] - longmin))
        coordlat <- floor(grid * (ShuffledLat[species_index[[i]][j], r] - latmin))
        cellnum <- coordlong + grid * (longmax - longmin) * coordlat
        ShuffledPresence[cellnum] <- 1 # Record the number of unique cells visited (aka presence, not occupancy)
        }
    SumShuffledOccurrences[i, r] <- sum(ShuffledPresence)
    }
  AvgShuffledOccurrences[i] <- mean(SumShuffledOccurrences[i,])
  }

  message("Calculating number of cells visited by original tracks, step 3/3")

  i <- 1
  for (i in 1:length(species_index)){ # For original tracks
    j < -1 # Do not comment this. This is needed to restart j for each animal to find position in index.
    Presence <- rep(0, totalcells)
    for (j in 1:length(species_index[[i]])){
      coordlong <- floor(grid * (as.numeric(species_df[species_index[[i]][j],2]) - longmin))
      coordlat <- floor(grid * (as.numeric(species_df[species_index[[i]][j],3]) - latmin))
      cellnum <- coordlong + grid * (longmax - longmin) * coordlat
      Presence[cellnum] <- 1 # Record the number of unique cells visited (aka presence, not occupancy)
    }
    SumOriginalOccurrences[i] <- sum(Presence)
  }
  plot.df <- cbind.data.frame(ref=unique(species_df$ref),CellsInOriginalTracks=SumOriginalOccurrences,
                              AvgCellsInRandomisedTracks=AvgShuffledOccurrences)

  if (plot==TRUE){
    a <- ggplot2::ggplot(plot.df, ggplot2::aes(x=plot.df[,2], y=plot.df[,3])) +
      ggplot2::geom_point(size=1.25)+
      ggplot2::theme_bw(base_size=12)+
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                          panel.grid.minor=ggplot2::element_blank(), axis.line=ggplot2::element_line(colour="black"),
                                          axis.text.x=ggplot2::element_text(margin=ggplot2::margin(t=10), colour="black"),
                                          axis.text.y=ggplot2::element_text(margin=ggplot2::margin(r=10), colour="black"))+
      ggplot2::xlab("Sum of cells in original track")+
      ggplot2::ylab(expression(atop("Average sum of cells", paste("in randomised tracks")))) #("Average sum of cells \n in Randomised tracks")
    if (lm==TRUE){
      a <- a +
        ggplot2::geom_abline(slope=1, intercept=1, col="black", lwd=0.5, lty=2)+
        ggplot2::stat_smooth(formula=y~x, method="lm", col="black", lwd=0.5)+
        ggplot2::geom_point(size=2) # Add points back in again to they are on top layer
    }
    plot(a)
  }

  randomResults <- list(plot.df, ShuffledLong, ShuffledLat)
  names(randomResults) <- c("resultsDF", "RandomisedLong", "RandomisedLat")

  if (lm==TRUE){
    fit <- lm(plot.df$AvgCellsInRandomisedTracks ~ plot.df$CellsInOriginalTracks, data=plot.df)
    randomResults[[4]] <- as.data.frame(broom::tidy(fit))
    names(randomResults) <- c("resultsDF", "RandomisedLong", "RandomisedLat", "lm")
    rm(fit)
  }

  return(randomResults)
}
