#' Randomizing trajectories
#'
#' Randomizing trajectories to break any existing spatial or temporal correlations between location estimates. Here, the sequences of the displacements for each observed trajectory are reshuffled while the origin and end location of each trajectory are maintained.
#' @param species_df A data frame containing location data (rows) and columns with the following headers: "ref", "lon", "lat", "day". "ref" is the unique id number for each animal
#'      (e.g., their satellite tag number; integer format). "lon" and "lat" are the longitude and latitide of each position estimate in decimal degrees (numeric format). "day"
#'      is the datetime stamp for each location estimate (POSIXct format following yyyy-mm-dd hh:mm:ss). See XXX data frame for an example.
#' @param RandTraj Number of randomised trajectories per individual
#' @param gridcell grid cell size in degrees. Default is 0.25.
#' @param Colour Colour of line for data. Default="Navy".
#' @return RandomizedResults data frame that includes the number of cells visited by each individual's original and reshuffled trajectories,RandomizedLat and RandomizedLon matricies for plotting tracks (see \code{\link{PlotRandomTracks}} function for further details),
#' and a plot illustrating the number of cells visited by the original and reshuffled trajectories, if desired.
#' @examples
#' Randomize(species_df)
#' Randomize(species_df, RandTraj=500, grid=4, Plot=TRUE)
#' @export

Randomize<-function(species_df, RandTraj=500, gridcell=0.25, plot=TRUE, pchtype=19, colours=c("black","black"), title="", legend=c(TRUE, "topleft")) {

  grid = 1/gridcell

  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  MyDiffLat <- MyDiffLong <- MyDiffTime <- rep(0, dim(species_df)[1]) # Create vector to store the differences in lat, long and time (time is needed for data that are not interpolated)
  for (i in 2:dim(species_df)[1]){
    # For all rows starting at 2, substract the difference between row i and the row before it.
    # Includes differences between individuals but those are called on in next section of code
    # MyDiff[i] = diff from row before it to current row
    # if(species_df[i,1] == species_df[i-1,1]){ # no need for this as long as we don't read values calculated at the index for origin for each individuals
    MyDiffLong[i] <- species_df[i,2] - species_df[i-1,2]
    MyDiffLat[i] <- species_df[i,3] - species_df[i-1,3]
    MyDiffTime[i] <- species_df[i,4] - species_df[i-1,4]
  }
  # Shuffle the differences per individual using sample without replacement
  ShuffledLong <- ShuffledLat <- ShuffledTime <- matrix(0, nrow = dim(species_df)[1], ncol = RandTraj) # Create vector to store the shuffled differences per individual

  message("Randomizing trajectories, step 1/3")

  for (i in 1:length(species_index)){
    for(T in 1:RandTraj){ ### number of randomised trajectories per individual
    # Randomize the order of the positions for each individual (eg instead of 1-65, the order might be 13,43,5)
      NewPositions <- sample(seq(1:(tail(species_index[[i]],1) - species_index[[i]][1])), tail(species_index[[i]],1) - species_index[[i]][1], replace = FALSE)
      # For each reshuffling (T), reserve the original first location per individual (i).
      ShuffledLong[species_index[[i]][1], T] <- species_df[species_index[[i]][1], 2] # first position for each individual is the origin
      ShuffledLat[species_index[[i]][1], T] <- species_df[species_index[[i]][1], 3]
      # ShuffledTime[species_index[[i]][1], T] <- species_df[species_index[[i]][1], 4]
      for(j in 1:length(NewPositions)){
        # Looping through new positions, length is 1 fewer than the number of locations per animal becauase first position is fixed
        # Create random locations based on the sum of previous location and the distance travelled between a random location and the next point in the trajectory (MyDiff)
        # If the first new position is 26, we want to add the difference between the previous location (the origin), and the myDiff[27],
        # which is the distance from the 26th to 27th points, moving forwards towards the end of the trajectory.
        # Since NewPositions is n-1, working backwards with the MyDiffs and adding 1 here makes sure the last MyDiff per species is included
        # (eg if a shark has 65 points, there are 64 new ones, but the code below will make sure the distance betwen the 65th and 64th point is included)
        # Since this loop is limited by length(NewPositions), we won't include the diffs calculated between individuals
        # This way, the math should result in the same final destination point for each trajectory
        # Double checked this part of the code with the raw data to confirm the origin and destination points match (2019-11-07)
        ShuffledLong[species_index[[i]][j+1], T] <- ShuffledLong[species_index[[i]][j], T] + MyDiffLong[species_index[[i]][NewPositions[j]+1]]
        ShuffledLat[species_index[[i]][j+1], T] <- ShuffledLat[species_index[[i]][j], T] + MyDiffLat[species_index[[i]][NewPositions[j]+1]]
        # ShuffledTime[species_index[[i]][j+1], T] <- ShuffledTime[species_index[[i]][j], T] + MyDiffTime[species_index[[i]][NewPositions[j]+1]]
      }
    }
  }

  assign("RandomizedLong",ShuffledLong, envir = .GlobalEnv)
  assign("RandomizedLat",ShuffledLat, envir = .GlobalEnv)

  message("Calculating average number of cells visited by randomized trajectories, step 2/3")

  # Compare the number of visited cells used in the original and reshuffled trajectories (i.e., with occupancy)
  # Create grid based on 0.25 degree cells
  longmin <- -180 # min(species_df[species_index[[1]],2])
  latmin <- -90 # min(species_df[species_index[[1]],3])
  longmax <- 180 # max(species_df[species_index[[1]],2])
  latmax <- 90 # max(species_df[species_index[[1]],3])
  longcells <- grid * (longmax - longmin) # 360*4 = 1440 cells of 0.25 degrees each in longitude ### 1/gridcell
  latcells <- grid * (latmax - latmin)  # 180*4 = 720 cells of 0.25 degrees each in latitude
  totalcells <- longcells * latcells

  SumShuffledOccurrences <- matrix(0, nrow = length(species_index), ncol = RandTraj)
  AvgShuffledOccurrences <- SumOriginalOccurrences <- c() # vector to store cell counts for each individual

  ### Loop through each shuffled position and store counts when individuals occur in each cell
  for (i in 1:length(species_index)){ #For each shark
    # print(i)
    # vector to store counts of ShuffledOccurrences in each grid cell and store in list per shark
    ### totalcells calcualted in occupancy code, shoudld = 1036800 for 0.25 deg res
    for(T in 1:RandTraj){ #For each random trajectory, determine what cell each point falls in 10){
      j<-1 #Do not turn this line off (it marks the position for each index per shark)
      ShuffledPresence <- rep(0, totalcells)
      for (j in 1:length(species_index[[i]])){ #For each point in a trajectory
        coordlong <- floor(grid * (ShuffledLong[species_index[[i]][j], T] - longmin))
        coordlat <- floor(grid * (ShuffledLat[species_index[[i]][j], T] - latmin))
        cellnum <- coordlong + grid * (longmax - longmin) * coordlat
        #print(cellnum)
        ShuffledPresence[cellnum] <- 1 # ShuffledPresence[cellnum] + 1 ## no need to count here as we will only look at presence in cells not occupancy, so we just want cell = 1
        }
    SumShuffledOccurrences[i, T]<-sum(ShuffledPresence)
    }
  AvgShuffledOccurrences[i]<-mean(SumShuffledOccurrences[i,])
  }

  message("Calculating number of cells visited by original trajectories, step 3/3")

  for (i in 1:length(species_index)){ # For original trajectories
    j<-1 # Do not comment this. This is needed to restart j for each animal to find position in index.
    Presence <- rep(0, totalcells)
    for (j in 1:length(species_index[[i]])){
      coordlong <- floor(grid * (as.numeric(species_df[species_index[[i]][j],2]) - longmin))
      coordlat <- floor(grid * (as.numeric(species_df[species_index[[i]][j],3]) - latmin))
      cellnum <- coordlong + grid * (longmax - longmin) * coordlat
      Presence[cellnum] <- 1 # Presence[cellnum] + 1 ## no need to count here as we will only look at presence in cells not occupancy, so we just want cell = 1
    }
    SumOriginalOccurrences[i] <- sum(Presence)
  }

  RandomizedResults<-cbind.data.frame(ref=unique(species_df$ref),SumOriginalOccurrences=SumOriginalOccurrences,AvgShuffledOccurrences=AvgShuffledOccurrences)
  assign("RandomizedResults",RandomizedResults, envir = .GlobalEnv)

### Plot the cells used in original and shuffled trajectories showing shuffled visit less places than original
  if (plot==TRUE){
  plot(SumOriginalOccurrences, AvgShuffledOccurrences,ylab="Avg Cells in Randomized Trajectories", xlab="Cells in Original Trajectory",
       cex=1, pch=pchtype, main=title, col=colours[1])
  abline(1,1, col=colours[2], lwd=2)
    if (legend[1]==TRUE){
      legend(legend[2], bty="n", c("Trajectories", "1:1 Relationship"), pch=c(pchtype,NA), lty=c(NA,1), lwd=c(NA,2), col=colours)
    }
  }
}
