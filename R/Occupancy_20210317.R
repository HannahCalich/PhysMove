#' Occupancy
#'
#' This function allows you to calculate the occupancy patterns of satellite tagged animals. and plot a pdf of the occupancy scores
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{plSample}}, \code{\link{expSample}}, or \code{\link{lnormSample}}.
#' @param gridCell Grid cell size in degrees. Valid options are Default is 0.25.
#' @param map Create a map illustrating where occupancy occurs. Default is TRUE.
#' @param colGrad  Colour gradient for occupancy map that illustrates low, moderate, and high occupancy, respectively
#' (applied to ggplot2::scale_fill_gradientn). Default is colGrad=c("blue", "light blue","red").
#' @param pdfPlot Create a  probability density line plot of the occupancy values. Default is TRUE.
#' @param nBins Number of bins used to calculate the pdf plot. Default is 20.
#' @return A data frame ('occupancyResults') that contains location and corresponding occupancy data, a map (if map = TRUE), a probability
#' density function line plot and a data frame with the data used to create the pdf plot ('occupancyPDFplot', if pdfPlot = TRUE).
#' @examples
#' Occupancy(expSample)
#' Occupancy(expSample, gridcell=0.25, map=TRUE, colGrad=c("blue", "light blue", "red"), pdfPlot=FALSE, nBins=20)
#' @export

Occupancy<-function(species_df, gridcell=0.25, map=TRUE, colGrad=c("blue", "light blue", "red"), pdfPlot=FALSE, nBins=20){

  grid <- 1/gridcell
  Radius <- 6371 #Earth Radius in km (disp are in km)
  rad <- 3.141592653589793/180 #to convert degrees to radians
  longmin <- -180
  latmin <- -90
  longmax <- 180
  latmax <- 90
  longcells <- grid * (longmax - longmin)
  latcells <- grid * (latmax - latmin)
  totalcells <- longcells * latcells

  #Loop through each location's coordinates and store counts when they occur in each cell (the cell numbers are exactly the same as python)
  Presence <- rep(0, totalcells)
  for (i in 1:dim(species_df)[1]){
    coordlong <- floor(grid * (species_df[i,2] - longmin)) #original
    coordlat <- floor(grid * (species_df[i,3] - latmin)) #How many rows up do we go from the 0-360 baseline? If point in 0-360deg range this value will be 0
    cellnum <- (coordlong + grid * (longmax - longmin) * coordlat)+1 #Otherwise we have cell 0 and this makes the calculations incorrect below (0 was valid in Python but not in R)
    Presence[cellnum] <- Presence[cellnum] + 1
  }

  ## Calculate the area of each grid cell with species were present by converting grid cell #s back to lat/lon then calculalting areas using spherical coordiantes
  AllOccup <- MyArea <- rep(0, totalcells)
  for(i in 1:length(Presence)){
    coordlat <- floor(i/(grid*(longmax-longmin)))
    ilat <- latmin + (coordlat / grid)
    MyArea[i] <- Radius^2 * abs(sin(rad * ilat) - sin(rad * (ilat + (1/grid)))) * (rad * (1/grid)) # Area for standard spherical coordinates in km^2 (same as Sphere_Cylindrical_Equal_Area projection in ArcGIS)
    AllOccup[i] <- Presence[i]/MyArea[i] #This occupancy vector keeps the original cell number
  }
  OccExp <- as.data.frame(cbind("CellNumber"=seq(1,totalcells,1),"Counts"=Presence,"Occupancy"=AllOccup, "Area"=MyArea, "Longitude"=c(0), "Latitude"=c(0)))
  OccExp<-OccExp[which(OccExp$Occupancy != 0),]

  for(i in 1:nrow(OccExp)){
    coordlat <- floor(OccExp[i,1]/(grid*(longmax-longmin))) #Take origin cell number and divide by number of cells in longitude of grid.
    coordlong <- OccExp[i,1] - (grid*(longmax-longmin)) * coordlat

    if (coordlong==0) {
      OccExp$Longitude[i]  <- (longmin + (360 / grid)) - 0.5*gridcell #midpoint of cell
      OccExp$Latitude[i] <- (latmin + (coordlat / grid)) - 0.5*gridcell
    } else {
      OccExp$Longitude[i]  <- (longmin + (coordlong/grid)) - 0.5*gridcell
      OccExp$Latitude[i] <- (latmin + (coordlat / grid)) + 0.5*gridcell
    }
  }
  assign("occupancyResults", OccExp, envir = .GlobalEnv)

  if (map==TRUE){
    xyz <- OccExp[,c(5,6,3)]

    z <- ggplot2::ggplot() +
      ggplot2::geom_tile(data = xyz, ggplot2::aes(x = Longitude, y = Latitude, fill =  Occupancy))+
      ggplot2::labs(x = "Longitude", y = "Latitude", fill = expression(atop("",atop(textstyle("Occupancy"), atop(textstyle("(counts"%*%"area"^-1*")"))))))+
      ggplot2::coord_sf(xlim = c(min(xyz$Longitude)- 0.5*gridcell, max(xyz$Longitude)+ 0.5*gridcell), ylim = c(min(xyz$Latitude)- 0.5*gridcell, max(xyz$Latitude)+ 0.5*gridcell))+
      ggplot2::theme_minimal()+
      ggplot2::scale_fill_gradientn(colours = c(colGrad))
    tryCatch({
      z <- z +
        ggplot2::borders("world", colour ="gray50", fill ="gray50", xlim = c(min(xyz$Longitude)- 0.5*gridcell, max(xyz$Longitude)+ 0.5*gridcell), ylim = c(min(xyz$Latitude)- 0.5*gridcell, max(xyz$Latitude)+ 0.5*gridcell))
      }, error = function(e){message('Note: World polygon does not overlap with occupancy data')})
    plot(z)
  }

  if (pdfPlot==TRUE){
    Occmin <- min(OccExp[which(OccExp$Occupancy!=0),3]) #what is the min occ value when occ > 0 (min occupancy of occupied cells)
    bw <- log(max(OccExp$Occupancy)/Occmin)/log(nBins)
    if (log(bw)<0){
      message ("Ratio of occupancy values to number of bins has resulted in a bin width less than 1, which cannot be log transformed. Bin width has been changed to 1.1 from ", round(bw,2) )
      bw <- 1.1
    }
    freq <- rep(0, nBins+1)
    for(i in 1:nrow(OccExp)){
      if (OccExp[i,3]>0){
        b <- floor(log(OccExp[i,3]/Occmin)/log(bw) + 0.5) # ceiling is used here because if b = 0 it'll throw errors (python code uses int(#) but 0 is valid in python)
        freq[b] <- freq[b] + 1
      }
    }
    if (anyNA(freq)){
      stop ("Cell size too small to create pdf plot") # If users do not increase nBins to make up for a small cell size then the above code will results in NA and invalid values, which cause the code to crash.
    }
    sumFreq <- sum(freq)
    freq <- c(totalcells-sumFreq, freq) #pdf plot should consider all cells in work so instead of processing through empty cells I just add the empty ones to the beginning of this vector
    norm <- sum(freq)
    freq <- freq[freq>0]
    xs <- ys <- rep(0, length(freq))

    for (i in 1:length(freq)){
        ys[i] <- freq[i]/(norm*Occmin*((bw^((i-1)+0.5))-(bw^((i-1)-0.5))))
        xs[i] <- Occmin*(bw^(i-1))
    }
    occplot <- data.frame(xs, ys)
    names(occplot) <- c("Occupancy(km^-2)","pdf")
    assign("occupancyPDFplot", occplot, envir = .GlobalEnv)
    plot(xs, ys,log = "xy", xlim = c(Occmin/2,max(OccExp$Occupancy)*2), type = "l", col = "black", ylab = "pdf",xlab = expression('Occupancy (km'^'-2'*')'))
    points(xs, ys, col = "black", pch = 19)

    }
}
