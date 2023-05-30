#' Occupancy
#'
#' This function allows you to calculate the occupancy patterns of location estimates and create a map.
#' A pdf plot of the occupancy values can be created with the \code{\link{pdfPlot}} function.
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{tracks}}.
#' @param gridCell Grid cell size in degrees. Default is 0.25.
#' @param map Create a map illustrating where occupancy occurs. Default is TRUE.
#' @param colGrad  Colour gradient for occupancy map that illustrates low, moderate, and high occupancy, respectively
#' (applied to ggplot2::scale_fill_gradientn). Default is colGrad=c("blue", "light blue","red").
#' @return A list containing occupancy results (first list object) and the total number of grid cells analyzed (second list object -
#' required by the \code{\link{pdfPlot}} function). Occupancy results include a data frame with occupancy values and corresponding
#' locations (provided as center value of each grid cell). If map=TRUE a map is created.
#' @importFrom rlang .data
#' @examples Occupancy(tracks)
#' @examples Occupancy(tracks, gridCell=0.25, map=TRUE, colGrad=c("blue", "light blue", "red"))
#' @export

Occupancy <- function(species_df, gridCell=0.25, map=TRUE, colGrad=c("blue", "light blue", "red")){

  grid <- 1/gridCell
  Radius <- 6371 #Earth Radius in km (disp are in km)
  rad <- 3.141592653589793/180 #to convert degrees to radians
  longmin <- -180
  latmin <- -90
  longmax <- 180
  latmax <- 90
  longcells <- grid * (longmax - longmin)
  latcells <- grid * (latmax - latmin)
  totalcells <- longcells * latcells

  # Loop through each location's coordinates and store counts when they occur in each cell (the cell numbers are exactly the same as python)
  Presence <- rep(0, totalcells)
  for (i in 1:dim(species_df)[1]){
    coordlong <- floor(grid * (species_df[i,2] - longmin))
    coordlat <- floor(grid * (species_df[i,3] - latmin))
    cellnum <- (coordlong + grid * (longmax - longmin) * coordlat) + 1 # Need to add 1 here otherwise we have cell number 0, which was valid in Python but not in R.
    Presence[cellnum] <- Presence[cellnum] + 1
  }

  ## Calculate the area of each grid cell with species were present by converting grid cell numbers back to lat/lon then calculating areas using spherical coordinates
  AllOccup <- MyArea <- rep(0, totalcells)
  for(i in 1:length(Presence)){
    coordlat <- floor(i/(grid*(longmax-longmin)))
    ilat <- latmin + (coordlat / grid)
    MyArea[i] <- Radius^2 * abs(sin(rad * ilat) - sin(rad * (ilat + (1/grid)))) * (rad * (1/grid)) # Area for standard spherical coordinates in km^2 (same as Sphere_Cylindrical_Equal_Area projection in ArcGIS)
    AllOccup[i] <- Presence[i]/MyArea[i]
  }
  OccExp <- as.data.frame(cbind("CellNumber"=seq(1,totalcells,1),"Counts"=Presence,"Occupancy"=AllOccup, "Area"=MyArea, "Longitude"=c(0), "Latitude"=c(0)))
  OccExp <- OccExp[which(OccExp$Occupancy != 0),]

  for(i in 1:nrow(OccExp)){
    coordlat <- floor(OccExp[i,1]/(grid*(longmax-longmin))) # Take origin cell number and divide by number of cells in longitude of grid.
    coordlong <- OccExp[i,1] - (grid*(longmax-longmin)) * coordlat

    if (coordlong==0) {
      OccExp$Longitude[i] <- (longmin + (360 / grid)) - 0.5*gridCell # midpoint of cell
      OccExp$Latitude[i] <- (latmin + (coordlat / grid)) - 0.5*gridCell
    } else {
      OccExp$Longitude[i] <- (longmin + (coordlong/grid)) - 0.5*gridCell
      OccExp$Latitude[i] <- (latmin + (coordlat / grid)) + 0.5*gridCell
    }
  }

  if (map==TRUE){
    xyz <- OccExp[,c(5,6,3)]
    z <- ggplot2::ggplot() +
      ggplot2::geom_tile(data = xyz, ggplot2::aes(x = .data$Longitude, y = .data$Latitude, fill = .data$Occupancy))+
      ggplot2::labs(x = "Longitude", y = "Latitude", fill = expression(atop("",atop(textstyle("Occupancy"),
                                                                                    atop(textstyle("(counts"%*%"area"^-1*")"))))))+
      ggplot2::coord_sf(xlim = c(min(xyz$Longitude)- 0.5*gridCell, max(xyz$Longitude)+ 0.5*gridCell),
                        ylim = c(min(xyz$Latitude)- 0.5*gridCell, max(xyz$Latitude)+ 0.5*gridCell))+
      ggplot2::theme_minimal(base_size=18)+
      ggplot2::scale_fill_gradientn(colours = c(colGrad))
    tryCatch({
      z <- z +
        ggplot2::borders("world", colour ="gray50", fill ="gray50", xlim = c(min(xyz$Longitude)-0.5*gridCell, max(xyz$Longitude)+0.5*gridCell), ylim = c(min(xyz$Latitude)- 0.5*gridCell, max(xyz$Latitude)+ 0.5*gridCell))
      }, error = function(e){message('Note: World polygon does not overlap with occupancy data')})
    plot(z)
  }

  OccExp <- OccExp[,c(6,5,4,2,3)]
  row.names(OccExp) <- 1:nrow(OccExp)
  # out <- list(OccExp, totalcells)
  # names(out) <- c("occResults", "totalcells")
  # return(out)
  return(OccExp)
}
