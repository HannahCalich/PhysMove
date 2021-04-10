#' Occupancy
#'
#' This function allows you to calculate the occupancy patterns of satellite tagged animals. and plot a pdf of the occupancy scores
#' @param species_df A data frame containing location data (rows) and columns with the following headers: "ref", "lon", "lat", "day". "ref" is the unique id number for each animal
#'      (e.g., their satellite tag number; integer format). "lon" and "lat" are the longitude and latitide of each position estimate in decimal degrees (numeric format). "day"
#'      is the datetime stamp for each location estimate (POSIXct format following yyyy-mm-dd hh:mm:ss). See XXX data frame for an example.
#' @param gridcell Grid cell size in degrees. Default is 0.25.
#' @param plot Create line plot of the probability density function of occupancy Default is TRUE.
#' @param nb Number of bins, used to determine the size of the bins used to calculate occupancy for plot. Default is 20.
#' @param map Create a map illustrating where occupancy occurs. Default is TRUE.
#' @return "MyOcc" data frame containing the location and corresponding occupancy data used in the plots, as well as a line plot and/or a map, if desired.
#' @examples
#' Occupancy(species_df)
#' Occupancy(species_df,grid=4,plot=TRUE,nb=20,map=TRUE)
#' @export

Occupancy<-function(species_df, gridcell=0.25, map=TRUE, plot=FALSE, nb=20){

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
      OccExp$Longitude[i]  <- (longmin + (360 / grid)) - 0.5*gridcell
      OccExp$Latitude[i] <- (latmin + (coordlat / grid)) - 0.5*gridcell
    } else {
      OccExp$Longitude[i]  <- (longmin + (coordlong/grid)) - 0.5*gridcell
      OccExp$Latitude[i] <- (latmin + (coordlat / grid)) + 0.5*gridcell
    }
  }

  assign("Occupancy_results", OccExp, envir = .GlobalEnv)

  if (map==TRUE){
    xyz <- OccExp[,c(5,6,3)]
    z <- ggplot2::ggplot() +
      ggplot2::geom_tile(data=xyz, ggplot2::aes(x=Longitude, y=Latitude, fill=Occupancy))+
      ggplot2::labs(x = "Longitude",y = "Latitude", fill = expression(atop("",atop(textstyle("Occupancy"),atop(textstyle("(counts"%*%"area"^-1*")"))))))+
      ggplot2::coord_sf(xlim = c(min(xyz$Longitude), max(xyz$Longitude)), ylim = c(min(xyz$Latitude), max(xyz$Latitude)))+
      ggplot2::theme_minimal()+
      ggplot2::scale_fill_gradientn(colours = c("blue", "light blue","red"))+
      ggplot2::borders("world", colour="gray50", fill="gray50", xlim = c(min(xyz$Longitude), max(xyz$Longitude)), ylim = c(min(xyz$Latitude), max(xyz$Latitude)))
    plot(z)
  }

  if (plot==TRUE){
    Occmin <- min(OccExp[which(OccExp$Occupancy!=0),3]) #what is the min occ value when occ > 0 (min occupancy of occupied cells)
    bw <- log(max(OccExp$Occupancy)/Occmin)/log(nb)
    freq <- rep(0, nb+1)
    for(i in 1:nrow(OccExp)){
      if (OccExp[i,3]>0){
        b <- floor(log(OccExp[i,3]/Occmin)/log(bw) + 0.5) + 1
        freq[b] <- freq[b] + 1 #hist[k] in python = freq[b] here
      }
      else (freq[1] <- freq[1] + 1)
    }
    freq <- freq[freq>0]
    norm<-sum(freq)
    xs<-ys<-rep(0, length(freq))
    i<-1
    for (i in 1:length(freq)){
      ys[i]<-freq[i]/(norm*Occmin*((bw^((i-1)+0.5))-(bw^((i-1)-0.5))))
      xs[i]<-Occmin*(bw^(i-1))
    }
    plot(xs,ys,log="xy", xlim=c(Occmin/2,max(OccExp$Occupancy)*2), type="l", col="black", ylab="pdf",xlab=expression('Occupancy (km'^'-2'*')'))
    points(xs,ys,col="black", pch=19)
  }
}

# test<-data.frame("Ref"=c(123,123,123), "Long"=c(-179.8,-179.8,-178.9,-179.2,-179.2,-178.2 ), "Lat"=c(-89.9,-88.9,-89.9,-89.9,-88.9,-89.9))
# species_df<-test
# species_df
