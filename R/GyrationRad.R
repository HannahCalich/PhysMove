#' Gyration Radius
#'
#' This function allows you to calculate the gyration radius of individual trajectories.
#' A pdf plot of the gyration radius values can be created with the \code{\link{plotPDF}} function.
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format,
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{tracks}}.
#' @param map Create a map illustrating the gyration radius of each trajectory. Default is TRUE.
#' @param mapCol Colours for points and gyration radii on map, respectively. Default is c("Black","Red).
#' @return Gyration radius values for each trajectory. If map=TRUE a map of the gyration radius results is created.
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#'
#' gyrationRad(tracks, map=TRUE, mapCol=c("Black","Red"))
#' }
#' @export

gyrationRad <- function (species_df, map=TRUE, mapCol=c("Black","Red")){

  MydistHaversine <- function(lon1, lat1, lon2, lat2) {
    radlat1 <- rad * lat1
    radlat2 <- rad * lat2
    dlat <- radlat2 - radlat1
    dlon <- rad * (lon2 - lon1)
    a <- (sin(dlat/2)^2) + cos(radlat1)*cos(radlat2)*(sin(dlon/2)^2)
    a <- 2*asin(sqrt(a))
    return(a*Radius)
  }

  if(length(mapCol)==1){
    message("Only one colour has been included in 'mapCol' so only the average locaton of each track will be displayed.
    To also display each track's gyration radius please include a second colour following the format, mapCol=c('black','red')")
  }

  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  Radius <- 6371 # Earth Radius in km (disp are in km)
  rad <- 3.141592653589793/180 # Convert degrees to radians
  deg <- 180/3.141592653589793 # Convert radians to degrees

  species_df$x <- Radius*sin((90-species_df$lat)*rad)*cos(species_df$lon*rad)
  species_df$y <- Radius*sin((90-species_df$lat)*rad)*sin(species_df$lon*rad)
  species_df$z <- Radius*sin(species_df$lat*rad)

  species_df$rG <- species_df$Loc.dist.from.avg <- species_df$lat.avg.deg <-species_df$hyp <- species_df$lon.avg.deg <- species_df$z.avg.rad <- species_df$y.avg.rad <- species_df$x.avg.rad <-c(0)

  # Calculate average of x,y,z per shark
  for (i in 1:length(species_index)){ #for each individual
    species_df[species_index[[i]],8] <- sum(species_df[species_index[[i]],"x"])/length(species_index[[i]]) # Average x in rad
    species_df[species_index[[i]],9] <- sum(species_df[species_index[[i]],"y"])/length(species_index[[i]]) # Average y in rad
    species_df[species_index[[i]],10] <- sum(species_df[species_index[[i]],"z"])/length(species_index[[i]]) # Average z in rad
  }

  # Convert average x,y,z to lat/lon in degrees
  species_df[,11] <- atan2(species_df$y.avg.rad,species_df$x.avg.rad)*deg # Lon = atan(y.avg,x.avg)
  species_df[,12] <- sqrt(species_df$x.avg.rad*species_df$x.avg.rad + species_df$y.avg.rad*species_df$y.avg.rad) # Hyp = sqrt(x.avg*x.avg + y.avg*y.avg)
  species_df[,13] <- atan2(species_df$z.avg.rad, species_df$hyp)*deg # Lat = atan(z.avg,hyp)

  # Calculate Gyration radius in KM (mydistHaversine in km)
  # Full rG equation: rG = 1/N*(sqrt(sum((distance between observed loc and avg loc)^2))
  for (i in 1:length(species_index)) { # for each individual
    for (j in 1:length(species_index[[i]])) { # for each location
      species_df[species_index[[i]][j],14]<- MydistHaversine(species_df[species_index[[i]][j],2],species_df[species_index[[i]][j],3], species_df[species_index[[i]][j],11],species_df[species_index[[i]][j],13]) # Distance in KM between observed and avg locations
    }
    species_df[species_index[[i]],15] <- sqrt((sum(species_df[species_index[[i]],14]^2))/(length(species_index[[i]])))
  }

  MyrG <- unique(species_df[,c(1,11,13,15)])

  if (map==TRUE){
    angle <- seq(1,360,1)
    circles <- as.data.frame(matrix(0, ncol = 3, nrow = length(angle)*length(MyrG$ref)))
    names(circles)<-c("Ref","lat","long")
    k <- 1 # do not comment, needed in loop
    for (i in 1:nrow(MyrG)){ # For each mean location
      d <- MyrG$rG[i]
      lat1 <- MyrG$lat.avg.deg[i]*(rad)
      long1 <- MyrG$lon.avg.deg[i]*(rad)
      circles$Ref[k:(k+(length(angle)-1))]<-MyrG$ref[i]
      for (j in 1:length(angle)){ # calculate lat/long locations of points making a circle (360deg) around mean location
        a <- angle[j]*(rad)
        lat2 <- asin(sin(lat1)*cos(d/Radius) + cos(lat1)*sin(d/Radius)*cos(a))
        circles$lat[j+(k-1)] <- lat2/rad # Calculate new lat in rad based on angle and distance, convert to deg
        circles$long[j+(k-1)] <- (long1 + atan2(sin(a)*sin(d/Radius)*cos(lat1),cos(d/Radius)-sin(lat1)*sin(lat2)))/rad # Calculate new long in radians based on angle and distance, convert to deg
      }
    k <- k+length(angle)
    }
    xyz <- MyrG[,c(3,2,4)]
    z <- ggplot2::ggplot() +
      ggplot2::geom_point(data = xyz, ggplot2::aes(.data$lon.avg.deg, .data$lat.avg.deg), size=2, color = mapCol[1])+
      ggplot2::coord_sf(xlim = c(min(circles$long), max(circles$long)), ylim = c(min(circles$lat), max(circles$lat)), datum = sf::st_crs(4326))+
      ggplot2::theme_minimal(base_size = 12)+
      ggplot2::geom_polygon(data = circles, ggplot2::aes(.data$long, .data$lat, group = .data$Ref), color = mapCol[2], alpha=0)+
      ggplot2::labs(x="Longitude", y="Latitude")

    tryCatch({ # This prevents the plot from crashing if the mapped area does not overlap with the world polygon (e.g., for pelagic species)
      z <- z +
        ggplot2::borders("world", colour="gray50", fill="gray50", xlim = c(min(circles$long), max(circles$long)), ylim = c(min(circles$lat), max(circles$lat)))
    }, error = function(e){message('Please note: World polygon does not overlap with gyration radius results')})

    print(z)
  }
  names(MyrG) <- c("ref","avg long", "avg lat", "rG (km)")
  return(MyrG)
}
