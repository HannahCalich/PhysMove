#' Gyration Radius
#'
#' This function allows you to calculate the gyration radius of individual trajectories.
#' @param species_df A data frame containing location data (rows) and columns with the following headers: "ref", "lon", "lat", "day". "ref" is the unique id number for each animal
#'      (e.g., their satellite tag number; integer format). "lon" and "lat" are the longitude and latitide of each position estimate in decimal degrees (numeric format). "day"
#'      is the datetime stamp for each location estimate (POSIXct format following yyyy-mm-dd hh:mm:ss). See XXX data frame for an example.
#' @param plot Line plot illustrating the probability density function of gyration radii scores.  Default is TRUE.
#' @param nb Number of bins, used to determine the size of the bins used to calculate predictability frequencies for plots. Default is 40.
#' @param map Create a map illustrating gyration radii. Default is TRUE.
#' @param Colours Colours for points and gyration radius, respectively. Default is c("Black","Red).
#' @return Vector containing gyration radius values for each trajectory, line plot, and/or map (if desired).
#' @examples
#' rG(species_df)
#' rG(species_df,plot=TRUE,nb, map=TRUE)
#' @export

GyrationRad <- function (species_df, map=TRUE, Colours=c("Black","Red"), plot=FALSE, nb=15){

  MydistHaversine <- function(lon1, lat1, lon2, lat2) {
    radlat1 = rad * lat1
    radlat2 = rad * lat2
    dlat = radlat2 - radlat1
    dlon = rad * (lon2 - lon1)
    a = (sin(dlat/2)^2) + cos(radlat1)*cos(radlat2)*(sin(dlon/2)^2)
    a = 2*asin(sqrt(a))
    return(a*Radius)
  }

  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x}) #Convert to Ana terms
  Radius <- 6371 #Earth Radius in km (disp are in km)
  rad <- 3.141592653589793/180 #to convert degrees to radians
  deg <- 180/3.141592653589793 #to convert radians to degrees

  species_df$x <- Radius*sin((90-species_df$lat)*rad)*cos(species_df$lon*rad)
  species_df$y <- Radius*sin((90-species_df$lat)*rad)*sin(species_df$lon*rad)
  species_df$z <- Radius*sin(species_df$lat*rad)

  species_df$rG <- species_df$Loc.dist.from.avg <- species_df$lat.avg.deg <-species_df$hyp <- species_df$lon.avg.deg <- species_df$z.avg.rad <- species_df$y.avg.rad <- species_df$x.avg.rad <-c(0)

  # Calculate average of x,y,z per shark
  for (i in 1:length(species_index)){ #for each individual
    species_df[species_index[[i]],8] <- sum(species_df[species_index[[i]],"x"])/length(species_index[[i]]) #average x in rad
    species_df[species_index[[i]],9] <- sum(species_df[species_index[[i]],"y"])/length(species_index[[i]]) #average y in rad
    species_df[species_index[[i]],10] <- sum(species_df[species_index[[i]],"z"])/length(species_index[[i]]) #average z in rad
  }

  # Convert average x,y,z to lat/lon in degrees
  species_df[,11] <- atan2(species_df$y.avg.rad,species_df$x.avg.rad)*deg #Lon = atan(y.avg,x.avg)
  species_df[,12] <- sqrt(species_df$x.avg.rad*species_df$x.avg.rad + species_df$y.avg.rad*species_df$y.avg.rad) #Hyp = sqrt(x.avg*x.avg + y.avg*y.avg)
  species_df[,13] <- atan2(species_df$z.avg.rad, species_df$hyp)*deg #Lat = atan(z.avg,hyp)

  # 4) Gyration radius in KM (because mydistHaversine in km)
  # Full rG equation: rG = 1/N*(sqrt(sum((distance between observed loc and avg loc)^2))
  for (i in 1:length(species_index)) { #for each shark
    for (j in 1:length(species_index[[i]])) { #for each location
      species_df[species_index[[i]][j],14]<- MydistHaversine(species_df[species_index[[i]][j],2],species_df[species_index[[i]][j],3], species_df[species_index[[i]][j],11],species_df[species_index[[i]][j],13]) #distance in KM between observed and avg locations
    }
    species_df[species_index[[i]],15] <- sqrt((sum(species_df[species_index[[i]],14]^2))/(length(species_index[[i]]))) #This is consistent with ArcGIS 1SD Standard Distance tool
  }

  MyrG <- unique(species_df[,c(1,11,13,15)])
  assign("GyrationRadius", MyrG, envir = .GlobalEnv)

  if (map==TRUE){
    angle <- seq(1,360,1)
    Radius <- 6371 #Earth Radius in km (disp are in km)
    rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
    circles <- as.data.frame(matrix(0, ncol = 3, nrow = length(angle)*length(MyrG$ref)))
    names(circles)<-c("Ref","lat","long")
    i<-j<-k<-1
    for (i in 1:nrow(MyrG)){ #for each mean location
      d <- MyrG$rG[i]
      lat1 <- MyrG$lat.avg.deg[i]*(rad) # convert to radians
      long1 <- MyrG$lon.avg.deg[i]*(rad) # convert to radians
      circles$Ref[k:(k+(length(angle)-1))]<-MyrG$ref[i]
      for (j in 1:length(angle)){ #calculate lat/long locations of points making a cirlce (360deg) around mean location
        a <- angle[j]*(rad)
        lat2 <- asin(sin(lat1)*cos(d/Radius) + cos(lat1)*sin(d/Radius)*cos(a))
        circles$lat[j+(k-1)] <- lat2/rad # Calculate new lat in radians based on angle and distance, convert to deg
        circles$long[j+(k-1)] <- (long1 + atan2(sin(a)*sin(d/Radius)*cos(lat1),cos(d/Radius)-sin(lat1)*sin(lat2)))/rad # Calculate new long in radians based on angle and distance, convert to deg
      }
    k <- k+length(angle)
    }
    xyz <- MyrG[,c(3,2,4)]
    z <- ggplot2::ggplot() +
      ggplot2::geom_point(data = xyz, ggplot2::aes(lon.avg.deg, lat.avg.deg), size=2, color = Colours[1])+
      ggplot2::coord_sf(xlim = c(min(circles$long), max(circles$long)), ylim = c(min(circles$lat), max(circles$lat)))+
      ggplot2::theme_minimal()+
      ggplot2::geom_polygon(data = circles, ggplot2::aes(long, lat, group = Ref), color = Colours[2], alpha=0)+
      ggplot2::labs(x="Longitude", y="Latitude")+
      ggplot2::borders("world", colour="gray50", fill="gray50")
    print(z)
  }

  if (plot==TRUE){
    rGmin <- min(MyrG$rG)
    bw <- (max(MyrG$rG)-rGmin)/nb
    freq <- xs <- rep(0, nb+1)
    for(i in 1:nrow(MyrG)){
      b <- floor((MyrG[i,4]-rGmin)/bw + 0.5)+1 #otherwise floor goes to 0, which isn't recorded in freq
      freq[b] <- freq[b] + 1 #hist[k] in python = freq[b] here
    }
    for(i in 1:(nb+1)){
      xs[i] <- rGmin+(i-1)*bw #bins for x axis on plot
    }
    lenrG = nrow(MyrG)
    for (i in 1:length(freq)){
      freq[i]<-freq[i]/(bw*lenrG)
    }
    # Plot RMS of displacements, and mean displacements on log-log scale
    plot(xs,freq,type="l",ylab="pdf", xlab=expression('r'[G]*' (km)'))
    points(xs,freq,col="black", pch=19)
  }

}
