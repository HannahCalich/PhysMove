#' Calculate turning angles from trajectories
#'
#' This function allows you to calculate turning angles between sets of three consecutive location estimates separated by set time window(s).
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{tracks}}.
#' @param min_hr Minimum number of hours to consider for calculations. Default is 24 hours (i.e., 1 day).
#' @param max_hr Maximum number of hours to consider for calculations. Default is 240 hours (i.e., 10 days).
#' @param interval_hr Time interval (in hours) used to set intervals between min_hr and max_hr. Default is 24 hours (i.e., 1 day).
#' @param range_hr Range (in hours) converts interval_hr into a time window (interval_hr +/-  range_hr) so the
#' code can identify location estimates that are close to, but not exactly separated by the interval_hr input value.
#' If multiple location estimates fall within this time window the location estimate closest to the interval_hr input value
#' will be used for calculations. For example, if interval_hr = 24 and range = 6, the algorithm will search for
#' locations spaced 18 to 32 hours apart. Default for range_hr is 6.
#' @param histPlot Plot a histogram showing the frequency of turning angles from all time windows combined (default) or
#' one specific time period. For example, histPlot=c(TRUE,1) to plot only the first time period.
#' Default is histPlot=c(TRUE, "all").
#' @return List of turning angles for each time window, the name of each list element corresponds with a time window in days. If histPlot = TRUE,
#' a histogram of results is created.
#' @examples TurningAngles(tracks)
#' @examples TurningAngles(tracks, min_hr=24, max_hr=240, interval_hr=24,range_hr=6, histPlot=c(FALSE, "all"))
#' @export

TurningAngles<-function(species_df, min_hr=24, max_hr=240, interval_hr=24, range_hr=6, histPlot=c(TRUE, "all")){

  min_hr <- min_hr*(60*60) # convert hours (input) to seconds
  max_hr <- max_hr*(60*60) # convert hours (input) to seconds
  interval_hr <- interval_hr*(60*60) # convert hours (input) to seconds
  range_hr <- range_hr*(60*60) # convert hours (input) to seconds

  MydistHaversine <- function(lon1, lat1, lon2, lat2) {
    radlat1 <- rad * lat1
    radlat2 <- rad * lat2
    dlat <- radlat2 - radlat1
    dlon <- rad * (lon2 - lon1)
    a <- (sin(dlat/2)^2) + cos(radlat1)*cos(radlat2)*(sin(dlon/2)^2)
    a <- 2*asin(sqrt(a))
    return(a*Radius)
  }

  Radius <- 6371 # Earth Radius in km (disp are in km)
  rad <- 3.141592653589793/180 # Python has more digits of pi than R, so value pasted here instead of "pi"
  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  MyTime <- c(seq(min_hr,max_hr,interval_hr)) #Time in seconds

  angleList <- list()
  bins <- 360 / 45
  Days <- MyTime/(24*60*60) #Time from seconds to days

  for (d in 1:length(MyTime)){
    angleList[[d]] <- 0 # dummy value to initialize the list of angles for each time period
    for (i in 1:length(species_index)){ # for each individual
      for (j in 1:length((species_index[[i]]))){ # for each tracked location
        # Find locations separated by MyTime[d]
        Jumpj <-  which(species_df[species_index[[i]],4] >= species_df[species_index[[i]][j],4] + MyTime[d] - range_hr & species_df[species_index[[i]],4] <= species_df[species_index[[i]][j],4] + MyTime[d] + range_hr)

        if(length(Jumpj) > 0 ){
          if(length(Jumpj) == 1){ # If only one jump is found, calculate distance
            J1 <- Jumpj
          } else {
            checkJump <- c()
            for (r in 1:length(Jumpj)){
              checkJump[r] <- abs(as.numeric(species_df[species_index[[i]][Jumpj[r]],4]) - as.numeric(species_df[species_index[[i]][j],4]) - MyTime[d])
            }
            mymin <- which(checkJump == min(checkJump))
            J1 <- Jumpj[mymin[1]] # mymin[1] to account for cases where there are two jumps where the time difference is identical
          }
          Jumpj <-  which(species_df[species_index[[i]],4] >= species_df[species_index[[i]][J1],4] + MyTime[d] - range_hr & species_df[species_index[[i]],4] <= species_df[species_index[[i]][J1],4] + MyTime[d] + range_hr)
          # If only one jump is found, calculate distance
          if(length(Jumpj) > 0 ){
            if(length(Jumpj) == 1){
              J2 <- Jumpj
            } else {
              checkJump <- c()
              for (r in 1:length(Jumpj)){
                checkJump[r] <- abs(as.numeric(species_df[species_index[[i]][Jumpj[r]],4]) - as.numeric(species_df[species_index[[i]][j],4]) - MyTime[d])
              }
              mymin <- which(checkJump == min(checkJump))
              J2 <- Jumpj[mymin[1]] # mymin[1] to account for cases where there are two jumps where the time difference is identical
            }

            ### Now we need to calculate the angles at J1 (i.e., between J and J2)
            # First we calculate the scalar for each component of the vectors between J and J1 (let's call it 'ax' and 'ay'), and J1 and J2 (let's call it 'bx' and 'by')
            ax <- MydistHaversine(species_df[species_index[[i]][j],2], species_df[species_index[[i]][j],3], species_df[species_index[[i]][J1],2], species_df[species_index[[i]][j],3])
            ay <- MydistHaversine(species_df[species_index[[i]][j],2], species_df[species_index[[i]][j],3], species_df[species_index[[i]][j],2], species_df[species_index[[i]][J1],3])
            bx <- MydistHaversine(species_df[species_index[[i]][J1],2], species_df[species_index[[i]][J1],3], species_df[species_index[[i]][J2],2], species_df[species_index[[i]][J1],3])
            by <- MydistHaversine(species_df[species_index[[i]][J1],2], species_df[species_index[[i]][J1],3], species_df[species_index[[i]][J1],2], species_df[species_index[[i]][J2],3])

            # Remember to correct direction in longitude and latitude (i.e., W-E, N-S) # but account for potential movement between longitudes -180 and 180 (e.g., going from -180 to -182, which is represented as 178)
            if(species_df[species_index[[i]][J1],2] - species_df[species_index[[i]][j],2] > 180) { ax = -ax }
            if(species_df[species_index[[i]][J1],2] - species_df[species_index[[i]][j],2] < 0) { ax = -ax }
            if(species_df[species_index[[i]][J1],3] - species_df[species_index[[i]][j],3] < 0) { ay = -ay }
            if(species_df[species_index[[i]][J2],2] - species_df[species_index[[i]][J1],2] < 0 & species_df[species_index[[i]][J2],2] - species_df[species_index[[i]][J1],2] > -180){ bx = -bx }
            if(species_df[species_index[[i]][J2],3] - species_df[species_index[[i]][J1],3] < 0) { by = -by }

            # Now compute the scalar product and vector product, to get the angle
            # if((ax*bx + ay*by) == 0) { ### prevent the case where tangent is infinite (when cosinus is zero) i.e., the individual did not move in one of the components (J = J1 or J1 = J2)
            #   if((ax*by - ay*bx) > 0){
            #     angle <- pi/2
            #     angleList[[d]] <- append(angleList[[d]], angle)
            #   } else if ((ax*by - ay*bx) < 0){
            #     angle <- -pi/2
            #     angleList[[d]] <- append(angleList[[d]], angle)
            #   }
            # } else {
              # angle <- atan((ax*by - ay*bx) / (ax*bx + ay*by)) ### in radians
              angle <- atan2((ax*by - ay*bx) , (ax*bx + ay*by)) ### in radians
              # if(angle > 0){
              #   if((ax*bx + ay*by) < 0) {
              #     angle <- angle - pi
              #   }
              # } else if(angle <= 0){
              #   if((ax*bx + ay*by) < 0) {
              #     angle <- angle + pi
              #   }
              # }
              angleList[[d]] <- append(angleList[[d]], angle)
            # }
          }
        }
      }
    }
    angleList[[d]] <- angleList[[d]][-1] # Remove dummy value from start of list for each individual
    angleList[[d]] <- angleList[[d]] / rad # Now transform all the angles from radians to degrees
    print(paste0(length(angleList[[d]])," angles in ", MyTime[d]/(60*60), " +/- ", range_hr/(60*60), " hour(s)"))
  }
  names(angleList) <- round(Days, 3)

  if (any(sapply(angleList, function(x) length(x)==0))==TRUE){
    warning("At least 1 of the angle list elements is empty, which means that no location estimates were separated \n  by at least 1 of the time windows supplied. Blank list elements are excluded from plots.")
  }

  if (histPlot[1]==TRUE){ # Histogram of all angles combined
    angleList <- angleList[lengths(angleList)>0] # Remove list elements with no data
    if (histPlot[2]=="all"){
      angles.df <- as.data.frame(unlist(angleList))
      names(angles.df) <- "Angles"
      h <- hist(angles.df$Angles, plot = FALSE, breaks = seq(-180, 180, bins)) # Plot all angels for all time periods from all individuals
    } else {
      angles.df <- as.data.frame(unlist(angleList[[histPlot[2]]]))
      names(angles.df) <- "Angles"
      h <- hist(angles.df$Angles, plot = FALSE, breaks = seq(-180, 180, bins)) # Plot all angels for selected time periods from all individuals
    }
    xlabels <- c("-180", "", "-120",  "",  "-60",  "",  "0", "",  "60", "", "120",  "",  "180")
    hist_plot <- ggplot2::ggplot(angles.df, ggplot2::aes(Angles))+
      ggplot2::geom_histogram(breaks=h$breaks, color="black", fill="darkgrey")+
      ggplot2::scale_x_continuous("Turning Angles", breaks=seq(-180,180,30), labels=xlabels)+
      ggplot2::labs(y="Frequency")+
      ggplot2::theme_classic(base_size=18)
    plot(hist_plot)
  }
  return(angleList)
}
