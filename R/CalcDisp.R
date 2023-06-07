#' Calculate displacements
#'
#' This function allows you to calculate the displacement distances travelled by individuals over set time windows.
#'
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each individual (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format,
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{tracks}}.
#' @param min_hr Minimum number of hours to consider for calculations. Default is 24 hours.
#' @param max_hr Maximum number of hours to consider for calculations. Default is 240 hours.
#' @param interval_hr Time interval (in hours) used to set intervals between min_hr and max_hr. Default is 24 hours.
#' @param range_hr Range (in hours) converts interval_hr into a time window (interval_hr +/-  range_hr) so the
#' code can identify location estimates that are close to, but not exactly separated by the interval_hr input value.
#' If multiple location estimates fall within this time window the location estimate closest to the interval_hr input value
#' will be used for calculations. For example, if interval_hr = 24 and range = 6, the algorithm will search for
#' locations spaced 18 to 32 hours apart. Default is 6.
#' @return A list containing the displacements in km recorded for each time window. Each list element corresponds with the time
#' windows set (i.e., the first list element is the first time window).
#' @examples CalcDisp(tracks)
#' @examples CalcDisp(tracks, min_hr=24, max_hr=240, interval_hr=24, range_hr=6)
#' @export

CalcDisp<-function(species_df,min_hr=24,max_hr=240,interval_hr=24,range_hr=6){

  min_hr<-min_hr*(60*60) # convert hours (input) to seconds
  max_hr<-max_hr*(60*60) # convert hours (input) to seconds
  interval_hr<-interval_hr*(60*60) # convert hours (input) to seconds
  range_hr<-range_hr*(60*60) # convert hours (input) to seconds

  MydistHaversine <- function(lon1, lat1, lon2, lat2) {
    radlat1 = rad * lat1
    radlat2 = rad * lat2
    dlat = radlat2 - radlat1
    dlon = rad * (lon2 - lon1)
    a= (sin(dlat/2)^2) + cos(radlat1)*cos(radlat2)*(sin(dlon/2)^2)
    a=2*asin(sqrt(a))
    return(a*Radius)
  }

  Radius <- 6371 # Earth Radius in km (disp are in km)
  rad = 3.141592653589793/180 # Python has more digits of pi than R, so value pasted here instead of "pi"
  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  MyTime <- c(seq(min_hr,max_hr,interval_hr))
  MyList <- list()
  distmin <- 1e-3 # Prevents displacements of 0
  for(d in 1:length(MyTime)){
    myrow <- 1
    MyDistance <- c()
    for (i in 1:length(species_index)){ # for all individuals
      for(j in 1:length((species_index[[i]]))){ # for each location
        Jumpj <-  which(species_df[species_index[[i]],4] >= species_df[species_index[[i]][j],4] + MyTime[d] - range_hr & species_df[species_index[[i]],4]
                        <= species_df[species_index[[i]][j],4] + MyTime[d] + range_hr) # which locations of animal i are within the time range?
        if(length(Jumpj) == 1){ # If there is only 1 "Jumpj" for an animal (i.e., only 2 points separated by this time period), calculate distance
          dist <- MydistHaversine(as.numeric(species_df[species_index[[i]][j],2]),as.numeric(species_df[species_index[[i]][j],3]),
                                  as.numeric(species_df[species_index[[i]][Jumpj],2]),as.numeric(species_df[species_index[[i]][Jumpj],3]))
          if (dist > distmin){ # If distance is greater than min distance (1m), record distance
            MyDistance[myrow] <- dist
            myrow <- myrow + 1
          }
        }
        else if(length(Jumpj) > 1){ #If Jumpj is >1 (i.e., there are multiple points within a time period), choose jumpj closest to time period (absolute value) and calculate distance
          checkJump <- c()
          for (r in 1:length(Jumpj)) {
            checkJump[r] <- abs(as.numeric(species_df[species_index[[i]][j],4]) - (as.numeric(species_df[species_index[[i]][Jumpj[r]],4]) - MyTime[d])) # What is the time period between location j and the jumpJ locations
          }
          mymin <- which(checkJump == min(checkJump))
          if (length(mymin)>1){ # If two points were separated by the exact same time, randomly pick one to calculate the distances as they are both valid displacements
            mymin<-sample(mymin,1)
          }
          dist <- MydistHaversine(as.numeric(species_df[species_index[[i]][j],2]),as.numeric(species_df[species_index[[i]][j],3]),
                                  as.numeric(species_df[species_index[[i]][Jumpj[mymin]],2]),as.numeric(species_df[species_index[[i]][Jumpj[mymin]],3])) # Record distance between location j and the closest location to MyTime[d]
          if (dist > distmin) {
            MyDistance[myrow] <- dist
            myrow <- myrow + 1
          }
        }
      }
    }
    if (is.null(MyDistance)){
      MyList[[d]] <- NULL
      print(paste0("0 displacements in ", MyTime[d]/(60*60), " +/- ", range_hr/(60*60), " hour(s)"))
    } else {
      MyList[[d]] <- MyDistance
      print(paste0(length(MyList[[d]])," displacements in ", MyTime[d]/(60*60), " +/- ", range_hr/(60*60), " hour(s)"))
      }
  }

  if (any(sapply(MyList, function(x) length(x)==0))==TRUE){
    warning("At least 1 of the displacement list elements is empty, which means that no location estimates were separated by at least 1 of the time windows supplied.
    To troubleshoot, review the list of displacements created with this function and update your time windows accordingly.")
  }
  displacements <- MyList
  return(displacements)
}
