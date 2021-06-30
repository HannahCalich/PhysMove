#' Calculate Displacements
#'
#' This function allows you to calculate the displacement distances traveled by individuals over set time windows.
#' @param species_df A data frame containing location data (rows) and columns with the following headers: "ref", "lon", "lat", "day". "ref" is the unique id number for each animal
#'      (e.g., their satellite tag number; integer format). "lon" and "lat" are the longitude and latitide of each position estimate in decimal degrees (numeric format). "day"
#'      is the datetime stamp for each location estimate (POSIXct format following yyyy-mm-dd hh:mm:ss). See XXX data frame for an example.
#' @param min_hr Minimum number of hours to consider for displacement calculations. Default is 24 hours.
#' @param max_hr Maximum number of hours to consider for displacement calculations (default is 240 hours)
#' @param interval_hr Time interval (in hours) used to identify time period intervals between min_hr and max_hr (default is 24 hours)
#' @param range_hr Range (in hours) applied to interval_hr. This value helps the algorithm identify location estimates that are close to, but not exactly separated by the interval_hr. If multiple location estimates fall within this range the location estimate closest to the interval_hr will be used for calculations.
#' @return A list containing the displacements (distances traveled in km) recorded for each time period
#' @examples CalculateDisplacements(species_df)
#' @examples CalculateDisplacements(species_df, min_hr=24, max_hr=240, interval_hr=24, range_hr=6)
#' @export

CalcDisp<-function(species_df,min_hr=24,max_hr=240,interval_hr=24,range_hr=6){

  min_hr<-min_hr*(60*60) #convert hours (input) to seconds
  max_hr<-max_hr*(60*60) #convert hours (input) to seconds
  interval_hr<-interval_hr*(60*60) #convert hours (input) to seconds
  range_hr<-range_hr*(60*60) #convert hours (input) to seconds

  MydistHaversine <- function(lon1, lat1, lon2, lat2) {
    radlat1 = rad * lat1
    radlat2 = rad * lat2
    dlat = radlat2 - radlat1
    dlon = rad * (lon2 - lon1)
    a= (sin(dlat/2)^2) + cos(radlat1)*cos(radlat2)*(sin(dlon/2)^2)
    a=2*asin(sqrt(a))
    return(a*Radius)
  }

  Radius <- 6371 #Earth Radius in km (disp are in km)
  rad = 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  MyTime <- c(seq(min_hr,max_hr,interval_hr))
  MyList <- list()
  distmin <- 1e-3 #Prevents displacements of 0
  for(d in 1:length(MyTime)){
    myrow <- 1
    MyDistance <- c()
    for (i in 1:length(species_index)){ #for all individuals
      for(j in 1:length((species_index[[i]]))){ #for each location
        Jumpj <-  which(species_df[species_index[[i]],4] >= species_df[species_index[[i]][j],4] + MyTime[d] - range_hr & species_df[species_index[[i]],4]
                        <= species_df[species_index[[i]][j],4] + MyTime[d] + range_hr) #Which locations of animal i are within the time range?
        if(length(Jumpj) == 1){ #If there is only 1 "Jumpj" for a shark (i.e., only 2 points separated by this time period), calculate distance
          dist <- MydistHaversine(as.numeric(species_df[species_index[[i]][j],2]),as.numeric(species_df[species_index[[i]][j],3]),
                                  as.numeric(species_df[species_index[[i]][Jumpj],2]),as.numeric(species_df[species_index[[i]][Jumpj],3]))
          if (dist > distmin){ #If distance is greater than min distance (1m), record distance
            MyDistance[myrow] <- dist
            myrow <- myrow + 1
          }
        }
        else if(length(Jumpj) > 1){ #If Jumpj is >1 (i.e., there are multiple points within a time period), choose jumpj closest to time period (absolute value) and calculate distance
          checkJump <- c()
          for (r in 1:length(Jumpj)) {
            checkJump[r] <- abs(as.numeric(species_df[species_index[[i]][j],4]) - (as.numeric(species_df[species_index[[i]][Jumpj[r]],4]) - MyTime[d])) #What is the time period between location j and the jumpJ locations
          }
          mymin <- which(checkJump == min(checkJump))
          if (length(mymin)>1){ #If two points were separated by the exact same time, randomly pick one to calcualte the distances as they are both valid displacements
            mymin<-sample(mymin,1)
          }
          dist <- MydistHaversine(as.numeric(species_df[species_index[[i]][j],2]),as.numeric(species_df[species_index[[i]][j],3]),
                                  as.numeric(species_df[species_index[[i]][Jumpj[mymin]],2]),as.numeric(species_df[species_index[[i]][Jumpj[mymin]],3])) #Record distance between location j and the closest location to MyTime[d]
          if (dist > distmin) {
            MyDistance[myrow] <- dist
            myrow <- myrow + 1
          }
        }
      }
    }
    MyList[[d]] <- MyDistance
    print(paste0(length(MyList[[d]])," Displacements in ", MyTime[d]/(60*60), " hour(s)"))
  }
  assign("Displacements", MyList, envir = .GlobalEnv)
  assign("TimeWindows",MyTime, envir = .GlobalEnv)
}
