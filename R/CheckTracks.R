#' Check data format
#'
#' This function is used to check the format of your telemetry data prior to running PhysMove metrics
#'
#' @param species_df A data frame containing location data in rows. Columns must have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each individual in numeric format (e.g., each track's unique satellite tag ID number),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format,
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#'
#' @return If data are formatted correctly the function will output "Your data are formatted correctly", else you will receive an error message describing the problem
#' @examples FitDist(species_df)
#' @export

CheckTracks <- function (species_df) {

  error_count <- 0
  if (!(is(species_df, "data.frame"))) {
    warning("Input data must be a data frame")
    error_count <- error_count+1
  }

  if (any(colnames(species_df)!=c("ref","lon","lat","day"))) {
    warning("Collumn names are either incorrect or in the wrong order. Column names must be: ",
            "ref, ", "lon, ", "lat, " ,"day", " (in that order)")
    error_count <- error_count+1
  }

  if (!(is(species_df$ref, "numeric"))) {
    warning("ref column must be numeric format")
    error_count <- error_count+1
  }

  if (!(is(species_df$lon, "numeric"))) {
    warning("lon column must be numeric format")
    error_count <- error_count+1
  }

  if (max(species_df$lon)>180) {
    warning("longitude value greater than 180")
    error_count <- error_count+1
  }

  if (min(species_df$lon)< -180) {
    warning("longitude value less than -180")
    error_count <- error_count+1
  }

  if (!(is(species_df$lat, "numeric"))) {
    warning("lat column must be numeric format")
    error_count <- error_count+1
  }

  if (max(species_df$lat)>90) {
    warning("latutide value greater than 90")
    error_count <- error_count+1
  }

  if (min(species_df$lat)< -90) {
    warning("latutide value less than -90")
    error_count <- error_count+1
  }

  if (!(is(species_df$day,"POSIXct"))) {
    warning("day column must be POSIXct format")
    error_count <- error_count+1
  }

  out <- tryCatch({
      test <- as.POSIXct(species_df$day, format='%Y-%m-%m %h:%m:s')
    },
    error=function(cond) {
      warning("day column is not formatted correctly, format must = '%Y-%m-%m %h:%m:s' ")
    })

  if (all(test != species_df$day)) {
      return(out)
      error_count <- error_count+1
    }

  if (error_count == 0)
    message("Your data are formatted correctly")

  if (error_count > 0){
    message(paste0("Please review formatting requirements: "))
  }
}
